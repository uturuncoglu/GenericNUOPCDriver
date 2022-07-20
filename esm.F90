!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
#include "macros.inc"
module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS                      => SetServices, &
    Driver_label_SetRunSequence   => label_SetRunSequence
  use NUOPC_Connector, only: cplSS => SetServices

  include 'comps.inc'

  implicit none

  private

  public SetServices

  ! type for storing component specific SetServices methods
  type ssPtr
     procedure(SetServices), pointer, nopass :: s_ptr => null()
  end type ssPtr

  ! clock and alarm options
  character(len=*), private, parameter :: &
     optNONE           = "none"      , &
     optNever          = "never"     , &
     optNSteps         = "nsteps"    , &
     optNSeconds       = "nseconds"  , &
     optNMinutes       = "nminutes"  , &
     optNHours         = "nhours"    , &
     optNDays          = "ndays"     , &
     optNMonths        = "nmonths"   , &
     optNYears         = "nyears"    , &
     optMonthly        = "monthly"   , &
     optYearly         = "yearly"    , &
     optDate           = "date"

  character(len=*),parameter :: u_FILE_u = &
    __FILE__

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    type(ESMF_Config)    :: config

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

   call NUOPC_CompSpecialize(driver, &
     specLabel=Driver_label_SetRunSequence, specRoutine=SetRunSequence, &
     rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set driver config
    call ESMF_GridCompSet(driver, configFile="nuopc.configure", rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: refTime
    type(ESMF_Time)               :: currTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: clock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Config)             :: config 
    type(NUOPC_FreeFormat)        :: ff
    type(ESMF_Alarm)        :: alarm_stop          ! alarm

    integer :: n, m
    integer :: compCount
    integer :: start_ymd           ! Start date (YYYYMMDD)
    integer :: start_tod           ! Start time of day (seconds)
    integer :: ref_ymd             ! Reference date (YYYYMMDD)
    integer :: ref_tod             ! Reference time of day (seconds)
    integer :: curr_ymd            ! Current ymd (YYYYMMDD)
    integer :: curr_tod            ! Current tod (seconds)
    integer :: stop_n              ! Number until stop
    integer :: stop_ymd            ! Stop date (YYYYMMDD)
    integer :: stop_tod            ! Stop time-of-day
    integer                 :: yr, mon, day, sec   ! Year, month, day, secs as integers
    character(len=256) :: stop_option         ! Stop option units
    integer, allocatable :: petList(:)
    character(len=32), allocatable :: compLabels(:)
    character(len=32) :: srcCompLabel, dstCompLabel
    character(len=256) :: namestr, cvalue
    type(ssPtr), allocatable :: compSS(:)

    rc = ESMF_SUCCESS

    ! Obtain configuration
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Obtain driver attributes
    call ReadAttributes(driver, config, "CLOCK_attributes::", formatprint=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ReadAttributes(driver, config, "ALLCOMP_attributes::", formatprint=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Obtain component count
    compCount = ESMF_ConfigGetLen(config, label="component_list:", rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Obtain component labels
    if (.not. allocated(compLabels)) allocate(compLabels(compCount))
    call ESMF_ConfigGetAttribute(config, valueList=compLabels, label="component_list:", count=compCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Sort component list, order of list needs to match with auto-generated code (include files)
    call ESMF_UtilSort(compLabels, ESMF_SORTFLAG_ASCENDING, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Set SetServices call pointer, setSS is a macro defined in auto-generated macros.inc
    if (.not. allocated(compSS)) allocate(compSS(compCount))
    setSS()

    ! Loop over components and add them as component 
    do n = 1, compCount
       namestr = ESMF_UtilStringUpperCase(compLabels(n))

       ! Obtain number of task for each component
       ff = NUOPC_FreeFormatCreate(config, label=trim(namestr)//"_petlist:", rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call NUOPC_IngestPetList(petList, ff, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Add component
       call NUOPC_DriverAddComp(driver, trim(namestr), compSS(n)%s_ptr, petList=petList, config=config, comp=child, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Set verbosity to high
       call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Obtain component attributes
       call ReadAttributes(child, config, "ALLCOMP_attributes::", formatprint=.true., rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call ReadAttributes(child, config, trim(namestr)//"_attributes::", formatprint=.true., relaxedflag=.true., rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Clear memory
       deallocate(petList)
    end do 

    ! Loop over components and create connectors between them
    do n = 1, compCount
       srcCompLabel = ESMF_UtilStringUpperCase(compLabels(n))
       do m = 1, compCount
          dstCompLabel = ESMF_UtilStringUpperCase(compLabels(m))

          ! Skip if source and destination components are same
          if (trim(srcCompLabel) == trim(dstCompLabel)) cycle

          ! SetServices for n,m pair
          call NUOPC_DriverAddComp(driver, srcCompLabel=trim(srcCompLabel), &
            dstCompLabel=trim(dstCompLabel), compSetServicesRoutine=cplSS, comp=connector, rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return

          call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
       end do
    end do   

    ! Set driver clock start time 
    call NUOPC_CompAttributeGet(driver, name="start_ymd", value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) start_ymd

    call NUOPC_CompAttributeGet(driver, name="start_tod", value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) start_tod

    call esm_time_date2ymd(start_ymd, yr, mon, day)
    call ESMF_TimeSet(startTime, yy=yr, mm=mon, dd=day, s=start_tod, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set driver current time
    ! TODO: need to be modified for the restart cases
    curr_ymd = start_ymd
    curr_tod = start_tod
    call esm_time_date2ymd(curr_ymd, yr, mon, day)
    call ESMF_TimeSet(currTime, yy=yr, mm=mon, dd=day, s=curr_tod, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return    

    ! Set driver clock reference time - HARD-CODED TO START TIME
    ref_ymd = start_ymd
    ref_tod = start_tod
    call esm_time_date2ymd(ref_ymd, yr, mon, day)
    call ESMF_TimeSet(refTime, yy=yr, mm=mon, dd=day, s=ref_tod, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set driver clock timestep
    ! TODO: fixed to 3600
    call ESMF_TimeIntervalSet(timeStep, s=3600, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Create the clock
    clock = ESMF_ClockCreate(timeStep, startTime, refTime=refTime, name='Application Clock', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set driver clock stop time
    call NUOPC_CompAttributeGet(driver, name="stop_option", value=stop_option, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompAttributeGet(driver, name="stop_n", value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) stop_n

    call NUOPC_CompAttributeGet(driver, name="stop_ymd", value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) stop_ymd

    call NUOPC_CompAttributeGet(driver, name="stop_tod", value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) stop_tod

    if (stop_ymd < 0) then
       stop_ymd = 99990101
       stop_tod = 0
    endif

    call esm_time_alarmInit(clock, &
         alarm   = alarm_stop,           &
         option  = stop_option,          &
         opt_n   = stop_n,               &
         opt_ymd = stop_ymd,             &
         opt_tod = stop_tod,             &
         refTime = CurrTime,             &
         alarmname = 'alarm_stop', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_AlarmGet(alarm_stop, RingTime=StopTime, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(clock, StopTime=StopTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridCompSet(driver, clock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)          :: name
    type(ESMF_Config)               :: config
    type(NUOPC_FreeFormat)          :: runSeqFF

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! read free format run sequence from config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReadAttributes(gcomp, config, label, relaxedflag, formatprint, rc)

    use ESMF  , only : ESMF_GridComp, ESMF_Config, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use NUOPC , only : NUOPC_FreeFormatCreate, NUOPC_CompAttributeIngest
    use NUOPC , only : NUOPC_FreeFormatDestroy, NUOPC_FreeFormat

    ! input/output arguments
    type(ESMF_GridComp) , intent(inout)        :: gcomp
    type(ESMF_Config)   , intent(in)           :: config
    character(len=*)    , intent(in)           :: label
    logical             , intent(in), optional :: relaxedflag
    logical             , intent(in), optional :: formatprint
    integer             , intent(inout)        :: rc

    ! local variables
    type(NUOPC_FreeFormat)  :: attrFF
    character(len=*), parameter :: subname = "(esm.F90:ReadAttributes)"
    !-------------------------------------------

    rc = ESMF_SUCCESS

    if (present(relaxedflag)) then
       attrFF = NUOPC_FreeFormatCreate(config, label=trim(label), relaxedflag=.true., rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    else
       attrFF = NUOPC_FreeFormatCreate(config, label=trim(label), rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if

    call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine ReadAttributes

  !-----------------------------------------------------------------------------

  logical function ChkErr(rc, line, file)
    integer, intent(in) :: rc            !< return code to check
    integer, intent(in) :: line          !< Integer source line number
    character(len=*), intent(in) :: file !< User-provided source file name
    integer :: lrc
    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
      ChkErr = .true.
    endif
  end function ChkErr

  !-----------------------------------------------------------------------------

   subroutine esm_time_date2ymd (date, year, month, day)

   ! input/output variables
   integer, intent(in)  :: date             ! coded-date (yyyymmdd)
   integer, intent(out) :: year,month,day   ! calendar year,month,day

   ! local variables
   integer :: tdate   ! temporary date
   character(*),parameter :: subName = "(esm_time_date2ymd)"
   !-------------------------------------------------------------------------------

   tdate = abs(date)
   year = int(tdate/10000)
   if (date < 0) then
      year = -year
   end if
   month = int( mod(tdate,10000)/  100)
   day = mod(tdate,  100)

  end subroutine esm_time_date2ymd

  !-----------------------------------------------------------------------------

 subroutine esm_time_alarmInit( clock, alarm, option, &
      opt_n, opt_ymd, opt_tod, RefTime, alarmname, rc)

   ! Setup an alarm in a clock
   ! Notes: The ringtime sent to AlarmCreate MUST be the next alarm
   ! time.  If you send an arbitrary but proper ringtime from the
   ! past and the ring interval, the alarm will always go off on the
   ! next clock advance and this will cause serious problems.  Even
   ! if it makes sense to initialize an alarm with some reference
   ! time and the alarm interval, that reference time has to be
   ! advance forward to be >= the current time.  In the logic below
   ! we set an appropriate "NextAlarm" and then we make sure to
   ! advance it properly based on the ring interval.

   ! input/output variables
   type(ESMF_Clock)            , intent(inout) :: clock     ! clock
   type(ESMF_Alarm)            , intent(inout) :: alarm     ! alarm
   character(len=*)            , intent(in)    :: option    ! alarm option
   integer          , optional , intent(in)    :: opt_n     ! alarm freq
   integer          , optional , intent(in)    :: opt_ymd   ! alarm ymd
   integer          , optional , intent(in)    :: opt_tod   ! alarm tod (sec)
   type(ESMF_Time)  , optional , intent(in)    :: RefTime   ! ref time
   character(len=*) , optional , intent(in)    :: alarmname ! alarm name
   integer                     , intent(inout) :: rc        ! Return code

   ! local variables
   type(ESMF_Calendar)     :: cal              ! calendar
   integer                 :: lymd             ! local ymd
   integer                 :: ltod             ! local tod
   integer                 :: cyy,cmm,cdd,csec ! time info
   character(len=64)       :: lalarmname       ! local alarm name
   logical                 :: update_nextalarm ! update next alarm
   type(ESMF_Time)         :: CurrTime         ! Current Time
   type(ESMF_Time)         :: NextAlarm        ! Next restart alarm time
   type(ESMF_TimeInterval) :: AlarmInterval    ! Alarm interval
   integer                 :: sec
   character(len=*), parameter :: subname = '(med_time_alarmInit): '
   !-------------------------------------------------------------------------------

   rc = ESMF_SUCCESS

   lalarmname = 'alarm_unknown'
   if (present(alarmname)) lalarmname = trim(alarmname)
   ltod = 0
   if (present(opt_tod)) ltod = opt_tod
   lymd = -1
   if (present(opt_ymd)) lymd = opt_ymd

   call ESMF_ClockGet(clock, CurrTime=CurrTime, rc=rc)
   if (ChkErr(rc,__LINE__,u_FILE_u)) return

   call ESMF_TimeGet(CurrTime, yy=cyy, mm=cmm, dd=cdd, s=csec, rc=rc )
   if (ChkErr(rc,__LINE__,u_FILE_u)) return

   ! initial guess of next alarm, this will be updated below
   if (present(RefTime)) then
      NextAlarm = RefTime
   else
      NextAlarm = CurrTime
   endif

   ! Get calendar from clock
   call ESMF_ClockGet(clock, calendar=cal, rc=rc)
   if (ChkErr(rc,__LINE__,u_FILE_u)) return

   ! Error checks
   if (trim(option) == optdate) then
      if (.not. present(opt_ymd)) then
         call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_ymd', ESMF_LOGMSG_ERROR)
         rc = ESMF_FAILURE
         return
      end if
      if (lymd < 0 .or. ltod < 0) then
         call ESMF_LogWrite(subname//trim(option)//'opt_ymd, opt_tod invalid', ESMF_LOGMSG_ERROR)
         rc = ESMF_FAILURE
         return
      end if
   else if (trim(option) == optNSteps   .or. &
        trim(option) == optNSeconds .or. &
        trim(option) == optNMinutes .or. &
        trim(option) == optNHours   .or. &
        trim(option) == optNDays    .or. &
        trim(option) == optNMonths  .or. &
        trim(option) == optNYears) then
      if (.not.present(opt_n)) then
         call ESMF_LogWrite(subname//trim(option)//' requires opt_n', ESMF_LOGMSG_ERROR)
         rc = ESMF_FAILURE
         return
      end if
      if (opt_n <= 0) then
         call ESMF_LogWrite(subname//trim(option)//' invalid opt_n', ESMF_LOGMSG_ERROR)
         rc = ESMF_FAILURE
         return
      end if
   end if

   ! Determine inputs for call to create alarm
   selectcase (trim(option))

   case (optNONE)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=9999, mm=12, dd=1, s=0, calendar=cal, rc=rc )
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .false.

   case (optDate)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call esm_time_date2ymd(opt_ymd, cyy, cmm, cdd)

      call ESMF_TimeSet( NextAlarm, yy=cyy, mm=cmm, dd=cdd, s=ltod, calendar=cal, rc=rc )
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .false.

   case (optNever)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=9999, mm=12, dd=1, s=0, calendar=cal, rc=rc )
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .false.

   case (optNSteps)
      call ESMF_ClockGet(clock, TimeStep=AlarmInterval, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optNSeconds)
      call ESMF_TimeIntervalSet(AlarmInterval, s=1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optNMinutes)
      call ESMF_TimeIntervalSet(AlarmInterval, s=60, rc=rc)
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optNHours)
      call ESMF_TimeIntervalSet(AlarmInterval, s=3600, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optNDays)
      call ESMF_TimeIntervalSet(AlarmInterval, d=1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optNMonths)
      call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optMonthly)
      call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=cyy, mm=cmm, dd=1, s=0, calendar=cal, rc=rc )
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .true.

   case (optNYears)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

   case (optYearly)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=cyy, mm=1, dd=1, s=0, calendar=cal, rc=rc )
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .true.

   case default
      call ESMF_LogWrite(subname//'unknown option '//trim(option), ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
      return

   end select

   ! --------------------------------------------------------------------------------
   ! --- AlarmInterval and NextAlarm should be set ---
   ! --------------------------------------------------------------------------------

   ! --- advance Next Alarm so it won't ring on first timestep for
   ! --- most options above. go back one alarminterval just to be careful

   if (update_nextalarm) then
      NextAlarm = NextAlarm - AlarmInterval
      do while (NextAlarm <= CurrTime)
         NextAlarm = NextAlarm + AlarmInterval
      enddo
   endif

   alarm = ESMF_AlarmCreate( name=lalarmname, clock=clock, ringTime=NextAlarm, &
        ringInterval=AlarmInterval, rc=rc)
   if (ChkErr(rc,__LINE__,u_FILE_u)) return

   end subroutine esm_time_alarmInit  

  !-----------------------------------------------------------------------------

end module
