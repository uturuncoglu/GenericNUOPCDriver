!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

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

  ! TODO: this part of code needs to be generated
  use cdeps_datm_comp, only: atmSS => SetServices
  use lnd_comp_nuopc , only: lndSS => SetServices

  implicit none

  private

  public SetServices

  type ssPtr
     procedure(SetServices), pointer, nopass :: s_ptr => null()
  end type ssPtr

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
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Config)             :: config 
    type(NUOPC_FreeFormat)        :: ff

    integer :: n
    integer :: compCount
    integer, allocatable :: petList(:)
    character(len=32), allocatable :: compLabels(:)
    character(len=256) :: namestr
    type(ssPtr), allocatable :: compSS(:)

    rc = ESMF_SUCCESS

    ! get configuration
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Obtain component count
    compCount = ESMF_ConfigGetLen(config, label="component_list:", rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Obtain component labels
    if (.not. allocated(compLabels)) allocate(compLabels(compCount))
    call ESMF_ConfigGetAttribute(config, valueList=compLabels, label="component_list:", count=compCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return    

    ! Set procedure pointer
    ! TODO: this part of code needs to be generated somehow
    if (.not. allocated(compSS)) allocate(compSS(compCount))
    compSS(1)%s_ptr => atmSS
    compSS(2)%s_ptr => lndSS

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

       ! Clear memory
       deallocate(petList)
    end do 

    ! Loop over components and create connectors between them
    do n = 1, compCount
       srcCompLabel = ESMF_UtilStringUpperCase(compLabels(n))
       do m = 1, compCount
          dstCompLabel = ESMF_UtilStringUpperCase(compLabels(m))

          ! SetServices for n,m pair
          call NUOPC_DriverAddComp(driver, srcCompLabel=trim(srcCompLabel), &
            dstCompLabel=trim(dstCompLabel), compSetServicesRoutine=cplSS, comp=connector, rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return

          call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", rc=rc)
          if (chkerr(rc,__LINE__,u_FILE_u)) return
       end do
    end do   
    

    return

    ! set the driver clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

end module
