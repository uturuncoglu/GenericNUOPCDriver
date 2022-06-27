cmake_minimum_required(VERSION 3.20)
project(driver LANGUAGES Fortran)
enable_language(Fortran)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

#------------------------------------------------------------------------------
# Add source files
list(APPEND _driver_files esmApp.F90 esm.F90) 

#------------------------------------------------------------------------------
# Find dependencies
if (TARGET esmf)
  message("Target esmf is already found. Skip find_package ...")
else()
  set(USE_ESMF_STATIC_LIBS TRUE)
  find_package(ESMF REQUIRED)
  if (ESMF_FOUND)
    set(CMAKE_Fortran_COMPILER "${ESMF_F90COMPILER}")
    set(CMAKE_Fortran_FLAGS "${ESMF_F90COMPILEOPTS} ${ESMF_F90COMPILEPATHS}")
    set(CMAKE_Fortran_LINK_FLAGS "${ESMF_F90LINKOPTS} ${ESMF_F90LINKPATHS} ${ESMF_F90LINKRPATHS} ${ESMF_F90ESMFLINKLIBS}") 
  endif()
endif()

#find_package(PIO REQUIRED COMPONENT C Fortran STATIC PATH ${PIO_ROOT})
find_package(PIO REQUIRED COMPONENTS C Fortran)
message(${PIO_FOUND} ${PIO_Fortran_STATIC_FOUND})
#if (PIO_FOUND)
#endif()
#set_target_properties(

#find_package(FMS REQUIRED COMPONENTS R4 R8)
#add_library(fms ALIAS FMS::fms_r8)

#------------------------------------------------------------------------------
# NUOPC Driver
add_executable(driver ${_driver_files})

#------------------------------------------------------------------------------
# Add external libraries
foreach(DEPS datm streams dshr cdeps_share FoX_dom FoX_wxml FoX_sax FoX_common FoX_utils FoX_fsys)
  add_library(${DEPS} STATIC IMPORTED)
  set_target_properties(${DEPS} PROPERTIES
    IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/../../CDEPS/install/lib/lib${DEPS}.a"
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_BINARY_DIR}/../../CDEPS/install/include")
endforeach(DEPS)

add_library(noahmp STATIC IMPORTED)
set_target_properties(noahmp PROPERTIES
	IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/../../noahmp/install/lib/libnoahmp.a"
	INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_BINARY_DIR}/../../noahmp/install/mod")

target_link_libraries(driver PUBLIC esmf noahmp datm streams dshr cdeps_share FoX_dom FoX_wxml FoX_sax FoX_common FoX_utils FoX_fsys PIO::PIO_Fortran)

#------------------------------------------------------------------------------
# Install
install(
  TARGETS driver
  EXPORT  driver-config
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX})

install(EXPORT      driver-config
        DESTINATION lib/cmake)
