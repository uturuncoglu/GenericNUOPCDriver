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

#------------------------------------------------------------------------------
# NUOPC Driver
add_executable(driver ${_driver_files})

#------------------------------------------------------------------------------
# Add external libraries
link_directories(${CMAKE_BINARY_DIR}/../../CDEPS/build/datm)
target_include_directories(driver PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../../CDEPS/build/datm>
                                         $<INSTALL_INTERFACE:mod>)

link_directories(${CMAKE_BINARY_DIR}/../../noahmp/build)
target_include_directories(driver PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../../noahmp/build/mod>
                                         $<INSTALL_INTERFACE:mod>)


link_directories(/opt/view/lib)
target_include_directories(driver PUBLIC $<BUILD_INTERFACE:/opt/view/include>
                                         $<INSTALL_INTERFACE:mod>)

target_link_libraries(driver PUBLIC esmf datm noupc netcdf netcdff pnetcdf xerces-c)

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
