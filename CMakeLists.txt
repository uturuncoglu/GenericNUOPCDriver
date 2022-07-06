cmake_minimum_required(VERSION 3.20)
project(driver LANGUAGES Fortran)
enable_language(Fortran)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

#------------------------------------------------------------------------------
# Add source files
list(APPEND _driver_files esmApp.F90 esm.F90) 

set_source_files_properties(esm.F90 PROPERTIES Fortran_PREPROCESS ON)

#------------------------------------------------------------------------------
# Find core dependencies
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

find_package( PythonInterp 2.7 REQUIRED )

#------------------------------------------------------------------------------
# Find configuration specific dependencies
# TODO: This needs to be dynamic
find_package(PIO REQUIRED COMPONENTS C Fortran)
find_package(FMS REQUIRED COMPONENTS R4 R8)
add_library(fms ALIAS FMS::fms_r8)

#------------------------------------------------------------------------------
# NUOPC Driver
add_executable(driver ${_driver_files})

#------------------------------------------------------------------------------
# Generate include files for CMake and driver code
execute_process(
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/scripts/gen_code.py --ifile=${CMAKE_CURRENT_SOURCE_DIR}/nuopc_drv.yaml --odir=${CMAKE_CURRENT_SOURCE_DIR})  

#------------------------------------------------------------------------------
# Add external libraries
# NOTE: extlib.txt is auto-generated from the YAML file
include(extlib.txt)
foreach(_COMP ${COMPS})
  message("Adding dependencies for component ${_COMP} ...")
  message("Library directory for ${_COMP} is ${${_COMP}_LIB_DIR}")
  message("Include directory for ${_COMP} is ${${_COMP}_INC_DIR}")
  foreach(_DEPS ${${_COMP}_LIBS})
    message("Adding library ${_DEPS}")
    add_library(${_DEPS} STATIC IMPORTED)
    set_target_properties(${_DEPS} PROPERTIES
      IMPORTED_LOCATION "${${_COMP}_LIB_DIR}/lib${_DEPS}.a"
      INTERFACE_INCLUDE_DIRECTORIES "${${_COMP}_INC_DIR}")
    target_link_libraries(driver PUBLIC ${_DEPS}) 
  endforeach(_DEPS)
endforeach(_COMP)

# TODO: This needs to be dynamic
target_link_libraries(driver PUBLIC esmf PIO::PIO_Fortran fms)

#------------------------------------------------------------------------------
# Install
install(
  TARGETS driver
  EXPORT  driver-config
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(EXPORT      driver-config
        DESTINATION lib/cmake)
