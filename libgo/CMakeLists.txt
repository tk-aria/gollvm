
# Building libgo is time-consuming. Allow developers to stub it out
# via cmake flag if desired.

if(NOT DISABLE_LIBGO_BUILD)
  return()
endif()

message(STATUS "starting libgo configuration.")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(ConfigSetup)

message(STATUS "libgo configuration complete.")

