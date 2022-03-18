include_directories(${INTER1DINCLUDE})

set(SRC
    lapack90.f90
    time_utils.f90)

add_library(tutils ${SRC})

find_package(LAPACK)
if(LAPACK_FOUND)
   set(lapack_libraries "${LAPACK_LIBRARIES}")
endif()

add_executable(test_nspl test_nspl.f90)
target_link_libraries(test_nspl interp1d)
add_executable(test_pp test_pp.f90)
target_link_libraries(test_pp tutils interp1d)
if(LAPACK_FOUND)
    add_executable(test_trdm test_trdm.f90)
    target_link_libraries(test_trdm tutils interp1d ${lapack_libraries})
endif()
# add_test(test_nspl "./test_nspl")
# add_test(test_pp "./test_pp")
# add_test(test_trdm "./test_trdm")
