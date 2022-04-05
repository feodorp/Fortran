include_directories(${SPLINE1DINCLUDE})
set(SRC
    quicksort.f90)

add_library(utils ${SRC})

add_executable(test_spline test_spline.f90)
target_link_libraries(test_spline utils spline1d)
add_executable(test_spline_maxima test_spline_maxima.f90)
target_link_libraries(test_spline_maxima utils spline1d)
# add_test(test_spline "./test_nspl")
# add_test(test_spline_maxima "./test_pp")
