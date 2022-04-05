include_directories(${SPLINE1DINCLUDE})

set(SRC
    kinds.f90
    messagehandler.f90
    spline1d.f90)

add_library(spline1d ${SRC})
