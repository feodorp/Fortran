include_directories(${INTER1DINCLUDE})

set(SRC
    kinds.f90
    messagehandler.f90
    interp1d.f90)

add_library(interp1d ${SRC})
