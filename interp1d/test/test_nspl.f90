program test_pp
    use kinds
    use interp1d

    implicit none
    type(ppoly(10,CubicInterp))             :: pp
    real(kind=rp)                           :: x(10), y(10)
    integer                                 :: i
    logical                                 :: stat

    x = real([(real(i,rp)*1.0_rp/9.0_rp, i=0,9)],rp)
    y = real([0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0],rp)

    if ( spline(pp,x,y,NaturalSpline) ) then
        do i = 1, pp%lmax
            print*,pp%coefs(i,1:pp%dmax)
        end do
    endif

end
