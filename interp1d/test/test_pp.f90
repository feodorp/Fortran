program test_pp
    use kinds
    use interp1d

    implicit none
    type(ppoly(20,CubicInterp))             :: pp
    real(kind=rp)                           :: x(11), y(11), dy(11)
    real(kind=rp)                           :: xx(400), yy(400)
    integer                                 :: i
    logical                                 :: stat

    x = real([1., 2., 3., 4., 5., 5.5, 7., 8., 9., 9.5, 10.],rp)
    y = real([0.0, 0.0, 0.0, 0.5, 0.4, 1.2, 1.2, 0.1, 0.0, 0.3, 0.0],rp)
    dy(:) = 0.0_rp
    xx(:)  = [(x(1)+real(i,rp)*(x(11)-x(1))/399.0_rp, i=0,399)]

    if ( hermite(pp,x,y,dy) ) then
        do i = 1, pp%lmax
            print*,pp%coefs(i,1:pp%dmax)
        end do

        print*
        yy = ppval(pp,xx,Increasing)
        print*,xx
        print*,yy
    endif

end
