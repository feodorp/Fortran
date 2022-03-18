module interp1d

    use kinds
    use messagehandler

    implicit none
    private

    integer, parameter, public              :: LinearInterp     = 2
    integer, parameter, public              :: QuadraticInterp  = 3
    integer, parameter, public              :: CubicInterp      = 4


    integer, parameter, public              :: NaturalSpline  = 0
    integer, parameter, public              :: NotAKnotSpline = 1
    integer, parameter, public              :: ClampedSpline  = 3

    integer, parameter, public              :: Increasing = 0
    integer, parameter, public              :: Decreasing = 1

    real(kind=rp), parameter                :: zero = 0.0_rp

    !
    ! One dimensional pieciwise polynomial function
    !
    ! ith polynomial is:
    !
    ! p_i(x) = coefs(i,1)*(x-x(i))^(d-1) + coefs(i,2)*(x-x(i))^(d-2) + ... + coefs(i,d)
    !
    type, public :: ppoly(lmax,dmax)
        integer, len                        :: lmax               ! maximum number of intervals
        integer, len                        :: dmax               ! maximum dimension of polynomial
        real(kind=rp)                       :: x(lmax+1)          ! array of knots
        real(kind=rp)                       :: coefs(lmax+1,dmax) ! array of polynomial coeficients
        integer                             :: l = 0              ! current number of intervals
        integer                             :: d = 0              ! current dimension of polynomial
    end type ppoly

    interface ppval
        module procedure ppoly_eval_scalar, &
                         ppoly_eval_array, &
                         ppoly_eval_array_ordered
    end interface ppval

    interface trdss
        module procedure tridsymss, &
                         tridgenss
    end interface trdss

    public :: spline, hermite, ppval, trdss

contains

    ! Cubic spline interpolation function
    logical function spline(pp, x, y, stype) result(stat)
        implicit none
        type(ppoly(*,*)), intent(inout)     :: pp
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp), intent(in)           :: y(:)
        integer, intent(in), optional       :: stype

        integer                             :: type_l
        integer                             :: n, m

        stat = .false.

        if (pp%dmax < CubicInterp) then
            call message_report(ErrorMessage,&
                'Spline interpolation requires cubic pieciwise polynomial!',__FILE__,__LINE__)
            return
        end if

        n = size(x,1)
        m = size(y,1)
        if (n-1 > pp%lmax) then
            call message_report(ErrorMessage,&
                'Pieciwise polynomial type with number of knots less then dimension of x!',__FILE__,__LINE__)
            return
        end if

        if (m /= n .and. m /= n+2) then
            call message_report(ErrorMessage,&
                'x and y arrays size mismatch!',__FILE__,__LINE__)
            return
        end if

        if (n < 2) then
            call message_report(ErrorMessage,&
                'Number of interpolating points is less then 2!',__FILE__,__LINE__)
            return
        end if

        if (any(x(2:n) - x(1:n-1) <= 0.0_rp)) then
            call message_report(errormessage,&
                'Knot points values must be strictly increasing!',__FILE__,__LINE__)
            return
        end if

        if (present(stype)) then
            if (stype == NaturalSpline .or. stype == NotAKnotSpline) then
                type_l = stype
            else if (stype == ClampedSpline .and. m/= n+2) then
                 call message_report(ErrorMessage,&
                     'For "ClampedSpline" size(y)=size(x)+2 with first and last elements in y as boundary condition!', &
                     __FILE__,__LINE__)
                 return
            else
                call message_report(ErrorMessage,&
                    'Spline type must be: "NaturalSpline", "NotAKnotSpline" or "ClampedSpline"!',&
                    __FILE__,__LINE__)
                return
            end if
        else
            if (n == m) then
                type_l = NaturalSpline
            else
                type_l = ClampedSpline
            end if
        end if

        select case (type_l)
        case (NaturalSpline)
            stat = spline_natural(pp,x,y)
        case (NotAKnotSpline)
            stat = spline_notaknot(pp,x,y)
        case (ClampedSpline)
            stat = spline_clamped(pp,x,y)
        end select

    end function spline

    logical function spline_natural(pp,x,y) result(stat)
        implicit none
        type(ppoly(*,*)), intent(out)       :: pp
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp), intent(in)           :: y(:)

        integer                             :: n, info
        real(kind=rp)                       :: c2n

        stat = .true.
        n = size(x,1)
        if (n == 2) then
            ! line
            pp%l = 1
            pp%d = LinearInterp
            pp%x(1:2) = x(1:2)
            pp%coefs(1,1) = (y(2)-y(1))/(x(2)-x(1))
            pp%coefs(1,2) = y(1)
        else
            associate (c1 => pp%coefs(1:n-1,1), &
                       c2 => pp%coefs(1:n-1,2), &
                       c3 => pp%coefs(1:n-1,3), &
                       c4 => pp%coefs(1:n-1,4), &
                       dx => pp%x(1:n-1))

                ! auxiliar
                dx(1:n-1) = x(2:n) - x(1:n-1)
                c3(1:n-1) = (y(2:n) - y(1:n-1))/dx(1:n-1) ! Dy/Dx

                ! RHS
                c2(1) = zero
                c2(2:n-1) = 3.0_rp*(c3(2:n-1) - c3(1:n-2))

                ! main diagonal (with boundary conditions)
                c4(2:n-3) = 2.0_rp*(x(4:n-1)-x(2:n-3))
                c4(1)   = 2.0_rp*(x(3)-x(1))
                c4(n-2) = 2.0_rp*(x(n)-x(n-2))

                ! sub-diagonal
                c1(1:n-3) = dx(2:n-2)

                !solve symmetric positive defined tridiagonal system
                call trdss(c4(1:n-2), c1(1:n-3), c2(2:n-1))

                c1(1:n-2) = (c2(2:n-1) - c2(1:n-2))/(3.0_rp*dx(1:n-2))
                c1(n-1) =  -c2(n-1)/(3.0_rp*dx(n-1))

                c3(1:n-2) = c3(1:n-2) - (2.0*c2(1:n-2) + c2(2:n-1))*dx(1:n-2)/3.0_rp
                c3(n-1) = c3(n-1) - 2.0*c2(n-1)*dx(n-1)/3.0_rp

                c4(1:n-1) = y(1:n-1)
            end associate
            pp%l = n-1
            pp%d = CubicInterp
            pp%x(1:n) = x(1:n)
        end if
    end function spline_natural

    logical function spline_notaknot(pp,x,y) result(stat)
        implicit none
        type(ppoly(*,*)), intent(out)       :: pp
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp), intent(in)           :: y(:)

        integer                             :: n, info
        real(kind=rp)                       :: du(1:size(x,1)-3)
        real(kind=rp)                       :: dt1, dtn, c2n

        stat = .true.
        n = size(x,1)
        if (n == 2) then
            ! line
            pp%l = 1
            pp%d = LinearInterp
            pp%x(1:2) = x(1:2)
            pp%coefs(1,1) = (y(2)-y(1))/(x(2)-x(1))
            pp%coefs(1,2) = y(1)
        elseif (n == 3) then
            ! 2nd order polynomial
            pp%l = 1
            pp%d = QuadraticInterp
            pp%x(1:2) = x(1:3:2)
            pp%coefs(1,1) = ((y(3)-y(1))/(x(3)-x(1))-(y(2)-y(1))/(x(2)-x(1)))/(x(3)-x(2))
            pp%coefs(1,2) = (y(2)-y(1))/(x(2)-x(1))-pp%coefs(1,2)*(x(2)-x(1))
            pp%coefs(1,3) = y(1)
        else
            associate (d   => pp%coefs(1:n-2,1), & ! diagonal
                       c   => pp%coefs(1:n,2), & ! rhs
                       dl  => pp%coefs(1:n-3,3), & ! lower diagonal
                       aux => pp%coefs(1:n-1,4), & ! auxiliar array Dy/Dx
                       dx  => pp%x(1:n-1))         ! Dx(i) = x(i+1)-x(i)

                ! auxiliar
                dx(1:n-1) = x(2:n) - x(1:n-1)
                aux(1:n-1) = (y(2:n) - y(1:n-1))/dx(1:n-1)
                dt1 = dx(1)**2/dx(2)
                dtn = dx(n-1)**2/dx(n-2)

                ! RHS
                c(2:n-1) = 3.0_rp*(aux(2:n-1) - aux(1:n-2))

                ! sub-diagonal
                dl(1:n-4) = dx(2:n-3)
                dl(n-3)   = dx(n-2) - dtn

                ! main diagonal with boundary conditions
                d(1)     = 3.0_rp*dx(1) + 2.0_rp*dx(2) + dt1
                d(2:n-3) = 2.0_rp*(x(4:n-1) - x(2:n-3))
                d(n-2)   = 3.0_rp*dx(n-1) + 2.0_rp*dx(n-2) + dtn

                !super-duagonal
                du(1)     = dx(2) - dt1
                du(2:n-3) = dx(3:n-2)

                !solve generic tridiagonal system
                call trdss(dl(1:n-3), d(1:n-2), du(1:n-3), c(2:n-1))

                c(1) = (c(2)-c(3))*dx(1)/dx(2)+c(2)
                c(n) = (c(n-1)-c(n-2))*dx(n-1)/dx(n-2)+c(n-1)

                ! c is associated with pp%coefs(1:n,2)
                pp%coefs(1:n-1,1) = (c(2:n) - c(1:n-1))/(3.0_rp*dx(1:n-1))
                pp%coefs(1:n-1,3) = aux(1:n-1) - (2.0*c(1:n-1) + c(2:n))*dx(1:n-1)/3.0_rp
                pp%coefs(1:n-1,4) = y(1:n-1)
            end associate
            pp%l = n-1
            pp%d = CubicInterp
            pp%x(1:n) = x(1:n)
        end if
    end function spline_notaknot

    logical function spline_clamped(pp,x,y) result(stat)
        implicit none
        type(ppoly(*,*)), intent(out)       :: pp
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp), intent(in)           :: y(0:)

        integer                             :: n, info


        stat = .true.
        n = size(x,1)

        associate (d   => pp%coefs(1:n,1),  & ! main diagonal
                   c   => pp%coefs(1:n,2),  & ! rhs
                   sd  => pp%coefs(1:n-1,3),& ! sub diagonal
                   aux => pp%coefs(1:n-1,4),& ! auxiliar array Dy/Dx
                   dx  => pp%x(1:n-1))        ! Dx(i) = x(i+1)-x(i)

            ! auxiliar
            dx(1:n-1) = x(2:n) - x(1:n-1)
            aux(1:n-1) = (y(2:n) - y(1:n-1))/dx(1:n-1) ! Dy/Dx

            ! RHS
            c(1) = 3.0_rp*(aux(1) - y(0))
            c(2:n-1) = 3.0_rp*(aux(2:n-1) - aux(1:n-2))
            c(n) = 3.0_rp*(y(n+1) - aux(n-1))

            ! main diagonal
            d(1) = 2.0_rp*dx(1)
            d(2:n-1) = 2.0_rp*(x(3:n)-x(1:n-2))
            d(n) = 2.0_rp*dx(n-1)

            ! sub-diagonal
            sd(1:n-1) = dx(1:n-1)

            !solve symmetric positive defined tridiagonal system
            call trdss(d(1:n), sd(1:n-1), c(1:n))

            ! c is associated with pp%coefs(1:n,2)
            pp%coefs(1:n-1,1) = (c(2:n) - c(1:n-1))/(3.0_rp*dx(1:n-1))
            pp%coefs(1:n-1,3) = aux(1:n-1) - (2.0*c(1:n-1) + c(2:n))*dx(1:n-1)/3.0_rp
            pp%coefs(1:n-1,4) = y(1:n-1)
        end associate
        pp%l = n-1
        pp%d = CubicInterp
        pp%x(1:n) = x(1:n)
    end function spline_clamped

    ! Hermite cubic interpolation function
    logical function hermite(pp,x,y,dy) result(stat)
        implicit none
        type(ppoly(*,*)), intent(inout)     :: pp
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp), intent(in)           :: y(:)
        real(kind=rp), intent(in)           :: dy(:)

        integer                             :: n, m

        stat = .false.

        if (pp%dmax < CubicInterp) then
            call message_report(ErrorMessage,&
                'Hermite cubic interpolation requires cubic pieciwise polynomial!',__FILE__,__LINE__)
            return
        end if

        n = size(x,1)
        if (n-1 > pp%lmax) then
            call message_report(ErrorMessage,&
                'Pieciwise polynomial type with number of knots less then dimension of x!',__FILE__,__LINE__)
            return
        end if

        if (n /= size(y,1) .or. n /= size(dy,1)) then
            call message_report(ErrorMessage,&
                'x, y  and dy arrays size mismatch!',__FILE__,__LINE__)
            return
        end if

        if (n < 2) then
            call message_report(ErrorMessage,&
                'Number of interpolating points is less then 2!',__FILE__,__LINE__)
            return
        end if

        if (any(x(2:n) - x(1:n-1) <= 0.0_rp)) then
            call message_report(errormessage,&
                'Knot points values must be strictly increasing!',__FILE__,__LINE__)
            return
        end if

        stat = .true.
        associate (deltay => pp%coefs(1:n-1,1),  & ! (y(i+1)-y(i))/dx(i)
                   del1   => pp%coefs(1:n-1,3),  & ! (dy(i) - deltay(i))/dx(i)
                   del2   => pp%coefs(1:n-1,4),  & ! (dy(i+1) - deltay(i))/dx(i)
                   dx  => pp%x(1:n-1))             ! dx(i) = x(i+1)-x(i)

            dx(1:n-1) = x(2:n) - x(1:n-1)
            deltay(1:n-1) = (y(2:n) - y(1:n-1))/dx(1:n-1)
            del1(1:n-1) = (dy(1:n-1) - deltay(1:n-1))/dx(1:n-1)
            del2(1:n-1) = (dy(2:n)   - deltay(1:n-1))/dx(1:n-1)

            pp%coefs(1:n-1,1) = del1(1:n-1) + del2(1:n-1)
            pp%coefs(1:n-1,2) = -pp%coefs(1:n-1,1) - del1(1:n-1)
            pp%coefs(1:n-1,1) = pp%coefs(1:n-1,1)/dx(1:n-1)
            pp%coefs(1:n-1,3) = dy(1:n-1)
            pp%coefs(1:n-1,4) = y(1:n-1)
        end associate
        pp%l = n-1
        pp%d = CubicInterp
        pp%x(1:n) = x(1:n)
    end function hermite

    pure function ppoly_eval_scalar(pp, x) result(y)
        implicit none
        type(ppoly(*,*)), intent(in)        :: pp
        real(kind=rp), intent(in)           :: x
        real(kind=rp)                       :: y

        integer                             :: i, k
        real(kind=rp)                       :: h

        if (pp%l == 0) then
            y = zero
        else
            i = isearch(x,pp%x(1:pp%l+1))
            h = (x-pp%x(i))

            y = pp%coefs(i,1)
            do k = 2, pp%d
                y = h*y + pp%coefs(i,k)
            end do
        end if
    end function ppoly_eval_scalar

    pure function ppoly_eval_array(pp, x) result(y)
        implicit none
        type(ppoly(*,*)), intent(in)        :: pp
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp)                       :: y(size(x,1))

        integer                             :: n, i, j, k
        real(kind=rp)                       :: h

        if (pp%l == 0) then
            y(:) = zero
        else
            n = size(x,1)

            i = isearch(x(1),pp%x(1:pp%l))
            h = (x(1)-pp%x(i))

            y(1) = pp%coefs(i,1)
            do k = 2, pp%d
                y(1) = h*y(1) + pp%coefs(i,k)
            end do

            do j = 2,n
                if (x(j) > x(j-1) .and. i < pp%l ) then
                    if (x(j) > pp%x(i+1)) i = i+isearch(x(j),pp%x(i+1:))
                else if (x(j) < pp%x(i) .and. i > 1) then
                    if (x(j) < pp%x(i-1)) then
                        i = isearch(x(j),pp%x(1:i-2))-1
                    else
                        i = i - 1
                    end if
                end if
                h = (x(j)-pp%x(i))

                y(j) = pp%coefs(i,1)
                do k = 2, pp%d
                    y(j) = h*y(j) + pp%coefs(i,k)
                end do
            end do
        end if

    end function ppoly_eval_array

    pure function ppoly_eval_array_ordered(pp, x, order) result(y)
        implicit none
        type(ppoly(*,*)), intent(in)        :: pp
        real(kind=rp), intent(in)           :: x(:)
        integer, intent(in)                 :: order
        real(kind=rp)                       :: y(size(x,1))

        integer                             :: n, i, j, k
        integer                             :: b, e, s
        real(kind=rp)                       :: h

        n = size(x,1)

        if (order == Increasing) then
            b = 1; e = n; s = 1
        else if (order == Decreasing) then
            b = n; e = 1; s = -1
        else
            y = ppval(pp,x)
        end if

        if (pp%l == 0) then
            y(:) = zero
        else
            i = isearch(x(b),pp%x(1:pp%l))
            h = (x(b)-pp%x(i))

            y(b) = pp%coefs(i,1)
            do k = 2, pp%d
                y(b) = h*y(b) + pp%coefs(i,k)
            end do

            do j = b+s,e,s
                if (x(j) > pp%x(i+1)) i = i+isearch(x(j),pp%x(i+1:))
                h = (x(j)-pp%x(i))
                y(j) = pp%coefs(i,1)
                do k = 2, pp%d
                    y(j) = h*y(j) + pp%coefs(i,k)
                end do
            end do
        end if

    end function ppoly_eval_array_ordered


    pure function isearch(x, xi) result(i)
        implicit none
        real(kind=rp), intent(in)           :: x
        real(kind=rp), intent(in)           :: xi(:)
        integer                             :: i

        integer                             :: n
        integer                             :: i2,ic

        n = size(xi,1)

        if (n <= 2) then
            i = 1
        elseif (x <= xi(2)) then ! first element
            i = 1
        elseif (x <= xi(3)) then ! second element
            i = 2
        elseif (x > xi(n-1)) then  ! right end
            i = n-1
        else
            ! bisection: xi(i) < x <= xi(i2)
            i = 3; i2 = n-1
            ic = ishft(i2-i,-1)
            do while (ic > 0)
            ic = i + ic
            if (x > xi(ic)) then
                i = ic
                if (x < xi(i+1)) exit
            else
                i2 = ic
                if (x > xi(ic-1)) then
                    i = ic-1
                    exit
                end if
            endif
            ic = ishft(i2-i,-1)
            end do
        end if
    end function isearch

    pure subroutine tridsymss(d,l,b)
    !
    ! Solve generic symmetric positive defined tridiagonal system by Gaussian elimination
    !
        implicit none
        real(kind=rp), intent(inout)        :: d(:)
        real(kind=rp), intent(inout)        :: l(:)
        real(kind=rp), intent(inout)        :: b(:)

        integer                             :: i, i4, j, n
        real(kind=rp)                       :: li, di

        n = size(b)
        li = l(1)
        l(1) = l(1)/d(1)
        b(1) = b(1)/d(1)
        do i=2,n
            di = 1.0_rp/(d(i) - l(i-1)*li)
            b(i)  = (b(i) - b(i-1)*li)*di
            li = l(i)
            l(i) = l(i)*di
        end do
        do i=n-1,1,-1
            b(i) = b(i) - l(i)*b(i+1)
        end do
    end subroutine tridsymss

    pure subroutine tridgenss(dl,d,du,b)
    !
    ! Solve generic positive defined tridiagonal system by Gaussian elimination
    !
        implicit none
        real(kind=rp), intent(inout)        :: dl(:)
        real(kind=rp), intent(inout)        :: d(:)
        real(kind=rp), intent(inout)        :: du(:)
        real(kind=rp), intent(inout)        :: b(:)

        integer                             :: i, n
        real(kind=rp)                       :: di

        n = size(b)
        du(1) = du(1)/d(1)
        b(1)  = b(1)/d(1)
        do i=2, n
            di = 1.0_rp/(d(i) - du(i-1)*dl(i-1))
            du(i) = du(i)*di
            b(i)  = (b(i) - b(i-1)*dl(i-1))*di
        end do
        do i=n-1,1,-1
            b(i) = b(i) - du(i)*b(i+1)
        end do
    end subroutine tridgenss
end module
