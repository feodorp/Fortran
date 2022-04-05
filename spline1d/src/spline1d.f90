module spline1d

    use kinds
    use messagehandler

    implicit none
    private

    integer, parameter, public              :: NaturalSpline  = 0
    integer, parameter, public              :: NotAKnotSpline = 1

    type :: scoef
        real(kind=rp)                       :: c(4)
    end type scoef

    type, public :: spline
        real(kind=rp), allocatable          :: x(:)
        real(kind=rp), allocatable          :: coefs(:,:)
        integer                             :: n
        logical                             :: init = .false.
    contains
        private
        procedure, pass                     :: spline_set
        procedure, pass                     :: spline_scalar_eval
        procedure, pass                     :: spline_vector_eval
        generic, public                     :: set => spline_set
        generic, public                     :: eval => spline_scalar_eval, spline_vector_eval
        procedure, pass, public             :: maxima => spline_maxima
        procedure, pass, public             :: minima => spline_minima
        procedure, pass, public             :: free => spline_free
        final                               :: spline_finalize
    end type spline

    interface trdss
        module procedure tridsymss, &
                         tridgenss
    end interface trdss

contains

    subroutine spline_set(this, x, y, stype)
        implicit none
        class(spline), intent(inout)        :: this
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp), intent(in)           :: y(:)
        integer, optional                   :: stype


        integer                             :: i, n
        integer                             :: splinetype
        integer                             :: info
        real(kind=rp)                       :: dx(size(x,1)-1), du(size(x,1)-3), dl(size(x,1)-3)
        real(kind=rp)                       :: dt1, dtn, c2n


        n = size(x,1)

        if (n /= size(y,1)) then
            call message_report(ErrorMessage,'Dimension of x and y are different!',__FILE__,__LINE__)
            return
        end if

        if (this%init .and. this%n /= n) call this%free()

        if (n < 2) then
            call message_report(ErrorMessage,'Number of interpolating points is less then 2!',__FILE__,__LINE__)
            return
        end if
        do i = 2,n
            if (x(i-1) >= x(i)) then
                call message_report(ErrorMessage,'x values must be strictly increasing!',__FILE__,__LINE__)
                return
            end if
        end do

        if (present(stype)) then
            if (stype == NaturalSpline .or. stype == NotAKnotSpline) then
                splinetype = stype
            else
                call message_report(ErrorMessage,'Spline type must be: "NaturalSpline" or "NotAKnotSPline"!',__FILE__,__LINE__)
                return
            end if
        else
            splinetype = NaturalSpline
        end if

        if (n == 2) then
            this%n = 2
            if (.not. allocated(this%x)) allocate(this%x(this%n))
            this%x(1:n) = x(1:n)
            if (.not. allocated(this%coefs)) allocate(this%coefs(1,4))
            this%coefs(1,1:2) = 0.0_rp
            this%coefs(1,3)   = (y(2)-y(1))/(x(2)-x(1))
            this%coefs(1,4)   = y(1)
        elseif (n == 3 .and. splinetype == NotAKnotSpline) then
            this%n = 2
            if (.not. allocated(this%x)) allocate(this%x(2))
            this%x(1:2) = [x(1),x(3)]
            if (.not. allocated(this%coefs)) allocate(this%coefs(1,4))
            this%coefs(1,1) = 0.0_rp
            this%coefs(1,2) = ((y(3)-y(1))/(x(3)-x(1))-(y(2)-y(1))/(x(2)-x(1)))/(x(3)-x(2))
            this%coefs(1,3) = (y(2)-y(1))/(x(2)-x(1))-this%coefs(1,2)*(x(2)-x(1))
            this%coefs(1,4) = y(1)
        else
            this%n = n
            if (.not. allocated(this%x)) allocate(this%x(n))
            this%x(1:n) = x(1:n)
            if (.not. allocated(this%coefs)) allocate(this%coefs(n-1,4))

            associate (c1 => this%coefs(1:n-1,1), &
                        c2 => this%coefs(1:n-1,2), &
                        c3 => this%coefs(1:n-1,3), &
                        c4 => this%coefs(1:n-1,4))

                dx(1:n-1) = x(2:n) - x(1:n-1)
                c3(1:n-1) = (y(2:n) - y(1:n-1))/dx(1:n-1)
                ! RHS
                c2(2:n-1) = 3.0_rp*(c3(2:n-1) - c3(1:n-2))
                ! diagonal
                c4(2:n-3) = 2.0_rp*(x(4:n-1)-x(2:n-3))
                ! sub-diagonal
                dl(1:n-3) = dx(2:n-2)
                if (splinetype == NotAKnotSpline) then
                    dt1 = dx(1)**2/dx(2)
                    dtn = dx(n-1)**2/dx(n-2)
                    !diagonal
                    c4(1)   = 3.0_rp*dx(1)+2.0_rp*dx(2)+dt1
                    c4(n-2) = 3.0_rp*dx(n-1)+2.0_rp*dx(n-2)+dtn
                    !super-duagonal
                    du(1:n-3) = dx(2:n-2)
                    du(1)     = du(1)-dt1
                    !last element of sub-diagonal
                    dl(n-3) = dl(n-3)-dtn
                    !solve generic tridiagonal system
                    call trdss(dl, c4(1:n-2), du, c2(2:n-1))

                    c2(1) = (c2(2)-c2(3))*dx(1)/dx(2)+c2(2)
                    c2n   = (c2(n-1)-c2(n-2))*dx(n-1)/dx(n-2)+c2(n-1)
                elseif (splinetype == NaturalSpline) then
                    !diagonal
                    c4(1)   = 2.0_rp*(x(3)-x(1))
                    c4(n-2) = 2.0_rp*(x(n)-x(n-2))
                    !solve symmetric positive defined tridiagonal system
                    call trdss(c4(1:n-2), dl, c2(2:n-1))

                    c2(1) = 0.0_rp
                    c2n   = 0.0_rp

                end if

                c1(1:n-1) = ([c2(2:n-1),c2n]-c2(1:n-1))/(3.0_rp*dx(1:n-1))
                c3(1:n-1) = c3(1:n-1) - (2.0*c2(1:n-1)+[c2(2:n-1),c2n])*dx(1:n-1)/3.0_rp
                c4(1:n-1) = y(1:n-1)
            end associate
        end if

        this%init = .true.

    end subroutine spline_set

    function spline_scalar_eval(this, x) result(y)
        implicit none
        class(spline), intent(in)           :: this
        real(kind=rp), intent(in)           :: x
        real(kind=rp)                       :: y

        integer                             :: i
        real(kind=rp)                       :: h

        if (.not.this%init) then
            call message_report(ErrorMessage,'Spline type must be initiated first!',__FILE__,__LINE__)
            y = 0.0_rp
            return
        end if

        i = isearch(x,this%x)
        h = (x-this%x(i))

        y = ((this%coefs(i,1)*h+this%coefs(i,2))*h+this%coefs(i,3))*h+this%coefs(i,4)

        return
    end function spline_scalar_eval

    function spline_vector_eval(this, x) result(y)
        implicit none
        class(spline), intent(in)           :: this
        real(kind=rp), intent(in)           :: x(:)
        real(kind=rp)                       :: y(size(x,1))

        integer                             :: k,i,n
        real(kind=rp)                       :: h

        if (.not.this%init) then
            call message_report(ErrorMessage,'Spline type must be initiated first!',__FILE__,__LINE__)
            y = 0.0_rp
            return
        end if

        n=size(x,1)

        do i=1,n-1
            if (x(i+1) < x(i)) then
                call message_report(ErrorMessage,'Spline must be evaluated on array with increasing values!',__FILE__,__LINE__)
                return
            end if
        end do

        i = 1
        do k=1,n
            i = i+isearch(x(k),this%x(i:))-1
            h = (x(k)-this%x(i))
            y(k) = ((this%coefs(i,1)*h+this%coefs(i,2))*h+this%coefs(i,3))*h+this%coefs(i,4)
        end do
        return
    end function spline_vector_eval

    function spline_maxima(this) result(xmax)
        implicit none
        class(spline), intent(in)           :: this
        real(kind=rp), allocatable          :: xmax(:)

        integer                             :: i, ncp, nmax
        real(kind=rp)                       :: xtmp(this%n-1), x
!        real(kind=rp)                       :: d(this%n-1)

        if (.not.this%init) then
            call message_report(ErrorMessage,'Spline type must be initiated first!',__FILE__,__LINE__)
            xmax = [real(kind=rp) ::]
            return
        end if


        associate (c1 => this%coefs(1:this%n-1,1), &
                   c2 => this%coefs(1:this%n-1,2), &
                   c3 => this%coefs(1:this%n-1,3), &
                   c4 => this%coefs(1:this%n-1,4))

!            d = c2**2-3.0_rp*c1*c3
!            xmax = pack(this%x(1:this%n-1) - (c2+sqrt(d))/(3.0_rp*c1), c1 /= 0.0_rp &
!                        .and. (d > 0.0_rp .and. ((c2+sqrt(d))/c1 <= 0.0_rp &
!                        .and.(c2+sqrt(d))/c1 >=3.0_rp*(this%x(1:this%n-1)-this%x(2:this%n)))))


           nmax = 0
           do i=1,this%n-1
               if (c1(i) /=0.0_rp) then
                   x = c2(i)**2-3.0_rp*c1(i)*c3(i)
                   if (x>0.0_rp) then
                       if (c2(i) >= 0.0_rp) then
                           if (c1(i) < 0.0_rp) then
                               x = this%x(i)-(c2(i)+sqrt(x))/(3.0_rp*c1(i))
                               if (x<this%x(i+1)) then
                                   nmax=nmax+1
                                   xtmp(nmax) = x
                               end if
                           end if
                       else
                           x = c2(i)+sqrt(x)
                           if ((x>=0.0_rp .and. c1(i)<0.0_rp) .or. (x<=0.0_rp .and. c1(i)>0.0_rp)) then
                               x = this%x(i)-x/(3.0_rp*c1(i))
                               if (x<this%x(i+1)) then
                                   nmax=nmax+1
                                   xtmp(nmax) = x
                               end if
                           end if
                       end if
                    end if
                elseif (c2(i) < 0.0_rp .and. c3(i)>=0.0_rp) then
                    x = this%x(i)-0.5_rp*c3(i)/c2(i)
                    if (this%x(i+1)>=x) then
                        nmax=nmax+1
                        xtmp(nmax) = x
                    end if
                end if
           end do
           !check if last point this%x(this%n) is maximum
           x = this%x(this%n)-this%x(this%n-1)
           if (3.0_rp*c1(this%n-1)*x**2+2.0_rp*c2(this%n-1)*x+c3(this%n-1) == 0.0_rp) then
               if (6.0_rp*c1(this%n-1)*x+2.0_rp*c2(this%n-1) >= 0) then
                   nmax=nmax+1
                   xtmp(nmax) = x
               end if
           end if
           xmax = xtmp(1:nmax)
        end associate
        return
    end function spline_maxima

    subroutine spline_minima(this)
        implicit none
        class(spline), intent(inout)        :: this
    end subroutine spline_minima

    subroutine spline_free(this)
        implicit none
        class(spline), intent(inout)         :: this

        if (this%init) then
            if (allocated(this%coefs)) deallocate(this%coefs)
            if (allocated(this%x)) deallocate(this%x)
            this%init = .false.
        end if
    end subroutine spline_free

    subroutine spline_finalize(this)
        implicit none
        type(spline), intent(inout)         :: this

        call this%free()
        return
    end subroutine spline_finalize

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
    ! Solve symmetric positive defined tridiagonal system by Gaussian elimination
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
