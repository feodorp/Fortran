program test_spline
      use spline1d
      use kinds
      use quicksort

      implicit none
      type(spline)      :: s
      integer, parameter:: n=500, m=10
      real(kind=rp)     :: x(m), y(m)
      real(kind=rp)     :: xv(n),yv(n)
      integer           :: ind(n)
      integer           :: i, info
      real(kind=rp)     :: a(10)


      do i=1,10000
      call random_number(x(2:m-1))
      x(2:m-1) = 10.0_rp*x(2:m-1)
      x(1)=0.0_rp
      x(m)=10.0_rp
      call random_number(y)
      call qsort(x)
      call s%set(x,y,NotAKnotSpline)

      end do
      call random_number(xv)
          xv = 11.0_rp*xv
      call qsort(xv)
      yv = s%eval(xv)

      do i=1, size(x,1)
          print*,x(i),y(i)
      end do
      print*

      do i=1, size(xv,1)
          print*,xv(i),yv(i)
      end do

      print*

      print*,s%maxima()
      print*,s%eval(s%maxima())

end
