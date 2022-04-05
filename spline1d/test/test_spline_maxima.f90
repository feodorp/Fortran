program test_spline
      use spline1d
      use kinds
      use quicksort

      implicit none
      type(spline)      :: s
      integer, parameter:: m=10
      integer           :: i
      real(kind=rp)     :: x(m+1), y(m+1)
      real(kind=rp), allocatable ::xmax(:)


     call random_number(y)
     x = [(0.0_rp + 10.0_rp*real(i,kind=rp)/real(m,kind=rp), i=0,m)]
     y = [(1.0_rp + cos(25.0_rp*3.14_rp*real(i,kind=rp)/real(m,kind=rp)), i=0,m)]
     call qsort(x)
      ! x =[-1.0_rp, 0.0_rp, 1.0_rp, 2.0_rp, 3.0_rp, 4.0_rp, 5.0_rp, 7.0_rp, 8.0_rp, 10.0_rp]
      ! y =[0.0_rp, 2.0_rp, 3.0_rp, 5.0_rp, 3.0_rp, 3.0_rp, 3.0_rp, 0.0_rp, 2.0_rp, 3.0_rp ]
      call s%set(x,y)
      xmax = s%maxima()
      print*, s%maxima()
end
