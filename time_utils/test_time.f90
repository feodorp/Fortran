program test_time
   use time_utils
   use, intrinsic :: iso_fortran_env, only : int64, real64,real128

   implicit none
   type(stopwatch)                          :: t(2)
   integer                                  :: i
   real                                     :: x


   do i = 1,10
      ! get time for subroutine sub1
      call t(1)%start()
      ! call sub1(x)
      ! call subroutine sub2: sleep
      call sub2()
      call t(1)%stop()

      ! get time for subroutine sub3
      call t(2)%start()
      call sub3(x)
      call t(2)%stop()
   end do

   print*,x

   print*, 'Print total time as string:'
   print*,'sub2: ', time2str(t(1)%total_time())
   print*,'sub3: ', time2str(t(2)%total_time())

   print*

   ! for testing propose, manualy change timer
   t(1)%timer_total = 78528330.0_real64
   t(2)%timer_total = 4435.3546253_real64
   print*,'Print manualy modified timer:'
   print*,'sub1: ', time2str(t(1)%total_time())
   print*,'sub3: ', time2str(t(2)%total_time())


contains
   subroutine sub1(x)
      implicit none
      real, intent(inout)                   :: x

      integer                               :: i

      x = 1.0
      do i=1,10000000
         x = x*sqrt(x+1.0)
      end do
   end subroutine

   subroutine sub2()
      implicit none
      integer   :: res
      res=nanosleep(0_int64,50000000_int64)
   end subroutine

   subroutine sub3(x)
      implicit none
      real, intent(inout)                   :: x

      integer                               :: i

      x = 1.0
      do i=1,10000000
         x = x+sin(x+1.0)
      end do
   end subroutine
end program
