! time_utils module
!
! Copyright (C) 2022 Feodor Pisnitchenko
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module time_utils
    use, intrinsic :: iso_c_binding, only : c_int, c_long, c_ptr, c_null_ptr
    use, intrinsic :: iso_fortran_env , only : int64, real64

    implicit none
    private

!------------------------------------------------------------------------------
! Public module elements
!------------------------------------------------------------------------------
    public :: stopwatch, time2str, nanosleep
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! C timespec structure and nanosleep interface
!------------------------------------------------------------------------------
    type, bind(c) :: c_timespec
        integer(kind=c_long)   :: tv_sec
        integer(kind=c_long)   :: tv_nsec
    end type c_timespec

    interface
        ! int nanosleep(const struct timespec *req, struct timespec *rem)
        function c_nanosleep(req,rem) bind(c, name='nanosleep')
            import :: c_timespec, c_ptr, c_int
            implicit none
            type(c_timespec), intent(in)                 :: req
            type(c_ptr), intent(in)                      :: rem
            integer(kind=c_int)                          :: c_nanosleep
        end function c_nanosleep
    end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! stopwatch derived type definitions
!------------------------------------------------------------------------------
    type :: stopwatch
        private
        real(kind=real64), public           :: timer
        real(kind=real64), public           :: timer_total = 0.0_real64
        integer(kind=int64)                 :: count_max = 0_int64
        integer(kind=int64)                 :: count_rate = 0_int64
        integer(kind=int64)                 :: count_tic
        integer(kind=int64)                 :: count_toc
    contains
        procedure, public, pass             :: start => stopwatch_start
        procedure, public, pass             :: stop => stopwatch_stop
        procedure, public, pass             :: reset => stopwatch_reset
        procedure, public, pass             :: time => stopwatch_time
        procedure, public, pass             :: total_time => stopwatch_total_time
    end type stopwatch
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Auxiliar function interface
!------------------------------------------------------------------------------
    interface ntoa
        module procedure ntoa_int, ntoa_real
    end interface ntoa
!------------------------------------------------------------------------------

contains


!------------------------------------------------------------------------------
! nanosleep: simple version of C function
!------------------------------------------------------------------------------
    integer function nanosleep(sec, nanosec) result(res)
        implicit none
        integer(kind=int64), intent(in)     :: sec
        integer(kind=int64), intent(in)     :: nanosec

        res = int(c_nanosleep(c_timespec(int(sec,kind=c_long),int(nanosec,kind=c_long)),c_null_ptr))
    end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! stopwatch procedures
!------------------------------------------------------------------------------
    subroutine stopwatch_start(this)
        implicit none
        class(stopwatch), intent(inout)      :: this

        this%timer = 0.0_real64
        call system_clock(count=this%count_tic,count_rate=this%count_rate,count_max=this%count_max)
    end subroutine stopwatch_start

    subroutine stopwatch_stop(this)
        implicit none
        class(stopwatch), intent(inout)      :: this

        if (this%count_rate /= 0_int64 .and. this%count_max /= 0_int64) then
            call system_clock(count=this%count_toc)
            if (this%count_toc >= this%count_tic) then
                this%timer = real(this%count_toc-this%count_tic,real64)/real(this%count_rate,real64)
            else
                this%timer = real(this%count_max-this%count_tic+this%count_toc,real64)/real(this%count_rate,real64)
            end if
            this%timer_total = this%timer_total + this%timer
        end if
    end subroutine stopwatch_stop

    pure subroutine stopwatch_reset(this)
        implicit none
        class(stopwatch), intent(inout)     :: this
        this%timer_total = 0.0_real64
    end subroutine stopwatch_reset

    real(kind=real64) pure function stopwatch_time(this) result(t)
        implicit none
        class(stopwatch), intent(in)        :: this
        t = this%timer
    end function stopwatch_time

    real(kind=real64) pure function stopwatch_total_time(this) result(t)
        implicit none
        class(stopwatch), intent(in)        :: this
        t = this%timer_total
    end function stopwatch_total_time
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Convert real64 seconds to string
!------------------------------------------------------------------------------
    function time2str(time) result(time_string)
        implicit none
        real(kind=real64), intent(in)       :: time
        character(len=:), allocatable       :: time_string

        real(kind=real64)                   :: convtime

        time_string = ''
        convtime = time
        if (convtime > 86400.0_real64) then
            time_string = ntoa(floor(convtime/86400.0_real64,int64))//' days '
            convtime=mod(convtime,86400.0_real64)
        end if
        if (convtime > 3600.0_real64) then
            time_string = time_string//ntoa(floor(convtime/3600.0_real64,int64))//' hours '
            convtime=mod(convtime,3600.0_real64)
        end if
        if (convtime > 60.0_real64) then
            time_string = time_string//ntoa(floor(convtime/60.0_real64,int64))//' minutes '
            convtime=mod(convtime,60.0_real64)
        end if
        time_string = time_string//ntoa(convtime)//' seconds'

    end function time2str
!----------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------
!   Auxiliar conversion functions
!----------------------------------------------------------------------------------------
   function ntoa_int(x) result(res)
       implicit none
       integer(kind=int64), intent(in)     :: x
       character(len=:),allocatable        :: res

       character(len=digits(x))            :: tmp

       write(tmp,'(i0)') x
       res = trim(tmp)
       return
   end function ntoa_int

   function ntoa_real(x) result(res)
       implicit none
       real(kind=real64), intent(in)       :: x
       character(len=:),allocatable        :: res

       character(len=digits(x))            :: tmp

       if (x < 0.5_real64) then
          write(tmp, '(ES15.6E2)') x
       else
          write(tmp, '(f15.6)') x
       end if
       res = trim(adjustl(tmp))
       return
   end function ntoa_real
!------------------------------------------------------------------------------

end module time_utils
