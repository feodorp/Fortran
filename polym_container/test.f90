program test_polyc
    use, intrinsic:: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
    use poly_container
    use time_utils
    implicit none
        integer, parameter                       :: n = 1000000

    call test_elem_math_func()
    call test_array_func()
    ! call test_loc_array_func()

contains
    function to_upper(strIn) result(strOut)
        implicit none
        character(len=*), intent(in)        :: strIn
        character(len=len(strIn))           :: strOut

        integer                             :: i, ic
        do i = 1, len(strIn)
            ic = iachar(strIn(i:i))
            if (ic >= iachar("a") .and. ic <= iachar("z")) then
                strOut(i:i) = achar(ic-32)
            else
                strOut(i:i) = strIn(i:i)
            end if
        end do
    end function to_upper

    subroutine print_function_name(fname)
        implicit none
        character(len=*), intent(in)        :: fname

        print'(/"Function: ",a)', trim(to_upper(fname))
    end subroutine print_function_name

    subroutine print_result(scalar, timer, absres, relerror)
        implicit none
        character(len=*), intent(in)        :: scalar
        type(stopwatch), intent(in)         :: timer
        real(kind=real64), intent(in)       :: absres
        real(kind=real64), intent(in), optional:: relerror

        character(len=:), allocatable       :: test_res

        if (present(relerror)) then
            if (relerror < 10.0_real64*epsilon(relerror)) then
                 test_res = "Test: PASS"
            else
                 test_res = "Test: FAIL"
            end if
        else
            test_res = ""
        end if
        print'(5x,a25, " time: ",f10.8, " sec.", 3x, "Result: ",e16.10, 3x, a)', to_upper(scalar), timer%time(), absres, test_res
    end subroutine print_result

    subroutine print_line()
        print'(a)', repeat('-',100)
    end subroutine print_line

    subroutine print_total_time(c, timer)
        implicit none
        character, intent(in), optional     :: c
        type(stopwatch), intent(in)         :: timer

        if (present(c)) then
            if (c == 'c') print'(a,x,a)', 'all scalar (complex 64 bits) tests:', time2str(timer%total_time())
            if (c == 'r') print'(a,x,a)', 'all scalar (real 64 bits) tests:', time2str(timer%total_time())
            if (c == 'i') print'(a,x,a)', 'all scalar (integer 64 bits) tests', time2str(timer%total_time())
        else
            print'(a,x,a)', 'all container (64 bits) tests', time2str(timer%total_time())
        end if
        print*
    end subroutine

    subroutine test_elem_math_func()
        implicit none
        real(kind=real64)                   :: a_real64(n), sum_real64, x_real64
        real(kind=real64), parameter        :: scal_real64 = 1.0_real64/real(n, kind=real64)
        type(polyc)                         :: a_cont(n), sum_cont, x_cont, error
        type(polyc), parameter              :: scal_cont = polyc(polyc_real,scal_real64)
        type(stopwatch)                     :: t_scalar_real64
        type(stopwatch)                     :: t_cont_real64
        integer                             :: i

        print*
        call print_line()
        print*,achar(27)//"[31m    Running tests for math intrinsic functions:"//achar(27)//"[0m"
        call print_line()

        call print_function_name("acos")

!       Function: ACOS


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = acos(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = acos(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("acosh")

!       Function: ACOSH


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = acosh(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = acosh(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("asin")

!       Function: ASIN


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = asin(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = asin(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("asinh")

!       Function: ASINH


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = asinh(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = asinh(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("atan")

!       Function: ATAN


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = atan(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = atan(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("atanh")

!       Function: ATANH


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = atanh(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = atanh(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("bessel_j0")

!       Function: BESSEL_J0


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = bessel_j0(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = bessel_j0(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("bessel_j1")

!       Function: BESSEL_J1


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = bessel_j1(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = bessel_j1(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("bessel_y0")

!       Function: BESSEL_Y0


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = bessel_y0(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = bessel_y0(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("bessel_y1")

!       Function: BESSEL_Y1


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = bessel_y1(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = bessel_y1(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("cos")

!       Function: COS


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = cos(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = cos(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("cosh")

!       Function: COSH


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = cosh(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = cosh(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("erf")

!       Function: ERF


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = erf(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = erf(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("erfc")

!       Function: ERFC


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = erfc(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = erfc(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("erfc_scaled")

!       Function: ERFC_SCALED


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = erfc_scaled(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = erfc_scaled(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("exp")

!       Function: EXP


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = exp(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = exp(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("gamma")

!       Function: GAMMA


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = gamma(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = gamma(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("hypot")

!       Function: HYPOT


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n-1
            x_real64 = hypot(a_real64(i), a_real64(i+1))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n-1
            x_cont = hypot(a_cont(i), a_cont(i+1))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("log")

!       Function: LOG


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = log(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = log(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("log10")

!       Function: LOG10


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = log10(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = log10(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("log_gamma")

!       Function: LOG_GAMMA


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = log_gamma(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = log_gamma(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("sin")

!       Function: SIN


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = sin(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = sin(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("sinh")

!       Function: SINH


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = sinh(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = sinh(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("sqrt")

!       Function: SQRT


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = sqrt(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = sqrt(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("tan")

!       Function: TAN


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = tan(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = tan(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("tanh")

!       Function: TANH


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = tanh(a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = tanh(a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))

        call print_function_name("Elemental bessel_jn")

!       Function: Elemental BESSEL_JN


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = bessel_jn(2+modulo(i,3), a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = bessel_jn(2+modulo(i,3), a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))

        call print_function_name("Transformational bessel_jn: n1=2, n2=16")
!       Function: Transformational BESSEL_JN


!       Scalar: REAL64
!
!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = maxval(bessel_jn(2, 16, a_real64(i)))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = maxval(bessel_jn(2, 16, a_cont(i)))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))
        call print_function_name("Elemental bessel_yn")

!       Function: Elemental BESSEL_YN


!       Scalar: REAL64
!
!       Scalar tests:
!       Get random array
        call random_number(a_real64(:))
        a_real64(:) = a_real64(:) + 1.0_real64
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = bessel_yn(2+modulo(i,3), a_real64(i))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!       copy scalar random array
        a_cont(:) = a_real64(:)
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = bessel_yn(2+modulo(i,3), a_cont(i))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))

        call print_function_name("Transformational bessel_yn: n1=2, n2=16")
!       Function: Transformational BESSEL_YN


!       Scalar: REAL64
!
!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
        sum_real64 = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_real64 = maxval(bessel_yn(2, 16, a_real64(i)))
            sum_real64 = sum_real64+x_real64*scal_real64
        end do
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(sum_real64),real64))

!       Container test:
!       Inititate time count
        call t_cont_real64%start()
        sum_cont  = 0.0_real64
!       Call function and accumulate normalized result
        do i = 1, n
            x_cont = maxval(bessel_yn(2, 16, a_cont(i)))
            sum_cont = sum_cont+x_cont*scal_cont
        end do
!       Stop time count
        call t_cont_real64%stop()
!       Get relative error between scalar and container result
        error = abs(sum_real64-sum_cont)
        if (abs(sum_real64) /= 0.0_real64 ) error = error/abs(sum_real64)
!       Print info
        call print_result("Container (real64)", t_cont_real64, real(abs(sum_cont)), real(error))

!       Print total time for each variable
        call print_line()
        print*,'TOTAL TIME:'
        print'(a,x,a)', '     REAL64 scalar tests:', time2str(t_scalar_real64%total_time())
        call t_scalar_real64%reset()
        print'(a,x,a)', '     REAL64 container tests:', time2str(t_cont_real64%total_time())
        call t_cont_real64%reset()
    end subroutine

    subroutine test_array_func()
        implicit none
        integer, parameter                  :: n1 = int(real(n, real64)**(1.0_real64/1.0_real64)), &
                                               n2 = int(real(n, real64)**(1.0_real64/2.0_real64)), &
                                               n3 = int(real(n, real64)**(1.0_real64/3.0_real64))
        integer(kind=int64)                 :: a_r1_int64(n1), &
                                               a_r2_int64(n2,n2), &
                                               a_r3_int64(n3,n3,n3)
        integer(kind=int64)                 :: result_int64
        real(kind=real64)                   :: a_r1_real64(n1), &
                                               a_r2_real64(n2,n2), &
                                               a_r3_real64(n3,n3,n3)
        real(kind=real64)                   :: result_real64
        type(polyc)                         :: a_r1_cont(n1), &
                                               a_r2_cont(n2,n2), &
                                               a_r3_cont(n3,n3,n3)
        type(polyc)                         :: result_cont
        type(polyc)                         :: error

        type(stopwatch)                     :: t_scalar_int64
        type(stopwatch)                     :: t_cont_int64
        type(stopwatch)                     :: t_scalar_real64
        type(stopwatch)                     :: t_cont_real64
        integer                             :: i, result

! ARRAY_FUNCTIONS = [('maxval','ir'), ('minval','ir'), ('norm2','r'), ('product','irc'), ('sum','irc')]

! ARRAY_LOC_FUNCTIONS = [('maxloc','ir'), ('minloc','ir'), ('findloc','irc')]

        print*
        call print_line()
        print*,achar(27)//"[31m    Running tests for rank-1 transformational functions that reduce arrays:"//achar(27)//"[0m"
        call print_line()

!       Get random array
        call random_number(a_r1_real64(:))
        a_r1_real64(:) = 0.05_real64*a_r1_real64(:) + 0.975_real64
        a_r1_int64(:) = 3.0_real64*a_r1_real64(:)

!
!       Function: MAXVAL(array(:))
!
        call print_function_name("maxval(array(:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(a_r1_int64(:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_int64(:)
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(a_r1_cont(:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(a_r1_real64(:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_real64(:)
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(a_r1_cont(:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(array(:))
!
        call print_function_name("minval(array(:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(a_r1_int64(:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_int64(:)
        call t_cont_int64%start()
!       Call function
        result_cont = minval(a_r1_cont(:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(a_r1_real64(:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_real64(:)
        call t_cont_real64%start()
!       Call function
        result_cont = minval(a_r1_cont(:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(array(:))
!
        call print_function_name("norm2(array(:))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(a_r1_real64(:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_real64(:)
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(a_r1_cont(:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(array(:))
!
        call print_function_name("product(array(:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(a_r1_int64(:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_int64(:)
        call t_cont_int64%start()
!       Call function
        result_cont = product(a_r1_cont(:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(a_r1_real64(:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_real64(:)
        call t_cont_real64%start()
!       Call function
        result_cont = product(a_r1_cont(:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(array(:))
!
        call print_function_name("sum(array(:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(a_r1_int64(:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_int64(:)
        call t_cont_int64%start()
!       Call function
        result_cont = sum(a_r1_cont(:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(a_r1_real64(:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r1_cont(:) = a_r1_real64(:)
        call t_cont_real64%start()
!       Call function
        result_cont = sum(a_r1_cont(:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

        print*
        call print_line()
        print*,achar(27)//"[31m    Running tests for rank-2 transformational functions that reduce arrays:"//achar(27)//"[0m"
        call print_line()

!       Get random array
        call random_number(a_r2_real64(:,:))
        a_r2_real64(:,:) = 0.05_real64*a_r2_real64(:,:) + 0.975_real64
        a_r2_int64(:,:) = 3.0_real64*a_r2_real64(:,:)

!
!       Function: MAXVAL(array(:,:))
!
        call print_function_name("maxval(array(:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(a_r2_int64(:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_int64(:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(a_r2_cont(:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(a_r2_real64(:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_real64(:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(a_r2_cont(:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MAXVAL(MAXVAL(array(:,:),dim=1))
!
        call print_function_name("maxval(maxval(array(:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(maxval(a_r2_int64(:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(maxval(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(maxval(a_r2_real64(:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(maxval(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MAXVAL(MAXVAL(array(:,:),dim=2))
!
        call print_function_name("maxval(maxval(array(:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(maxval(a_r2_int64(:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(maxval(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(maxval(a_r2_real64(:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(maxval(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(array(:,:))
!
        call print_function_name("minval(array(:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(a_r2_int64(:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_int64(:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = minval(a_r2_cont(:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(a_r2_real64(:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_real64(:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = minval(a_r2_cont(:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(MINVAL(array(:,:),dim=1))
!
        call print_function_name("minval(minval(array(:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(minval(a_r2_int64(:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = minval(minval(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(minval(a_r2_real64(:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = minval(minval(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(MINVAL(array(:,:),dim=2))
!
        call print_function_name("minval(minval(array(:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(minval(a_r2_int64(:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = minval(minval(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(minval(a_r2_real64(:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = minval(minval(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(array(:,:))
!
        call print_function_name("norm2(array(:,:))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(a_r2_real64(:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_real64(:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(a_r2_cont(:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(NORM2(array(:,:),dim=1))
!
        call print_function_name("norm2(norm2(array(:,:),dim=1))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(norm2(a_r2_real64(:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(norm2(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(NORM2(array(:,:),dim=2))
!
        call print_function_name("norm2(norm2(array(:,:),dim=2))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(norm2(a_r2_real64(:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(norm2(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(array(:,:))
!
        call print_function_name("product(array(:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(a_r2_int64(:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_int64(:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = product(a_r2_cont(:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(a_r2_real64(:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_real64(:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = product(a_r2_cont(:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(PRODUCT(array(:,:),dim=1))
!
        call print_function_name("product(product(array(:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(product(a_r2_int64(:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = product(product(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(product(a_r2_real64(:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = product(product(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(PRODUCT(array(:,:),dim=2))
!
        call print_function_name("product(product(array(:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(product(a_r2_int64(:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = product(product(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(product(a_r2_real64(:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = product(product(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(array(:,:))
!
        call print_function_name("sum(array(:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(a_r2_int64(:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_int64(:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = sum(a_r2_cont(:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(a_r2_real64(:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r2_cont(:,:) = a_r2_real64(:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = sum(a_r2_cont(:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(SUM(array(:,:),dim=1))
!
        call print_function_name("sum(sum(array(:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(sum(a_r2_int64(:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = sum(sum(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(sum(a_r2_real64(:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = sum(sum(a_r2_cont(:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(SUM(array(:,:),dim=2))
!
        call print_function_name("sum(sum(array(:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(sum(a_r2_int64(:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_int64(:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = sum(sum(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(sum(a_r2_real64(:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r2_cont(:,:) = a_r2_real64(:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = sum(sum(a_r2_cont(:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

        print*
        call print_line()
        print*,achar(27)//"[31m    Running tests for rank-3 transformational functions that reduce arrays:"//achar(27)//"[0m"
        call print_line()

!       Get random array
        call random_number(a_r3_real64(:,:,:))
        a_r3_real64(:,:,:) = 0.05_real64*a_r3_real64(:,:,:) + 0.975_real64
        a_r3_int64(:,:,:) = 3.0_real64*a_r3_real64(:,:,:)

!
!       Function: MAXVAL(array(:,:,:))
!
        call print_function_name("maxval(array(:,:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(a_r3_int64(:,:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(a_r3_real64(:,:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MAXVAL(MAXVAL(array(:,:,:),dim=1))
!
        call print_function_name("maxval(maxval(array(:,:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(maxval(a_r3_int64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(maxval(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(maxval(a_r3_real64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(maxval(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MAXVAL(MAXVAL(array(:,:,:),dim=2))
!
        call print_function_name("maxval(maxval(array(:,:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(maxval(a_r3_int64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(maxval(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(maxval(a_r3_real64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(maxval(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MAXVAL(MAXVAL(array(:,:,:),dim=3))
!
        call print_function_name("maxval(maxval(array(:,:,:),dim=3))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = maxval(maxval(a_r3_int64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = maxval(maxval(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = maxval(maxval(a_r3_real64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = maxval(maxval(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(array(:,:,:))
!
        call print_function_name("minval(array(:,:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(a_r3_int64(:,:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = minval(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(a_r3_real64(:,:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = minval(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(MINVAL(array(:,:,:),dim=1))
!
        call print_function_name("minval(minval(array(:,:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(minval(a_r3_int64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = minval(minval(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(minval(a_r3_real64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = minval(minval(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(MINVAL(array(:,:,:),dim=2))
!
        call print_function_name("minval(minval(array(:,:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(minval(a_r3_int64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = minval(minval(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(minval(a_r3_real64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = minval(minval(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: MINVAL(MINVAL(array(:,:,:),dim=3))
!
        call print_function_name("minval(minval(array(:,:,:),dim=3))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = minval(minval(a_r3_int64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = minval(minval(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = minval(minval(a_r3_real64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = minval(minval(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(array(:,:,:))
!
        call print_function_name("norm2(array(:,:,:))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(a_r3_real64(:,:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(NORM2(array(:,:,:),dim=1))
!
        call print_function_name("norm2(norm2(array(:,:,:),dim=1))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(norm2(a_r3_real64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(norm2(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(NORM2(array(:,:,:),dim=2))
!
        call print_function_name("norm2(norm2(array(:,:,:),dim=2))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(norm2(a_r3_real64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(norm2(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: NORM2(NORM2(array(:,:,:),dim=3))
!
        call print_function_name("norm2(norm2(array(:,:,:),dim=3))")

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = norm2(norm2(a_r3_real64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = norm2(norm2(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(array(:,:,:))
!
        call print_function_name("product(array(:,:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(a_r3_int64(:,:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = product(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(a_r3_real64(:,:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = product(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(PRODUCT(array(:,:,:),dim=1))
!
        call print_function_name("product(product(array(:,:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(product(a_r3_int64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = product(product(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(product(a_r3_real64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = product(product(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(PRODUCT(array(:,:,:),dim=2))
!
        call print_function_name("product(product(array(:,:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(product(a_r3_int64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = product(product(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(product(a_r3_real64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = product(product(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: PRODUCT(PRODUCT(array(:,:,:),dim=3))
!
        call print_function_name("product(product(array(:,:,:),dim=3))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = product(product(a_r3_int64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = product(product(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = product(product(a_r3_real64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = product(product(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(array(:,:,:))
!
        call print_function_name("sum(array(:,:,:))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(a_r3_int64(:,:,:))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
        call t_cont_int64%start()
!       Call function
        result_cont = sum(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(a_r3_real64(:,:,:))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
!       Inititate time count
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
        call t_cont_real64%start()
!       Call function
        result_cont = sum(a_r3_cont(:,:,:))
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(SUM(array(:,:,:),dim=1))
!
        call print_function_name("sum(sum(array(:,:,:),dim=1))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(sum(a_r3_int64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = sum(sum(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(sum(a_r3_real64(:,:,:),dim=1))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = sum(sum(a_r3_cont(:,:,:),dim=1,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(SUM(array(:,:,:),dim=2))
!
        call print_function_name("sum(sum(array(:,:,:),dim=2))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(sum(a_r3_int64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = sum(sum(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(sum(a_r3_real64(:,:,:),dim=2))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = sum(sum(a_r3_cont(:,:,:),dim=2,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))

!
!       Function: SUM(SUM(array(:,:,:),dim=3))
!
        call print_function_name("sum(sum(array(:,:,:),dim=3))")

!       Scalar: INT64

!       Scalar tests:
!       Inititate time count
        call t_scalar_int64%start()
!       Call function
        result_int64 = sum(sum(a_r3_int64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_int64%stop()
!       Print info
        call print_result("int64", t_scalar_int64, real(abs(result_int64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_int64(:,:,:)
!       Inititate time count
        call t_cont_int64%start()
!       Call function
        result_cont = sum(sum(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_int64%stop()
!       Print info
        error = abs(result_int64-result_cont)
        if (abs(result_int64) /= 0.0_real64 ) error = error/abs(result_int64)
        call print_result("Container (int64)", t_cont_int64, real(abs(result_cont)), real(error))

!       Scalar: REAL64

!       Scalar tests:
!       Inititate time count
        call t_scalar_real64%start()
!       Call function
        result_real64 = sum(sum(a_r3_real64(:,:,:),dim=3))
!       Stop time count
        call t_scalar_real64%stop()
!       Print info
        call print_result("real64", t_scalar_real64, real(abs(result_real64),real64))

!       Container tests:
        a_r3_cont(:,:,:) = a_r3_real64(:,:,:)
!       Inititate time count
        call t_cont_real64%start()
!       Call function
        result_cont = sum(sum(a_r3_cont(:,:,:),dim=3,uniform=.true.),uniform=.true.)
!       Stop time count
        call t_cont_real64%stop()
!       Print info
        error = abs(result_real64-result_cont)
        if (abs(result_real64) /= 0.0_real64 ) error = error/abs(result_real64)
        call print_result("Container (real64)", t_cont_real64, real(abs(result_cont)), real(error))


!       Print total time for each variable
        call print_line()
        print*,'TOTAL TIME:'
        print'(a,x,a)', '     INT64 scalar tests:', time2str(t_scalar_int64%total_time())
        call t_scalar_int64%reset()
        print'(a,x,a)', '     INT64 container tests:', time2str(t_cont_int64%total_time())
        call t_cont_int64%reset()
        print'(a,x,a)', '     REAL64 scalar tests:', time2str(t_scalar_real64%total_time())
        call t_scalar_real64%reset()
        print'(a,x,a)', '     REAL64 container tests:', time2str(t_cont_real64%total_time())
        call t_cont_real64%reset()
    end subroutine test_array_func
end
