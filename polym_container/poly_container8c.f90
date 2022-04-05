module poly_container

    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
    implicit none
    private

    integer(kind=int8), public, parameter   :: polyc_unset      = 0_int8, &
                                               polyc_integer    = 1_int8, &
                                               polyc_real       = 2_int8

    public :: assignment(=), operator(<), operator(<=), operator(>), &
              operator(>=), operator(==), operator(/=), operator(+), &
              operator(-), operator(*), operator(/), operator(**), &
              is_integer, is_real, kind, is_uniform, acos, acosh, asin, &
              asinh, atan, atanh, bessel_j0, bessel_j1, bessel_y0, &
              bessel_y1, cos, cosh, erf, erfc, erfc_scaled, exp, gamma, &
              hypot, log, log10, log_gamma, sin, sinh, sqrt, tan, tanh, &
              bessel_jn, bessel_yn, abs, aimag, aint, anint, ceiling, &
              cmplx, floor, int, nint, real, conjg, max, min, mod, modulo, &
              sign, maxval, minval, norm2, product, sum, dot_product, &
              matmul

    ! scalar polymorphic container
    type, public :: polyc
        integer(kind=int8)                  :: kind = polyc_unset
        real(kind=real64)                   :: x = 0.0_real64
    end type polyc

    interface polyc
        module procedure polyc_int8, &
                         polyc_int16, &
                         polyc_int32, &
                         polyc_int64, &
                         polyc_real32, &
                         polyc_real64, &
                         polyc_real128, &
                         polyc_complex32, &
                         polyc_complex64, &
                         polyc_complex128
    end interface polyc

    interface is_integer
        module procedure is_integer_polyc
    end interface is_integer

    interface is_real
        module procedure is_real_polyc
    end interface is_real

    interface kind
        module procedure kind_polyc
    end interface kind

    interface is_uniform
        module procedure is_uniform_r1_polyc, &
                         is_uniform_r2_polyc, &
                         is_uniform_r3_polyc
    end interface is_uniform

    interface assignment(=)
        module procedure assign_polyc_polyc, &
                         assign_polyc_int8, &
                         assign_polyc_int16, &
                         assign_polyc_int32, &
                         assign_polyc_int64, &
                         assign_polyc_real32, &
                         assign_polyc_real64, &
                         assign_polyc_real128, &
                         assign_polyc_complex32, &
                         assign_polyc_complex64, &
                         assign_polyc_complex128, &
                         assign_int8_polyc, &
                         assign_int16_polyc, &
                         assign_int32_polyc, &
                         assign_int64_polyc, &
                         assign_real32_polyc, &
                         assign_real64_polyc, &
                         assign_real128_polyc, &
                         assign_complex32_polyc, &
                         assign_complex64_polyc, &
                         assign_complex128_polyc
    end interface assignment(=)

    interface operator(<)
        module procedure lt_polyc_polyc, &
                         lt_polyc_int8, &
                         lt_polyc_int16, &
                         lt_polyc_int32, &
                         lt_polyc_int64, &
                         lt_polyc_real32, &
                         lt_polyc_real64, &
                         lt_polyc_real128, &
                         lt_polyc_complex32, &
                         lt_polyc_complex64, &
                         lt_polyc_complex128, &
                         lt_int8_polyc, &
                         lt_int16_polyc, &
                         lt_int32_polyc, &
                         lt_int64_polyc, &
                         lt_real32_polyc, &
                         lt_real64_polyc, &
                         lt_real128_polyc, &
                         lt_complex32_polyc, &
                         lt_complex64_polyc, &
                         lt_complex128_polyc
    end interface operator(<)

    interface operator(<=)
        module procedure le_polyc_polyc, &
                         le_polyc_int8, &
                         le_polyc_int16, &
                         le_polyc_int32, &
                         le_polyc_int64, &
                         le_polyc_real32, &
                         le_polyc_real64, &
                         le_polyc_real128, &
                         le_polyc_complex32, &
                         le_polyc_complex64, &
                         le_polyc_complex128, &
                         le_int8_polyc, &
                         le_int16_polyc, &
                         le_int32_polyc, &
                         le_int64_polyc, &
                         le_real32_polyc, &
                         le_real64_polyc, &
                         le_real128_polyc, &
                         le_complex32_polyc, &
                         le_complex64_polyc, &
                         le_complex128_polyc
    end interface operator(<=)

    interface operator(>)
        module procedure gt_polyc_polyc, &
                         gt_polyc_int8, &
                         gt_polyc_int16, &
                         gt_polyc_int32, &
                         gt_polyc_int64, &
                         gt_polyc_real32, &
                         gt_polyc_real64, &
                         gt_polyc_real128, &
                         gt_polyc_complex32, &
                         gt_polyc_complex64, &
                         gt_polyc_complex128, &
                         gt_int8_polyc, &
                         gt_int16_polyc, &
                         gt_int32_polyc, &
                         gt_int64_polyc, &
                         gt_real32_polyc, &
                         gt_real64_polyc, &
                         gt_real128_polyc, &
                         gt_complex32_polyc, &
                         gt_complex64_polyc, &
                         gt_complex128_polyc
    end interface operator(>)

    interface operator(>=)
        module procedure ge_polyc_polyc, &
                         ge_polyc_int8, &
                         ge_polyc_int16, &
                         ge_polyc_int32, &
                         ge_polyc_int64, &
                         ge_polyc_real32, &
                         ge_polyc_real64, &
                         ge_polyc_real128, &
                         ge_polyc_complex32, &
                         ge_polyc_complex64, &
                         ge_polyc_complex128, &
                         ge_int8_polyc, &
                         ge_int16_polyc, &
                         ge_int32_polyc, &
                         ge_int64_polyc, &
                         ge_real32_polyc, &
                         ge_real64_polyc, &
                         ge_real128_polyc, &
                         ge_complex32_polyc, &
                         ge_complex64_polyc, &
                         ge_complex128_polyc
    end interface operator(>=)

    interface operator(==)
        module procedure eq_polyc_polyc, &
                         eq_polyc_int8, &
                         eq_polyc_int16, &
                         eq_polyc_int32, &
                         eq_polyc_int64, &
                         eq_polyc_real32, &
                         eq_polyc_real64, &
                         eq_polyc_real128, &
                         eq_polyc_complex32, &
                         eq_polyc_complex64, &
                         eq_polyc_complex128, &
                         eq_int8_polyc, &
                         eq_int16_polyc, &
                         eq_int32_polyc, &
                         eq_int64_polyc, &
                         eq_real32_polyc, &
                         eq_real64_polyc, &
                         eq_real128_polyc, &
                         eq_complex32_polyc, &
                         eq_complex64_polyc, &
                         eq_complex128_polyc
    end interface operator(==)

    interface operator(/=)
        module procedure ne_polyc_polyc, &
                         ne_polyc_int8, &
                         ne_polyc_int16, &
                         ne_polyc_int32, &
                         ne_polyc_int64, &
                         ne_polyc_real32, &
                         ne_polyc_real64, &
                         ne_polyc_real128, &
                         ne_polyc_complex32, &
                         ne_polyc_complex64, &
                         ne_polyc_complex128, &
                         ne_int8_polyc, &
                         ne_int16_polyc, &
                         ne_int32_polyc, &
                         ne_int64_polyc, &
                         ne_real32_polyc, &
                         ne_real64_polyc, &
                         ne_real128_polyc, &
                         ne_complex32_polyc, &
                         ne_complex64_polyc, &
                         ne_complex128_polyc
    end interface operator(/=)

    interface operator(+)
        module procedure add_polyc_polyc, &
                         add_polyc, &
                         add_polyc_int8, &
                         add_polyc_int16, &
                         add_polyc_int32, &
                         add_polyc_int64, &
                         add_polyc_real32, &
                         add_polyc_real64, &
                         add_polyc_real128, &
                         add_polyc_complex32, &
                         add_polyc_complex64, &
                         add_polyc_complex128, &
                         add_int8_polyc, &
                         add_int16_polyc, &
                         add_int32_polyc, &
                         add_int64_polyc, &
                         add_real32_polyc, &
                         add_real64_polyc, &
                         add_real128_polyc, &
                         add_complex32_polyc, &
                         add_complex64_polyc, &
                         add_complex128_polyc
    end interface operator(+)

    interface operator(-)
        module procedure subtract_polyc_polyc, &
                         subtract_polyc, &
                         subtract_polyc_int8, &
                         subtract_polyc_int16, &
                         subtract_polyc_int32, &
                         subtract_polyc_int64, &
                         subtract_polyc_real32, &
                         subtract_polyc_real64, &
                         subtract_polyc_real128, &
                         subtract_polyc_complex32, &
                         subtract_polyc_complex64, &
                         subtract_polyc_complex128, &
                         subtract_int8_polyc, &
                         subtract_int16_polyc, &
                         subtract_int32_polyc, &
                         subtract_int64_polyc, &
                         subtract_real32_polyc, &
                         subtract_real64_polyc, &
                         subtract_real128_polyc, &
                         subtract_complex32_polyc, &
                         subtract_complex64_polyc, &
                         subtract_complex128_polyc
    end interface operator(-)

    interface operator(*)
        module procedure multiply_polyc_polyc, &
                         multiply_polyc_int8, &
                         multiply_polyc_int16, &
                         multiply_polyc_int32, &
                         multiply_polyc_int64, &
                         multiply_polyc_real32, &
                         multiply_polyc_real64, &
                         multiply_polyc_real128, &
                         multiply_polyc_complex32, &
                         multiply_polyc_complex64, &
                         multiply_polyc_complex128, &
                         multiply_int8_polyc, &
                         multiply_int16_polyc, &
                         multiply_int32_polyc, &
                         multiply_int64_polyc, &
                         multiply_real32_polyc, &
                         multiply_real64_polyc, &
                         multiply_real128_polyc, &
                         multiply_complex32_polyc, &
                         multiply_complex64_polyc, &
                         multiply_complex128_polyc
    end interface operator(*)

    interface operator(/)
        module procedure divide_polyc_polyc, &
                         divide_polyc_int8, &
                         divide_polyc_int16, &
                         divide_polyc_int32, &
                         divide_polyc_int64, &
                         divide_polyc_real32, &
                         divide_polyc_real64, &
                         divide_polyc_real128, &
                         divide_polyc_complex32, &
                         divide_polyc_complex64, &
                         divide_polyc_complex128, &
                         divide_int8_polyc, &
                         divide_int16_polyc, &
                         divide_int32_polyc, &
                         divide_int64_polyc, &
                         divide_real32_polyc, &
                         divide_real64_polyc, &
                         divide_real128_polyc, &
                         divide_complex32_polyc, &
                         divide_complex64_polyc, &
                         divide_complex128_polyc
    end interface operator(/)

    interface operator(**)
        module procedure power_polyc_polyc, &
                         power_polyc_int8, &
                         power_polyc_int16, &
                         power_polyc_int32, &
                         power_polyc_int64, &
                         power_polyc_real32, &
                         power_polyc_real64, &
                         power_polyc_real128, &
                         power_polyc_complex32, &
                         power_polyc_complex64, &
                         power_polyc_complex128, &
                         power_int8_polyc, &
                         power_int16_polyc, &
                         power_int32_polyc, &
                         power_int64_polyc, &
                         power_real32_polyc, &
                         power_real64_polyc, &
                         power_real128_polyc, &
                         power_complex32_polyc, &
                         power_complex64_polyc, &
                         power_complex128_polyc
    end interface operator(**)


    interface acos
        module procedure acos_polyc
    end interface acos

    interface acosh
        module procedure acosh_polyc
    end interface acosh

    interface asin
        module procedure asin_polyc
    end interface asin

    interface asinh
        module procedure asinh_polyc
    end interface asinh

    interface atan
        module procedure atan_polyc
    end interface atan

    interface atanh
        module procedure atanh_polyc
    end interface atanh

    interface bessel_j0
        module procedure bessel_j0_polyc
    end interface bessel_j0

    interface bessel_j1
        module procedure bessel_j1_polyc
    end interface bessel_j1

    interface bessel_y0
        module procedure bessel_y0_polyc
    end interface bessel_y0

    interface bessel_y1
        module procedure bessel_y1_polyc
    end interface bessel_y1

    interface cos
        module procedure cos_polyc
    end interface cos

    interface cosh
        module procedure cosh_polyc
    end interface cosh

    interface erf
        module procedure erf_polyc
    end interface erf

    interface erfc
        module procedure erfc_polyc
    end interface erfc

    interface erfc_scaled
        module procedure erfc_scaled_polyc
    end interface erfc_scaled

    interface exp
        module procedure exp_polyc
    end interface exp

    interface gamma
        module procedure gamma_polyc
    end interface gamma

    interface hypot
        module procedure hypot_polyc
    end interface hypot

    interface log
        module procedure log_polyc
    end interface log

    interface log10
        module procedure log10_polyc
    end interface log10

    interface log_gamma
        module procedure log_gamma_polyc
    end interface log_gamma

    interface sin
        module procedure sin_polyc
    end interface sin

    interface sinh
        module procedure sinh_polyc
    end interface sinh

    interface sqrt
        module procedure sqrt_polyc
    end interface sqrt

    interface tan
        module procedure tan_polyc
    end interface tan

    interface tanh
        module procedure tanh_polyc
    end interface tanh

    interface abs
        module procedure abs_polyc
    end interface abs

    interface aimag
        module procedure aimag_polyc
    end interface aimag

    interface aint
        module procedure aint_polyc
    end interface aint

    interface anint
        module procedure anint_polyc
    end interface anint

    interface ceiling
        module procedure ceiling_polyc
    end interface ceiling

    interface cmplx
        module procedure cmplx_polyc
    end interface cmplx

    interface floor
        module procedure floor_polyc
    end interface floor

    interface int
        module procedure int_polyc
    end interface int

    interface nint
        module procedure nint_polyc
    end interface nint

    interface real
        module procedure real_polyc
    end interface real

    interface conjg
        module procedure conjg_polyc
    end interface conjg

    interface max
        module procedure max_polyc
    end interface max

    interface min
        module procedure min_polyc
    end interface min

    interface mod
        module procedure mod_polyc
    end interface mod

    interface modulo
        module procedure modulo_polyc
    end interface modulo

    interface sign
        module procedure sign_polyc
    end interface sign

    interface bessel_jn
        module procedure bessel_jn_int8_polyc, &
                         bessel_jn_int16_polyc, &
                         bessel_jn_int32_polyc, &
                         bessel_jn_int64_polyc, &
                         bessel_jn_int8_int8_polyc, &
                         bessel_jn_int8_int16_polyc, &
                         bessel_jn_int8_int32_polyc, &
                         bessel_jn_int8_int64_polyc, &
                         bessel_jn_int16_int8_polyc, &
                         bessel_jn_int16_int16_polyc, &
                         bessel_jn_int16_int32_polyc, &
                         bessel_jn_int16_int64_polyc, &
                         bessel_jn_int32_int8_polyc, &
                         bessel_jn_int32_int16_polyc, &
                         bessel_jn_int32_int32_polyc, &
                         bessel_jn_int32_int64_polyc, &
                         bessel_jn_int64_int8_polyc, &
                         bessel_jn_int64_int16_polyc, &
                         bessel_jn_int64_int32_polyc, &
                         bessel_jn_int64_int64_polyc
    end interface bessel_jn

    interface bessel_yn
        module procedure bessel_yn_int8_polyc, &
                         bessel_yn_int16_polyc, &
                         bessel_yn_int32_polyc, &
                         bessel_yn_int64_polyc, &
                         bessel_yn_int8_int8_polyc, &
                         bessel_yn_int8_int16_polyc, &
                         bessel_yn_int8_int32_polyc, &
                         bessel_yn_int8_int64_polyc, &
                         bessel_yn_int16_int8_polyc, &
                         bessel_yn_int16_int16_polyc, &
                         bessel_yn_int16_int32_polyc, &
                         bessel_yn_int16_int64_polyc, &
                         bessel_yn_int32_int8_polyc, &
                         bessel_yn_int32_int16_polyc, &
                         bessel_yn_int32_int32_polyc, &
                         bessel_yn_int32_int64_polyc, &
                         bessel_yn_int64_int8_polyc, &
                         bessel_yn_int64_int16_polyc, &
                         bessel_yn_int64_int32_polyc, &
                         bessel_yn_int64_int64_polyc
    end interface bessel_yn

    interface dot_product
        module procedure dot_product_polyc
    end interface dot_product

    interface matmul
        module procedure matmul_r2_r2_polyc, &
                         matmul_r2_r1_polyc, &
                         matmul_r1_r2_polyc
    end interface matmul

    interface maxval
        module procedure maxval_r1_polyc, &
                         maxval_r2_polyc, &
                         maxval_r3_polyc, &
                         maxval_r1_dim_polyc, &
                         maxval_r2_dim_polyc, &
                         maxval_r3_dim_polyc
    end interface maxval

    interface minval
        module procedure minval_r1_polyc, &
                         minval_r2_polyc, &
                         minval_r3_polyc, &
                         minval_r1_dim_polyc, &
                         minval_r2_dim_polyc, &
                         minval_r3_dim_polyc
    end interface minval

    interface norm2
        module procedure norm2_r1_polyc, &
                         norm2_r2_polyc, &
                         norm2_r3_polyc, &
                         norm2_r1_dim_polyc, &
                         norm2_r2_dim_polyc, &
                         norm2_r3_dim_polyc
    end interface norm2

    interface product
        module procedure product_r1_polyc, &
                         product_r2_polyc, &
                         product_r3_polyc, &
                         product_r1_dim_polyc, &
                         product_r2_dim_polyc, &
                         product_r3_dim_polyc
    end interface product

    interface sum
        module procedure sum_r1_polyc, &
                         sum_r2_polyc, &
                         sum_r3_polyc, &
                         sum_r1_dim_polyc, &
                         sum_r2_dim_polyc, &
                         sum_r3_dim_polyc
    end interface sum

    interface maxloc
        module procedure maxloc_r1_polyc, &
                         maxloc_r2_polyc, &
                         maxloc_r3_polyc, &
                         maxloc_r1_dim_polyc, &
                         maxloc_r2_dim_polyc, &
                         maxloc_r3_dim_polyc
    end interface maxloc

    interface minloc
        module procedure minloc_r1_polyc, &
                         minloc_r2_polyc, &
                         minloc_r3_polyc, &
                         minloc_r1_dim_polyc, &
                         minloc_r2_dim_polyc, &
                         minloc_r3_dim_polyc
    end interface minloc

    interface findloc
        module procedure findloc_r1_polyc, &
                         findloc_r2_polyc, &
                         findloc_r3_polyc, &
                         findloc_r1_dim_polyc, &
                         findloc_r2_dim_polyc, &
                         findloc_r3_dim_polyc
    end interface findloc

contains
!----------------------------------------------------------------------------------------
!        Scalar to container convertion
!----------------------------------------------------------------------------------------
    elemental type(polyc) function polyc_int8(a) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: a

        c%kind = polyc_integer
        c%x = transfer(int(a,kind=int64),c%x)
    end function polyc_int8

    elemental type(polyc) function polyc_int16(a) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: a

        c%kind = polyc_integer
        c%x = transfer(int(a,kind=int64),c%x)
    end function polyc_int16

    elemental type(polyc) function polyc_int32(a) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: a

        c%kind = polyc_integer
        c%x = transfer(int(a,kind=int64),c%x)
    end function polyc_int32

    elemental type(polyc) function polyc_int64(a) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: a

        c%kind = polyc_integer
        c%x = transfer(a,c%x)
    end function polyc_int64

    elemental type(polyc) function polyc_real32(a) result(c)
        implicit none
        real(kind=real32), intent(in)       :: a

        c%kind = polyc_real
        c%x = a
    end function polyc_real32

    elemental type(polyc) function polyc_real64(a) result(c)
        implicit none
        real(kind=real64), intent(in)       :: a

        c%kind = polyc_real
        c%x = a
    end function polyc_real64

    elemental type(polyc) function polyc_real128(a) result(c)
        implicit none
        real(kind=real128), intent(in)      :: a

        c%kind = polyc_real
        c%x = a
    end function polyc_real128

    elemental type(polyc) function polyc_complex32(a) result(c)
        implicit none
        complex(kind=real32), intent(in)    :: a

        c%kind = polyc_real
        c%x = a
    end function polyc_complex32

    elemental type(polyc) function polyc_complex64(a) result(c)
        implicit none
        complex(kind=real64), intent(in)    :: a

        c%kind = polyc_real
        c%x = a
    end function polyc_complex64

    elemental type(polyc) function polyc_complex128(a) result(c)
        implicit none
        complex(kind=real128), intent(in)   :: a

        c%kind = polyc_real
        c%x = a
    end function polyc_complex128

!----------------------------------------------------------------------------------------
!        Container inquiring function
!----------------------------------------------------------------------------------------
    elemental logical function is_integer_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        if (a%kind == polyc_integer) then
            c = .true.
        else
            c = .false.
        end if
    end function is_integer_polyc

    elemental logical function is_real_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        if (a%kind == polyc_real) then
            c = .true.
        else
            c = .false.
        end if
    end function is_real_polyc

    elemental integer(kind=int8) function kind_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        c = a%kind
    end function kind_polyc

    pure logical function is_uniform_r1_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:)

        if (any(a(:)%kind /= a(1)%kind)) then
            c = .false.
        else
            c = .true.
        end if
    end function is_uniform_r1_polyc

    pure logical function is_uniform_r2_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:,:)

        if (any(a(:,:)%kind /= a(1,1)%kind)) then
            c = .false.
        else
            c = .true.
        end if
    end function is_uniform_r2_polyc

    pure logical function is_uniform_r3_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:,:,:)

        if (any(a(:,:,:)%kind /= a(1,1,1)%kind)) then
            c = .false.
        else
            c = .true.
        end if
    end function is_uniform_r3_polyc


!----------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------
!       Assignment operation
!----------------------------------------------------------------------------------------
    elemental subroutine assign_polyc_polyc(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        type(polyc), intent(in)             :: b
        a%kind = b%kind
        a%x = b%x
    end subroutine assign_polyc_polyc

    elemental subroutine assign_polyc_int8(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        integer(kind=int8), intent(in)      :: b

        a%kind = polyc_integer
        a%x = transfer(int(b,kind=int64),a%x)
    end subroutine assign_polyc_int8

    elemental subroutine assign_polyc_int16(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        integer(kind=int16), intent(in)     :: b

        a%kind = polyc_integer
        a%x = transfer(int(b,kind=int64),a%x)
    end subroutine assign_polyc_int16

    elemental subroutine assign_polyc_int32(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        integer(kind=int32), intent(in)     :: b

        a%kind = polyc_integer
        a%x = transfer(int(b,kind=int64),a%x)
    end subroutine assign_polyc_int32

    elemental subroutine assign_polyc_int64(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        integer(kind=int64), intent(in)     :: b

        a%kind = polyc_integer
        a%x = transfer(b,a%x)
    end subroutine assign_polyc_int64

    elemental subroutine assign_polyc_real32(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        real(kind=real32), intent(in)       :: b

        a%kind = polyc_real
        a%x = b
    end subroutine assign_polyc_real32

    elemental subroutine assign_polyc_real64(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        real(kind=real64), intent(in)       :: b

        a%kind = polyc_real
        a%x = b
    end subroutine assign_polyc_real64

    elemental subroutine assign_polyc_real128(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        real(kind=real128), intent(in)      :: b

        a%kind = polyc_real
        a%x = b
    end subroutine assign_polyc_real128

    elemental subroutine assign_polyc_complex32(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        complex(kind=real32), intent(in)    :: b

        a%kind = polyc_real
        a%x = b
    end subroutine assign_polyc_complex32

    elemental subroutine assign_polyc_complex64(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        complex(kind=real64), intent(in)    :: b

        a%kind = polyc_real
        a%x = b
    end subroutine assign_polyc_complex64

    elemental subroutine assign_polyc_complex128(a,b)
        implicit none
        type(polyc), intent(out)            :: a
        complex(kind=real128), intent(in)   :: b

        a%kind = polyc_real
        a%x = b
    end subroutine assign_polyc_complex128

    elemental subroutine assign_int8_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(out)     :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_int8_polyc

    elemental subroutine assign_int16_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(out)    :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_int16_polyc

    elemental subroutine assign_int32_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(out)    :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_int32_polyc

    elemental subroutine assign_int64_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(out)    :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_int64_polyc

    elemental subroutine assign_real32_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(out)      :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_real32_polyc

    elemental subroutine assign_real64_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(out)      :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_real64_polyc

    elemental subroutine assign_real128_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(out)     :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_real128_polyc

    elemental subroutine assign_complex32_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(out)   :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_complex32_polyc

    elemental subroutine assign_complex64_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(out)   :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_complex64_polyc

    elemental subroutine assign_complex128_polyc(b,a)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(out)  :: b

        if (a%kind == polyc_integer) then
            b = transfer(a%x,0_int64)
        else
            b = a%x
        end if
    end subroutine assign_complex128_polyc

!----------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------
!       Logical operations: '<','<=','>','>=','==','/='
!----------------------------------------------------------------------------------------
    logical elemental function lt_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if

    end function lt_polyc_polyc

    logical elemental function lt_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_int8

    logical elemental function lt_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_int16

    logical elemental function lt_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_int32

    logical elemental function lt_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_int64

    logical elemental function lt_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_real32

    logical elemental function lt_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_real64

    logical elemental function lt_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b
        else
            c = a%x < b
        end if
    end function lt_polyc_real128

    logical elemental function lt_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b%re
        else
            c = a%x < b%re
        end if
    end function lt_polyc_complex32

    logical elemental function lt_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b%re
        else
            c = a%x < b%re
        end if
    end function lt_polyc_complex64

    logical elemental function lt_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) < b%re
        else
            c = a%x < b%re
        end if
    end function lt_polyc_complex128

    logical elemental function lt_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_int8_polyc

    logical elemental function lt_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_int16_polyc

    logical elemental function lt_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_int32_polyc

    logical elemental function lt_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_int64_polyc

    logical elemental function lt_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_real32_polyc

    logical elemental function lt_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_real64_polyc

    logical elemental function lt_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b < transfer(a%x,0_int64)
        else
            c = b < a%x
        end if

    end function lt_real128_polyc

    logical elemental function lt_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re < transfer(a%x,0_int64)
        else
            c = b%re < a%x
        end if

    end function lt_complex32_polyc

    logical elemental function lt_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re < transfer(a%x,0_int64)
        else
            c = b%re < a%x
        end if

    end function lt_complex64_polyc

    logical elemental function lt_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = b%re < transfer(a%x,0_int64)
        else
            c = b%re < a%x
        end if

    end function lt_complex128_polyc

    logical elemental function le_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if

    end function le_polyc_polyc

    logical elemental function le_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_int8

    logical elemental function le_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_int16

    logical elemental function le_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_int32

    logical elemental function le_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_int64

    logical elemental function le_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_real32

    logical elemental function le_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_real64

    logical elemental function le_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b
        else
            c = a%x <= b
        end if
    end function le_polyc_real128

    logical elemental function le_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b%re
        else
            c = a%x <= b%re
        end if
    end function le_polyc_complex32

    logical elemental function le_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b%re
        else
            c = a%x <= b%re
        end if
    end function le_polyc_complex64

    logical elemental function le_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) <= b%re
        else
            c = a%x <= b%re
        end if
    end function le_polyc_complex128

    logical elemental function le_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_int8_polyc

    logical elemental function le_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_int16_polyc

    logical elemental function le_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_int32_polyc

    logical elemental function le_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_int64_polyc

    logical elemental function le_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_real32_polyc

    logical elemental function le_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_real64_polyc

    logical elemental function le_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b <= transfer(a%x,0_int64)
        else
            c = b <= a%x
        end if

    end function le_real128_polyc

    logical elemental function le_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re <= transfer(a%x,0_int64)
        else
            c = b%re <= a%x
        end if

    end function le_complex32_polyc

    logical elemental function le_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re <= transfer(a%x,0_int64)
        else
            c = b%re <= a%x
        end if

    end function le_complex64_polyc

    logical elemental function le_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = b%re <= transfer(a%x,0_int64)
        else
            c = b%re <= a%x
        end if

    end function le_complex128_polyc

    logical elemental function gt_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if

    end function gt_polyc_polyc

    logical elemental function gt_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_int8

    logical elemental function gt_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_int16

    logical elemental function gt_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_int32

    logical elemental function gt_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_int64

    logical elemental function gt_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_real32

    logical elemental function gt_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_real64

    logical elemental function gt_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b
        else
            c = a%x > b
        end if
    end function gt_polyc_real128

    logical elemental function gt_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b%re
        else
            c = a%x > b%re
        end if
    end function gt_polyc_complex32

    logical elemental function gt_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b%re
        else
            c = a%x > b%re
        end if
    end function gt_polyc_complex64

    logical elemental function gt_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) > b%re
        else
            c = a%x > b%re
        end if
    end function gt_polyc_complex128

    logical elemental function gt_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_int8_polyc

    logical elemental function gt_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_int16_polyc

    logical elemental function gt_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_int32_polyc

    logical elemental function gt_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_int64_polyc

    logical elemental function gt_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_real32_polyc

    logical elemental function gt_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_real64_polyc

    logical elemental function gt_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b > transfer(a%x,0_int64)
        else
            c = b > a%x
        end if

    end function gt_real128_polyc

    logical elemental function gt_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re > transfer(a%x,0_int64)
        else
            c = b%re > a%x
        end if

    end function gt_complex32_polyc

    logical elemental function gt_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re > transfer(a%x,0_int64)
        else
            c = b%re > a%x
        end if

    end function gt_complex64_polyc

    logical elemental function gt_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = b%re > transfer(a%x,0_int64)
        else
            c = b%re > a%x
        end if

    end function gt_complex128_polyc

    logical elemental function ge_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if

    end function ge_polyc_polyc

    logical elemental function ge_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_int8

    logical elemental function ge_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_int16

    logical elemental function ge_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_int32

    logical elemental function ge_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_int64

    logical elemental function ge_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_real32

    logical elemental function ge_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_real64

    logical elemental function ge_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b
        else
            c = a%x >= b
        end if
    end function ge_polyc_real128

    logical elemental function ge_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b%re
        else
            c = a%x >= b%re
        end if
    end function ge_polyc_complex32

    logical elemental function ge_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b%re
        else
            c = a%x >= b%re
        end if
    end function ge_polyc_complex64

    logical elemental function ge_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) >= b%re
        else
            c = a%x >= b%re
        end if
    end function ge_polyc_complex128

    logical elemental function ge_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_int8_polyc

    logical elemental function ge_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_int16_polyc

    logical elemental function ge_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_int32_polyc

    logical elemental function ge_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_int64_polyc

    logical elemental function ge_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_real32_polyc

    logical elemental function ge_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_real64_polyc

    logical elemental function ge_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b >= transfer(a%x,0_int64)
        else
            c = b >= a%x
        end if

    end function ge_real128_polyc

    logical elemental function ge_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re >= transfer(a%x,0_int64)
        else
            c = b%re >= a%x
        end if

    end function ge_complex32_polyc

    logical elemental function ge_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b%re >= transfer(a%x,0_int64)
        else
            c = b%re >= a%x
        end if

    end function ge_complex64_polyc

    logical elemental function ge_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = b%re >= transfer(a%x,0_int64)
        else
            c = b%re >= a%x
        end if

    end function ge_complex128_polyc

    logical elemental function eq_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if

    end function eq_polyc_polyc

    logical elemental function eq_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_int8

    logical elemental function eq_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_int16

    logical elemental function eq_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_int32

    logical elemental function eq_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_int64

    logical elemental function eq_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_real32

    logical elemental function eq_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_real64

    logical elemental function eq_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_real128

    logical elemental function eq_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_complex32

    logical elemental function eq_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_complex64

    logical elemental function eq_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) == b
        else
            c = a%x == b
        end if
    end function eq_polyc_complex128

    logical elemental function eq_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_int8_polyc

    logical elemental function eq_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_int16_polyc

    logical elemental function eq_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_int32_polyc

    logical elemental function eq_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_int64_polyc

    logical elemental function eq_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_real32_polyc

    logical elemental function eq_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_real64_polyc

    logical elemental function eq_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x
        end if

    end function eq_real128_polyc

    logical elemental function eq_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x

        end if

    end function eq_complex32_polyc

    logical elemental function eq_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x

        end if

    end function eq_complex64_polyc

    logical elemental function eq_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = b == transfer(a%x,0_int64)
        else
            c = b == a%x

        end if

    end function eq_complex128_polyc

    logical elemental function ne_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if

    end function ne_polyc_polyc

    logical elemental function ne_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_int8

    logical elemental function ne_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_int16

    logical elemental function ne_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_int32

    logical elemental function ne_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_int64

    logical elemental function ne_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_real32

    logical elemental function ne_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_real64

    logical elemental function ne_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_real128

    logical elemental function ne_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_complex32

    logical elemental function ne_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_complex64

    logical elemental function ne_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) /= b
        else
            c = a%x /= b
        end if
    end function ne_polyc_complex128

    logical elemental function ne_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_int8_polyc

    logical elemental function ne_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_int16_polyc

    logical elemental function ne_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_int32_polyc

    logical elemental function ne_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_int64_polyc

    logical elemental function ne_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_real32_polyc

    logical elemental function ne_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_real64_polyc

    logical elemental function ne_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x
        end if

    end function ne_real128_polyc

    logical elemental function ne_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x

        end if

    end function ne_complex32_polyc

    logical elemental function ne_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x

        end if

    end function ne_complex64_polyc

    logical elemental function ne_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c = b /= transfer(a%x,0_int64)
        else
            c = b /= a%x

        end if

    end function ne_complex128_polyc

!----------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------
!       Arithmetic operations
!----------------------------------------------------------------------------------------
    type(polyc) elemental function add_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        c = a
    end function add_polyc

    type(polyc) elemental function add_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == b%kind) then
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) + transfer(b%x,0_int64)
            else
                c = a%x + b%x
            end if
        else
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) + b
            else
                c = a%x + transfer(b%x,0_int64)
            end if
        end if
    end function add_polyc_polyc

    type(polyc) elemental function add_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_int8

    type(polyc) elemental function add_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_int16

    type(polyc) elemental function add_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_int32

    type(polyc) elemental function add_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_int64

    type(polyc) elemental function add_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_real32

    type(polyc) elemental function add_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_real64

    type(polyc) elemental function add_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) + b
        else
            c%kind = polyc_real
            c%x = a%x + b
        end if
    end function add_polyc_real128

    type(polyc) elemental function add_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) + b%re
        else
            c%kind = polyc_real
            c%x = a%x + b%re
        end if
    end function add_polyc_complex32

    type(polyc) elemental function add_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) + b%re
        else
            c%kind = polyc_real
            c%x = a%x + b%re
        end if
    end function add_polyc_complex64

    type(polyc) elemental function add_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) + b%re
        else
            c%kind = polyc_real
            c%x = a%x + b%re
        end if
    end function add_polyc_complex128

    type(polyc) elemental function add_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_int8_polyc

    type(polyc) elemental function add_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_int16_polyc

    type(polyc) elemental function add_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_int32_polyc

    type(polyc) elemental function add_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_int64_polyc

    type(polyc) elemental function add_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_real32_polyc

    type(polyc) elemental function add_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_real64_polyc

    type(polyc) elemental function add_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b + a%x
        end if
    end function add_real128_polyc

    type(polyc) elemental function add_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re + a%x
        end if
    end function add_complex32_polyc

    type(polyc) elemental function add_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re + a%x
        end if
    end function add_complex64_polyc

    type(polyc) elemental function add_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re + transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re + a%x
        end if
    end function add_complex128_polyc

    type(polyc) elemental function subtract_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        c%kind = a%kind
        if (a%kind == polyc_integer) then
            c%x = transfer(-transfer(a%x,0_int64),c%x)
        else
            c%x = -a%x
        end if
    end function subtract_polyc

    type(polyc) elemental function subtract_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == b%kind) then
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) - transfer(b%x,0_int64)
            else
                c = a%x - b%x
            end if
        else
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) - b
            else
                c = a%x - transfer(b%x,0_int64)
            end if
        end if
    end function subtract_polyc_polyc

    type(polyc) elemental function subtract_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_int8

    type(polyc) elemental function subtract_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_int16

    type(polyc) elemental function subtract_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_int32

    type(polyc) elemental function subtract_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_int64

    type(polyc) elemental function subtract_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_real32

    type(polyc) elemental function subtract_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_real64

    type(polyc) elemental function subtract_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) - b
        else
            c%kind = polyc_real
            c%x = a%x - b
        end if
    end function subtract_polyc_real128

    type(polyc) elemental function subtract_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) - b%re
        else
            c%kind = polyc_real
            c%x = a%x - b%re
        end if
    end function subtract_polyc_complex32

    type(polyc) elemental function subtract_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) - b%re
        else
            c%kind = polyc_real
            c%x = a%x - b%re
        end if
    end function subtract_polyc_complex64

    type(polyc) elemental function subtract_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) - b%re
        else
            c%kind = polyc_real
            c%x = a%x - b%re
        end if
    end function subtract_polyc_complex128

    type(polyc) elemental function subtract_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_int8_polyc

    type(polyc) elemental function subtract_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_int16_polyc

    type(polyc) elemental function subtract_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_int32_polyc

    type(polyc) elemental function subtract_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_int64_polyc

    type(polyc) elemental function subtract_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_real32_polyc

    type(polyc) elemental function subtract_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_real64_polyc

    type(polyc) elemental function subtract_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b - a%x
        end if
    end function subtract_real128_polyc

    type(polyc) elemental function subtract_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re - a%x
        end if
    end function subtract_complex32_polyc

    type(polyc) elemental function subtract_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re - a%x
        end if
    end function subtract_complex64_polyc

    type(polyc) elemental function subtract_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re - transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re - a%x
        end if
    end function subtract_complex128_polyc


    type(polyc) elemental function multiply_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == b%kind) then
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) * transfer(b%x,0_int64)
            else
                c = a%x * b%x
            end if
        else
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) * b
            else
                c = a%x * transfer(b%x,0_int64)
            end if
        end if
    end function multiply_polyc_polyc

    type(polyc) elemental function multiply_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_int8

    type(polyc) elemental function multiply_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_int16

    type(polyc) elemental function multiply_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_int32

    type(polyc) elemental function multiply_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_int64

    type(polyc) elemental function multiply_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_real32

    type(polyc) elemental function multiply_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_real64

    type(polyc) elemental function multiply_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) * b
        else
            c%kind = polyc_real
            c%x = a%x * b
        end if
    end function multiply_polyc_real128

    type(polyc) elemental function multiply_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) * b%re
        else
            c%kind = polyc_real
            c%x = a%x * b%re
        end if
    end function multiply_polyc_complex32

    type(polyc) elemental function multiply_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) * b%re
        else
            c%kind = polyc_real
            c%x = a%x * b%re
        end if
    end function multiply_polyc_complex64

    type(polyc) elemental function multiply_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) * b%re
        else
            c%kind = polyc_real
            c%x = a%x * b%re
        end if
    end function multiply_polyc_complex128

    type(polyc) elemental function multiply_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_int8_polyc

    type(polyc) elemental function multiply_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_int16_polyc

    type(polyc) elemental function multiply_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_int32_polyc

    type(polyc) elemental function multiply_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_int64_polyc

    type(polyc) elemental function multiply_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_real32_polyc

    type(polyc) elemental function multiply_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_real64_polyc

    type(polyc) elemental function multiply_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b * a%x
        end if
    end function multiply_real128_polyc

    type(polyc) elemental function multiply_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re * a%x
        end if
    end function multiply_complex32_polyc

    type(polyc) elemental function multiply_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re * a%x
        end if
    end function multiply_complex64_polyc

    type(polyc) elemental function multiply_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re * transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re * a%x
        end if
    end function multiply_complex128_polyc


    type(polyc) elemental function divide_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == b%kind) then
            if (a%kind == polyc_integer) then
                c = real(transfer(a%x,0_int64),kind=real64) / real(transfer(b%x,0_int64),kind=real64)
            else
                c = a%x / b%x
            end if
        else
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) / b
            else
                c = a%x / transfer(b%x,0_int64)
            end if
        end if
    end function divide_polyc_polyc

    type(polyc) elemental function divide_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_int8

    type(polyc) elemental function divide_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_int16

    type(polyc) elemental function divide_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_int32

    type(polyc) elemental function divide_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_int64

    type(polyc) elemental function divide_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_real32

    type(polyc) elemental function divide_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_real64

    type(polyc) elemental function divide_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) / b
        else
            c%kind = polyc_real
            c%x = a%x / b
        end if
    end function divide_polyc_real128

    type(polyc) elemental function divide_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) / b%re
        else
            c%kind = polyc_real
            c%x = a%x / b%re
        end if
    end function divide_polyc_complex32

    type(polyc) elemental function divide_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) / b%re
        else
            c%kind = polyc_real
            c%x = a%x / b%re
        end if
    end function divide_polyc_complex64

    type(polyc) elemental function divide_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) / b%re
        else
            c%kind = polyc_real
            c%x = a%x / b%re
        end if
    end function divide_polyc_complex128

    type(polyc) elemental function divide_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_int8_polyc

    type(polyc) elemental function divide_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_int16_polyc

    type(polyc) elemental function divide_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_int32_polyc

    type(polyc) elemental function divide_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_int64_polyc

    type(polyc) elemental function divide_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_real32_polyc

    type(polyc) elemental function divide_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_real64_polyc

    type(polyc) elemental function divide_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b / a%x
        end if
    end function divide_real128_polyc

    type(polyc) elemental function divide_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re / a%x
        end if
    end function divide_complex32_polyc

    type(polyc) elemental function divide_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re / a%x
        end if
    end function divide_complex64_polyc

    type(polyc) elemental function divide_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re / transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re / a%x
        end if
    end function divide_complex128_polyc


    type(polyc) elemental function power_polyc_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a%kind == b%kind) then
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) ** transfer(b%x,0_int64)
            else
                c = a%x ** b%x
            end if
        else
            if (a%kind == polyc_integer) then
                c = transfer(a%x,0_int64) ** b
            else
                c = a%x ** transfer(b%x,0_int64)
            end if
        end if
    end function power_polyc_polyc

    type(polyc) elemental function power_polyc_int8(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_int8

    type(polyc) elemental function power_polyc_int16(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_int16

    type(polyc) elemental function power_polyc_int32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_int32

    type(polyc) elemental function power_polyc_int64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_int64

    type(polyc) elemental function power_polyc_real32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_real32

    type(polyc) elemental function power_polyc_real64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_real64

    type(polyc) elemental function power_polyc_real128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) ** b
        else
            c%kind = polyc_real
            c%x = a%x ** b
        end if
    end function power_polyc_real128

    type(polyc) elemental function power_polyc_complex32(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) ** b%re
        else
            c%kind = polyc_real
            c%x = a%x ** b%re
        end if
    end function power_polyc_complex32

    type(polyc) elemental function power_polyc_complex64(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) ** b%re
        else
            c%kind = polyc_real
            c%x = a%x ** b%re
        end if
    end function power_polyc_complex64

    type(polyc) elemental function power_polyc_complex128(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = transfer(a%x,0_int64) ** b%re
        else
            c%kind = polyc_real
            c%x = a%x ** b%re
        end if
    end function power_polyc_complex128

    type(polyc) elemental function power_int8_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int8), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_int8_polyc

    type(polyc) elemental function power_int16_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int16), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_int16_polyc

    type(polyc) elemental function power_int32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int32), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_int32_polyc

    type(polyc) elemental function power_int64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        integer(kind=int64), intent(in)     :: b

        if (a%kind == polyc_integer) then
            c = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_int64_polyc

    type(polyc) elemental function power_real32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real32), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_real32_polyc

    type(polyc) elemental function power_real64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real64), intent(in)       :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_real64_polyc

    type(polyc) elemental function power_real128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        real(kind=real128), intent(in)      :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b ** a%x
        end if
    end function power_real128_polyc

    type(polyc) elemental function power_complex32_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real32), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re ** a%x
        end if
    end function power_complex32_polyc

    type(polyc) elemental function power_complex64_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real64), intent(in)    :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re ** a%x
        end if
    end function power_complex64_polyc

    type(polyc) elemental function power_complex128_polyc(b,a) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        complex(kind=real128), intent(in)   :: b

        if (a%kind == polyc_integer) then
            c%kind = polyc_real
            c%x = b%re ** transfer(a%x,0_int64)
        else
            c%kind = polyc_real
            c%x = b%re ** a%x
        end if
    end function power_complex128_polyc

!----------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------
!      Intrinsic functions
!----------------------------------------------------------------------------------------
!   complex128(x)
    type(polyc) elemental function acos_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = acos(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  acos(x%x)
        end if
    end function acos_polyc

    type(polyc) elemental function acosh_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = acosh(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  acosh(x%x)
        end if
    end function acosh_polyc

    type(polyc) elemental function asin_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = asin(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  asin(x%x)
        end if
    end function asin_polyc

    type(polyc) elemental function asinh_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = asinh(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  asinh(x%x)
        end if
    end function asinh_polyc

    type(polyc) elemental function atan_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = atan(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  atan(x%x)
        end if
    end function atan_polyc

    type(polyc) elemental function atanh_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = atanh(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  atanh(x%x)
        end if
    end function atanh_polyc

    type(polyc) elemental function bessel_j0_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = bessel_j0(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  bessel_j0(x%x)
        end if
    end function bessel_j0_polyc

    type(polyc) elemental function bessel_j1_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = bessel_j1(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  bessel_j1(x%x)
        end if
    end function bessel_j1_polyc

    type(polyc) elemental function bessel_y0_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = bessel_y0(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  bessel_y0(x%x)
        end if
    end function bessel_y0_polyc

    type(polyc) elemental function bessel_y1_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = bessel_y1(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  bessel_y1(x%x)
        end if
    end function bessel_y1_polyc

    type(polyc) elemental function cos_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = cos(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  cos(x%x)
        end if
    end function cos_polyc

    type(polyc) elemental function cosh_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = cosh(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  cosh(x%x)
        end if
    end function cosh_polyc

    type(polyc) elemental function erf_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = erf(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  erf(x%x)
        end if
    end function erf_polyc

    type(polyc) elemental function erfc_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = erfc(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  erfc(x%x)
        end if
    end function erfc_polyc

    type(polyc) elemental function erfc_scaled_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = erfc_scaled(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  erfc_scaled(x%x)
        end if
    end function erfc_scaled_polyc

    type(polyc) elemental function exp_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = exp(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  exp(x%x)
        end if
    end function exp_polyc

    type(polyc) elemental function gamma_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = gamma(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  gamma(x%x)
        end if
    end function gamma_polyc

    type(polyc) elemental function hypot_polyc(x,y) result(c)
        implicit none
        type(polyc), intent(in)             :: x
        type(polyc), intent(in)             :: y

        c%kind = polyc_real
        c%x = hypot(real(x),real(y))
    end function hypot_polyc

    type(polyc) elemental function log_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = log(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  log(x%x)
        end if
    end function log_polyc

    type(polyc) elemental function log10_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = log10(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  log10(x%x)
        end if
    end function log10_polyc

    type(polyc) elemental function log_gamma_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = log_gamma(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  log_gamma(x%x)
        end if
    end function log_gamma_polyc

    type(polyc) elemental function sin_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = sin(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  sin(x%x)
        end if
    end function sin_polyc

    type(polyc) elemental function sinh_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = sinh(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  sinh(x%x)
        end if
    end function sinh_polyc

    type(polyc) elemental function sqrt_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = sqrt(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  sqrt(x%x)
        end if
    end function sqrt_polyc

    type(polyc) elemental function tan_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = tan(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  tan(x%x)
        end if
    end function tan_polyc

    type(polyc) elemental function tanh_polyc(x) result(c)
        implicit none
        type(polyc), intent(in)             :: x

        if (x%kind == polyc_integer) then
            c = tanh(real(transfer(x%x,0_int64),kind=real64))
        else
            c%kind = polyc_real
            c%x =  tanh(x%x)
        end if
    end function tanh_polyc

    type(polyc) elemental function bessel_jn_int8_polyc(n,x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n
        type(polyc), intent(in)             :: x

        c = bessel_jn(n,real(x))
    end function bessel_jn_int8_polyc

    type(polyc) elemental function bessel_jn_int16_polyc(n,x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n
        type(polyc), intent(in)             :: x

        c = bessel_jn(n,real(x))
    end function bessel_jn_int16_polyc

    type(polyc) elemental function bessel_jn_int32_polyc(n,x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n
        type(polyc), intent(in)             :: x

        c = bessel_jn(n,real(x))
    end function bessel_jn_int32_polyc

    type(polyc) elemental function bessel_jn_int64_polyc(n,x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n
        type(polyc), intent(in)             :: x

        c = bessel_jn(n,real(x))
    end function bessel_jn_int64_polyc

    type(polyc) elemental function bessel_yn_int8_polyc(n,x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n
        type(polyc), intent(in)             :: x

        c = bessel_yn(n,real(x))
    end function bessel_yn_int8_polyc

    type(polyc) elemental function bessel_yn_int16_polyc(n,x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n
        type(polyc), intent(in)             :: x

        c = bessel_yn(n,real(x))
    end function bessel_yn_int16_polyc

    type(polyc) elemental function bessel_yn_int32_polyc(n,x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n
        type(polyc), intent(in)             :: x

        c = bessel_yn(n,real(x))
    end function bessel_yn_int32_polyc

    type(polyc) elemental function bessel_yn_int64_polyc(n,x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n
        type(polyc), intent(in)             :: x

        c = bessel_yn(n,real(x))
    end function bessel_yn_int64_polyc


!----------------------------------------------------------------------------------------
!       Type convertion (from container to scalar) elemental numerical function
!----------------------------------------------------------------------------------------

!   aimag(z), z: complex
    real(kind=real64) elemental function aimag_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        c = 0.0_real64
    end function aimag_polyc

!   aint(a), a: real
    real(kind=real64) elemental function aint_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = aint(a%x)
        end if
    end function aint_polyc

!   anint(a), a: real
    real(kind=real64) elemental function anint_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = anint(a%x)
        end if
    end function anint_polyc

!   ceiling(a), a: real
    integer(kind=int64) elemental function ceiling_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = ceiling(a%x,kind=int64)
        end if
    end function ceiling_polyc

!   cmplx(a), a: integer, real or complex
    complex(kind=real64) elemental function cmplx_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = a%x
        end if
    end function cmplx_polyc

!   floor(a), a: real
    integer(kind=int64) elemental function floor_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = floor(a%x,kind=int64)
        end if
    end function floor_polyc

!   int(a), a: integer, real or complex
    integer(kind=int64) elemental function int_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = a%x
        end if
    end function int_polyc

!   nint(a), a: real
    integer(kind=int64) elemental function nint_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = transfer(a%x,0_int64)
        else
            c = nint(a%x,kind=int64)
        end if
    end function nint_polyc

!   real(a), a: integer, real or complex
    real(kind=real64) elemental function real_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = real(transfer(a%x,0_int64),kind=real64)
        else
            c = a%x
        end if
    end function real_polyc

!----------------------------------------------------------------------------------------
!       Elemental numerical function (does not convert)
!----------------------------------------------------------------------------------------

!   abs(a), a: integer, real or complex
    type(polyc) elemental function abs_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        if (a%kind == polyc_integer) then
            c = abs(transfer(a%x,0_int64))
        else
            c%kind = polyc_real
            c%x =  abs(a%x)
        end if
    end function abs_polyc

!   conjg(z), z: complex
    type(polyc) elemental function conjg_polyc(a) result(c)
        implicit none
        type(polyc), intent(in)             :: a

        c = a
    end function conjg_polyc

!   max(a,b), a, b: integer or real - just two element version
    type(polyc) elemental function max_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a > b) then
            c = a
        else if (a < b) then
            c = b
        else
            if (a%kind >= b%kind) then
                c = a
            else
                c = b
            end if
        end if
    end function max_polyc

!   min(a,b), a,b: integer or real - just two element version
    type(polyc) elemental function min_polyc(a,b) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: b

        if (a > b) then
            c = b
        else if (a < b) then
            c = a
        else
            if (a%kind >= b%kind) then
                c = a
            else
                c = b
            end if
        end if
    end function min_polyc

!   mod(a,p), a,p: integer or real
    type(polyc) elemental function mod_polyc(a,p) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: p

        if (a%kind == polyc_integer .and. p%kind == polyc_integer) then
            c = mod(transfer(a%x,0_int64), transfer(p%x,0_int64))
        else
            c = mod(real(a),real(p))
        end if
    end function mod_polyc

!   modulo(a,p), a,p: integer or real
    type(polyc) elemental function modulo_polyc(a,p) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: p

        if (a%kind == polyc_integer .and. p%kind == polyc_integer) then
            c = modulo(transfer(a%x,0_int64), transfer(p%x,0_int64))
        else
            c = modulo(real(a),real(p))
        end if
    end function modulo_polyc

!   sign(a,b), a,b: integer or real
    type(polyc) elemental function sign_polyc(a,p) result(c)
        implicit none
        type(polyc), intent(in)             :: a
        type(polyc), intent(in)             :: p

        if (a%kind == polyc_integer .and. p%kind == polyc_integer) then
            c = sign(transfer(a%x,0_int64), transfer(p%x,0_int64))
        else
            c = sign(real(a),real(p))
        end if
    end function sign_polyc

!----------------------------------------------------------------------------------------
!       Transformational Bessel functions
!----------------------------------------------------------------------------------------

    pure function bessel_jn_int8_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int8_int8_polyc

    pure function bessel_jn_int8_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int8_int16_polyc

    pure function bessel_jn_int8_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int8_int32_polyc

    pure function bessel_jn_int8_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int8_int64_polyc

    pure function bessel_jn_int16_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int16_int8_polyc

    pure function bessel_jn_int16_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int16))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int16_int16_polyc

    pure function bessel_jn_int16_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int32))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int16_int32_polyc

    pure function bessel_jn_int16_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int16_int64_polyc

    pure function bessel_jn_int32_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int32_int8_polyc

    pure function bessel_jn_int32_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int32))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int32_int16_polyc

    pure function bessel_jn_int32_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int32))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int32_int32_polyc

    pure function bessel_jn_int32_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int32_int64_polyc

    pure function bessel_jn_int64_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int64_int8_polyc

    pure function bessel_jn_int64_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int64_int16_polyc

    pure function bessel_jn_int64_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int64_int32_polyc

    pure function bessel_jn_int64_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_jn(n1,n2,real(x))
    end function bessel_jn_int64_int64_polyc

    pure function bessel_yn_int8_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int8_int8_polyc

    pure function bessel_yn_int8_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int8_int16_polyc

    pure function bessel_yn_int8_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int8_int32_polyc

    pure function bessel_yn_int8_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int8), intent(in)      :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int8_int64_polyc

    pure function bessel_yn_int16_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int16_int8_polyc

    pure function bessel_yn_int16_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int16))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int16_int16_polyc

    pure function bessel_yn_int16_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int32))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int16_int32_polyc

    pure function bessel_yn_int16_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int16), intent(in)     :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int16_int64_polyc

    pure function bessel_yn_int32_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int32_int8_polyc

    pure function bessel_yn_int32_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int32))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int32_int16_polyc

    pure function bessel_yn_int32_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int32))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int32_int32_polyc

    pure function bessel_yn_int32_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int32), intent(in)     :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int32_int64_polyc

    pure function bessel_yn_int64_int8_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int8), intent(in)      :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int8))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int64_int8_polyc

    pure function bessel_yn_int64_int16_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int16), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int64_int16_polyc

    pure function bessel_yn_int64_int32_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int32), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int64_int32_polyc

    pure function bessel_yn_int64_int64_polyc(n1, n2, x) result(c)
        implicit none
        integer(kind=int64), intent(in)     :: n1
        integer(kind=int64), intent(in)     :: n2
        type(polyc), intent(in)             :: x
        type(polyc)                         :: c(max(n2-n1+1,0_int64))

        c = bessel_yn(n1,n2,real(x))
    end function bessel_yn_int64_int64_polyc

!----------------------------------------------------------------------------------------
!       Transformational array functions
!----------------------------------------------------------------------------------------

    type(polyc) pure function maxval_r1_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:)
    logical, intent(in), optional       :: mask(:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1)%kind == polyc_integer) then
            c = maxval(int(x(:)),mask=mask)
        else
            c = maxval(x(:)%x,mask=mask)
        end if
    else
            c = maxval(real(x(:)),mask=mask)
        end if
    end function maxval_r1_polyc

    type(polyc) pure function maxval_r2_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:)
    logical, intent(in), optional       :: mask(:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1)%kind == polyc_integer) then
            c = maxval(int(x(:,:)),mask=mask)
        else
            c = maxval(x(:,:)%x,mask=mask)
        end if
    else
            c = maxval(real(x(:,:)),mask=mask)
        end if
    end function maxval_r2_polyc

    type(polyc) pure function maxval_r3_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:,:)
    logical, intent(in), optional       :: mask(:,:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1,1)%kind == polyc_integer) then
            c = maxval(int(x(:,:,:)),mask=mask)
        else
            c = maxval(x(:,:,:)%x,mask=mask)
        end if
    else
            c = maxval(real(x(:,:,:)),mask=mask)
        end if
    end function maxval_r3_polyc

    pure function maxval_r1_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = maxval(int(x(:)),dim=dim,mask=mask)
            else
                c%kind = polyc_real
                c%x = maxval(x(:)%x,dim=dim,mask=mask)
            end if
        else
                c%kind = polyc_real
                c%x = maxval(real(x(:)),dim=dim,mask=mask)
            end if
    end function maxval_r1_dim_polyc

    pure function maxval_r2_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c(:) = maxval(int(x(:,:)),dim=dim,mask=mask)
            else
                c(:)%kind = polyc_real
                c(:)%x = maxval(x(:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:)%kind = polyc_real
                c(:)%x = maxval(real(x(:,:)),dim=dim,mask=mask)
            end if
    end function maxval_r2_dim_polyc

    pure function maxval_r3_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c(:,:) = maxval(int(x(:,:,:)),dim=dim,mask=mask)
            else
                c(:,:)%kind = polyc_real
                c(:,:)%x = maxval(x(:,:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:,:)%kind = polyc_real
                c(:,:)%x = maxval(real(x(:,:,:)),dim=dim,mask=mask)
            end if
    end function maxval_r3_dim_polyc

    type(polyc) pure function minval_r1_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:)
    logical, intent(in), optional       :: mask(:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1)%kind == polyc_integer) then
            c = minval(int(x(:)),mask=mask)
        else
            c = minval(x(:)%x,mask=mask)
        end if
    else
            c = minval(real(x(:)),mask=mask)
        end if
    end function minval_r1_polyc

    type(polyc) pure function minval_r2_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:)
    logical, intent(in), optional       :: mask(:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1)%kind == polyc_integer) then
            c = minval(int(x(:,:)),mask=mask)
        else
            c = minval(x(:,:)%x,mask=mask)
        end if
    else
            c = minval(real(x(:,:)),mask=mask)
        end if
    end function minval_r2_polyc

    type(polyc) pure function minval_r3_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:,:)
    logical, intent(in), optional       :: mask(:,:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1,1)%kind == polyc_integer) then
            c = minval(int(x(:,:,:)),mask=mask)
        else
            c = minval(x(:,:,:)%x,mask=mask)
        end if
    else
            c = minval(real(x(:,:,:)),mask=mask)
        end if
    end function minval_r3_polyc

    pure function minval_r1_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = minval(int(x(:)),dim=dim,mask=mask)
            else
                c%kind = polyc_real
                c%x = minval(x(:)%x,dim=dim,mask=mask)
            end if
        else
                c%kind = polyc_real
                c%x = minval(real(x(:)),dim=dim,mask=mask)
            end if
    end function minval_r1_dim_polyc

    pure function minval_r2_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c(:) = minval(int(x(:,:)),dim=dim,mask=mask)
            else
                c(:)%kind = polyc_real
                c(:)%x = minval(x(:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:)%kind = polyc_real
                c(:)%x = minval(real(x(:,:)),dim=dim,mask=mask)
            end if
    end function minval_r2_dim_polyc

    pure function minval_r3_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c(:,:) = minval(int(x(:,:,:)),dim=dim,mask=mask)
            else
                c(:,:)%kind = polyc_real
                c(:,:)%x = minval(x(:,:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:,:)%kind = polyc_real
                c(:,:)%x = minval(real(x(:,:,:)),dim=dim,mask=mask)
            end if
    end function minval_r3_dim_polyc

    real(kind=real64) pure function norm2_r1_polyc(x, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:))) uniform_l = .true.
    end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = norm2(real(x(:)))
            else
                c = norm2(x(:)%x)
            end if
        else
            c = norm2(real(x(:)))
        end if
    end function norm2_r1_polyc

    real(kind=real64) pure function norm2_r2_polyc(x, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:))) uniform_l = .true.
    end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c = norm2(real(x(:,:)))
            else
                c = norm2(x(:,:)%x)
            end if
        else
            c = norm2(real(x(:,:)))
        end if
    end function norm2_r2_polyc

    real(kind=real64) pure function norm2_r3_polyc(x, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:,:))) uniform_l = .true.
    end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c = norm2(real(x(:,:,:)))
            else
                c = norm2(x(:,:,:)%x)
            end if
        else
            c = norm2(real(x(:,:,:)))
        end if
    end function norm2_r3_polyc

    pure function norm2_r1_dim_polyc(x, dim, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

            if (uniform_l) then
                if (x(1)%kind == polyc_integer) then
                    c = norm2(real(x(:)),dim=dim)
                else
                    c = norm2(x(:)%x,dim=dim)
                end if
            else
                c = norm2(real(x(:)),dim=dim)
            end if
    end function norm2_r1_dim_polyc

    pure function norm2_r2_dim_polyc(x, dim, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

            if (uniform_l) then
                if (x(1,1)%kind == polyc_integer) then
                    c(:) = norm2(real(x(:,:)),dim=dim)
                else
                    c(:) = norm2(x(:,:)%x,dim=dim)
                end if
            else
                c(:) = norm2(real(x(:,:)),dim=dim)
            end if
    end function norm2_r2_dim_polyc

    pure function norm2_r3_dim_polyc(x, dim, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

            if (uniform_l) then
                if (x(1,1,1)%kind == polyc_integer) then
                    c(:,:) = norm2(real(x(:,:,:)),dim=dim)
                else
                    c(:,:) = norm2(x(:,:,:)%x,dim=dim)
                end if
            else
                c(:,:) = norm2(real(x(:,:,:)),dim=dim)
            end if
    end function norm2_r3_dim_polyc

    type(polyc) pure function product_r1_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:)
    logical, intent(in), optional       :: mask(:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1)%kind == polyc_integer) then
            c = product(int(x(:)),mask=mask)
        else
            c = product(x(:)%x,mask=mask)
        end if
    else
            c = product(real(x(:)),mask=mask)
        end if
    end function product_r1_polyc

    type(polyc) pure function product_r2_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:)
    logical, intent(in), optional       :: mask(:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1)%kind == polyc_integer) then
            c = product(int(x(:,:)),mask=mask)
        else
            c = product(x(:,:)%x,mask=mask)
        end if
    else
            c = product(real(x(:,:)),mask=mask)
        end if
    end function product_r2_polyc

    type(polyc) pure function product_r3_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:,:)
    logical, intent(in), optional       :: mask(:,:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1,1)%kind == polyc_integer) then
            c = product(int(x(:,:,:)),mask=mask)
        else
            c = product(x(:,:,:)%x,mask=mask)
        end if
    else
            c = product(real(x(:,:,:)),mask=mask)
        end if
    end function product_r3_polyc

    pure function product_r1_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = product(int(x(:)),dim=dim,mask=mask)
            else
                c%kind = polyc_real
                c%x = product(x(:)%x,dim=dim,mask=mask)
            end if
        else
                c%kind = polyc_real
                c%x = product(real(x(:)),dim=dim,mask=mask)
            end if
    end function product_r1_dim_polyc

    pure function product_r2_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c(:) = product(int(x(:,:)),dim=dim,mask=mask)
            else
                c(:)%kind = polyc_real
                c(:)%x = product(x(:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:)%kind = polyc_real
                c(:)%x = product(real(x(:,:)),dim=dim,mask=mask)
            end if
    end function product_r2_dim_polyc

    pure function product_r3_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c(:,:) = product(int(x(:,:,:)),dim=dim,mask=mask)
            else
                c(:,:)%kind = polyc_real
                c(:,:)%x = product(x(:,:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:,:)%kind = polyc_real
                c(:,:)%x = product(real(x(:,:,:)),dim=dim,mask=mask)
            end if
    end function product_r3_dim_polyc

    type(polyc) pure function sum_r1_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:)
    logical, intent(in), optional       :: mask(:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1)%kind == polyc_integer) then
            c = sum(int(x(:)),mask=mask)
        else
            c = sum(x(:)%x,mask=mask)
        end if
    else
            c = sum(real(x(:)),mask=mask)
        end if
    end function sum_r1_polyc

    type(polyc) pure function sum_r2_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:)
    logical, intent(in), optional       :: mask(:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1)%kind == polyc_integer) then
            c = sum(int(x(:,:)),mask=mask)
        else
            c = sum(x(:,:)%x,mask=mask)
        end if
    else
            c = sum(real(x(:,:)),mask=mask)
        end if
    end function sum_r2_polyc

    type(polyc) pure function sum_r3_polyc(x, mask, uniform) result(c)
    implicit none
    type(polyc), intent(in)             :: x(:,:,:)
    logical, intent(in), optional       :: mask(:,:,:)
    logical, intent(in), optional       :: uniform

    logical                             :: uniform_l
    uniform_l = .false.

    if (present(uniform)) then
        if (uniform) uniform_l = .true.
    else
        if (is_uniform(x(:,:,:))) uniform_l = .true.
    end if

    if (uniform_l) then
        if (x(1,1,1)%kind == polyc_integer) then
            c = sum(int(x(:,:,:)),mask=mask)
        else
            c = sum(x(:,:,:)%x,mask=mask)
        end if
    else
            c = sum(real(x(:,:,:)),mask=mask)
        end if
    end function sum_r3_polyc

    pure function sum_r1_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = sum(int(x(:)),dim=dim,mask=mask)
            else
                c%kind = polyc_real
                c%x = sum(x(:)%x,dim=dim,mask=mask)
            end if
        else
                c%kind = polyc_real
                c%x = sum(real(x(:)),dim=dim,mask=mask)
            end if
    end function sum_r1_dim_polyc

    pure function sum_r2_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c(:) = sum(int(x(:,:)),dim=dim,mask=mask)
            else
                c(:)%kind = polyc_real
                c(:)%x = sum(x(:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:)%kind = polyc_real
                c(:)%x = sum(real(x(:,:)),dim=dim,mask=mask)
            end if
    end function sum_r2_dim_polyc

    pure function sum_r3_dim_polyc(x, dim, mask, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c(:,:) = sum(int(x(:,:,:)),dim=dim,mask=mask)
            else
                c(:,:)%kind = polyc_real
                c(:,:)%x = sum(x(:,:,:)%x,dim=dim,mask=mask)
            end if
        else
                c(:,:)%kind = polyc_real
                c(:,:)%x = sum(real(x(:,:,:)),dim=dim,mask=mask)
            end if
    end function sum_r3_dim_polyc

!----------------------------------------------------------------------------------------
!       Transformational array location functions
!----------------------------------------------------------------------------------------

    pure function maxloc_r1_polyc(x, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(1)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = maxloc(int(x(:)),mask=mask,back=back)
            else
                c = maxloc(x(:)%x,mask=mask,back=back)
            end if
        else
            if (any(x(:)%kind == polyc_integer)) then
                c = maxloc(real(x(:)),mask=mask,back=back)
            else
                c = maxloc(x(:)%x,mask=mask,back=back)
            endif
        end if
    end function maxloc_r1_polyc

    pure function maxloc_r2_polyc(x, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(2)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c = maxloc(int(x(:,:)),mask=mask,back=back)
            else
                c = maxloc(x(:,:)%x,mask=mask,back=back)
            end if
        else
            if (any(x(:,:)%kind == polyc_integer)) then
                c = maxloc(real(x(:,:)),mask=mask,back=back)
            else
                c = maxloc(x(:,:)%x,mask=mask,back=back)
            endif
        end if
    end function maxloc_r2_polyc

    pure function maxloc_r3_polyc(x, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(3)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c = maxloc(int(x(:,:,:)),mask=mask,back=back)
            else
                c = maxloc(x(:,:,:)%x,mask=mask,back=back)
            end if
        else
            if (any(x(:,:,:)%kind == polyc_integer)) then
                c = maxloc(real(x(:,:,:)),mask=mask,back=back)
            else
                c = maxloc(x(:,:,:)%x,mask=mask,back=back)
            endif
        end if
    end function maxloc_r3_polyc

    pure function maxloc_r1_dim_polyc(x, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = maxloc(int(x(:)),dim=dim,mask=mask,back=back)
            else
                c = maxloc(x(:)%x,dim=dim,mask=mask,back=back)
            end if
        else
            if (any(x(:)%kind == polyc_integer)) then
                c = maxloc(real(x(:)),dim=dim,mask=mask,back=back)
            else
                c = maxloc(x(:)%x,dim=dim,mask=mask,back=back)
            endif
        end if
    end function maxloc_r1_dim_polyc

    pure function maxloc_r2_dim_polyc(x, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c = maxloc(int(x(:,:)),dim=dim,mask=mask,back=back)
            else
                c = maxloc(x(:,:)%x,dim=dim,mask=mask,back=back)
            end if
        else
            if (any(x(:,:)%kind == polyc_integer)) then
                c = maxloc(real(x(:,:)),dim=dim,mask=mask,back=back)
            else
                c = maxloc(x(:,:)%x,dim=dim,mask=mask,back=back)
            endif
        end if
    end function maxloc_r2_dim_polyc

    pure function maxloc_r3_dim_polyc(x, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c = maxloc(int(x(:,:,:)),dim=dim,mask=mask,back=back)
            else
                c = maxloc(x(:,:,:)%x,dim=dim,mask=mask,back=back)
            end if
        else
            if (any(x(:,:,:)%kind == polyc_integer)) then
                c = maxloc(real(x(:,:,:)),dim=dim,mask=mask,back=back)
            else
                c = maxloc(x(:,:,:)%x,dim=dim,mask=mask,back=back)
            endif
        end if
    end function maxloc_r3_dim_polyc

    pure function minloc_r1_polyc(x, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(1)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = minloc(int(x(:)),mask=mask,back=back)
            else
                c = minloc(x(:)%x,mask=mask,back=back)
            end if
        else
            if (any(x(:)%kind == polyc_integer)) then
                c = minloc(real(x(:)),mask=mask,back=back)
            else
                c = minloc(x(:)%x,mask=mask,back=back)
            endif
        end if
    end function minloc_r1_polyc

    pure function minloc_r2_polyc(x, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(2)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c = minloc(int(x(:,:)),mask=mask,back=back)
            else
                c = minloc(x(:,:)%x,mask=mask,back=back)
            end if
        else
            if (any(x(:,:)%kind == polyc_integer)) then
                c = minloc(real(x(:,:)),mask=mask,back=back)
            else
                c = minloc(x(:,:)%x,mask=mask,back=back)
            endif
        end if
    end function minloc_r2_polyc

    pure function minloc_r3_polyc(x, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(3)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c = minloc(int(x(:,:,:)),mask=mask,back=back)
            else
                c = minloc(x(:,:,:)%x,mask=mask,back=back)
            end if
        else
            if (any(x(:,:,:)%kind == polyc_integer)) then
                c = minloc(real(x(:,:,:)),mask=mask,back=back)
            else
                c = minloc(x(:,:,:)%x,mask=mask,back=back)
            endif
        end if
    end function minloc_r3_polyc

    pure function minloc_r1_dim_polyc(x, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                c = minloc(int(x(:)),dim=dim,mask=mask,back=back)
            else
                c = minloc(x(:)%x,dim=dim,mask=mask,back=back)
            end if
        else
            if (any(x(:)%kind == polyc_integer)) then
                c = minloc(real(x(:)),dim=dim,mask=mask,back=back)
            else
                c = minloc(x(:)%x,dim=dim,mask=mask,back=back)
            endif
        end if
    end function minloc_r1_dim_polyc

    pure function minloc_r2_dim_polyc(x, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                c = minloc(int(x(:,:)),dim=dim,mask=mask,back=back)
            else
                c = minloc(x(:,:)%x,dim=dim,mask=mask,back=back)
            end if
        else
            if (any(x(:,:)%kind == polyc_integer)) then
                c = minloc(real(x(:,:)),dim=dim,mask=mask,back=back)
            else
                c = minloc(x(:,:)%x,dim=dim,mask=mask,back=back)
            endif
        end if
    end function minloc_r2_dim_polyc

    pure function minloc_r3_dim_polyc(x, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                c = minloc(int(x(:,:,:)),dim=dim,mask=mask,back=back)
            else
                c = minloc(x(:,:,:)%x,dim=dim,mask=mask,back=back)
            end if
        else
            if (any(x(:,:,:)%kind == polyc_integer)) then
                c = minloc(real(x(:,:,:)),dim=dim,mask=mask,back=back)
            else
                c = minloc(x(:,:,:)%x,dim=dim,mask=mask,back=back)
            endif
        end if
    end function minloc_r3_dim_polyc

    pure function findloc_r1_polyc(x, value, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        type(polyc), intent(in)             :: value
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(1)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                if (value%kind == polyc_integer) then
                    c = findloc(int(x(:)),int(value),mask=mask,back=back)
                else
                    c = findloc(int(x(:)),value%x,mask=mask,back=back)
                end if
            else
                if (value%kind == polyc_integer) then
                    c = findloc(x(:)%x,int(value),mask=mask,back=back)
                else
                    c = findloc(x(:)%x,value%x,mask=mask,back=back)
                end if
            end if
        else
            if (value%kind == polyc_integer) then
                c = findloc(real(x(:)),int(value),mask=mask,back=back)
            else
                c = findloc(real(x(:)),value%x,mask=mask,back=back)
            end if
        end if
    end function findloc_r1_polyc

    pure function findloc_r2_polyc(x, value, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        type(polyc), intent(in)             :: value
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(2)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                if (value%kind == polyc_integer) then
                    c = findloc(int(x(:,:)),int(value),mask=mask,back=back)
                else
                    c = findloc(int(x(:,:)),value%x,mask=mask,back=back)
                end if
            else
                if (value%kind == polyc_integer) then
                    c = findloc(x(:,:)%x,int(value),mask=mask,back=back)
                else
                    c = findloc(x(:,:)%x,value%x,mask=mask,back=back)
                end if
            end if
        else
            if (value%kind == polyc_integer) then
                c = findloc(real(x(:,:)),int(value),mask=mask,back=back)
            else
                c = findloc(real(x(:,:)),value%x,mask=mask,back=back)
            end if
        end if
    end function findloc_r2_polyc

    pure function findloc_r3_polyc(x, value, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        type(polyc), intent(in)             :: value
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(3)

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                if (value%kind == polyc_integer) then
                    c = findloc(int(x(:,:,:)),int(value),mask=mask,back=back)
                else
                    c = findloc(int(x(:,:,:)),value%x,mask=mask,back=back)
                end if
            else
                if (value%kind == polyc_integer) then
                    c = findloc(x(:,:,:)%x,int(value),mask=mask,back=back)
                else
                    c = findloc(x(:,:,:)%x,value%x,mask=mask,back=back)
                end if
            end if
        else
            if (value%kind == polyc_integer) then
                c = findloc(real(x(:,:,:)),int(value),mask=mask,back=back)
            else
                c = findloc(real(x(:,:,:)),value%x,mask=mask,back=back)
            end if
        end if
    end function findloc_r3_polyc

    pure function findloc_r1_dim_polyc(x, value, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:)
        type(polyc), intent(in)             :: value
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1)%kind == polyc_integer) then
                if (value%kind == polyc_integer) then
                    c = findloc(int(x(:)),int(value),dim=dim,mask=mask,back=back)
                else
                    c = findloc(int(x(:)),value%x,dim=dim,mask=mask,back=back)
                end if
            else
                if (value%kind == polyc_integer) then
                    c = findloc(x(:)%x,int(value),dim=dim,mask=mask,back=back)
                else
                    c = findloc(x(:)%x,value%x,dim=dim,mask=mask,back=back)
                end if
            end if
        else
            if (value%kind == polyc_integer) then
                c = findloc(real(x(:)),int(value),dim=dim,mask=mask,back=back)
            else
                c = findloc(real(x(:)),value%x,dim=dim,mask=mask,back=back)
            end if
        end if
    end function findloc_r1_dim_polyc

    pure function findloc_r2_dim_polyc(x, value, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:)
        type(polyc), intent(in)             :: value
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(minval(pack(shape(x),[1,2]/=dim)))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1)%kind == polyc_integer) then
                if (value%kind == polyc_integer) then
                    c = findloc(int(x(:,:)),int(value),dim=dim,mask=mask,back=back)
                else
                    c = findloc(int(x(:,:)),value%x,dim=dim,mask=mask,back=back)
                end if
            else
                if (value%kind == polyc_integer) then
                    c = findloc(x(:,:)%x,int(value),dim=dim,mask=mask,back=back)
                else
                    c = findloc(x(:,:)%x,value%x,dim=dim,mask=mask,back=back)
                end if
            end if
        else
            if (value%kind == polyc_integer) then
                c = findloc(real(x(:,:)),int(value),dim=dim,mask=mask,back=back)
            else
                c = findloc(real(x(:,:)),value%x,dim=dim,mask=mask,back=back)
            end if
        end if
    end function findloc_r2_dim_polyc

    pure function findloc_r3_dim_polyc(x, value, dim, mask, back, uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: x(:,:,:)
        type(polyc), intent(in)             :: value
        integer, intent(in)                 :: dim
        logical, intent(in), optional       :: mask(:,:,:)
        logical, intent(in), optional       :: back
        logical, intent(in), optional       :: uniform
        integer                             :: c(minval(pack(shape(x),[1,2,3]/=dim),[1,2]==1), &
                                                 minval(pack(shape(x),[1,2,3]/=dim),[1,2]==2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(x(:,:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (x(1,1,1)%kind == polyc_integer) then
                if (value%kind == polyc_integer) then
                    c = findloc(int(x(:,:,:)),int(value),dim=dim,mask=mask,back=back)
                else
                    c = findloc(int(x(:,:,:)),value%x,dim=dim,mask=mask,back=back)
                end if
            else
                if (value%kind == polyc_integer) then
                    c = findloc(x(:,:,:)%x,int(value),dim=dim,mask=mask,back=back)
                else
                    c = findloc(x(:,:,:)%x,value%x,dim=dim,mask=mask,back=back)
                end if
            end if
        else
            if (value%kind == polyc_integer) then
                c = findloc(real(x(:,:,:)),int(value),dim=dim,mask=mask,back=back)
            else
                c = findloc(real(x(:,:,:)),value%x,dim=dim,mask=mask,back=back)
            end if
        end if
    end function findloc_r3_dim_polyc

!----------------------------------------------------------------------------------------
!       Vector and matrix multiplication functions
!----------------------------------------------------------------------------------------

    type(polyc) pure function dot_product_polyc(a,b,uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:)
        type(polyc), intent(in)             :: b(:)
        logical, intent(in), optional       :: uniform

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(a(:)) .and. is_uniform(b(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (a(1)%kind == polyc_real) then
                if (b(1)%kind == polyc_real) then
                    c = dot_product(a(:)%x,b(:)%x)
                else
                    c = dot_product(a(:)%x,int(b(:)))
                end if
            else
                if (b(1)%kind == polyc_real) then
                    c = dot_product(int(a(:)),b(:)%x)
                else
                    c = dot_product(int(a(:)),int(b(:)))
                end if
            end if
        else
            if (any(a(:)%kind == polyc_real) .or. any(b(:)%kind == polyc_real)) then
                c = dot_product(real(a(:)),real(b(:)))
            else
                c = dot_product(int(a(:)),int(b(:)))
            end if
        end if
    end function dot_product_polyc

    pure function matmul_r2_r2_polyc(a,b,uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:,:)
        type(polyc), intent(in)             :: b(:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(size(a,1), size(b,2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(a(:,:)) .and. is_uniform(b(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (a(1,1)%kind == polyc_real) then
                c(:,:)%kind = polyc_real
                if (b(1,1)%kind == polyc_real) then
                    c(:,:)%x = matmul(a(:,:)%x,b(:,:)%x)
                else
                    c%x = matmul(a(:,:)%x,real(b(:,:)))
                end if
            else
                if (b(1,1)%kind == polyc_real) then
                    c(:,:)%kind = polyc_real
                    c(:,:)%x = matmul(real(a(:,:)),b(:,:)%x)
                else
                    c(:,:)%kind = polyc_integer
                    c(:,:)%x = matmul(int(a(:,:)),int(b(:,:)))
                end if
            end if
        else
            if (any(a(:,:)%kind == polyc_real) .or. any(b(:,:)%kind == polyc_real)) then
                c(:,:)%kind = polyc_real
                c(:,:)%x = matmul(real(a(:,:)),real(b(:,:)))
            else
                c(:,:)%kind = polyc_integer
                c(:,:)%x = matmul(int(a(:,:)),int(b(:,:)))
            end if
        end if
    end function matmul_r2_r2_polyc

    pure function matmul_r2_r1_polyc(a,b,uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:,:)
        type(polyc), intent(in)             :: b(:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(size(a,1))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(a(:,:)) .and. is_uniform(b(:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (a(1,1)%kind == polyc_real) then
                c(:)%kind = polyc_real
                if (b(1)%kind == polyc_real) then
                    c(:)%x = matmul(a(:,:)%x,b(:)%x)
                else
                    c%x = matmul(a(:,:)%x,real(b(:)))
                end if
            else
                if (b(1)%kind == polyc_real) then
                    c(:)%kind = polyc_real
                    c(:)%x = matmul(real(a(:,:)),b(:)%x)
                else
                    c(:)%kind = polyc_integer
                    c(:)%x = matmul(int(a(:,:)),int(b(:)))
                end if
            end if
        else
            if (any(a(:,:)%kind == polyc_real) .or. any(b(:)%kind == polyc_real)) then
                c(:)%kind = polyc_real
                c(:)%x = matmul(real(a(:,:)),real(b(:)))
            else
                c(:)%kind = polyc_integer
                c(:)%x = matmul(int(a(:,:)),int(b(:)))
            end if
        end if
    end function matmul_r2_r1_polyc

    pure function matmul_r1_r2_polyc(a,b,uniform) result(c)
        implicit none
        type(polyc), intent(in)             :: a(:)
        type(polyc), intent(in)             :: b(:,:)
        logical, intent(in), optional       :: uniform
        type(polyc)                         :: c(size(b,2))

        logical                             :: uniform_l
        uniform_l = .false.

        if (present(uniform)) then
            if (uniform) uniform_l = .true.
        else
            if (is_uniform(a(:)) .and. is_uniform(b(:,:))) uniform_l = .true.
        end if

        if (uniform_l) then
            if (a(1)%kind == polyc_real) then
                c(:)%kind = polyc_real
                if (b(1,1)%kind == polyc_real) then
                    c(:)%x = matmul(a(:)%x,b(:,:)%x)
                else
                    c%x = matmul(a(:)%x,real(b(:,:)))
                end if
            else
                if (b(1,1)%kind == polyc_real) then
                    c(:)%kind = polyc_real
                    c(:)%x = matmul(real(a(:)),b(:,:)%x)
                else
                    c(:)%kind = polyc_integer
                    c(:)%x = matmul(int(a(:)),int(b(:,:)))
                end if
            end if
        else
            if (any(a(:)%kind == polyc_real) .or. any(b(:,:)%kind == polyc_real)) then
                c(:)%kind = polyc_real
                c(:)%x = matmul(real(a(:)),real(b(:,:)))
            else
                c(:)%kind = polyc_integer
                c(:)%x = matmul(int(a(:)),int(b(:,:)))
            end if
        end if
    end function matmul_r1_r2_polyc

end module poly_container
