module messagehandler
    use, intrinsic :: iso_fortran_env, only: output_unit
    use kinds

    implicit none
    private

    type :: mtype
        integer             :: value
        character(len=15)   :: header
    contains
        procedure, pass(a)  :: eq_mtype
        generic, public     :: operator(==) => eq_mtype
    end type

    type(mtype), public, parameter        :: ErrorMessage   = mtype(int(B'1'),achar(27)//'[31mERROR'//achar(27)//'[0m')
    type(mtype), public, parameter        :: WarningMessage = mtype(int(B'10'),achar(27)//'[95mWarning'//achar(27)//'[0m')
    type(mtype), public, parameter        :: DebugMessage   = mtype(int(B'100'),'Debug')
    type(mtype), public, parameter        :: InfoMessage    = mtype(int(B'1000'),'')

    interface message_report
        module procedure message_report_screen
        module procedure message_report_file
    end interface message_report

    interface itoa
        module procedure itoa_int16
        module procedure itoa_int32
        module procedure itoa_int64
    end interface itoa

    interface rtoa
        module procedure rtoa_real32
        module procedure rtoa_real64
    end interface rtoa

    public :: message_report, itoa, rtoa

contains

    logical pure function eq_mtype(a,b) result(equal)
        class(mtype), intent(in)                 :: a
        class(mtype), intent(in)                 :: b
        equal = (a%value == b%value)
    end function


    subroutine message_report_screen(msgtype, message, filename, line)
        implicit none
        type(mtype), intent(in)                  :: msgtype
        character(len=*), intent(in)             :: message
        character(len=*), intent(in), optional   :: filename
        integer, intent(in), optional            :: line

        if (present(filename) .and. present(line)) then
            call message_report_file(output_unit, msgtype, message, filename, line)
        else
            call message_report_file(output_unit, msgtype, message)
        end if
        return
    end subroutine message_report_screen

    subroutine message_report_file(fid, msgtype, message, filename, line)
        implicit none
        integer, intent(in)                      :: fid
        type(mtype), intent(in)                  :: msgtype
        character(len=*), intent(in)             :: message
        character(len=*), intent(in), optional   :: filename
        integer, intent(in), optional            :: line

        character(len=:), allocatable            :: pos

        if (present(filename) .and. present(line)) then
            pos = '('//trim(filename) //':'// itoa(line)//')'
        else
            pos = ''
        end if

        write(fid,'(a, a,": ", a)') trim(msgtype%header), pos, message
        return
    end subroutine message_report_file

    function itoa_int16(i) result(res)
        implicit none
        integer(kind=int16),intent(in)      :: i
        character(len=:),allocatable        :: res

        character(len=digits(i))            :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
        return
    end function

    function itoa_int32(i) result(res)
        implicit none
        integer(kind=int32),intent(in)      :: i
        character(len=:),allocatable        :: res

        character(len=digits(i))            :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
        return
    end function

    function itoa_int64(i) result(res)
        implicit none
        integer(kind=int64),intent(in)      :: i
        character(len=:),allocatable        :: res

        character(len=digits(i))            :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
        return
    end function

    function rtoa_real32(x) result(res)
        implicit none
        real(kind=real32),intent(in)        :: x
        character(len=:),allocatable        :: res

        character(len=12)                   :: tmp

        write(tmp, '(ES12.5E2)') x
        res = trim(adjustl(tmp))
        return
    end function

    function rtoa_real64(x) result(res)
        implicit none
        real(kind=real64),intent(in)        :: x
        character(len=:),allocatable        :: res

        character(len=12)                   :: tmp

        write(tmp, '(ES12.5E2)') x
        res = trim(adjustl(tmp))
        return
    end function

end module messagehandler
