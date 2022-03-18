module lapack90

    use, intrinsic :: iso_fortran_env, only: real32, real64

    interface f77_gtsv
        pure subroutine sgtsv(n,nrhs,dl,d,du,b,ldb,info)
            import                          :: real32
            integer, intent(in)             :: n
            integer, intent(in)             :: nrhs
            real(kind=real32), intent(inout):: dl(*)
            real(kind=real32), intent(inout):: d(*)
            real(kind=real32), intent(inout):: du(*)
            real(kind=real32), intent(inout):: b(ldb,*)
            integer, intent(in)             :: ldb
            integer, intent(out)            :: info
        end subroutine sgtsv
        pure subroutine dgtsv(n,nrhs,dl,d,du,b,ldb,info)
            import                          :: real64
            integer, intent(in)             :: n
            integer, intent(in)             :: nrhs
            real(kind=real64), intent(inout):: dl(*)
            real(kind=real64), intent(inout):: d(*)
            real(kind=real64), intent(inout):: du(*)
            real(kind=real64), intent(inout):: b(ldb,*)
            integer, intent(in)             :: ldb
            integer, intent(out)            :: info
        end subroutine dgtsv
    end interface

    interface f77_ptsv
        pure subroutine sptsv(n,nrhs,d,e,b,ldb,info)
            import                          :: real32
            integer, intent(in)             :: n
            integer, intent(in)             :: nrhs
            real(kind=real32), intent(inout):: d(*)
            real(kind=real32), intent(inout):: e(*)
            real(kind=real32), intent(inout):: b(ldb,*)
            integer, intent(in)             :: ldb
            integer, intent(out)            :: info
        end subroutine sptsv
        pure subroutine dptsv(n,nrhs,d,e,b,ldb,info)
            import                          :: real64
            integer, intent(in)             :: n
            integer, intent(in)             :: nrhs
            real(kind=real64), intent(inout):: d(*)
            real(kind=real64), intent(inout):: e(*)
            real(kind=real64), intent(inout):: b(ldb,*)
            integer, intent(in)             :: ldb
            integer, intent(out)            :: info
        end subroutine dptsv
    end interface

    interface gtsv
        module procedure f90_dgtsv, f90_dgtsv1, f90_sgtsv, f90_sgtsv1
    end interface gtsv

    interface ptsv
        module procedure f90_dptsv, f90_dptsv1, f90_sptsv, f90_sptsv1
    end interface ptsv

contains
    pure subroutine f90_dgtsv1(dl,d,du,b,info)
        implicit none
        real(kind=real64), intent(inout)    :: dl(:)
        real(kind=real64), intent(inout)    :: d(:)
        real(kind=real64), intent(inout)    :: du(:)
        real(kind=real64), target, intent(inout)    :: b(:)
        integer, intent(out), optional      :: info

        integer                             :: linfo, n, ldb
        real(kind=real64), pointer          :: b2(:,:)

        ldb = max(1,size(b,1))
        n=size(d)
        b2(1:ldb,1:1) => b

        call f77_gtsv(n,1,dl,d,du,b2,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_dgtsv1

    pure subroutine f90_sgtsv1(dl,d,du,b,info)
        implicit none
        real(kind=real32), intent(inout)    :: dl(:)
        real(kind=real32), intent(inout)    :: d(:)
        real(kind=real32), intent(inout)    :: du(:)
        real(kind=real32), target, intent(inout)    :: b(:)
        integer, intent(out), optional      :: info

        integer                             :: linfo,n,ldb
        real(kind=real32), pointer          :: b2(:,:)

        ldb = max(1,size(b,1))
        n = size(d)
        b2(1:ldb,1:1) => b

        call f77_gtsv(n,1,dl,d,du,b2,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_sgtsv1

    pure subroutine f90_dgtsv(dl,d,du,b,info)
        implicit none
        real(kind=real64), intent(inout)    :: dl(:)
        real(kind=real64), intent(inout)    :: d(:)
        real(kind=real64), intent(inout)    :: du(:)
        real(kind=real64), intent(inout)    :: b(:,:)
        integer, intent(out), optional      :: info

        integer                             :: linfo, n, nrhs, ldb

        ldb = max(1,size(b,1))
        n = size(d)
        nrhs = size(b,2)
        call f77_gtsv(n,nrhs,dl,d,du,b,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_dgtsv

    pure subroutine f90_sgtsv(dl,d,du,b,info)
        implicit none
        real(kind=real32), intent(inout)    :: dl(:)
        real(kind=real32), intent(inout)    :: d(:)
        real(kind=real32), intent(inout)    :: du(:)
        real(kind=real32), intent(inout)    :: b(:,:)
        integer, intent(out), optional      :: info

        integer                             :: linfo, n, nrhs, ldb

        ldb = max(1,size(b,1))
        n = size(d)
        nrhs = size(b,2)
        call f77_gtsv(n,nrhs,dl,d,du,b,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_sgtsv

    pure subroutine f90_dptsv1(d,e,b,info)
        implicit none
        real(kind=real64), intent(inout)    :: d(:)
        real(kind=real64), intent(inout)    :: e(:)
        real(kind=real64), target, intent(inout)    :: b(:)
        integer, intent(out), optional      :: info

        integer                             :: linfo, n, nrhs, ldb
        real(kind=real64), pointer          :: b2(:,:)

        ldb = max(1,size(b,1))
        n=size(d)
        nrhs = 1
        b2(1:ldb,1:1) => b

        call f77_ptsv(n,nrhs,d,e,b2,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_dptsv1

    pure subroutine f90_sptsv1(d,e,b,info)
        implicit none
        real(kind=real32), intent(inout)    :: d(:)
        real(kind=real32), intent(inout)    :: e(:)
        real(kind=real32), target, intent(inout)    :: b(:)
        integer, intent(out), optional      :: info

        integer                             :: linfo,n,ldb
        real(kind=real32), pointer          :: b2(:,:)

        ldb = max(1,size(b,1))
        n = size(d)
        b2(1:ldb,1:1) => b

        call f77_ptsv(n,1,d,e,b2,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_sptsv1

    pure subroutine f90_dptsv(d,e,b,info)
        implicit none
        real(kind=real64), intent(inout)    :: d(:)
        real(kind=real64), intent(inout)    :: e(:)
        real(kind=real64), intent(inout)    :: b(:,:)
        integer, intent(out), optional      :: info

        integer                             :: linfo, n, nrhs, ldb

        ldb = max(1,size(b,1))
        n = size(d)
        nrhs = size(b,2)
        call f77_ptsv(n,nrhs,d,e,b,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_dptsv

    pure subroutine f90_sptsv(d,e,b,info)
        implicit none
        real(kind=real32), intent(inout)    :: d(:)
        real(kind=real32), intent(inout)    :: e(:)
        real(kind=real32), intent(inout)    :: b(:,:)
        integer, intent(out), optional      :: info

        integer                             :: linfo, n, nrhs, ldb

        ldb = max(1,size(b,1))
        n = size(d)
        nrhs = size(b,2)
        call f77_ptsv(n,nrhs,d,e,b,ldb,linfo)
        if (present(info)) info=linfo
    end subroutine f90_sptsv
end module lapack90
