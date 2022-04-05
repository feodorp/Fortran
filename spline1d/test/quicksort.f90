module quicksort

    use kinds

    implicit none
    private

    integer, parameter                      :: mselect = 20

    type :: limits
       integer                              :: ileft, iright
    end type limits

    interface qsort
        module procedure qsort_index_rp
        module procedure qsort_rp
    end interface qsort

    interface choosepiv
        module procedure choosepiv_rp
    end interface choosepiv

    public :: qsort

contains

    subroutine qsort_index_rp(x, ind)
        implicit none
        real(kind=rp), intent(in)       :: x(:)
        integer, intent(out)                :: ind(:)

        ! local variables
        integer                             :: n, i, j, itmp
        integer                             :: stkpos, ileft, iright
        type(limits)                        :: stack(32)
        real(kind=rp)                   :: xipv

        n = size(x,1)
        if (n <= 0) then
            print*,'Dimension of x is less then 1!'
            return
        end if
        if (n /= size(ind,1)) then
            print*,'ERROR: Dimension of x and index array must be same!'
            return
        end if
        if (n == 1) then
            ind(1) = 1
            return
        end if

        ind = [(i,i=1,n)]

        stkpos = 1
        stack(1)%ileft  = 1
        stack(1)%iright = n

        do while (stkpos > 0)
            ileft = stack(stkpos)%ileft
            iright = stack(stkpos)%iright
            stkpos = stkpos - 1

            if (iright-ileft <= mselect .and. iright-ileft > 0) then
                do i = ileft+1, iright
                    do j = i, ileft+1, -1
                       if (x(ind(j-1)) > x(ind(j))) then
                           itmp     = ind(j)
                           ind(j)   = ind(j-1)
                           ind(j-1) = itmp
                       else
                           exit
                       end if
                    end do
                end do
            elseif (iright-ileft > mselect) then
                xipv = choosepiv(x, ileft, iright, ind)
                i = ileft
                j = iright
                do
                    do while (x(ind(i)) < xipv)
                        i = i+1
                    end do
                    do while (x(ind(j)) > xipv)
                        j = j-1
                    end do
                    if (i < j) then
                        itmp = ind(i)
                        ind(i) = ind(j)
                        ind(j) = itmp
                        i = i+1
                        j = j-1
                    else
                        exit
                    end if
                end do
                if (j-ileft > iright-j-1) then
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = ileft
                    stack(stkpos)%iright = j
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = j+1
                    stack(stkpos)%iright = iright
                else
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = j+1
                    stack(stkpos)%iright = iright
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = ileft
                    stack(stkpos)%iright = j
                end if
            end if
        end do
        return
    end subroutine qsort_index_rp


    subroutine qsort_rp(x)
        implicit none
        real(kind=rp), intent(inout)    :: x(:)

        ! local variables
        integer                             :: n, i, j
        integer                             :: stkpos, ileft, iright
        real(kind=rp)                   :: xtmp
        type(limits)                        :: stack(32)
        real(kind=rp)                   :: xipv

        n = size(x)
        if (n <= 0) then
            print*,'Dimension of x is less then 1!'
            return
        end if
        if (n == 1) then
            return
        end if

         stkpos = 1
         stack(1)%ileft  = 1
         stack(1)%iright = n

         do while (stkpos > 0)
            ileft = stack(stkpos)%ileft
            iright = stack(stkpos)%iright
            stkpos = stkpos - 1

            if (iright-ileft <= mselect .and. iright-ileft > 0) then
                do i = ileft+1, iright
                    do j = i, ileft+1, -1
                       if (x(j-1) > x(j)) then
                           xtmp   = x(j)
                           x(j)   = x(j-1)
                           x(j-1) = xtmp
                       else
                           exit
                       end if
                    end do
                end do
            elseif (iright-ileft > mselect) then
                xipv = choosepiv(x, ileft, iright)
                i = ileft
                j = iright
                do
                    do while (x(i) < xipv)
                        i = i+1
                    end do
                    do while (x(j) > xipv)
                        j = j-1
                    end do
                    if (i < j) then
                        xtmp = x(i)
                        x(i) = x(j)
                        x(j) = xtmp
                        i = i+1
                        j = j-1
                    else
                        exit
                    end if
                end do
                if (j-ileft > iright-j-1) then
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = ileft
                    stack(stkpos)%iright = j
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = j+1
                    stack(stkpos)%iright = iright
                else
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = j+1
                    stack(stkpos)%iright = iright
                    stkpos = stkpos + 1
                    stack(stkpos)%ileft = ileft
                    stack(stkpos)%iright = j
                end if
            end if
         end do
         return
    end subroutine qsort_rp

    pure real(kind=rp) function choosepiv_rp(x, ileft, iright, ind) result (xipv)
        implicit none
        real(kind=rp), intent(in)       :: x(:)
        integer, intent(in)                 :: ileft, iright
        integer, intent(in), optional       :: ind(:)

        integer                             :: imid
        real(kind=rp)                   :: xleft, xright, xmid

        imid = ileft+shiftr(iright-ileft,1)

        if (present(ind)) then
            xleft = x(ind(ileft))
            xright = x(ind(iright))
            xmid = x(ind(imid))
        else
            xleft = x(ileft)
            xright = x(iright)
            xmid = x(imid)
        end if

        if (xleft < xright) then
            if (xmid < xleft) then
                xipv = xleft
            elseif (xmid < xright) then
                xipv = xmid
            else
                xipv = xright
            end if
        else
            if (xmid < xright) then
                xipv = xright
            elseif (xmid < xleft) then
                xipv = xmid
            else
                xipv = xleft
            end if
        end if
    end function choosepiv_rp

end module
