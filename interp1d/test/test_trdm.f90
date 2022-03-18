program test_trdm
    use kinds
    use interp1d
    use lapack90
    use time_utils, only: stopwatch, time2str
    implicit none

    integer, parameter                      :: n=1000000, m = 100
    real(kind=rp)                           :: d(n),l(n),b(n),u(n)
    real(kind=rp)                           :: d1(n),l1(n),u1(n),b1(n),b2(n)
    integer                                 :: info, i
    type(stopwatch)                         :: t

    call random_number(l)
    call random_number(b)

    l(:) = l(:)+1.0_rp
    d(:) = [2.0_rp*l(1),2.0_rp*(l(1:n-2)+l(2:n-1)), 2.0_rp*l(n-1)]

    do i = 1, m
        d1(:) = d(:)
        l1(:) = l(:)
        b1(:) = b(:)
        call t%start()
        call trdss(d1,l1,b1)
        call t%stop()
    end do
    print*, 'Tridiagonal symetric system solver: ', time2str(t%total_time())
    b2(1) = d(1)*b1(1)+l(1)*b1(2)
    do i=2,n-1
        b2(i) = l(i-1)*b1(i-1)+d(i)*b1(i)+l(i)*b1(i+1)
    end do
    b2(n) = l(n-1)*b1(n-1)+d(n)*b1(n)
    print*,'Error: ',norm2(b2(:) - b(:))/norm2(b(:))

    call t%reset()
    do i = 1, m
        d1(:) = d(:)
        l1(:) = l(:)
        u1(:) = l(:)
        b1(:) = b(:)
        call t%start()
        call trdss(l1,d1,u1,b1)
        call t%stop()
    end do
    print*, 'Tridiagonal generic system solver: ', time2str(t%total_time())
    b2(1) = d(1)*b1(1)+l(1)*b1(2)
    do i=2,n-1
        b2(i) = l(i-1)*b1(i-1)+d(i)*b1(i)+l(i)*b1(i+1)
    end do
    b2(n) = l(n-1)*b1(n-1)+d(n)*b1(n)
    print*,'Error: ',norm2(b2(:) - b(:))/norm2(b(:))


    call t%reset()
    do i = 1, m
        d1(:) = d(:)
        l1(:) = l(:)
        b1(:) = b(:)
        call t%start()
        call dptsv(n,1,d1(1:n),l1(1:n-1),b1(1:n),n,info)
        call t%stop()
    end do
    print*, info
    print*, 'PTSV: ', time2str(t%total_time())
    b2(1) = d(1)*b1(1)+l(1)*b1(2)
    do i=2,n-1
        b2(i) = l(i-1)*b1(i-1)+d(i)*b1(i)+l(i)*b1(i+1)
    end do
    b2(n) = l(n-1)*b1(n-1)+d(n)*b1(n)
    print*,'Error: ',norm2(b2(:) - b(:))/norm2(b(:))



end
