program test_deterministic_antialiasing_negative
    !! Determinism test for antialiasing with negative coordinates (fixes #691)
    !! Ensures identical output across repeated renders in a single run.

    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    character(len=*), parameter :: fn1 = 'build/test/output/aa_neg_run1.png'
    character(len=*), parameter :: fn2 = 'build/test/output/aa_neg_run2.png'

    real(wp) :: x(101), y(101)
    integer :: i
    logical :: ok

    ! Prepare data spanning negative and positive coordinates
    do i = 1, 101
        x(i) = real(i-51, wp) * 0.2_wp  ! range ~[-10, 10]
        y(i) = x(i)                      ! diagonal across origin
    end do

    ! First render
    call figure()
    call plot(x, y, 'neg-diag', 'b-')
    call grid(.true.)
    call title('AA determinism')
    call savefig(fn1)

    ! Second render (fresh figure) to the same content
    call figure()
    call plot(x, y, 'neg-diag', 'b-')
    call grid(.true.)
    call title('AA determinism')
    call savefig(fn2)

    call assert_same_file(fn1, fn2, ok)
    if (.not. ok) then
        print *, 'FAIL: Antialiasing output differs between runs for negative coords'
        error stop 1
    else
        print *, 'PASS: Antialiasing output identical across runs for negative coords'
    end if

contains
    subroutine assert_same_file(a, b, equal)
        character(len=*), intent(in) :: a, b
        logical, intent(out) :: equal
        integer :: ua, ub, sa, sb, na, nb, i, ios
        integer(1), allocatable :: ba(:), bb(:)

        inquire(file=a, size=sa)
        inquire(file=b, size=sb)
        if (sa /= sb .or. sa <= 0) then
            equal = .false.
            return
        end if

        open(newunit=ua, file=a, access='stream', form='unformatted', status='old', iostat=ios)
        if (ios /= 0) then
            equal = .false.
            return
        end if
        open(newunit=ub, file=b, access='stream', form='unformatted', status='old', iostat=ios)
        if (ios /= 0) then
            close(ua)
            equal = .false.
            return
        end if

        allocate(ba(sa))
        allocate(bb(sb))
        read(ua) ba
        read(ub) bb
        close(ua)
        close(ub)

        equal = .true.
        na = size(ba)
        nb = size(bb)
        if (na /= nb) then
            equal = .false.
        else
            do i = 1, na
                if (ba(i) /= bb(i)) then
                    equal = .false.
                    exit
                end if
            end do
        end if

        if (allocated(ba)) deallocate(ba)
        if (allocated(bb)) deallocate(bb)
    end subroutine assert_same_file

end program test_deterministic_antialiasing_negative
