program test_pdf_dpi_parity
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: W = 800, H = 600
    type(pdf_context) :: ctx
    character(len=:), allocatable :: path
    integer :: u, ios
    character(len=1024) :: line
    logical :: found
    real(wp) :: w_pt, h_pt
    logical :: dir_ok

    ! Create a simple PDF with a known pixel canvas size
    ctx = create_pdf_canvas(W, H)
    call create_directory_runtime('test/output', dir_ok)
    path = 'test/output/pdf_dpi_parity.pdf'
    call ctx%save(path)

    ! Scan for the MediaBox line and parse page dimensions (points)
    open(newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(path)
        stop 1
    end if

    found = .false.
    do
        read(u, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, '/MediaBox [0 0') > 0) then
            ! Expect format like: /MediaBox [0 0 <w> <h>]
            call parse_media_box(line, w_pt, h_pt, found)
            exit
        end if
    end do
    close(u)

    if (.not. found) then
        print *, 'FAIL: MediaBox not found in ', trim(path)
        stop 2
    end if

    ! 800x600 px at 100 DPI => 8x6 inches => 576x432 points
    if (abs(w_pt - 576.0_wp) > 0.5_wp .or. abs(h_pt - 432.0_wp) > 0.5_wp) then
        print *, 'FAIL: PDF MediaBox not 576x432 pt. Got:', w_pt, h_pt
        stop 3
    end if

    print *, 'PASS: PDF MediaBox scaled to 576x432 points for 800x600 px'

contains

    subroutine parse_media_box(s, w, h, ok)
        character(len=*), intent(in) :: s
        real(wp), intent(out) :: w, h
        logical, intent(out) :: ok
        integer :: i1, i2
        character(len=256) :: nums
        ok = .false.
        w = -1.0_wp; h = -1.0_wp
        i1 = index(s, '/MediaBox [0 0')
        if (i1 <= 0) return
        i2 = index(s(i1:), ']')
        if (i2 <= 0) return
        nums = adjustl(s(i1+len('/MediaBox [0 0'):i1+i2-2))
        read(nums, *, iostat=ios) w, h
        if (ios == 0) ok = .true.
    end subroutine parse_media_box

end program test_pdf_dpi_parity

