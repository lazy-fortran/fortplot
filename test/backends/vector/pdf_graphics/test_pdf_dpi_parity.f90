program test_pdf_dpi_parity
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_context, only: plot_context
    use fortplot_utils, only: initialize_backend
    use fortplot_test_helpers, only: test_get_temp_path
    implicit none

    type(pdf_context) :: ctx
    class(plot_context), allocatable :: backend
    character(len=:), allocatable :: path

    ctx = create_pdf_canvas(800, 600)
    path = test_get_temp_path('pdf_dpi_parity.pdf')
    call ctx%save(path)
    call check_page(path, 576.0_wp, 432.0_wp)

    ! Matplotlib's default 6.4 by 4.8 inches has fractional point dimensions.
    ctx = create_pdf_canvas(640, 480)
    path = test_get_temp_path('pdf_default_size.pdf')
    call ctx%save(path)
    call check_page(path, 460.8_wp, 345.6_wp)

    ! Doubling raster density must preserve physical PDF page size.
    call initialize_backend(backend, 'pdf', 1280, 960, dpi=200.0_wp)
    path = test_get_temp_path('pdf_200dpi_size.pdf')
    call backend%save(path)
    call check_page(path, 460.8_wp, 345.6_wp)

    ! Odd dimensions expose premature rounding in both pixel and point units.
    call initialize_backend(backend, 'pdf', 801, 601, dpi=150.0_wp)
    path = test_get_temp_path('pdf_150dpi_size.pdf')
    call backend%save(path)
    call check_page(path, 384.48_wp, 288.48_wp)

    print *, 'PASS: PDF pages preserve physical dimensions at 100, 150 and 200 DPI'

contains

    subroutine check_page(filename, expected_width, expected_height)
        character(len=*), intent(in) :: filename
        real(wp), intent(in) :: expected_width, expected_height
        integer :: u, ios
        character(len=1024) :: line
        logical :: found
        real(wp) :: w_pt, h_pt

        open (newunit=u, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) error stop 'Cannot open PDF page extent fixture'
        found = .false.
        do
            read (u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '/MediaBox [0 0') > 0) then
                call parse_media_box(line, w_pt, h_pt, found)
                exit
            end if
        end do
        close (u)
        if (.not. found) error stop 'PDF MediaBox is missing'
        if (abs(w_pt - expected_width) > 1.0e-6_wp .or. &
            abs(h_pt - expected_height) > 1.0e-6_wp) then
            print *, 'FAIL: expected point dimensions:', expected_width, expected_height
            print *, 'Actual point dimensions:', w_pt, h_pt
            error stop 'Incorrect physical PDF page size'
        end if
    end subroutine check_page

    subroutine parse_media_box(s, w, h, ok)
        character(len=*), intent(in) :: s
        real(wp), intent(out) :: w, h
        logical, intent(out) :: ok
        integer :: i1, i2, ios
        character(len=256) :: nums
        ok = .false.
        w = -1.0_wp
        h = -1.0_wp
        i1 = index(s, '/MediaBox [0 0')
        if (i1 <= 0) return
        i2 = index(s(i1:), ']')
        if (i2 <= 0) return
        nums = adjustl(s(i1 + len('/MediaBox [0 0'):i1 + i2 - 2))
        read (nums, *, iostat=ios) w, h
        if (ios == 0) ok = .true.
    end subroutine parse_media_box

end program test_pdf_dpi_parity
