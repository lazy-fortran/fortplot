program test_pdf_line_style_patterns
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    type(pdf_context) :: ctx
    integer, parameter :: W = 200, H = 150
    character(len=:), allocatable :: path
    logical :: ok

    call create_directory_runtime('test/output', ok)

    ! Dashed: expect "[6 3] 0 d" in stream
    path = 'test/output/pdf_dashed_pattern.pdf'
    call draw_and_save('--', path)
    call assert_pdf_contains(path, '[6 3] 0 d', 'Dashed pattern [6 3] present')

    ! Dotted: expect "[1 3] 0 d" in stream
    path = 'test/output/pdf_dotted_pattern.pdf'
    call draw_and_save(':', path)
    call assert_pdf_contains(path, '[1 3] 0 d', 'Dotted pattern [1 3] present')

    ! Dash-dot: expect "[6 3 1 3] 0 d" in stream
    path = 'test/output/pdf_dashdot_pattern.pdf'
    call draw_and_save('-.', path)
    call assert_pdf_contains(path, '[6 3 1 3] 0 d', 'Dashdot pattern [6 3 1 3] present')

contains

    subroutine draw_and_save(style, filename)
        character(len=*), intent(in) :: style
        character(len=*), intent(in) :: filename
        real(wp) :: x1, y1, x2, y2

        ctx = create_pdf_canvas(W, H)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        call ctx%set_line_style(style)
        x1 = 1.0_wp; y1 = 1.0_wp
        x2 = 9.0_wp; y2 = 9.0_wp
        call ctx%line(x1, y1, x2, y2)
        call ctx%save(filename)
    end subroutine draw_and_save

    subroutine assert_pdf_contains(filename, expected, description)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: description
        integer :: unit, ios
        character(len=65536) :: buf
        integer :: read_len
        logical :: found

        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(*,'(A)') 'FAIL: could not open '//trim(filename)
            error stop 1
        end if
        buf = ''
        read_len = 0
        do
            read(unit, '(A)', iostat=ios) buf(read_len+1:)
            if (ios /= 0) exit
            read_len = len_trim(buf)
            if (read_len > len(buf) - 256) exit
        end do
        close(unit)

        found = index(buf, expected) > 0
        if (.not. found) then
            write(*,'(A)') 'FAIL: expected token not found: '//trim(expected)
            write(*,'(A)') trim(description)
            error stop 2
        end if
        write(*,'(A)') 'PASS: '//trim(description)
    end subroutine assert_pdf_contains

end program test_pdf_line_style_patterns

