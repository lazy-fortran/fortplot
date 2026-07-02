program test_ascii_charset_2060
    !! Text-backend charset selection (issue #2060).
    !!
    !! Proves that the default/ASCII charset keeps pure-ASCII output bytes, that
    !! the Unicode charset draws a box-drawing frame plus Unicode markers/arrows,
    !! and that Unicode annotation text survives Unicode output while the ASCII
    !! charset sanitizes it. Covers the positive and negative fixtures from the
    !! issue.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    character(len=*), parameter :: U_TL = achar(226)//achar(148)//achar(140)
    character(len=*), parameter :: U_HLINE = achar(226)//achar(148)//achar(128)
    character(len=*), parameter :: U_VLINE = achar(226)//achar(148)//achar(130)
    character(len=*), parameter :: U_RIGHT = achar(226)//achar(134)//achar(146)
    character(len=*), parameter :: U_LEFT = achar(226)//achar(134)//achar(144)
    character(len=*), parameter :: U_UP = achar(226)//achar(134)//achar(145)
    character(len=*), parameter :: U_NE = achar(226)//achar(134)//achar(151)
    character(len=*), parameter :: U_SE = achar(226)//achar(134)//achar(152)
    character(len=*), parameter :: GREEK_ALPHA = achar(206)//achar(177)

    logical :: dir_ok

    call create_directory_runtime('build/test/output', dir_ok)

    call test_ascii_is_pure_ascii()
    call test_unicode_frame_is_box_drawing()
    call test_unicode_has_markers_or_arrows()
    call test_unicode_preserves_annotation()

    print *, 'PASS: test_ascii_charset_2060'

contains

    subroutine build_line_figure(fig)
        type(figure_t), intent(out) :: fig
        real(wp) :: x(60), sx(60)
        integer :: i

        x = [(real(i, wp), i=0, size(x) - 1)]/6.0_wp
        sx = sin(x)
        call fig%initialize(80, 24)
        call fig%set_title('Charset Test')
        call fig%set_xlabel('X')
        call fig%add_plot(x, sx, label='sin(x)')
    end subroutine build_line_figure

    subroutine test_ascii_is_pure_ascii()
        !! Positive + negative fixture: the ASCII charset must emit only
        !! printable ASCII bytes plus whitespace for a simple line plot.
        character(len=*), parameter :: outfile = &
            'build/test/output/charset_ascii_2060.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes
        integer :: i, v

        call build_line_figure(fig)
        call fig%set_text_charset('ascii')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (len(bytes) <= 0) then
            print *, 'FAIL: ASCII charset produced no output'
            stop 1
        end if
        do i = 1, len(bytes)
            v = iachar(bytes(i:i))
            if (v == 9 .or. v == 10 .or. v == 13) cycle
            if (v >= 32 .and. v <= 126) cycle
            print *, 'FAIL: ASCII charset emitted non-ASCII byte', v, 'at', i
            stop 1
        end do
        if (index(bytes, '+') == 0) then
            print *, 'FAIL: ASCII frame missing + corners'
            stop 1
        end if
    end subroutine test_ascii_is_pure_ascii

    subroutine test_unicode_frame_is_box_drawing()
        !! Positive fixture: Unicode frame uses box-drawing glyphs. Negative
        !! fixture: the frame must not stay all +, -, | borders.
        character(len=*), parameter :: outfile = &
            'build/test/output/charset_unicode_2060.txt'
        type(figure_t) :: fig
        character(len=:), allocatable :: bytes

        call build_line_figure(fig)
        call fig%set_text_charset('unicode')
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        if (index(bytes, U_TL) == 0) then
            print *, 'FAIL: Unicode frame missing top-left box corner'
            stop 1
        end if
        if (index(bytes, U_HLINE) == 0) then
            print *, 'FAIL: Unicode frame missing box horizontal line'
            stop 1
        end if
        if (index(bytes, U_VLINE) == 0) then
            print *, 'FAIL: Unicode frame missing box vertical line'
            stop 1
        end if
        if (.not. first_border_is_box(bytes)) then
            print *, 'FAIL: Unicode frame still an all-ASCII border'
            stop 1
        end if
    end subroutine test_unicode_frame_is_box_drawing

    subroutine test_unicode_has_markers_or_arrows()
        !! Positive fixture: Unicode markers/arrows render where applicable. A
        !! circular quiver field emits compass glyphs that map to Unicode arrows.
        character(len=*), parameter :: outfile = &
            'build/test/output/charset_unicode_quiver_2060.txt'
        integer, parameter :: ng = 7, n = ng*ng
        type(figure_t) :: fig
        real(wp) :: x(n), y(n), u(n), v(n)
        real(wp) :: xi, yj
        integer :: i, j, k
        character(len=:), allocatable :: bytes
        logical :: has_arrow

        k = 0
        do j = 1, ng
            do i = 1, ng
                k = k + 1
                xi = -2.0_wp + 4.0_wp*real(i - 1, wp)/real(ng - 1, wp)
                yj = -2.0_wp + 4.0_wp*real(j - 1, wp)/real(ng - 1, wp)
                x(k) = xi
                y(k) = yj
                u(k) = -yj
                v(k) = xi
            end do
        end do

        call fig%initialize(80, 24)
        call fig%set_text_charset('unicode')
        call fig%quiver(x, y, u, v)
        call fig%savefig(outfile)

        bytes = read_file_bytes(outfile)
        has_arrow = index(bytes, U_RIGHT) > 0
        has_arrow = has_arrow .or. index(bytes, U_LEFT) > 0
        has_arrow = has_arrow .or. index(bytes, U_UP) > 0
        has_arrow = has_arrow .or. index(bytes, U_NE) > 0
        has_arrow = has_arrow .or. index(bytes, U_SE) > 0
        if (.not. has_arrow) then
            print *, 'FAIL: Unicode quiver emitted no Unicode arrow glyphs'
            stop 1
        end if
    end subroutine test_unicode_has_markers_or_arrows

    subroutine test_unicode_preserves_annotation()
        !! Positive fixture: the Unicode charset preserves a Unicode ylabel that
        !! the ASCII charset must sanitize away.
        character(len=*), parameter :: ascii_file = &
            'build/test/output/charset_annot_ascii_2060.txt'
        character(len=*), parameter :: unicode_file = &
            'build/test/output/charset_annot_unicode_2060.txt'
        type(figure_t) :: fig_a, fig_u
        character(len=:), allocatable :: ascii_bytes, unicode_bytes

        call build_line_figure(fig_a)
        call fig_a%set_ylabel(GREEK_ALPHA)
        call fig_a%set_text_charset('ascii')
        call fig_a%savefig(ascii_file)

        call build_line_figure(fig_u)
        call fig_u%set_ylabel(GREEK_ALPHA)
        call fig_u%set_text_charset('unicode')
        call fig_u%savefig(unicode_file)

        ascii_bytes = read_file_bytes(ascii_file)
        unicode_bytes = read_file_bytes(unicode_file)

        if (index(unicode_bytes, GREEK_ALPHA) == 0) then
            print *, 'FAIL: Unicode charset dropped Unicode annotation text'
            stop 1
        end if
        if (index(ascii_bytes, GREEK_ALPHA) > 0) then
            print *, 'FAIL: ASCII charset leaked raw Unicode annotation bytes'
            stop 1
        end if
    end subroutine test_unicode_preserves_annotation

    logical function first_border_is_box(bytes) result(is_box)
        !! The first frame border line must start with the box top-left corner,
        !! not an all-ASCII '+---+' border.
        character(len=*), intent(in) :: bytes
        integer :: p

        p = index(bytes, U_TL)
        is_box = (p > 0)
        if (.not. is_box) return
        if (index(bytes, '+' // repeat('-', 80) // '+') > 0) is_box = .false.
    end function first_border_is_box

    function read_file_bytes(fname) result(bytes)
        character(len=*), intent(in) :: fname
        character(len=:), allocatable :: bytes
        integer :: unit, nbytes, ios

        open(newunit=unit, file=fname, access='stream', form='unformatted', &
            status='old', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', fname
            stop 1
        end if
        inquire(unit=unit, size=nbytes)
        if (nbytes < 0) nbytes = 0
        allocate(character(len=nbytes) :: bytes)
        if (nbytes > 0) read(unit) bytes
        close(unit)
    end function read_file_bytes

end program test_ascii_charset_2060
