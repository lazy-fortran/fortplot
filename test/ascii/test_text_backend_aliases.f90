program test_text_backend_aliases
    !! Text-family alias API for the character-cell backend (issue #2059).
    !!
    !! The preferred names live in fortplot_text_backend and must behave
    !! identically to the fortplot_ascii compatibility API. text_context and
    !! ascii_context are the same derived type, so one draw routine accepts
    !! canvases built through either factory and both save byte-identical text.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text_backend, only: text_context, create_text_canvas, &
        render_text_legend, backend_is_text
    use fortplot_ascii, only: ascii_context, create_ascii_canvas
    use fortplot_legend, only: legend_t, create_legend
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    type(text_context) :: tctx
    type(ascii_context) :: actx
    type(legend_t) :: legend
    character(len=*), parameter :: text_file = 'build/test/output/text_alias_text.txt'
    character(len=*), parameter :: ascii_file = 'build/test/output/text_alias_ascii.txt'
    logical :: dir_ok
    character(len=:), allocatable :: text_render, ascii_render

    call create_directory_runtime('build/test/output', dir_ok)

    tctx = create_text_canvas(80, 24)
    if (.not. backend_is_text(tctx)) then
        print *, 'FAIL: backend_is_text did not recognise text canvas'
        stop 1
    end if

    legend = create_legend()
    call legend%add_entry('sin(x)', [0.0_wp, 0.0_wp, 1.0_wp])
    call render_text_legend(legend, tctx, 2.0_wp, 2.0_wp)

    tctx = create_text_canvas(80, 24)
    actx = create_ascii_canvas(80, 24)
    call draw_reference_plot(tctx)
    call draw_reference_plot(actx)

    call tctx%save(text_file)
    call actx%save(ascii_file)

    text_render = read_whole_file(text_file)
    ascii_render = read_whole_file(ascii_file)

    if (len(text_render) == 0) then
        print *, 'FAIL: text backend produced empty output'
        stop 1
    end if
    if (text_render /= ascii_render) then
        print *, 'FAIL: text and ascii backends produced different output'
        stop 1
    end if

    print *, 'PASS: text-family aliases match ascii backend'

contains

    subroutine draw_reference_plot(ctx)
        type(text_context), intent(inout) :: ctx

        call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
        call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
        call ctx%line(0.0_wp, 0.2_wp, 1.0_wp, 0.8_wp)
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)
        call ctx%line(0.0_wp, 0.8_wp, 1.0_wp, 0.2_wp)
    end subroutine draw_reference_plot

    function read_whole_file(filename) result(contents)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: contents
        character(len=512) :: line
        integer :: unit, ios

        contents = ''
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot read ', filename
            stop 1
        end if
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            contents = contents//trim(line)//new_line('a')
        end do
        close(unit)
    end function read_whole_file

end program test_text_backend_aliases
