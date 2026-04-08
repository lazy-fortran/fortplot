program fortplot_spec_parity
    !! Generate reference Vega-Lite JSON specs for parity testing.
    !! Produces JSON files that the Python API must match structurally.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot_spec_builder, only: vl_line, vl_point, vl_bar, &
                                     vl_layer_add, spec_savefig
    use fortplot_spec_types, only: spec_t
    implicit none

    character(len=256) :: outdir
    integer :: nargs

    nargs = command_argument_count()
    if (nargs < 1) then
        write (*, '(a)') 'Usage: fortplot_spec_parity <output_dir>'
        error stop 1
    end if
    call get_command_argument(1, outdir)

    call generate_line_spec(trim(outdir))
    call generate_scatter_spec(trim(outdir))
    call generate_bar_spec(trim(outdir))
    call generate_labeled_spec(trim(outdir))

    write (*, '(a)') 'All reference specs generated successfully'

contains

    subroutine generate_line_spec(dir)
        character(len=*), intent(in) :: dir
        type(spec_t) :: spec
        real(dp) :: x(5), y(5)
        integer :: i, status

        do i = 1, 5
            x(i) = real(i, dp)
            y(i) = real(i*i, dp)
        end do

        spec = vl_line(x, y, title='Parity Line', &
                       xlabel='X Axis', ylabel='Y Axis', &
                       width=400, height=300)
        call spec_savefig(spec, dir//'/line.vl.json', status)
        if (status /= 0) error stop 'Failed to write line spec'
    end subroutine generate_line_spec

    subroutine generate_scatter_spec(dir)
        character(len=*), intent(in) :: dir
        type(spec_t) :: spec
        real(dp) :: x(5), y(5)
        integer :: i, status

        do i = 1, 5
            x(i) = real(i, dp)
            y(i) = real(i*i, dp)
        end do

        spec = vl_point(x, y, title='Parity Scatter', &
                        xlabel='X Axis', ylabel='Y Axis', &
                        width=400, height=300)
        call spec_savefig(spec, dir//'/scatter.vl.json', status)
        if (status /= 0) error stop 'Failed to write scatter spec'
    end subroutine generate_scatter_spec

    subroutine generate_bar_spec(dir)
        character(len=*), intent(in) :: dir
        type(spec_t) :: spec
        real(dp) :: x(5), y(5)
        integer :: i, status

        do i = 1, 5
            x(i) = real(i, dp)
            y(i) = real(i*i, dp)
        end do

        spec = vl_bar(x, y, title='Parity Bar', &
                      xlabel='X Axis', ylabel='Y Axis', &
                      width=400, height=300)
        call spec_savefig(spec, dir//'/bar.vl.json', status)
        if (status /= 0) error stop 'Failed to write bar spec'
    end subroutine generate_bar_spec

    subroutine generate_labeled_spec(dir)
        character(len=*), intent(in) :: dir
        type(spec_t) :: spec
        real(dp) :: x(5), y1(5), y2(5)
        integer :: i, status

        do i = 1, 5
            x(i) = real(i, dp)
            y1(i) = real(i*i, dp)
            y2(i) = real(i*i*i, dp)
        end do

        spec = vl_line(x, y1, title='Parity Labeled', &
                       xlabel='X Axis', ylabel='Y Axis', &
                       width=400, height=300)
        call vl_layer_add(spec, 'line', x, y2, label='cubic')
        call spec_savefig(spec, dir//'/labeled.vl.json', status)
        if (status /= 0) error stop 'Failed to write labeled spec'
    end subroutine generate_labeled_spec

end program fortplot_spec_parity
