program mpl_parity
    !! Local-only harness: render a battery of plots for matplotlib parity
    !! comparison. Not committed. Output dir passed as argv(1).
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    character(len=256) :: outdir
    integer :: n

    if (command_argument_count() < 1) then
        write (*, '(a)') 'Usage: mpl_parity <outdir>'
        error stop 1
    end if
    call get_command_argument(1, outdir)
    outdir = trim(outdir)

    call line_plot()
    call scatter_plot()
    call bar_plot()
    call hist_plot()
    call errorbar_plot()
    call logy_plot()

contains

    function p(name) result(path)
        character(len=*), intent(in) :: name
        character(len=512) :: path
        path = trim(outdir)//'/'//name
    end function p

    subroutine line_plot()
        real(wp) :: x(100), sx(100), cx(100)
        integer :: i
        x = [(real(i, wp), i=0, 99)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        call figure()
        call plot(x, sx, label='sin(x)')
        call plot(x, cx, label='cos(x)')
        call xlabel('x')
        call ylabel('y')
        call title('Sine and Cosine Functions')
        call legend()
        call savefig(trim(p('fp_line.png')))
    end subroutine line_plot

    subroutine scatter_plot()
        real(wp) :: x(20), y(20)
        integer :: i
        x = [(real(i, wp), i=1, 20)]
        y = [(real(i*i, wp), i=1, 20)]
        call figure()
        call scatter(x, y)
        call xlabel('x')
        call ylabel('y')
        call title('Scatter')
        call savefig(trim(p('fp_scatter.png')))
    end subroutine scatter_plot

    subroutine bar_plot()
        real(wp) :: x(4), h(4)
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        h = [4.5_wp, 5.8_wp, 6.1_wp, 6.7_wp]
        call figure()
        call bar(x, h)
        call xlabel('category')
        call ylabel('value')
        call title('Bar')
        call savefig(trim(p('fp_bar.png')))
    end subroutine bar_plot

    subroutine hist_plot()
        real(wp) :: d(200)
        integer :: i
        do i = 1, 200
            d(i) = real(mod(i*13 + 7, 100), wp)/10.0_wp
        end do
        call figure()
        call hist(d, bins=10)
        call xlabel('value')
        call ylabel('count')
        call title('Histogram')
        call savefig(trim(p('fp_hist.png')))
    end subroutine hist_plot

    subroutine errorbar_plot()
        real(wp) :: x(5), y(5), ye(5)
        integer :: i
        x = [(real(i, wp), i=1, 5)]
        y = [2.0_wp, 4.0_wp, 5.0_wp, 4.5_wp, 6.0_wp]
        ye = 0.5_wp
        call figure()
        call errorbar(x, y, yerr=ye)
        call xlabel('x')
        call ylabel('y')
        call title('Errorbar')
        call savefig(trim(p('fp_errorbar.png')))
    end subroutine errorbar_plot

    subroutine logy_plot()
        real(wp) :: x(50), y(50)
        integer :: i
        x = [(real(i, wp), i=1, 50)]
        y = [(10.0_wp**(real(i, wp)/12.0_wp), i=1, 50)]
        call figure()
        call plot(x, y)
        call set_yscale('log')
        call xlabel('x')
        call ylabel('y')
        call title('Log Y')
        call savefig(trim(p('fp_logy.png')))
    end subroutine logy_plot

end program mpl_parity
