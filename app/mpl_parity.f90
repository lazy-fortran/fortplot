program mpl_parity
    !! Render paired PNG/PDF cases for scripts/ref_matplotlib.py's oracle.
    !! Output directory must exist and is passed as argv(1).
    use fortplot, only: wp, figure, plot, xlabel, ylabel, title, legend, &
                        savefig, scatter, bar, hist, errorbar, set_yscale, &
                        grid, fill_between, subplot
    implicit none

    character(len=256) :: outdir

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
    call markers_plot()
    call grid_plot()
    call fill_plot()
    call subplots_plot()
    call math_plot('math_scripts', '$x_i^2 + y_{i+1}^{n-1} = e^{-x/3}$')
    call math_plot('math_fraction', '$\frac{1}{2} + \frac{x^2}{1+x}$')
    call math_plot('math_radical', '$\sqrt{x^2+y^2} = \alpha + \beta$')

contains

    function p(name) result(path)
        character(len=*), intent(in) :: name
        character(len=512) :: path
        path = trim(outdir)//'/'//name
    end function p

    subroutine save_pair(name)
        character(len=*), intent(in) :: name
        call savefig(trim(p('fp_'//name//'.png')))
        call savefig(trim(p('fp_'//name//'.pdf')))
    end subroutine save_pair

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
        call save_pair('line')
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
        call save_pair('scatter')
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
        call save_pair('bar')
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
        call save_pair('hist')
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
        call save_pair('errorbar')
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
        call save_pair('logy')
    end subroutine logy_plot

    subroutine markers_plot()
        real(wp) :: x(9)
        integer :: i
        x = [(real(i, wp), i=0, 8)]
        call figure()
        call plot(x, sin(x), linestyle='--', marker='o', label='circles')
        call plot(x, cos(x), linestyle=':', marker='s', label='squares')
        call xlabel('x')
        call ylabel('y')
        call title('Markers and Line Styles')
        call legend()
        call save_pair('markers')
    end subroutine markers_plot

    subroutine grid_plot()
        real(wp) :: x(100)
        integer :: i
        x = [(real(i, wp), i=0, 99)]/10.0_wp
        call figure()
        call plot(x, sin(x))
        call grid(.true.)
        call xlabel('x')
        call ylabel('y')
        call title('Default Grid')
        call save_pair('grid')
    end subroutine grid_plot

    subroutine fill_plot()
        real(wp) :: x(41)
        integer :: i
        x = [(real(i, wp), i=0, 40)]/10.0_wp
        call figure()
        call fill_between(x, sin(x), 0.5_wp*sin(x))
        call xlabel('x')
        call ylabel('y')
        call title('Filled Band')
        call save_pair('fill_between')
    end subroutine fill_plot

    subroutine subplots_plot()
        real(wp) :: x(30)
        character(len=7) :: label
        integer :: i
        x = [(real(i, wp), i=0, 29)]/5.0_wp
        call figure()
        do i = 1, 4
            call subplot(2, 2, i)
            call plot(x, sin(x + real(i, wp)))
            call xlabel('x')
            call ylabel('y')
            write (label, '(a,i1)') 'Panel ', i
            call title(label)
        end do
        call save_pair('subplots')
    end subroutine subplots_plot

    subroutine math_plot(name, expression)
        character(len=*), intent(in) :: name, expression
        real(wp) :: x(21)
        integer :: i
        x = [(real(i, wp), i=0, 20)]/10.0_wp
        call figure()
        call plot(x, x**2)
        call xlabel('$x_i$')
        call ylabel('$y^2$')
        call title(expression)
        call save_pair(name)
    end subroutine math_plot

end program mpl_parity
