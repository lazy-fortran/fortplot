program test_tight_layout_public_api
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, title, xlabel, ylabel, suptitle, &
                        tight_layout, savefig
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), dimension(5) :: x, y1, y2, y3, y4
    character(len=:), allocatable :: output_dir

    call ensure_test_output_dir('tight_layout_public_api', output_dir)

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y1 = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
    y2 = [25.0_wp, 16.0_wp, 9.0_wp, 4.0_wp, 1.0_wp]
    y3 = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y4 = [5.0_wp, 4.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]

    call figure(figsize=[8.0_wp, 6.0_wp])
    call subplot(2, 2, 1)
    call plot(x, y1, label='Plot 1')
    call title('Subplot (1,1)')
    call xlabel('X axis')
    call ylabel('Y axis')

    call subplot(2, 2, 2)
    call plot(x, y2, label='Plot 2')
    call title('Subplot (1,2)')
    call xlabel('X axis')
    call ylabel('Y axis')

    call subplot(2, 2, 3)
    call plot(x, y3, label='Plot 3')
    call title('Subplot (2,1)')
    call xlabel('X axis')
    call ylabel('Y axis')

    call subplot(2, 2, 4)
    call plot(x, y4, label='Plot 4')
    call title('Subplot (2,2)')
    call xlabel('X axis')
    call ylabel('Y axis')

    call suptitle('2x2 Subplot Grid')
    call tight_layout()
    call savefig(trim(output_dir)//'test_tight_layout_public_api.png')
end program test_tight_layout_public_api
