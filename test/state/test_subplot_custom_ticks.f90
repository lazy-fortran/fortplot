program test_subplot_custom_ticks
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, set_xticks, &
        set_yticks, xlim, ylim, savefig, get_global_figure, figure_t
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none
    integer, parameter :: width = 800, height = 800
    real(wp) :: labelled(width, height, 3), blank(width, height, 3)
    type(figure_t), pointer :: fig
    character(len=:), allocatable :: output_dir
    character(len=4096) :: line
    integer :: unit, status, alpha_count, beta_count, up_count, down_count

    call ensure_test_output_dir('subplot_custom_ticks', output_dir)
    call render_panels(.true., labelled)
    call savefig(output_dir//'panels.png')
    call savefig(output_dir//'panels.pdf')
    call savefig(output_dir//'panels.svg')
    call render_panels(.false., blank)
    ! The requested words must add raster ink in each configured panel.
    if (count(abs(labelled(:, :height/3, :) - blank(:, :height/3, :)) > 0.2_wp) &
        < 100) error stop 'upper-panel custom tick labels were not drawn'
    if (count(abs(labelled(:, height/3 + 1:2*height/3, :) - &
        blank(:, height/3 + 1:2*height/3, :)) > 0.2_wp) < 100) &
        error stop 'middle-panel custom tick labels were not drawn'
    if (any(abs(labelled(:, 2*height/3 + 1:, :) - &
        blank(:, 2*height/3 + 1:, :)) > 0.2_wp)) &
        error stop 'custom ticks leaked into the unconfigured panel'

    alpha_count = 0; beta_count = 0; up_count = 0; down_count = 0
    open(newunit=unit, file=output_dir//'panels.svg', status='old')
    do
        read(unit, '(a)', iostat=status) line
        if (status /= 0) exit
        if (index(line, '>ALPHA</text>') > 0) then
            alpha_count = alpha_count + 1
            if (index(line, 'x="255.000"') == 0) &
                error stop 'SVG upper tick moved to another panel position'
        end if
        if (index(line, '>BETA</text>') > 0) beta_count = beta_count + 1
        if (index(line, '>UPPER</text>') > 0) up_count = up_count + 1
        if (index(line, '>LOWER</text>') > 0) down_count = down_count + 1
    end do
    close(unit)
    if (alpha_count /= 1 .or. beta_count /= 1 .or. up_count /= 1 .or. &
        down_count /= 1) error stop 'SVG lost or leaked per-panel custom labels'
    print '(a)', 'PASS per-panel custom x/y ticks in raster, PDF and SVG outputs'
contains
    subroutine render_panels(with_labels, rgb)
        logical, intent(in) :: with_labels
        real(wp), intent(out) :: rgb(:, :, :)
        character(len=5) :: xlabels(2), ylabels(2)
        integer :: panel

        xlabels = ['ALPHA', 'BETA ']
        ylabels = ['UPPER', 'LOWER']
        if (.not. with_labels) then
            xlabels = ''
            ylabels = ''
        end if
        call figure(figsize=[8.0_wp, 8.0_wp])
        do panel = 1, 3
            call subplot(3, 1, panel)
            call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
            call xlim(0.0_wp, 1.0_wp)
            call ylim(0.0_wp, 1.0_wp)
            select case (panel)
            case (1)
                call set_xticks([0.0_wp, 0.5_wp, 1.0_wp])
                call set_xticks([0.25_wp], xlabels(:1))
                call set_yticks([0.25_wp], ylabels(:1))
            case (2)
                call set_xticks([0.75_wp], xlabels(2:))
                call set_yticks([0.75_wp], ylabels(2:))
            end select
        end do
        ! Revisit the first panel to catch setters that accidentally target the last.
        call subplot(3, 1, 1)
        fig => get_global_figure()
        call fig%set_xtick_labels(xlabels(:1))
        call fig%setup_png_backend_for_animation()
        call fig%extract_rgb_data_for_animation(rgb)
    end subroutine render_panels
end program test_subplot_custom_ticks
