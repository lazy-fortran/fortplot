program test_subplot_axes_properties
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot, only: figure, subplot, plot, title, xlabel, ylabel, legend, &
                        xlim, ylim, tight_layout, savefig, get_global_figure, figure_t
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none
    type(figure_t), pointer :: fig
    integer, parameter :: width = 640, height = 640
    real(wp) :: rgb(width, height, 3)
    character(len=:), allocatable :: output_dir
    integer :: axis_bottom, first_ink, last_tick, first_label, row, pixels
    integer :: legend_rows(3), legend_count
    logical :: ink, previous_ink

    call ensure_test_output_dir('subplot_axes_properties', output_dir)
    call figure(figsize=[6.4_wp, 6.4_wp])
    call subplot(2, 1, 1)
    call title('Upper panel')
    call xlabel('UPPERLABEL')
    call ylabel('Upper quantity')
    ! Set bounds before artists, as consumers commonly do.
    call xlim(0.0_wp, 2.0_wp)
    call ylim(0.0_wp, 0.07_wp)
    call plot([1.0_wp, 1.0_wp], [0.0625_wp, 0.0625_wp], &
              marker='o', label='UPPER ONLY', color=[0.0_wp, 0.0_wp, 1.0_wp])
    call plot([1.0_wp, 1.0_wp], [0.0625_wp, 0.0625_wp], &
              label='UPPER TWO', color=[0.0_wp, 0.0_wp, 1.0_wp])
    call plot([1.0_wp, 1.0_wp], [0.0625_wp, 0.0625_wp], &
              label='UPPER THREE', color=[0.0_wp, 0.0_wp, 1.0_wp])
    call legend(loc='upper right')
    call subplot(2, 1, 2)
    call title('Lower panel')
    call xlabel('BOTTOMLABEL')
    call ylabel('Lower quantity')
    call xlim(-2.0_wp, 2.0_wp)
    call ylim(-1.0_wp, 1.0_wp)
    call plot([0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp], &
              marker='o', label='LOWER ONLY', color=[1.0_wp, 0.0_wp, 0.0_wp])
    call legend(loc='lower left')
    call tight_layout()
    fig => get_global_figure()
    ! Independent requested-value oracle, including bound retention after plot().
    call expect(fig%subplots_array(1, 1)%x_min, 0.0_wp, 'upper requested xmin')
    call expect(fig%subplots_array(1, 1)%x_max, 2.0_wp, 'upper requested xmax')
    call expect(fig%subplots_array(1, 1)%y_min, 0.0_wp, 'upper requested ymin')
    call expect(fig%subplots_array(1, 1)%y_max, 0.07_wp, 'upper requested ymax')
    call expect(fig%subplots_array(2, 1)%x_min, -2.0_wp, 'lower requested xmin')
    call expect(fig%subplots_array(2, 1)%x_max, 2.0_wp, 'lower requested xmax')
    call expect(fig%subplots_array(2, 1)%y_min, -1.0_wp, 'lower requested ymin')
    call expect(fig%subplots_array(2, 1)%y_max, 1.0_wp, 'lower requested ymax')
    if (fig%state%show_legend) error stop 'pyplot axes legend became figure legend'
    call fig%setup_png_backend_for_animation()
    call fig%extract_rgb_data_for_animation(rgb)
    ! Red series/legend must occur only in the lower panel; blue only in upper.
    pixels = count(rgb(:, 1:height/2, 1) - rgb(:, 1:height/2, 3) > 0.5_wp)
    if (pixels > 0) error stop 'lower legend leaked into upper panel'
    pixels = count(rgb(:, height/2 + 1:, 3) - rgb(:, height/2 + 1:, 1) > 0.5_wp)
    if (pixels > 0) error stop 'upper legend leaked into lower panel'
    pixels = count(rgb(:, 1:height/2, 3) - rgb(:, 1:height/2, 1) > 0.5_wp)
    if (pixels < 30) error stop 'upper artist and legend missing'
    pixels = count(rgb(:, height/2 + 1:, 1) - rgb(:, height/2 + 1:, 3) > 0.5_wp)
    if (pixels < 30) error stop 'lower artist and legend missing'
    pixels = count(rgb(400:620, 30:120, 3) - rgb(400:620, 30:120, 1) > 0.5_wp)
    if (pixels < 10) error stop 'upper-right axes legend was not rendered'
    pixels = count(rgb(60:260, 480:590, 1) - rgb(60:260, 480:590, 3) > 0.5_wp)
    if (pixels < 10) error stop 'lower-left axes legend was not rendered'
    legend_count = 0; previous_ink = .false.
    do row = 30, 125
        ink = count(rgb(400:620, row, 3) - rgb(400:620, row, 1) > 0.5_wp) > 5
        if (ink .and. .not. previous_ink) then
            legend_count = legend_count + 1
            if (legend_count > 3) error stop 'unexpected legend row outside entries'
            legend_rows(legend_count) = row
        end if
        previous_ink = ink
    end do
    if (legend_count /= 3) error stop 'multirow axes legend missing entries'
    if (minval(legend_rows(2:) - legend_rows(:2)) < 14) &
        error stop 'subplot legend rows overlap their text glyphs'
    ! The point at 0.0625 in [0,.07] belongs near the upper edge, not the center.
    pixels = count(rgb(270:400, 45:125, 3) - rgb(270:400, 45:125, 1) > 0.5_wp)
    if (pixels < 5) error stop 'render ignored explicit upper-panel y limits'
    select type (backend => fig%state%backend)
    class is (raster_context)
        call expect(backend%x_min, -2.0_wp, 'rendered lower xmin')
        call expect(backend%x_max, 2.0_wp, 'rendered lower xmax')
        call expect(backend%y_min, -1.0_wp, 'rendered lower ymin')
        call expect(backend%y_max, 1.0_wp, 'rendered lower ymax')
        axis_bottom = backend%plot_area%bottom + backend%plot_area%height
    class default
        error stop 'PNG reference backend unavailable'
    end select
    ! Actual raster ink must contain separate tick-label and xlabel row bands.
    ! A blank gap of three rows rules out canvas-clamped text over tick labels.
    first_ink = 0; last_tick = 0; first_label = 0; previous_ink = .false.
    do row = axis_bottom + 8, height - 1
        ink = any(maxval(rgb(180:500, row, :), dim=2) < 0.5_wp)
        if (first_ink == 0 .and. ink) first_ink = row
        if (first_ink > 0 .and. previous_ink .and. .not. ink) last_tick = row - 1
        if (last_tick > 0 .and. ink) then
            first_label = row
            exit
        end if
        previous_ink = ink
    end do
    if (first_ink == 0 .or. first_label == 0) error stop 'bottom label ink missing'
    if (first_label - last_tick < 4) error stop 'bottom label overlaps tick-label band'
    call savefig(output_dir//'axes_properties.png')
    call savefig(output_dir//'axes_properties.pdf')
    print '(a)', 'PASS per-panel limits, legends, rendered pixels and bottom-label gap'
contains
    subroutine expect(actual, expected, label)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: label
        if (.not. ieee_is_finite(actual)) error stop 'nonfinite axes property'
        if (abs(actual - expected) > 1.0e-12_wp) then
            print '(a,2es16.8)', label, actual, expected
            error stop 'requested axes property was not retained'
        end if
    end subroutine
end program
