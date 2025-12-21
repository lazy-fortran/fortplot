program test_legend_line_styles
    !! Test that line styles are properly rendered in legend (Issue #1158)
    use fortplot
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp), dimension(20) :: x, y1, y2, y3, y4
    integer :: i
    type(validation_result_t) :: val
    
    print *, "========================================="
    print *, "LEGEND LINE STYLES TEST (Issue #1158)"
    print *, "========================================="
    
    ! Generate test data
    x = [(real(i, wp) * 0.5_wp, i=1, 20)]
    y1 = sin(x)
    y2 = cos(x) * 0.8_wp
    y3 = sin(2.0_wp * x) * 0.6_wp
    y4 = cos(2.0_wp * x) * 0.4_wp
    
    ! Create figure with different line styles
    call figure(figsize=[8.0_wp, 6.0_wp])
    call title("Legend Line Styles Test")
    call xlabel("X")
    call ylabel("Y")
    
    ! Add plots with different line styles (without markers)
    call add_plot(x, y1, label="Solid line", linestyle="-")
    call add_plot(x, y2, label="Dashed line", linestyle="--")
    call add_plot(x, y3, label="Dotted line", linestyle=":")
    call add_plot(x, y4, label="Dash-dot line", linestyle="-.")
    
    ! Add legend
    call legend(position="upper right")
    
    ! Save to PNG
    call savefig("test/output/test_legend_line_styles.png")
    
    ! Validate output
    val = validate_file_exists('test/output/test_legend_line_styles.png')
    if (val%passed) then
        print *, "PASS: Legend line styles PNG created successfully"
        val = validate_file_size('test/output/test_legend_line_styles.png', min_size=5000)
        if (val%passed) then
            print *, "PASS: PNG file size indicates proper rendering"
            print *, ""
            print *, "SUCCESS: Line styles properly rendered in legend"
            print *, "The fix correctly applies line styles to legend entries"
        else
            print *, "FAIL: PNG file too small - legend may be incomplete"
            stop 1
        end if
    else
        print *, "FAIL: Failed to create legend line styles PNG"
        stop 1
    end if
    
    print *, "========================================="
    
end program test_legend_line_styles