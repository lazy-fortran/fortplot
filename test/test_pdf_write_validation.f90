program test_pdf_write_validation
    !! Test validation for PDF write functions
    !! 
    !! This test verifies that all PDF write functions handle invalid inputs
    !! gracefully, including NaN, infinity, and out-of-range values.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
                                             ieee_positive_inf, ieee_negative_inf
    use fortplot_pdf_drawing, only: pdf_stream_writer
    use fortplot_logging, only: set_log_level, LOG_LEVEL_DEBUG
    implicit none
    
    type(pdf_stream_writer) :: writer
    real(wp) :: nan_val, pos_inf_val, neg_inf_val
    
    ! Enable debug logging to verify corrections
    call set_log_level(LOG_LEVEL_DEBUG)
    
    ! Initialize special values
    nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
    pos_inf_val = ieee_value(0.0_wp, ieee_positive_inf)
    neg_inf_val = ieee_value(0.0_wp, ieee_negative_inf)
    
    print *, "=== PDF Write Validation Test ==="
    print *, ""
    
    ! Test pdf_write_move with invalid coordinates
    print *, "Testing pdf_write_move with invalid coordinates:"
    call writer%write_move(nan_val, 10.0_wp)
    call writer%write_move(10.0_wp, nan_val)
    call writer%write_move(nan_val, nan_val)
    call writer%write_move(pos_inf_val, 10.0_wp)
    call writer%write_move(10.0_wp, neg_inf_val)
    print *, ""
    
    ! Test pdf_write_move with valid coordinates (no corrections)
    print *, "Testing pdf_write_move with valid coordinates:"
    call writer%write_move(100.0_wp, 200.0_wp)
    call writer%write_move(-50.0_wp, 75.0_wp)
    call writer%write_move(0.0_wp, 0.0_wp)
    print *, ""
    
    ! Test pdf_write_line with invalid coordinates
    print *, "Testing pdf_write_line with invalid coordinates:"
    call writer%write_line(nan_val, 20.0_wp)
    call writer%write_line(20.0_wp, nan_val)
    call writer%write_line(nan_val, nan_val)
    call writer%write_line(pos_inf_val, 20.0_wp)
    call writer%write_line(20.0_wp, neg_inf_val)
    print *, ""
    
    ! Test pdf_write_line with valid coordinates (no corrections)
    print *, "Testing pdf_write_line with valid coordinates:"
    call writer%write_line(150.0_wp, 250.0_wp)
    call writer%write_line(-25.0_wp, 125.0_wp)
    call writer%write_line(0.0_wp, 0.0_wp)
    print *, ""
    
    ! Test pdf_write_line_width with invalid widths
    print *, "Testing pdf_write_line_width with invalid widths:"
    call writer%write_line_width(nan_val)
    call writer%write_line_width(pos_inf_val)
    call writer%write_line_width(neg_inf_val)
    call writer%write_line_width(0.0_wp)      ! Zero width (invalid)
    call writer%write_line_width(-1.0_wp)     ! Negative width (invalid)
    call writer%write_line_width(-999.0_wp)   ! Large negative width
    print *, ""
    
    ! Test pdf_write_line_width with valid widths (no corrections)
    print *, "Testing pdf_write_line_width with valid widths:"
    call writer%write_line_width(1.0_wp)
    call writer%write_line_width(0.5_wp)
    call writer%write_line_width(2.5_wp)
    call writer%write_line_width(10.0_wp)
    print *, ""
    
    ! Test pdf_write_color with edge cases (already validated)
    print *, "Testing pdf_write_color with edge cases:"
    call writer%write_color(nan_val, 0.5_wp, 0.7_wp)
    call writer%write_color(0.5_wp, pos_inf_val, 0.7_wp)
    call writer%write_color(0.5_wp, 0.7_wp, neg_inf_val)
    call writer%write_color(-0.5_wp, 1.5_wp, 2.0_wp)    ! Out of range
    call writer%write_color(0.2_wp, 0.5_wp, 0.8_wp)     ! Valid (no corrections)
    print *, ""
    
    print *, "PDF write validation test completed successfully."
    print *, "All functions handled invalid inputs gracefully."
    
end program test_pdf_write_validation