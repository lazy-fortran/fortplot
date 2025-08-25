program test_debug_color_logging
    !! Test debug logging for RGB color value corrections
    !! 
    !! This test verifies that debug logging works correctly when RGB
    !! values are corrected due to invalid or out-of-range inputs.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fortplot_pdf_drawing, only: pdf_stream_writer
    use fortplot_logging, only: set_log_level, LOG_LEVEL_DEBUG
    implicit none
    
    type(pdf_stream_writer) :: writer
    real(wp) :: nan_val, inf_val
    
    ! Enable debug logging
    call set_log_level(LOG_LEVEL_DEBUG)
    
    ! Test with NaN values
    nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
    print *, "Testing with NaN values:"
    call writer%write_color(nan_val, 0.5_wp, 0.7_wp)
    
    ! Test with infinity
    inf_val = huge(1.0_wp)
    print *, "Testing with infinity:"
    call writer%write_color(inf_val, 0.5_wp, 0.7_wp)
    
    ! Test with out-of-range values
    print *, "Testing with out-of-range values:"
    call writer%write_color(-0.5_wp, 1.5_wp, 2.0_wp)
    
    ! Test with valid values (should not log corrections)
    print *, "Testing with valid values:"
    call writer%write_color(0.2_wp, 0.5_wp, 0.8_wp)
    
    print *, "Debug color logging test completed."
    
end program test_debug_color_logging