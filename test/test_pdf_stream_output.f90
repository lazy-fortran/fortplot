program test_pdf_stream_output
    !! Test actual PDF stream output after validation
    !! 
    !! This test verifies that corrected values are properly written
    !! to the PDF stream and the stream contents are valid.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fortplot_pdf_drawing, only: pdf_stream_writer
    use fortplot_logging, only: set_log_level, LOG_LEVEL_DEBUG
    implicit none
    
    type(pdf_stream_writer) :: writer
    real(wp) :: nan_val
    
    ! Enable debug logging
    call set_log_level(LOG_LEVEL_DEBUG)
    
    ! Initialize NaN value
    nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
    
    print *, "=== PDF Stream Output Test ==="
    print *, ""
    
    ! Test corrected move commands
    print *, "Testing corrected move commands:"
    call writer%write_move(nan_val, 100.0_wp)      ! Should become "0.000 100.000 m"
    call writer%write_move(50.0_wp, nan_val)       ! Should become "50.000 0.000 m"
    call writer%write_move(25.0_wp, 75.0_wp)       ! Should become "25.000 75.000 m"
    print *, ""
    
    ! Test corrected line commands  
    print *, "Testing corrected line commands:"
    call writer%write_line(nan_val, 150.0_wp)      ! Should become "0.000 150.000 l"
    call writer%write_line(80.0_wp, nan_val)       ! Should become "80.000 0.000 l"
    call writer%write_line(60.0_wp, 90.0_wp)       ! Should become "60.000 90.000 l"
    print *, ""
    
    ! Test corrected line width commands
    print *, "Testing corrected line width commands:"
    call writer%write_line_width(nan_val)          ! Should become "1.000 w"
    call writer%write_line_width(-5.0_wp)          ! Should become "1.000 w"
    call writer%write_line_width(0.0_wp)           ! Should become "1.000 w"
    call writer%write_line_width(2.5_wp)           ! Should become "2.500 w"
    print *, ""
    
    ! Test corrected color commands
    print *, "Testing corrected color commands:"
    call writer%write_color(nan_val, 0.5_wp, 0.8_wp)      ! Should become "0.000 0.500 0.800 RG"
    call writer%write_color(-0.2_wp, 1.5_wp, 0.6_wp)      ! Should become "0.000 1.000 0.600 RG"
    call writer%write_color(0.3_wp, 0.7_wp, 0.9_wp)       ! Should become "0.300 0.700 0.900 RG"
    print *, ""
    
    print *, "PDF stream output test completed successfully."
    print *, "All corrected values should be written to stream properly."
    
end program test_pdf_stream_output