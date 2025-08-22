program test_pdf_user_scenarios
    !! Real-world user scenarios to validate PDF Y-axis overlap fix from user perspective
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_scientific_data_near_origin()
    call test_financial_small_variations()
    call test_measurement_precision()
    
    print *, "All PDF user scenario tests completed successfully!"
    
contains

    subroutine test_scientific_data_near_origin()
        !! Common scientific scenario: measuring small values around zero
        type(figure_t) :: fig
        real(wp), parameter :: time_points(11) = [0.0_wp, 0.1_wp, 0.2_wp, 0.3_wp, 0.4_wp, &
                                                  0.5_wp, 0.6_wp, 0.7_wp, 0.8_wp, 0.9_wp, 1.0_wp]
        real(wp), parameter :: voltage_readings(11) = [-0.032_wp, -0.025_wp, -0.018_wp, -0.012_wp, &
                                                       -0.005_wp, 0.000_wp, 0.008_wp, 0.015_wp, &
                                                       0.022_wp, 0.028_wp, 0.035_wp]
        
        print *, "=== Testing scientific measurement scenario ==="
        
        call fig%initialize(800, 600)
        call fig%add_plot(time_points, voltage_readings, label="Voltage Sensor")
        call fig%set_xlim(0.0_wp, 1.0_wp)
        call fig%set_ylim(-0.04_wp, 0.04_wp)  ! Tight range around zero
        call fig%set_xlabel("Time (seconds)")
        call fig%set_ylabel("Voltage (V)")
        call fig%set_title("Voltage Measurement Near Zero - PDF Overlap Test")
        call figure_legend(fig, )
        
        call figure_savefig(fig, get_test_output_path("/tmp/test_scientific_near_origin.pdf"))
        print *, "Created: /tmp/test_scientific_near_origin.pdf"
        print *, "Expected: Clean Y-axis labels without overlap, readable voltage values"
    end subroutine test_scientific_data_near_origin
    
    subroutine test_financial_small_variations()
        !! Financial scenario: small percentage changes around zero
        type(figure_t) :: fig
        real(wp), parameter :: days(8) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
        real(wp), parameter :: returns(8) = [-0.015_wp, 0.008_wp, -0.003_wp, 0.012_wp, &
                                             -0.009_wp, 0.002_wp, 0.006_wp, -0.011_wp]
        
        print *, "=== Testing financial returns scenario ==="
        
        call fig%initialize(700, 500)
        call fig%add_plot(days, returns, label="Daily Returns (%)")
        call fig%set_xlim(0.5_wp, 8.5_wp)
        call fig%set_ylim(-0.02_wp, 0.015_wp)  ! Small percentage range
        call fig%set_xlabel("Trading Day")
        call fig%set_ylabel("Return (%)")
        call fig%set_title("Daily Stock Returns - PDF Y-axis Test")
        call figure_legend(fig, )
        
        call figure_savefig(fig, get_test_output_path("/tmp/test_financial_returns.pdf"))
        print *, "Created: /tmp/test_financial_returns.pdf"
        print *, "Expected: Professional-looking chart with non-overlapping Y-labels"
    end subroutine test_financial_small_variations
    
    subroutine test_measurement_precision()
        !! Precision measurement scenario: very small deviations from reference
        type(figure_t) :: fig
        real(wp), parameter :: sample_numbers(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp), parameter :: deviations(6) = [-0.0008_wp, 0.0003_wp, -0.0012_wp, 0.0007_wp, -0.0004_wp, 0.0009_wp]
        
        print *, "=== Testing precision measurement scenario ==="
        
        call fig%initialize(600, 450)
        call fig%add_plot(sample_numbers, deviations, label="Measurement Deviation")
        call fig%set_xlim(0.5_wp, 6.5_wp)
        call fig%set_ylim(-0.0015_wp, 0.0012_wp)  ! Very tight precision range
        call fig%set_xlabel("Sample Number")
        call fig%set_ylabel("Deviation from Reference (units)")
        call fig%set_title("Precision Measurement Analysis - PDF Format")
        call figure_legend(fig, )
        
        call figure_savefig(fig, get_test_output_path("/tmp/test_precision_measurement.pdf"))
        print *, "Created: /tmp/test_precision_measurement.pdf"
        print *, "Expected: High-precision labels without crowding, scientific notation if needed"
    end subroutine test_measurement_precision

end program test_pdf_user_scenarios