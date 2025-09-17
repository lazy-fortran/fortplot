program test_scientific_workflow
    !! Realistic scientific data analysis workflow with histograms
    !! Simulates typical use cases in scientific computing
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    real(wp), parameter :: pi = 3.14159265359_wp
    type(figure_t) :: fig
    
    write(*,*) '=== SCIENTIFIC WORKFLOW TESTING ==='
    
    call test_measurement_distribution()
    call test_simulation_results()
    call test_comparative_analysis()
    call test_publication_quality()
    
    write(*,*) '=== SCIENTIFIC WORKFLOW COMPLETED ==='
    
contains

    subroutine test_measurement_distribution()
        !! Test typical experimental measurement distribution
        real(wp) :: measurements(500)
        integer :: i
        
        write(*,*) 'Testing measurement distribution analysis...'
        
        ! Simulate noisy experimental measurements around a mean
        do i = 1, 500
            measurements(i) = 5.0_wp + &
                             sin(real(i, wp) * 0.01_wp) * 0.5_wp + &
                             cos(real(i, wp) * 0.02_wp) * 0.3_wp + &
                             (real(mod(i, 100), wp) - 50.0_wp) * 0.02_wp
        end do
        
        call fig%initialize(800, 600)
        call fig%add_hist(measurements, bins=30, density=.true., &
                     label='Experimental Data', &
                     color=[0.2_wp, 0.4_wp, 0.8_wp])
        call fig%set_title('Experimental Measurement Distribution')
        call fig%set_xlabel('Measured Value')
        call fig%set_ylabel('Probability Density')
        call fig%legend()
        call fig%savefig("test/output/scientific_measurements.png")
        write(*,*) '  ✓ Measurement distribution analysis complete'
        
    end subroutine test_measurement_distribution
    
    subroutine test_simulation_results()
        !! Test simulation result analysis
        real(wp) :: simulation_data(1000)
        integer :: i
        
        write(*,*) 'Testing simulation result analysis...'
        
        ! Simulate Monte Carlo or numerical simulation results
        do i = 1, 1000
            simulation_data(i) = sqrt(real(i, wp)) * 0.1_wp + &
                                sin(real(i, wp) * 0.001_wp) * 10.0_wp + &
                                cos(real(i, wp) * 0.003_wp) * 5.0_wp
        end do
        
        call fig%initialize(800, 600)
        call fig%add_hist(simulation_data, bins=40, &
                     label='Simulation Results', &
                     color=[0.8_wp, 0.2_wp, 0.2_wp])
        call fig%set_title('Monte Carlo Simulation Results')
        call fig%set_xlabel('Computed Value')
        call fig%set_ylabel('Frequency')
        call fig%legend()
        call fig%savefig("test/output/scientific_simulation.png")
        write(*,*) '  ✓ Simulation result analysis complete'
        
    end subroutine test_simulation_results
    
    subroutine test_comparative_analysis()
        !! Test comparative analysis between datasets
        real(wp) :: control_group(300), treatment_group(300)
        integer :: i
        
        write(*,*) 'Testing comparative analysis...'
        
        ! Generate control and treatment group data
        do i = 1, 300
            control_group(i) = 10.0_wp + sin(real(i, wp) * 0.02_wp) * 2.0_wp
            treatment_group(i) = 12.0_wp + sin(real(i, wp) * 0.02_wp) * 1.8_wp + &
                                cos(real(i, wp) * 0.01_wp) * 1.0_wp
        end do
        
        call fig%initialize(1000, 600)
        call fig%add_hist(control_group, bins=25, density=.true., &
                     label='Control Group', &
                     color=[0.3_wp, 0.7_wp, 0.3_wp])
        call fig%add_hist(treatment_group, bins=25, density=.true., &
                     label='Treatment Group', &
                     color=[0.7_wp, 0.3_wp, 0.7_wp])
        call fig%set_title('Control vs Treatment Group Comparison')
        call fig%set_xlabel('Response Variable')
        call fig%set_ylabel('Probability Density')
        call fig%legend()
        call fig%savefig("test/output/scientific_comparison.png")
        write(*,*) '  ✓ Comparative analysis complete'
        
    end subroutine test_comparative_analysis
    
    subroutine test_publication_quality()
        !! Test publication-quality histogram with proper formatting
        real(wp) :: publication_data(800)
        integer :: i
        
        write(*,*) 'Testing publication-quality output...'
        
        ! Generate clean, publication-ready data
        do i = 1, 800
            publication_data(i) = 100.0_wp + &
                                 cos(2.0_wp * pi * real(i, wp) / 800.0_wp) * 20.0_wp + &
                                 sin(4.0_wp * pi * real(i, wp) / 800.0_wp) * 10.0_wp
        end do
        
        call fig%initialize(1200, 800)
        call fig%add_hist(publication_data, bins=35, density=.true., &
                     label='Dataset A', &
                     color=[0.0_wp, 0.447_wp, 0.698_wp])
        call fig%set_title('Publication Quality Histogram')
        call fig%set_xlabel('Parameter X (units)')
        call fig%set_ylabel('Probability Density (1/units)')
        call fig%legend()
        call fig%savefig("test/output/scientific_publication.png")
        call fig%savefig("test/output/scientific_publication.pdf")
        write(*,*) '  ✓ Publication-quality output complete'
        
    end subroutine test_publication_quality

end program test_scientific_workflow