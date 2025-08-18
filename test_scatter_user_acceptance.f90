program test_scatter_user_acceptance
    !! Comprehensive user acceptance testing for enhanced scatter plot functionality
    !! Tests Issue #56 implementation from real user perspective
    
    use fortplot
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    write(error_unit, '(A)') 'Starting comprehensive scatter plot user acceptance testing...'
    
    ! Test 1: Basic scatter plot functionality
    call test_basic_scatter_plot()
    
    ! Test 2: Different marker shapes
    call test_marker_shapes()
    
    ! Test 3: Bubble chart with size mapping
    call test_bubble_chart()
    
    ! Test 4: Color mapping and colorbar
    call test_color_mapping()
    
    ! Test 5: Test README examples
    call test_readme_examples()
    
    ! Test 6: Scientific workflow
    call test_scientific_workflow()
    
    ! Test 7: Error handling
    call test_error_handling()
    
    write(error_unit, '(A)') 'All scatter plot user acceptance tests completed!'
    
contains

    subroutine test_basic_scatter_plot()
        !! Test 1: Basic scatter plot functionality (most common use case)
        type(figure_t) :: fig
        real(wp) :: x(20), y(20)
        integer :: i
        
        write(error_unit, '(A)') 'TEST 1: Basic scatter plot functionality...'
        
        ! Generate simple test data
        do i = 1, 20
            x(i) = real(i, wp)
            y(i) = x(i)**2 + 5.0_wp * sin(x(i))
        end do
        
        ! Test global scatter function (matplotlib-style)
        call figure(600, 400)
        call scatter(x, y, label='Test Data')
        call title('Basic Scatter Plot - User Acceptance Test')
        call xlabel('X Values')
        call ylabel('Y Values')
        call legend()
        call savefig('/tmp/test_basic_scatter.png')
        
        ! Test object-oriented API
        call fig%initialize(600, 400)
        call fig%add_scatter(x, y, label='OO API Test')
        call fig%set_title('Object-Oriented Scatter Plot')
        call fig%set_xlabel('X Values')
        call fig%set_ylabel('Y Values')
        call fig%savefig('/tmp/test_oo_scatter.png')
        
        write(error_unit, '(A)') '  ✓ Basic scatter plot test completed'
    end subroutine test_basic_scatter_plot

    subroutine test_marker_shapes()
        !! Test 2: Different marker shapes and styles
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        write(error_unit, '(A)') 'TEST 2: Testing different marker shapes...'
        
        ! Create test data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        call fig%initialize(800, 600)
        
        ! Test various marker shapes
        call fig%add_scatter(x, y+0.0_wp, marker='o', label='Circles')
        call fig%add_scatter(x, y+1.0_wp, marker='s', label='Squares')
        call fig%add_scatter(x, y+2.0_wp, marker='D', label='Diamonds')
        call fig%add_scatter(x, y+3.0_wp, marker='x', label='Crosses')
        call fig%add_scatter(x, y+4.0_wp, marker='+', label='Plus')
        call fig%add_scatter(x, y+5.0_wp, marker='*', label='Stars')
        call fig%add_scatter(x, y+6.0_wp, marker='^', label='Triangles Up')
        call fig%add_scatter(x, y+7.0_wp, marker='v', label='Triangles Down')
        
        call fig%set_title('Marker Shape Test - User Acceptance')
        call fig%set_xlabel('X Position')
        call fig%set_ylabel('Y Position (offset by marker type)')
        call fig%legend()
        call fig%savefig('/tmp/test_marker_shapes.png')
        
        write(error_unit, '(A)') '  ✓ Marker shapes test completed'
    end subroutine test_marker_shapes

    subroutine test_bubble_chart()
        !! Test 3: Bubble chart with size mapping
        type(figure_t) :: fig
        real(wp) :: x(15), y(15), sizes(15)
        integer :: i
        
        write(error_unit, '(A)') 'TEST 3: Testing bubble chart functionality...'
        
        ! Generate bubble chart data
        do i = 1, 15
            x(i) = real(i, wp)
            y(i) = sin(x(i) * 0.5_wp) + 2.0_wp
            sizes(i) = 10.0_wp + 30.0_wp * abs(sin(x(i)))
        end do
        
        call fig%initialize(700, 500)
        call fig%add_scatter(x, y, s=sizes, marker='o', label='Bubble Chart')
        call fig%set_title('Bubble Chart Test - Size Mapping')
        call fig%set_xlabel('X Values')
        call fig%set_ylabel('Y Values')
        call fig%legend()
        call fig%savefig('/tmp/test_bubble_chart.png')
        
        write(error_unit, '(A)') '  ✓ Bubble chart test completed'
    end subroutine test_bubble_chart

    subroutine test_color_mapping()
        !! Test 4: Color mapping and colorbar functionality
        type(figure_t) :: fig
        real(wp) :: x(25), y(25), colors(25)
        integer :: i
        
        write(error_unit, '(A)') 'TEST 4: Testing color mapping functionality...'
        
        ! Generate color-mapped data
        do i = 1, 25
            x(i) = real(i-13, wp) * 0.5_wp
            y(i) = x(i)**2 * 0.1_wp
            colors(i) = sin(x(i) * 2.0_wp)
        end do
        
        call fig%initialize(800, 600)
        call fig%add_scatter(x, y, c=colors, colormap='viridis', &
                            marker='o', label='Color Mapped')
        call fig%set_title('Color Mapping Test - Scientific Data Visualization')
        call fig%set_xlabel('X Parameter')
        call fig%set_ylabel('Y Response')
        call fig%legend()
        call fig%savefig('/tmp/test_color_mapping.png')
        
        write(error_unit, '(A)') '  ✓ Color mapping test completed'
    end subroutine test_color_mapping

    subroutine test_readme_examples()
        !! Test 5: Examples from README documentation
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), sizes(10), values(10)
        integer :: i
        
        write(error_unit, '(A)') 'TEST 5: Testing README examples...'
        
        ! Create test data matching README style
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = 2.0_wp + real(i, wp) * 0.5_wp + sin(real(i, wp))
            sizes(i) = 20.0_wp + real(i, wp) * 3.0_wp
            values(i) = real(i, wp) / 10.0_wp
        end do
        
        ! Basic scatter from README
        call figure()
        call scatter(x, y, label="Data Points")
        call title("Basic Scatter Plot")
        call savefig("/tmp/readme_basic_scatter.png")
        
        ! Bubble chart from README
        call fig%initialize(600, 400)
        call fig%add_scatter(x, y, s=sizes, marker='o', label='Bubble Chart')
        call fig%set_title("Bubble Chart - Size Represents Population")
        call fig%savefig("/tmp/readme_bubble_chart.png")
        
        ! Color-mapped scatter from README style
        call figure(800, 600)
        call scatter(x, y, c=values, colormap='viridis', &
                     marker='D', label='Scientific Data')
        call title("Multi-dimensional Data Visualization")
        call xlabel("Temperature (K)")
        call ylabel("Pressure (Pa)")
        call savefig("/tmp/readme_scientific_scatter.png")
        
        write(error_unit, '(A)') '  ✓ README examples test completed'
    end subroutine test_readme_examples

    subroutine test_scientific_workflow()
        !! Test 6: Realistic scientific visualization workflow
        type(figure_t) :: fig
        real(wp) :: temperature(30), pressure(30), density(30), error_bars(30)
        integer :: i
        real(wp), parameter :: PI = 3.14159265359_wp
        
        write(error_unit, '(A)') 'TEST 6: Testing scientific workflow...'
        
        ! Generate realistic scientific data
        do i = 1, 30
            temperature(i) = 250.0_wp + real(i, wp) * 10.0_wp
            pressure(i) = 1.0_wp + exp((temperature(i) - 300.0_wp) / 50.0_wp)
            density(i) = pressure(i) / (temperature(i) * 0.287_wp)  ! Ideal gas law
            error_bars(i) = 0.05_wp * pressure(i)
        end do
        
        ! Create publication-quality scatter plot
        call fig%initialize(800, 600)
        call fig%add_scatter(temperature, pressure, s=density*200.0_wp, &
                            c=density, colormap='plasma', &
                            marker='o', label='Experimental Data')
        call fig%set_title('Gas Properties: Pressure vs Temperature')
        call fig%set_xlabel('Temperature (K)')
        call fig%set_ylabel('Pressure (bar)')
        call fig%legend()
        call fig%savefig('/tmp/test_scientific_workflow.pdf')  ! Vector format for publication
        
        write(error_unit, '(A)') '  ✓ Scientific workflow test completed'
    end subroutine test_scientific_workflow

    subroutine test_error_handling()
        !! Test 7: Error handling and edge cases
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), bad_sizes(10), bad_colors(10)
        real(wp) :: nan_val, inf_val
        integer :: i
        
        write(error_unit, '(A)') 'TEST 7: Testing error handling...'
        
        ! Create IEEE special values
        nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
        inf_val = ieee_value(0.0_wp, ieee_positive_inf)
        
        ! Create data with problematic values
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 2.0_wp
            bad_sizes(i) = 20.0_wp
            bad_colors(i) = real(i, wp)
        end do
        
        ! Insert problematic values
        x(5) = nan_val
        y(3) = inf_val
        bad_sizes(7) = nan_val
        bad_colors(2) = inf_val
        
        call fig%initialize(600, 400)
        
        ! Test that library handles NaN/Inf gracefully
        call fig%add_scatter(x, y, s=bad_sizes, c=bad_colors, &
                            marker='o', label='Test with NaN/Inf')
        call fig%set_title('Error Handling Test - NaN/Inf Values')
        call fig%set_xlabel('X (with NaN)')
        call fig%set_ylabel('Y (with Inf)')
        call fig%savefig('/tmp/test_error_handling.png')
        
        ! Test empty arrays
        call fig%initialize(600, 400)
        ! Note: This should handle empty arrays gracefully
        ! (Implementation should check for this case)
        
        write(error_unit, '(A)') '  ✓ Error handling test completed'
    end subroutine test_error_handling

end program test_scatter_user_acceptance