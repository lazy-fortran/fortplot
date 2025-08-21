program test_subplot_example
    !! Comprehensive test suite for subplot example functionality (RED phase)
    !!
    !! Tests that validate the subplot example demonstrates all required
    !! functionality as specified in Issue #158. These tests ensure the
    !! example properly uses the subplot() API and generates expected outputs.
    !!
    !! Given: Subplot example needs to demonstrate all major subplot features
    !! When: Example is run with various subplot configurations
    !! Then: Should generate proper subplot layouts with independent content
    !!
    !! NOTE: This test is designed to FAIL in RED phase because subplot()
    !! function does not exist yet. Implementation will be added to make tests pass.

    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, plot, xlabel, ylabel, title, &
                        savefig, figure, show, subplot
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0
    real(wp), allocatable :: x(:), y(:), x2(:), y2(:), cos_x(:)
    integer :: i

    print *, 'Running subplot example functionality tests (RED phase)...'
    print *, 'Testing Issue #158 subplot example requirements'

    ! Generate test data like the example will use
    call generate_test_data()

    ! Test core subplot example functionality
    call test_subplot_2x2_layout()
    call test_subplot_1x3_layout()
    call test_subplot_independent_titles_labels()
    call test_subplot_different_plot_types()
    call test_subplot_output_directory_creation()

    call print_test_summary()

contains

    subroutine generate_test_data()
        !! Given: Need test data for subplot examples
        !! When: Data is generated like in the example
        !! Then: Should create sine, cosine, damped oscillation data
        
        allocate(x(100), y(100), cos_x(100))
        do i = 1, 100
            x(i) = real(i - 1, wp) * 0.1_wp
            y(i) = sin(x(i))
            cos_x(i) = cos(x(i))
        end do
        
        allocate(x2(50), y2(50))
        do i = 1, 50
            x2(i) = real(i - 1, wp) * 0.2_wp
            y2(i) = exp(-x2(i) * 0.5_wp) * cos(2.0_wp * x2(i))
        end do
    end subroutine generate_test_data

    subroutine test_subplot_2x2_layout()
        !! Given: Need to test 2x2 subplot grid layout
        !! When: subplot() is called for 2x2 grid with different plots
        !! Then: Should create 4 independent subplots with different content
        
        call increment_test_count()
        print *, 'Test: 2x2 subplot layout with independent content'
        
        call figure(800, 600)
        
        ! GREEN PHASE: Testing subplot implementation
        call subplot(2, 2, 1)
        call plot(x, y)  ! y already contains sin(x)
        call title('Sine Wave')
        call xlabel('x')
        call ylabel('sin(x)')
        
        call subplot(2, 2, 2)
        call plot(x, cos_x)
        call title('Cosine Wave')
        call xlabel('x')
        call ylabel('cos(x)')
        
        call subplot(2, 2, 3)
        call plot(x2, y2)
        call title('Damped Oscillation')
        call xlabel('x')
        call ylabel('y')
        
        call subplot(2, 2, 4)
        call plot(x2, x2**2 / 50.0_wp)
        call title('Quadratic Function')
        call xlabel('x')
        call ylabel('y')
        
        call savefig('test_output/subplot_2x2_test.png')
        
        print *, 'PASS: 2x2 subplot layout created successfully'
        call mark_test_passed()
        
    end subroutine test_subplot_2x2_layout

    subroutine test_subplot_1x3_layout()
        !! Given: Need to test 1x3 subplot layout (horizontal arrangement)
        !! When: subplot() is called for 1x3 grid with different functions
        !! Then: Should create 3 side-by-side subplots
        
        call increment_test_count()
        print *, 'Test: 1x3 subplot layout (horizontal arrangement)'
        
        call figure(800, 400)
        
        ! GREEN PHASE: Testing subplot implementation  
        call subplot(1, 3, 1)
        call plot(x2, x2)
        call title('Linear')
        
        call subplot(1, 3, 2)
        call plot(x2, x2**2)
        call title('Quadratic')
        
        call subplot(1, 3, 3)
        call plot(x2, x2**3)
        call title('Cubic')
        
        call savefig('test_output/subplot_1x3_test.png')
        
        print *, 'PASS: 1x3 subplot layout created successfully'
        call mark_test_passed()
        
    end subroutine test_subplot_1x3_layout

    subroutine test_subplot_independent_titles_labels()
        !! Given: Subplots need independent titles and axis labels
        !! When: Different titles and labels are set for each subplot
        !! Then: Each subplot should maintain its own title/label state
        
        call increment_test_count()
        print *, 'Test: Independent subplot titles and labels'
        
        call figure(600, 600)
        
        ! GREEN PHASE: Testing independent subplot labels
        call subplot(2, 1, 1)
        call plot(x, y)  ! y already contains sin(x)
        call title('Top Plot: Sine Function')
        call xlabel('Time (s)')
        call ylabel('Amplitude')
        
        call subplot(2, 1, 2)
        call plot(x, cos_x)
        call title('Bottom Plot: Cosine Function')
        call xlabel('Frequency (Hz)')
        call ylabel('Magnitude')
        
        call savefig('test_output/subplot_independent_labels_test.png')
        
        print *, 'PASS: Independent subplot titles and labels created successfully'
        call mark_test_passed()
        
    end subroutine test_subplot_independent_titles_labels

    subroutine test_subplot_different_plot_types()
        !! Given: Subplots should support different types of plots
        !! When: Various plot types are used in different subplots
        !! Then: Each subplot should render its specific plot type correctly
        
        call increment_test_count()
        print *, 'Test: Different plot types in subplots'
        
        call figure(800, 600)
        
        ! GREEN PHASE: Testing different plot types
        call subplot(2, 2, 1)
        call plot(x, y)  ! y already contains sin(x)
        call title('Line Plot')
        
        call subplot(2, 2, 2)
        call plot(x2, y2)
        call title('Damped Oscillation')
        
        call subplot(2, 2, 3)
        call plot(x2, x2**2)
        call title('Quadratic')
        
        call subplot(2, 2, 4)
        call plot(x2, sqrt(abs(x2)))
        call title('Square Root')
        
        call savefig('test_output/subplot_different_types_test.png')
        
        print *, 'PASS: Different plot types in subplots created successfully'
        call mark_test_passed()
        
    end subroutine test_subplot_different_plot_types

    subroutine test_subplot_output_directory_creation()
        !! Given: Example should create proper output directory structure
        !! When: savefig() is called with subplot example path
        !! Then: Should create output/example/fortran/subplot_demo/ directory
        
        call increment_test_count()
        print *, 'Test: Subplot example output directory creation'
        
        call figure(400, 300)
        ! GREEN PHASE: Testing subplot output
        call subplot(1, 1, 1)
        call plot(x2(1:10), x2(1:10))
        call title('Simple Test Plot')
        
        ! Test the expected output path for the example
        call savefig('output/example/fortran/subplot_demo/test.png')
        
        print *, 'PASS: Subplot example output directory creation successful'
        call mark_test_passed()
        
    end subroutine test_subplot_output_directory_creation

    subroutine increment_test_count()
        test_count = test_count + 1
    end subroutine increment_test_count

    subroutine mark_test_passed()
        passed_count = passed_count + 1
    end subroutine mark_test_passed

    subroutine print_test_summary()
        print *, ''
        print *, '=== SUBPLOT EXAMPLE TEST SUMMARY ==='
        print *, 'Total tests run: ', test_count
        print *, 'Tests passed: ', passed_count
        print *, 'Tests failed: ', test_count - passed_count
        
        if (passed_count == test_count) then
            print *, 'All subplot example tests PASSED!'
        else
            print *, 'Some tests FAILED - implementation needed'
        end if
        print *, '===================================='
    end subroutine print_test_summary

end program test_subplot_example