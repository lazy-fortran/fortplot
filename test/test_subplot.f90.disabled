program test_subplot
    !! Test suite for subplot functionality
    !! Following Test Driven Development - tests written first
    
    use iso_fortran_env, only: real64, wp => real64
    use fortplot, only: figure_t
    implicit none
    
    print *, 'Running subplot tests...'
    
    ! Test basic subplot creation
    call test_subplot_creation()
    
    ! Test subplot grid layouts
    call test_subplot_layouts()
    
    ! Test plotting on subplots
    call test_subplot_plotting()
    
    ! Test subplot independence
    call test_subplot_independence()
    
    print *, 'All subplot tests PASSED!'
    
contains

    subroutine test_subplot_creation()
        !! Test basic subplot creation and grid setup
        type(figure_t) :: fig
        
        print *, 'Testing: Subplot creation'
        
        call fig%initialize(800, 600)
        call fig%subplots(2, 2)  ! Create 2x2 grid
        
        if (fig%subplot_rows == 2 .and. fig%subplot_cols == 2) then
            print *, '  PASS: Subplot grid created'
        else
            print *, '  FAIL: Subplot grid creation failed'
            stop 1
        end if
        
        if (allocated(fig%subplots_array)) then
            print *, '  PASS: Subplot array allocated'
        else
            print *, '  FAIL: Subplot array not allocated'
            stop 1
        end if
        
        print *, 'Test completed'
    end subroutine test_subplot_creation
    
    subroutine test_subplot_layouts()
        !! Test different subplot grid configurations
        type(figure_t) :: fig
        
        print *, 'Testing: Subplot layouts'
        
        ! Test 1x2 layout
        call fig%initialize(800, 600)
        call fig%subplots(1, 2)
        
        if (fig%subplot_rows == 1 .and. fig%subplot_cols == 2) then
            print *, '  PASS: 1x2 layout'
        else
            print *, '  FAIL: 1x2 layout failed'
            stop 1
        end if
        
        ! Test 3x2 layout
        call fig%initialize(800, 600)
        call fig%subplots(3, 2)
        
        if (fig%subplot_rows == 3 .and. fig%subplot_cols == 2) then
            print *, '  PASS: 3x2 layout'
        else
            print *, '  FAIL: 3x2 layout failed'
            stop 1
        end if
        
        print *, 'Test completed'
    end subroutine test_subplot_layouts
    
    subroutine test_subplot_plotting()
        !! Test plotting on individual subplots
        type(figure_t) :: fig
        real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y_data(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
        
        print *, 'Testing: Subplot plotting'
        
        call fig%initialize(800, 600)
        call fig%subplots(2, 2)
        
        ! Plot on specific subplots
        call fig%subplot_plot(1, 1, x_data, y_data, label='Plot 1')
        call fig%subplot_plot(1, 2, x_data, y_data*2.0_wp, label='Plot 2')
        
        if (fig%subplot_plot_count(1, 1) == 1) then
            print *, '  PASS: Plot added to subplot (1,1)'
        else
            print *, '  FAIL: Plot not added to subplot (1,1)'
            stop 1
        end if
        
        if (fig%subplot_plot_count(1, 2) == 1) then
            print *, '  PASS: Plot added to subplot (1,2)'
        else
            print *, '  FAIL: Plot not added to subplot (1,2)'
            stop 1
        end if
        
        print *, 'Test completed'
    end subroutine test_subplot_plotting
    
    subroutine test_subplot_independence()
        !! Test that subplots maintain independent properties
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y1_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y2_data(3) = [10.0_wp, 20.0_wp, 30.0_wp]
        
        print *, 'Testing: Subplot independence'
        
        call fig%initialize(800, 600)
        call fig%subplots(1, 2)
        
        ! Set different titles and labels
        call fig%subplot_set_title(1, 1, 'First Plot')
        call fig%subplot_set_title(1, 2, 'Second Plot')
        call fig%subplot_set_xlabel(1, 1, 'X1')
        call fig%subplot_set_xlabel(1, 2, 'X2')
        
        ! Plot different data ranges
        call fig%subplot_plot(1, 1, x_data, y1_data)
        call fig%subplot_plot(1, 2, x_data, y2_data)
        
        ! Check that titles are different
        if (fig%subplot_title(1, 1) /= fig%subplot_title(1, 2)) then
            print *, '  PASS: Independent subplot titles'
        else
            print *, '  FAIL: Subplot titles not independent'
            stop 1
        end if
        
        print *, 'Test completed'
    end subroutine test_subplot_independence

end program test_subplot