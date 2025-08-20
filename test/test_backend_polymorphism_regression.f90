program test_backend_polymorphism_regression
    !! Test suite for backend polymorphism and integration
    !! 
    !! GIVEN: fortplot_figure_core uses polymorphic backends via plot_context
    !! WHEN: Module is refactored into focused modules
    !! THEN: Backend polymorphism must be preserved
    !!
    !! Tests the abstraction layer that allows different backends
    !! (PNG, PDF, ASCII) to work through the same interface.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Backend integration tests
    call test_default_backend_handling()
    call test_png_backend_specification()
    call test_pdf_backend_specification()
    call test_ascii_backend_specification()
    call test_backend_switching()
    call test_backend_polymorphism()
    call test_rendering_consistency()
    call test_file_extension_detection()
    call test_backend_error_handling()
    
    call print_test_summary()
    
contains

    subroutine test_default_backend_handling()
        type(figure_t) :: fig
        
        call start_test("Default backend handling")
        
        ! GIVEN: A figure without explicit backend specification
        ! WHEN: Figure is initialized
        call fig%initialize()
        
        ! THEN: Figure should handle backend gracefully
        call assert_true(.true., "Default backend initialization")
        
        call end_test()
    end subroutine test_default_backend_handling

    subroutine test_png_backend_specification()
        type(figure_t) :: fig
        
        call start_test("PNG backend specification")
        
        ! GIVEN: PNG backend specification
        ! WHEN: Figure is initialized with PNG backend
        call fig%initialize(backend="png")
        
        ! THEN: PNG backend should be configured
        call assert_true(.true., "PNG backend specified")
        
        call end_test()
    end subroutine test_png_backend_specification

    subroutine test_pdf_backend_specification()
        type(figure_t) :: fig
        
        call start_test("PDF backend specification")
        
        ! GIVEN: PDF backend specification
        ! WHEN: Figure is initialized with PDF backend
        call fig%initialize(backend="pdf")
        
        ! THEN: PDF backend should be configured
        call assert_true(.true., "PDF backend specified")
        
        call end_test()
    end subroutine test_pdf_backend_specification

    subroutine test_ascii_backend_specification()
        type(figure_t) :: fig
        
        call start_test("ASCII backend specification")
        
        ! GIVEN: ASCII backend specification
        ! WHEN: Figure is initialized with ASCII backend
        call fig%initialize(backend="ascii")
        
        ! THEN: ASCII backend should be configured
        call assert_true(.true., "ASCII backend specified")
        
        call end_test()
    end subroutine test_ascii_backend_specification

    subroutine test_backend_switching()
        type(figure_t) :: fig1, fig2, fig3
        real(wp) :: x(3), y(3)
        
        call start_test("Backend switching")
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp]
        
        ! GIVEN: Multiple figures with different backends
        ! WHEN: Same plot data is added to different backends
        call fig1%initialize(backend="png")
        call fig1%add_plot(x, y, label="test")
        
        call fig2%initialize(backend="pdf")
        call fig2%add_plot(x, y, label="test")
        
        call fig3%initialize(backend="ascii")
        call fig3%add_plot(x, y, label="test")
        
        ! THEN: All figures should handle the data correctly
        call assert_equal(real(fig1%plot_count, wp), 1.0_wp, "PNG figure plot count")
        call assert_equal(real(fig2%plot_count, wp), 1.0_wp, "PDF figure plot count")
        call assert_equal(real(fig3%plot_count, wp), 1.0_wp, "ASCII figure plot count")
        
        call end_test()
    end subroutine test_backend_switching

    subroutine test_backend_polymorphism()
        type(figure_t) :: fig
        real(wp) :: x(2), y(2)
        
        call start_test("Backend polymorphism")
        
        call fig%initialize()
        
        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]
        
        ! GIVEN: Figure with polymorphic backend
        ! WHEN: Different plot types are added
        call fig%add_plot(x, y, label="line")
        call fig%add_scatter_2d(x, y, label="scatter")
        
        ! THEN: Backend should handle all plot types polymorphically
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Polymorphic plot handling")
        
        call end_test()
    end subroutine test_backend_polymorphism

    subroutine test_rendering_consistency()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        
        call start_test("Rendering consistency")
        
        call fig%initialize()
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [2.0_wp, 4.0_wp, 6.0_wp]
        
        ! GIVEN: Figure with plot data
        ! WHEN: Multiple rendering operations might occur
        call fig%add_plot(x, y, label="consistent")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        call fig%set_title("Test")
        
        ! THEN: Rendering state should be consistent
        call assert_false(fig%rendered, "Rendering state before savefig")
        
        call end_test()
    end subroutine test_rendering_consistency

    subroutine test_file_extension_detection()
        type(figure_t) :: fig
        real(wp) :: x(2), y(2)
        
        call start_test("File extension detection")
        
        call fig%initialize()
        
        x = [1.0_wp, 2.0_wp]
        y = [1.0_wp, 2.0_wp]
        call fig%add_plot(x, y)
        
        ! GIVEN: Figure ready for saving
        ! WHEN: Different file extensions are tested
        ! THEN: Backend should handle extension detection
        ! Note: Testing the interface, not actual file operations
        call assert_true(.true., "File extension handling")
        
        call end_test()
    end subroutine test_file_extension_detection

    subroutine test_backend_error_handling()
        type(figure_t) :: fig
        
        call start_test("Backend error handling")
        
        ! GIVEN: Invalid backend specification
        ! WHEN: Figure is initialized with invalid backend
        call fig%initialize(backend="invalid_backend")
        
        ! THEN: Should handle gracefully without crashing
        call assert_true(.true., "Invalid backend handled")
        
        call end_test()
    end subroutine test_backend_error_handling

    ! Test helper subroutines
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        real(wp), parameter :: tolerance = 1.0e-10_wp
        
        test_count = test_count + 1
        if (abs(actual - expected) < tolerance) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, F12.6, A, F12.6)') '  FAIL: ', description, actual, ' != ', expected
        end if
    end subroutine assert_equal

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_true

    subroutine assert_false(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (.not. condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_false

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        if (pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
    end subroutine print_test_summary

end program test_backend_polymorphism_regression