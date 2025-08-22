! test_critical_failure_detection.f90 - Tests for critical failure mode detection
! Critical failure mode: Unit tests pass but actual plot generation is broken
program test_critical_failure_detection
    use fortplot
    use fortplot_validation
    use fortplot_security, only: get_test_output_path
    implicit none
    
    call test_plot_generation_vs_unit_test_mismatch()
    call test_backend_specific_failures()
    call test_silent_format_corruption()
    call test_dependency_failure_detection()
    
    print *, "All critical failure detection tests completed"
    
contains
    
    ! Given: Unit tests that check internal state but miss actual output generation
    ! When: Plot generation silently fails despite passing unit tests
    ! Then: Functional validation should catch the discrepancy
    subroutine test_plot_generation_vs_unit_test_mismatch()
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        real(wp), dimension(10) :: x, y
        integer :: i
        character(len=*), parameter :: test_output = get_test_output_path("output/test/critical_mismatch.png")
        
        ! Arrange: Create test data
        do i = 1, 10
            x(i) = real(i-1, wp) * 0.5_wp
            y(i) = sin(x(i))
        end do
        
        ! Act: Perform operations that might pass unit tests but fail output generation
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="sine wave")
        call fig%set_title("Critical Test")
        
        ! Unit tests might check:
        ! - Figure is initialized (PASS)
        ! - Plot data is stored (PASS) 
        ! - Title is set (PASS)
        ! But miss actual file generation failure
        
        ! Simulate the actual savefig operation
        call fig%savefig(test_output)
        
        ! Assert: Functional validation must verify actual output exists
        validation = validate_file_exists(test_output)
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE DETECTED: Plot generation failed despite unit test success"
            print *, "This is exactly the failure mode functional validation should catch"
            print *, "Message: ", trim(validation%message)
            stop 1
        end if
        
        ! Assert: Verify actual file format is correct
        validation = validate_png_format(test_output)
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE DETECTED: Invalid PNG format generated"
            print *, "Message: ", trim(validation%message)
            stop 1
        end if
        
        print *, "PASS: Plot generation and format validation successful"
    end subroutine
    
    ! Given: Different backends that might fail independently
    ! When: One backend fails while others succeed
    ! Then: Backend-specific validation should catch partial failures
    subroutine test_backend_specific_failures()
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        real(wp), dimension(5) :: x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), dimension(5) :: y = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        
        ! Test PNG backend
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="backend test")
        call fig%savefig(get_test_output_path("output/test/backend_test.png"))
        
        validation = validate_png_format(get_test_output_path("output/test/backend_test.png"))
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE: PNG backend failed - ", trim(validation%message)
            stop 1
        end if
        
        ! Test PDF backend
        call fig%initialize(400, 300)
        call fig%add_plot(x, y, label="backend test")
        call fig%savefig(get_test_output_path("output/test/backend_test.pdf"))
        
        validation = validate_pdf_format(get_test_output_path("output/test/backend_test.pdf"))
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE: PDF backend failed - ", trim(validation%message)
            stop 1
        end if
        
        ! Test ASCII backend
        call fig%initialize(60, 20)
        call fig%add_plot(x, y, label="backend test")
        call fig%savefig(get_test_output_path("output/test/backend_test.txt"))
        
        validation = validate_ascii_format(get_test_output_path("output/test/backend_test.txt"))
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE: ASCII backend failed - ", trim(validation%message)
            stop 1
        end if
        
        print *, "PASS: All backends generate valid output"
    end subroutine
    
    ! Given: Output files that appear to exist but have corrupted content
    ! When: File generation completes but content is invalid
    ! Then: Format validation should detect silent corruption
    subroutine test_silent_format_corruption()
        type(validation_result_t) :: validation
        character(len=*), parameter :: corrupted_file = get_test_output_path("output/test/silent_corruption.png")
        integer :: unit
        
        ! Simulate a file that exists but has corrupted PNG header
        open(newunit=unit, file=corrupted_file, form='unformatted', access='stream')
        write(unit) 'CORRUPT_'  ! Corrupted PNG signature (8 bytes like real PNG)
        close(unit)
        
        ! File exists (would pass basic existence check)
        validation = validate_file_exists(corrupted_file)
        if (.not. validation%passed) then
            print *, "ERROR: Test setup failed - corrupted file should exist"
            stop 1
        end if
        
        ! But format validation should catch corruption
        validation = validate_png_format(corrupted_file)
        if (validation%passed) then
            print *, "CRITICAL FAILURE: Format validation should detect corruption"
            stop 1
        end if
        
        print *, "PASS: Format validation detects silent corruption"
    end subroutine
    
    ! Given: External dependencies that might fail (STB libraries, etc.)
    ! When: Core plotting logic works but output generation fails
    ! Then: Validation should detect dependency-related failures
    subroutine test_dependency_failure_detection()
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        real(wp), dimension(8) :: x, y
        integer :: i
        
        ! Arrange: Create complex plot that exercises dependencies
        do i = 1, 8
            x(i) = real(i, wp)
            y(i) = x(i)**2
        end do
        
        ! Act: Create plot with features that depend on external libraries
        call fig%initialize(500, 400)
        call fig%add_plot(x, y, label="dependency test")
        call fig%set_title("Dependency Validation Test")
        call fig%set_xlabel("X Values")
        call fig%set_ylabel("Y Values")
        call fig%legend()  ! Requires text rendering (STB TrueType)
        call fig%savefig(get_test_output_path("output/test/dependency_test.png"))  ! Requires PNG encoding (STB Image Write)
        
        ! Assert: Verify dependencies worked correctly
        validation = validate_file_exists(get_test_output_path("output/test/dependency_test.png"))
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE: Dependency-based plot generation failed"
            print *, "This indicates STB library or text rendering issues"
            print *, "Message: ", trim(validation%message)
            stop 1
        end if
        
        validation = validate_file_size(get_test_output_path("output/test/dependency_test.png"), MIN_PNG_SIZE * 2)
        if (.not. validation%passed) then
            print *, "CRITICAL FAILURE: Complex plot with dependencies produced undersized output"
            print *, "Message: ", trim(validation%message)
            stop 1
        end if
        
        print *, "PASS: Dependency-based plot generation successful"
    end subroutine
    
end program test_critical_failure_detection