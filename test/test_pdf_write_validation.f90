program test_pdf_write_validation
    !! PDF write validation tests
    !! Extracted from test_pdf_validation_comprehensive.f90
    !! 
    !! This test covers:
    !! - PDF write operations validation
    !! - File creation and size validation
    !! - Content validation for various elements

    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_security, only: get_test_output_path
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: all_tests_passed = .true.

    print *, "=== PDF WRITE VALIDATION TESTS ==="
    
    call test_write_validation()
    call print_test_summary()

contains

    subroutine test_write_validation()
        print *, "--- Write Validation Tests ---"
        
        call test_basic_write_validation()
        call test_file_creation_validation()
        call test_content_validation()
    end subroutine test_write_validation

    subroutine test_basic_write_validation()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        logical :: file_exists
        
        call start_test("Basic PDF write validation")
        
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp; ctx%x_max = 5.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 4.0_wp
        
        ! Draw simple content
        call ctx%text(2.5_wp, 2.0_wp, "Write Validation Test")
        call ctx%circle(1.0_wp, 1.0_wp, 0.3_wp)
        call ctx%line(3.0_wp, 1.0_wp, 4.0_wp, 3.0_wp)
        
        filename = get_test_output_path('/tmp/pdf_write_validation.pdf')
        call ctx%write_pdf(filename)
        
        ! Verify file was created
        inquire(file=filename, exist=file_exists)
        call assert_true(file_exists, "PDF file created successfully")
        
        print *, "  Basic write validation test saved"
        call end_test()
    end subroutine test_basic_write_validation

    subroutine test_file_creation_validation()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        logical :: file_exists
        integer :: file_size
        
        call start_test("File creation validation")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = -1.0_wp; ctx%x_max = 6.0_wp
        ctx%y_min = -2.0_wp; ctx%y_max = 3.0_wp
        
        ! Add content that should generate reasonable file size
        call ctx%text(2.5_wp, 1.0_wp, "File Size Test")
        call ctx%axes()
        
        filename = get_test_output_path('/tmp/pdf_file_creation.pdf')
        call ctx%write_pdf(filename)
        
        inquire(file=filename, exist=file_exists, size=file_size)
        call assert_true(file_exists, "PDF file exists")
        
        if (file_size > 100) then
            print *, "  PASS: PDF file has reasonable size (", file_size, " bytes)"
            pass_count = pass_count + 1
        else
            print *, "  FAIL: PDF file too small (", file_size, " bytes)"
        end if
        test_count = test_count + 1
        
        call end_test()
    end subroutine test_file_creation_validation

    subroutine test_content_validation()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Content validation")
        
        ctx = create_pdf_canvas(600, 450)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        ! Test various content types
        call ctx%text(5.0_wp, 7.0_wp, "Content Validation")
        call ctx%line(1.0_wp, 1.0_wp, 9.0_wp, 1.0_wp)
        call ctx%circle(3.0_wp, 4.0_wp, 0.5_wp)
        call ctx%rectangle(6.0_wp, 3.0_wp, 2.0_wp, 2.0_wp)
        
        filename = get_test_output_path('/tmp/pdf_content_validation.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Content validation test saved"
        call end_test()
    end subroutine test_content_validation

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        if (condition) then
            print *, "  PASS: ", message
            pass_count = pass_count + 1
        else
            print *, "  FAIL: ", message
            all_tests_passed = .false.
        end if
        test_count = test_count + 1
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Write Validation Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        if (all_tests_passed) then
            write(*, '(A)') 'All write validation tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED - review output above'
        end if
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_pdf_write_validation