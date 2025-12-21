program test_backend_switching
    !! Test that savefig automatically switches backend based on file extension
    !! This tests the fix for Issue #323
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation, only: validate_pdf_format, validate_png_format, validation_result_t
    implicit none
    
    type(figure_t) :: fig
    type(validation_result_t) :: validation
    logical :: all_passed = .true.
    real(wp) :: x(10), y(10)
    integer :: i
    
    print *, "========================================"
    print *, "BACKEND SWITCHING TEST (Issue #323 fix)"
    print *, "========================================"
    print *, ""
    
    ! Generate simple test data
    do i = 1, 10
        x(i) = real(i-1, wp)
        y(i) = sin(x(i))
    end do
    
    ! Test 1: Initialize with PNG backend, save as PDF
    print *, "Test 1: PNG backend -> PDF file"
    call fig%initialize(80, 24, backend='png')  ! Explicitly use PNG backend
    call fig%add_plot(x, y)
    call fig%savefig("test/output/test_png_to_pdf.pdf")  ! Should auto-switch to PDF
    
    validation = validate_pdf_format('test/output/test_png_to_pdf.pdf')
    if (validation%passed) then
        print *, "  PASS: Passed: Auto-switched from PNG to PDF backend"
    else
        print *, "  FAIL: Failed:", trim(validation%message)
        all_passed = .false.
    end if
    
    ! Test 2: Initialize with PDF backend, save as PNG
    print *, ""
    print *, "Test 2: PDF backend -> PNG file"
    call fig%initialize(80, 24, backend='pdf')  ! Explicitly use PDF backend
    call fig%add_plot(x, y*2.0_wp)
    call fig%savefig("test/output/test_pdf_to_png.png")  ! Should auto-switch to PNG
    
    validation = validate_png_format('test/output/test_pdf_to_png.png')
    if (validation%passed) then
        print *, "  PASS: Passed: Auto-switched from PDF to PNG backend"
    else
        print *, "  FAIL: Failed:", trim(validation%message)
        all_passed = .false.
    end if
    
    ! Test 3: Initialize with ASCII backend, save as PDF
    print *, ""
    print *, "Test 3: ASCII backend -> PDF file"
    call fig%initialize(80, 24, backend='ascii')  ! Use ASCII backend
    call fig%add_plot(x, y*0.5_wp)
    call fig%savefig("test/output/test_ascii_to_pdf.pdf")  ! Should auto-switch to PDF
    
    validation = validate_pdf_format('test/output/test_ascii_to_pdf.pdf')
    if (validation%passed) then
        print *, "  PASS: Passed: Auto-switched from ASCII to PDF backend"
    else
        print *, "  FAIL: Failed:", trim(validation%message)
        all_passed = .false.
    end if
    
    ! Test 4: Multiple saves with different formats from same figure
    print *, ""
    print *, "Test 4: Multiple format saves from same figure"
    call fig%initialize(80, 24)  ! Default backend
    call fig%add_plot(x, cos(x))
    
    ! Save as PDF
    call fig%savefig("test/output/test_multi.pdf")
    validation = validate_pdf_format('test/output/test_multi.pdf')
    if (.not. validation%passed) then
        print *, "  FAIL: Failed PDF:", trim(validation%message)
        all_passed = .false.
    end if
    
    ! Save same figure as PNG
    call fig%savefig("test/output/test_multi.png")
    validation = validate_png_format('test/output/test_multi.png')
    if (.not. validation%passed) then
        print *, "  FAIL: Failed PNG:", trim(validation%message)
        all_passed = .false.
    else
        print *, "  PASS: Passed: Same figure saved in multiple formats"
    end if
    
    ! Summary
    print *, ""
    print *, "========================================"
    if (all_passed) then
        print *, "PASS: ALL BACKEND SWITCHING TESTS PASSED!"
        print *, "savefig correctly auto-switches backends"
    else
        print *, "FAIL: SOME TESTS FAILED"
        stop 1
    end if
    
end program test_backend_switching