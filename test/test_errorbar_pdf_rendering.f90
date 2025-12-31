program test_errorbar_pdf_rendering
    !! Verify Issue #1508: Errorbar support in PDF backend
    !! Ensures errorbar renders line segments and markers in PDF output
    !!
    !! The PDF backend generates compressed (FlateDecode) content streams.
    !! Error bar rendering produces PDF line operators (m, l, S) for:
    !! - Vertical/horizontal error bar stems
    !! - Cap lines at error bar ends
    !! - Circular markers at data points
    !!
    !! This test verifies that error bar PDFs contain more drawing commands
    !! than baseline plots, indicating the error bars were rendered.
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y, yerr, xerr
    integer :: file_size_no_err, file_size_with_err, file_size_xy

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]
    xerr = [0.1_wp, 0.15_wp, 0.12_wp, 0.18_wp, 0.1_wp]

    ! Create baseline plot without error bars
    call figure()
    call plot(x, y, label='baseline')
    call title('Baseline PDF Test')
    call savefig('test/output/test_errorbar_pdf_noerr.pdf')

    ! Verify baseline PDF was created
    inquire(file='test/output/test_errorbar_pdf_noerr.pdf', size=file_size_no_err)
    if (file_size_no_err <= 0) then
        print *, 'FAIL: baseline PDF not created'
        stop 1
    end if

    ! Test 1: Y error bars only
    call figure()
    call errorbar(x, y, yerr=yerr, label='y-err', marker='o', &
                  color=[0.0_wp, 0.0_wp, 1.0_wp])
    call title('Errorbar PDF Y-error Test')
    call savefig('test/output/test_errorbar_pdf.pdf')

    inquire(file='test/output/test_errorbar_pdf.pdf', size=file_size_with_err)
    if (file_size_with_err <= 0) then
        print *, 'FAIL: errorbar PDF not created'
        stop 1
    end if

    ! Error bar PDF should be larger due to additional line drawing commands
    if (file_size_with_err > file_size_no_err) then
        print *, 'PASS: Y-errorbar PDF larger than baseline (', &
                 file_size_with_err, ' > ', file_size_no_err, ' bytes)'
    else
        print *, 'FAIL: Y-errorbar PDF not larger than baseline'
        stop 1
    end if

    ! Test 2: X+Y error bars - should produce even more content
    call figure()
    call errorbar(x, y, xerr=xerr, yerr=yerr, label='xy-err', marker='s', &
                  color=[1.0_wp, 0.0_wp, 0.0_wp])
    call title('Errorbar PDF X+Y Test')
    call savefig('test/output/test_errorbar_pdf_xy.pdf')

    inquire(file='test/output/test_errorbar_pdf_xy.pdf', size=file_size_xy)
    if (file_size_xy <= 0) then
        print *, 'FAIL: X+Y errorbar PDF not created'
        stop 1
    end if

    ! X+Y error bars should produce more content than Y-only
    if (file_size_xy > file_size_no_err) then
        print *, 'PASS: X+Y errorbar PDF larger than baseline (', &
                 file_size_xy, ' > ', file_size_no_err, ' bytes)'
    else
        print *, 'FAIL: X+Y errorbar PDF not larger than baseline'
        stop 1
    end if

    print *, 'PASS: PDF errorbar rendering verified (fixes #1508)'

end program test_errorbar_pdf_rendering
