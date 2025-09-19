program test_pdf_text_sqrt_width
    !! Verify PDF text width accounts for sqrt radical head
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf_text, only: estimate_pdf_text_width
    implicit none

    real(wp) :: w_x, w_sqrt
    real(wp), parameter :: fs = 12.0_wp

    w_x = estimate_pdf_text_width('x', fs)
    w_sqrt = estimate_pdf_text_width('\\sqrt{x}', fs)

    if (w_x <= 0.0_wp .or. w_sqrt <= 0.0_wp) then
        print *, 'FAIL: expected positive widths (x=', w_x, ', sqrt=', w_sqrt, ')'
        stop 1
    end if

    if (w_sqrt <= w_x) then
        print *, 'FAIL: sqrt width (', w_sqrt, ') not greater than radicand width (', w_x, ')'
        stop 1
    end if

    print *, 'PASS: sqrt width exceeds radicand width (metrics include radical head)'
end program test_pdf_text_sqrt_width

