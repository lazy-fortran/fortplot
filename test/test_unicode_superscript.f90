program test_unicode_superscript
    use fortplot, only: figure, plot, title, xlabel, ylabel, savefig
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: x(10), y(10)
    integer :: i
    
    ! Generate simple data
    do i = 1, 10
        x(i) = real(i, dp)
        y(i) = x(i)**2
    end do
    
    ! Test PNG rendering with Unicode superscript
    call figure()
    call plot(x, y)
    call title("PNG Test: mc² physics formula")
    call xlabel("x")
    call ylabel("y")
    call savefig('test_png_unicode.png')
    print *, "Created PNG test file"
    
    ! Test PDF rendering with Unicode superscript
    call figure()
    call plot(x, y)
    call title("PDF Test: mc² physics formula")
    call xlabel("x")
    call ylabel("y")
    call savefig('test_pdf_unicode.pdf')
    print *, "Created PDF test file"
    
end program test_unicode_superscript
