program test_unicode_superscript
    use fortplot, only: figure, plot, title, xlabel, ylabel, savefig
    use test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: x(10), y(10)
    integer :: i
    character(len=:), allocatable :: output_dir

    call ensure_test_output_dir('unicode_superscript', output_dir)

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
    call savefig(output_dir//'test_png_unicode.png')
    print *, 'Created PNG test file at '//output_dir//'test_png_unicode.png'

    ! Test PDF rendering with Unicode superscript
    call figure()
    call plot(x, y)
    call title("PDF Test: mc² physics formula")
    call xlabel("x")
    call ylabel("y")
    call savefig(output_dir//'test_pdf_unicode.pdf')
    print *, 'Created PDF test file at '//output_dir//'test_pdf_unicode.pdf'

end program test_unicode_superscript
