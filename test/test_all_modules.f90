module test_all_modules
    use fortplot_imagemagick
    use fortplot_png_validation
    use fortplot_spy_backend
    use fortplot_test_helpers
    use fortplot_testing
    use fortplot_test_output_helpers
    use fortplot_test_pdf_utils
end module test_all_modules

program test_all_modules_main
    use test_all_modules
    implicit none
    print *, "All support modules compiled successfully"
end program test_all_modules_main
