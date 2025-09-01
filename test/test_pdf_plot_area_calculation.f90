program test_pdf_plot_area_calculation
    !! Verifies calculate_pdf_plot_area() after refactor to coordinate module.
    !! Ensures mathematical Y-up coordinates are handled correctly.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: plot_margins_t, plot_area_t
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    implicit none

    integer :: cw, ch
    type(plot_margins_t) :: m
    type(plot_area_t) :: a

    cw = 640
    ch = 480

    m%left = 0.10_wp
    m%right = 0.90_wp
    m%bottom = 0.10_wp
    m%top = 0.90_wp

    call calculate_pdf_plot_area(cw, ch, m, a)

    call assert_equal_int(a%left, 64, 'left')
    call assert_equal_int(a%width, 512, 'width')
    call assert_equal_int(a%bottom, 48, 'bottom')
    call assert_equal_int(a%height, 384, 'height')

    print *, 'PASS: calculate_pdf_plot_area returns expected plot area'

contains

    subroutine assert_equal_int(got, expect, label)
        integer, intent(in) :: got, expect
        character(len=*), intent(in) :: label
        if (got /= expect) then
            print *, 'FAIL: ', trim(label), ' got=', got, ' expect=', expect
            stop 1
        end if
    end subroutine assert_equal_int

end program test_pdf_plot_area_calculation

