program test_annotation_styling_1437
    !! Regression guard for Issue #1437:
    !! - rotated text uses rotation matrix in PDF
    !! - bbox draws a filled/stroked rectangle in PDF
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = &
                                   'test/output/test_annotation_styling_1437.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    class(figure_t), pointer :: fig_ptr

    call figure(figsize=[6.4_wp, 4.8_wp])
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])

    call text(0.5_wp, 0.5_wp, "Rotated", coord_type=COORD_DATA, &
              font_size=12.0_wp, rotation=90.0_wp, alignment="center", &
              has_bbox=.true.)

    fig_ptr => get_global_figure()
    if (.not. associated(fig_ptr)) then
        print *, 'FAIL: global figure pointer not associated'
        stop 1
    end if
    if (fig_ptr%annotation_count < 1) then
        print *, 'FAIL: expected at least one annotation after text()'
        stop 1
    end if

    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: could not extract PDF stream text'
        stop 1
    end if

    call assert_contains_any(stream_text, &
                             [character(len=36) :: &
                              '0.000000 1.000000 -1.000000 0.000000', &
                              '.000000 1.000000 -1.000000 .000000', &
                              '-.000000 1.000000 -1.000000 -.000000'], &
                             'Rotation matrix present')
    call assert_contains(stream_text, '1 1 1 rg', 'BBox fill color set')
    call assert_contains(stream_text, ' re B', 'BBox rectangle drawn and filled')

    print *, 'PASS: annotation styling PDF guards present (Issue #1437)'

contains

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) <= 0) then
            print *, 'FAIL: ', trim(message), ' (missing: ', trim(pattern), ')'
            stop 1
        end if
    end subroutine assert_contains

    subroutine assert_contains_any(text, patterns, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: patterns(:)
        character(len=*), intent(in) :: message

        integer :: i

        do i = 1, size(patterns)
            if (index(text, trim(patterns(i))) > 0) return
        end do

        print *, 'FAIL: ', trim(message)
        do i = 1, size(patterns)
            print *, '  missing: ', trim(patterns(i))
        end do
        stop 1
    end subroutine assert_contains_any

end program test_annotation_styling_1437
