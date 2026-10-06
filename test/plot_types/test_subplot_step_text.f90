program test_subplot_step_text
    !! step() and text() must draw into the selected subplot, as plot() does.
    !!
    !! Left panel: a red step curve; right panel: data-coordinate text at the
    !! centre of its axes. Oracle (PNG raster buffer and pdftoppm raster of
    !! the PDF): red pixels exist in the left half only, and dark text pixels
    !! exist near the centre of the right axes but not at the mirrored spot
    !! of the left axes. pdftotext must also find the text in the PDF.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: color_bbox, rasterize_pdf, &
                                          have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    character(len=:), allocatable :: dir
    integer :: failures
    integer(1), allocatable :: img(:)
    integer :: w, h
    logical :: ok

    call ensure_test_output_dir('subplot_step_text', dir)
    failures = 0

    call figure(figsize=[9.6_wp, 3.6_wp])
    call subplot(1, 2, 1)
    call step([0.1_wp, 0.4_wp, 0.6_wp, 0.9_wp], [0.2_wp, 0.8_wp, 0.3_wp, 0.7_wp], &
              color='red')
    call xlim(0.0_wp, 1.0_wp)
    call ylim(0.0_wp, 1.0_wp)
    call subplot(1, 2, 2)
    call xlim(0.0_wp, 1.0_wp)
    call ylim(0.0_wp, 1.0_wp)
    call text(0.5_wp, 0.5_wp, 'Panel', font_size=16.0_wp, ha='center')
    call savefig(dir//'step_text.png')
    select type (bk => global_figure%state%backend)
    class is (raster_context)
        call check(bk%raster%image_data, bk%width, bk%height, 'PNG')
    class default
        print *, 'FAIL: expected a raster backend'
        failures = failures + 1
    end select

    if (have_command('pdftoppm') .and. have_command('pdftotext')) then
        call savefig(dir//'step_text.pdf')
        call rasterize_pdf(dir//'step_text.pdf', 100, img, w, h, ok)
        if (ok) then
            call check(img, w, h, 'PDF')
        else
            print *, 'FAIL: could not rasterise the PDF'
            failures = failures + 1
        end if
        call check_pdf_text(dir//'step_text.pdf')
    else
        print *, 'SKIPPED PDF part: poppler tools not available'
    end if

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' check(s) failed'
        stop 1
    end if
    print *, 'PASS: step and text draw into their subplots'

contains

    subroutine check(img, w, h, what)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        character(len=*), intent(in) :: what
        integer :: x0, x1, y0, y1, n_left, n_right, n_text, n_mirror

        call color_bbox(img, w, h, [255, 0, 0], 60, x0, x1, y0, y1, n_left, &
                        region=[0, w/2 - 1, 0, h - 1])
        call color_bbox(img, w, h, [255, 0, 0], 60, x0, x1, y0, y1, n_right, &
                        region=[w/2, w - 1, 0, h - 1])
        print '(1x,2a,i0,a,i0)', what, ': red pixels left/right ', n_left, &
            ' / ', n_right
        if (n_left < 100 .or. n_right > 0) then
            print *, 'FAIL: step curve not drawn in the left subplot (', what, ')'
            failures = failures + 1
        end if
        ! Axes centres lie near 0.29 w and 0.71 w for this 1x2 layout.
        call dark_count(img, w, h, nint(0.66_wp*w), nint(0.78_wp*w), n_text)
        call dark_count(img, w, h, nint(0.24_wp*w), nint(0.36_wp*w), n_mirror)
        print '(1x,2a,i0,a,i0)', what, ': text pixels right/left ', n_text, &
            ' / ', n_mirror
        if (n_text < 30 .or. n_mirror > 0) then
            print *, 'FAIL: text not drawn in the right subplot (', what, ')'
            failures = failures + 1
        end if
    end subroutine check

    subroutine dark_count(img, w, h, c0, c1, n)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h, c0, c1
        integer, intent(out) :: n
        integer :: x0, x1, y0, y1
        call color_bbox(img, w, h, [0, 0, 0], 110, x0, x1, y0, y1, n, &
                        region=[c0, c1, nint(0.38_wp*h), nint(0.62_wp*h)])
    end subroutine dark_count

    subroutine check_pdf_text(pdf)
        character(len=*), intent(in) :: pdf
        integer :: unit, ios, stat
        character(len=512) :: line
        logical :: found
        call execute_command_line('pdftotext "'//pdf//'" "'//pdf//'.txt"', &
                                  exitstat=stat)
        found = .false.
        if (stat == 0) then
            open (newunit=unit, file=pdf//'.txt', status='old', iostat=ios)
            do while (ios == 0)
                read (unit, '(a)', iostat=ios) line
                if (ios == 0) found = found .or. index(line, 'Panel') > 0
            end do
            close (unit)
        end if
        if (.not. found) then
            print *, 'FAIL: pdftotext does not find the subplot text'
            failures = failures + 1
        end if
    end subroutine check_pdf_text

end program test_subplot_step_text
