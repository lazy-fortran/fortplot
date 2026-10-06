module fortplot_test_raster_probe
    !! Pixel probes for rendering tests.
    !!
    !! Images are packed RGB bytes, row-major from the top-left corner, the
    !! layout of fortplot's raster buffer and of a binary PPM (P6) body. PDF
    !! output is rasterised by poppler's pdftoppm, an implementation that
    !! shares no code with fortplot, so PDF probes are independent oracles.
    implicit none

    private
    public :: color_bbox, read_ppm, rasterize_pdf, have_command

contains

    subroutine color_bbox(img, w, h, rgb, tol, x0, x1, y0, y1, count, region)
        !! Bounding box (0-based columns x0..x1, rows y0..y1) and pixel count
        !! of pixels within tol of rgb in every channel. count = 0 when none.
        !! region = [col_lo, col_hi, row_lo, row_hi] limits the search.
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h, rgb(3), tol
        integer, intent(out) :: x0, x1, y0, y1, count
        integer, intent(in), optional :: region(4)
        integer :: row, col, k, c(3), lim(4)
        logical :: hit

        lim = [0, w - 1, 0, h - 1]
        if (present(region)) lim = region
        x0 = huge(1); y0 = huge(1); x1 = -1; y1 = -1; count = 0
        do row = max(0, lim(3)), min(h - 1, lim(4))
            do col = max(0, lim(1)), min(w - 1, lim(2))
                do k = 1, 3
                    c(k) = iand(int(img(3*(row*w + col) + k)), 255)
                end do
                hit = all(abs(c - rgb) <= tol)
                if (.not. hit) cycle
                count = count + 1
                x0 = min(x0, col); x1 = max(x1, col)
                y0 = min(y0, row); y1 = max(y1, row)
            end do
        end do
    end subroutine color_bbox

    subroutine read_ppm(path, img, w, h, ok)
        !! Read a binary PPM (P6, maxval 255) as written by pdftoppm.
        character(len=*), intent(in) :: path
        integer(1), allocatable, intent(out) :: img(:)
        integer, intent(out) :: w, h
        logical, intent(out) :: ok
        integer :: unit, ios, pos, maxval_ppm
        character(len=1) :: ch
        integer :: fields(3), nf, val
        logical :: in_num

        ok = .false.; w = 0; h = 0
        open (newunit=unit, file=path, access='stream', form='unformatted', &
              status='old', iostat=ios)
        if (ios /= 0) return
        read (unit, iostat=ios) ch
        if (ios /= 0 .or. ch /= 'P') then
            close (unit); return
        end if
        read (unit, iostat=ios) ch
        if (ios /= 0 .or. ch /= '6') then
            close (unit); return
        end if
        nf = 0; val = 0; in_num = .false.
        do while (nf < 3)
            read (unit, iostat=ios) ch
            if (ios /= 0) then
                close (unit); return
            end if
            if (ch == '#') then
                do
                    read (unit, iostat=ios) ch
                    if (ios /= 0 .or. ch == achar(10)) exit
                end do
                cycle
            end if
            if (ch >= '0' .and. ch <= '9') then
                val = 10*val + (iachar(ch) - iachar('0'))
                in_num = .true.
            else if (in_num) then
                nf = nf + 1; fields(nf) = val; val = 0; in_num = .false.
            end if
        end do
        ! One whitespace byte follows maxval; it was consumed by the loop.
        w = fields(1); h = fields(2); maxval_ppm = fields(3)
        if (maxval_ppm /= 255 .or. w <= 0 .or. h <= 0) then
            close (unit); return
        end if
        inquire (unit=unit, pos=pos)
        allocate (img(3*w*h))
        read (unit, pos=pos, iostat=ios) img
        close (unit)
        ok = ios == 0
    end subroutine read_ppm

    logical function have_command(name) result(found)
        character(len=*), intent(in) :: name
        integer :: stat
        call execute_command_line('command -v '//name//' >/dev/null 2>&1', &
                                  exitstat=stat)
        found = stat == 0
    end function have_command

    subroutine rasterize_pdf(pdf, dpi, img, w, h, ok)
        !! Rasterise page 1 of pdf with pdftoppm at dpi (no anti-aliasing).
        character(len=*), intent(in) :: pdf
        integer, intent(in) :: dpi
        integer(1), allocatable, intent(out) :: img(:)
        integer, intent(out) :: w, h
        logical, intent(out) :: ok
        character(len=16) :: dpi_s
        integer :: stat

        ok = .false.; w = 0; h = 0
        write (dpi_s, '(i0)') dpi
        call execute_command_line('pdftoppm -r '//trim(dpi_s)// &
                                  ' -aa no -aaVector no -singlefile "'//pdf// &
                                  '" "'//pdf//'"', exitstat=stat)
        if (stat /= 0) return
        call read_ppm(pdf//'.ppm', img, w, h, ok)
    end subroutine rasterize_pdf

end module fortplot_test_raster_probe
