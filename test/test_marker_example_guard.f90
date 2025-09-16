program test_marker_example_guard
    !! Ensure marker example plots produce visible output across backends

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation, only: validate_file_exists, validate_file_size, validation_result_t
    implicit none

    real(wp) :: x(10), y_circle(10), y_square(10), y_diamond(10), y_cross(10)
    integer :: i, ascii_unit, ios
    integer :: count_circle, count_square, count_diamond, count_cross
    character(len=512) :: line
    type(validation_result_t) :: validation
    logical :: passed

    character(len=*), parameter :: png_file = 'test/output/all_marker_types_guard.png'
    character(len=*), parameter :: pdf_file = 'test/output/all_marker_types_guard.pdf'
    character(len=*), parameter :: txt_file = 'test/output/all_marker_types_guard.txt'

    ! Generate marker data following the example
    do i = 1, 10
        x(i) = real(i, wp) * 0.5_wp
        y_circle(i) = sin(x(i)) + 3.0_wp
        y_square(i) = cos(x(i)) + 2.0_wp
        y_diamond(i) = sin(x(i) * 2.0_wp) + 1.0_wp
        y_cross(i) = cos(x(i) * 1.5_wp)
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call title('All Marker Types Guard Test')
    call xlabel('X Values')
    call ylabel('Y Values')

    call scatter(x, y_circle, marker='o', label='Circle')
    call scatter(x, y_square, marker='s', label='Square')
    call scatter(x, y_diamond, marker='D', label='Diamond')
    call scatter(x, y_cross, marker='x', label='Cross')
    call legend()

    call savefig(png_file)
    call savefig(pdf_file)
    call savefig(txt_file)

    passed = .true.

    ! Basic file presence checks
    validation = validate_file_exists(png_file)
    if (.not. validation%passed) then
        print *, 'FAIL: PNG file missing - ', trim(validation%message)
        passed = .false.
    end if

    validation = validate_file_exists(pdf_file)
    if (.not. validation%passed) then
        print *, 'FAIL: PDF file missing - ', trim(validation%message)
        passed = .false.
    end if

    validation = validate_file_exists(txt_file)
    if (.not. validation%passed) then
        print *, 'FAIL: TXT file missing - ', trim(validation%message)
        passed = .false.
    end if

    ! Size heuristics catch obviously blank renders (tuned to example assets)
    validation = validate_file_size(png_file, 8000)
    if (.not. validation%passed) then
        print *, 'FAIL: PNG file too small - ', trim(validation%message)
        passed = .false.
    end if

    validation = validate_file_size(pdf_file, 1200)
    if (.not. validation%passed) then
        print *, 'FAIL: PDF file too small - ', trim(validation%message)
        passed = .false.
    end if

    ! Parse ASCII export to ensure each marker type produced glyphs
    count_circle = 0
    count_square = 0
    count_diamond = 0
    count_cross = 0

    open(newunit=ascii_unit, file=txt_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: Cannot open ', trim(txt_file)
        passed = .false.
    else
        do
            read(ascii_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            do i = 1, len_trim(line)
                select case (line(i:i))
                case('o')
                    count_circle = count_circle + 1
                case('#')
                    count_square = count_square + 1
                case('%')
                    count_diamond = count_diamond + 1
                case('x')
                    count_cross = count_cross + 1
                end select
            end do
        end do
        close(ascii_unit)
    end if

    if (count_circle == 0) then
        print *, 'FAIL: No circle markers found in ASCII output'
        passed = .false.
    end if
    if (count_square == 0) then
        print *, 'FAIL: No square markers found in ASCII output'
        passed = .false.
    end if
    if (count_diamond == 0) then
        print *, 'FAIL: No diamond markers found in ASCII output'
        passed = .false.
    end if
    if (count_cross == 0) then
        print *, 'FAIL: No cross markers found in ASCII output'
        passed = .false.
    end if

    if (passed) then
        print *, 'PASS: Marker example guard verified PNG/PDF/TXT outputs'
        stop 0
    else
        stop 1
    end if

end program test_marker_example_guard
