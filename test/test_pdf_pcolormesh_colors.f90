program test_pdf_pcolormesh_colors
    use fortplot
    use iso_fortran_env, only: wp => real64
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pcolormesh_colors.pdf'
    real(wp) :: x(4), y(3), c(2,3)
    logical :: ok

    integer :: unit, ios
    character(len=65536) :: buf
    integer :: read_len
    logical :: has_nonblack_fill

    call create_directory_runtime('test/output', ok)

    x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    y = [0.0_wp, 0.5_wp, 1.0_wp]
    c = reshape([0.1_wp, 0.4_wp, 0.6_wp, &
                 0.2_wp, 0.8_wp, 0.9_wp], [2,3])

    call figure()
    call pcolormesh(x, y, c)
    call title('PDF pcolormesh color test')
    call savefig(out_pdf)

    ! Read the generated PDF and verify that at least one non-black
    ! non-stroking color ("rg") command appears in the stream.
    open(newunit=unit, file=out_pdf, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(*,'(A)') 'FAIL: could not open '//trim(out_pdf)
        error stop 1
    end if
    buf = ''
    read_len = 0
    do
        read(unit, '(A)', iostat=ios) buf(read_len+1:)
        if (ios /= 0) exit
        read_len = len_trim(buf)
        if (read_len > len(buf) - 256) exit
    end do
    close(unit)

    ! Check for any ' rg' occurrence that is not the initial '0 0 0 rg'
    has_nonblack_fill = index(buf, ' rg') > 0 .and. index(buf, '0.000 0.000 0.000 rg') == 0
    if (has_nonblack_fill) then
        write(*,'(A)') 'PASS: PDF contains non-black fill color commands (rg)'
        stop 0
    else
        write(*,'(A)') 'FAIL: Missing non-black fill color (rg) in PDF stream'
        error stop 2
    end if
end program test_pdf_pcolormesh_colors

