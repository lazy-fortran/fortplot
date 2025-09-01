program test_png_zlib_integrity_983
    use fortplot, only: figure, plot, savefig, wp
    use fortplot_png_validation, only: validate_png_file
    implicit none

    character(len=*), parameter :: fn = 'test/output/png_zlib_integrity_983.png'
    logical :: exists
    integer :: unit
    integer(8) :: fsize

    call plot([0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp], [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp])
    call savefig(fn)

    call validate_png_file(fn, 'Issue #983: PNG zlib integrity')

    inquire(file=fn, exist=exists)
    if (.not. exists) then
        print *, 'ERROR: PNG not created: ', fn
        error stop 1
    end if

    open(newunit=unit, file=fn, access='stream', form='unformatted', status='old')
    inquire(unit=unit, size=fsize)
    close(unit)

    print *, 'PNG size (bytes):', fsize
    if (fsize > 200000_8) then
        print *, 'ERROR: PNG too large for simple plot (possible compression failure)'
        error stop 2
    end if

    print *, 'SUCCESS: PNG compression integrity validated (Issue #983)'
end program test_png_zlib_integrity_983
