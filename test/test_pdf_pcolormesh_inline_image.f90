program test_pdf_pcolormesh_inline_image
    !! Verify that pcolormesh PDF contains an inline image (BI ... ID ... EI)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    character(len=*), parameter :: fn = 'test/output/test_pdf_inline_image.pdf'
    integer :: unit, ios
    character(len=8192) :: line
    logical :: has_bi, has_id, has_ei
    integer :: i

    call figure()
    call pcolormesh([0.0_wp,0.5_wp,1.0_wp],[0.0_wp,0.5_wp,1.0_wp], reshape([0.1_wp,0.2_wp,0.3_wp, &
                   0.4_wp,0.5_wp,0.6_wp, 0.7_wp,0.8_wp,0.9_wp],[3,3]))
    call savefig(fn)

    has_bi = .false.; has_id = .false.; has_ei = .false.
    open(newunit=unit, file=fn, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', fn
        stop 1
    end if
    do i = 1, 2000
        read(unit,'(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line,' BI ')>0 .or. index(line,'BI /W')>0) has_bi = .true.
        if (index(line,' ID')>0 .or. index(line,' ID ')>0) has_id = .true.
        if (index(line,'EI')>0) has_ei = .true.
    end do
    close(unit)

    if (.not. (has_bi .and. has_id .and. has_ei)) then
        print *, 'FAIL: inline image markers not found (BI/ID/EI)'
        stop 2
    end if
    print *, 'PASS: inline image present in pcolormesh PDF'
end program test_pdf_pcolormesh_inline_image
