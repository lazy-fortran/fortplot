program test_pdf_dash_reset
    !! Verify that PDF axes frame/ticks use solid dash pattern regardless of prior plot styles
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, legend, savefig, title
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/pdf_dash_reset.pdf'
    integer :: unit, ios
    logical :: exists
    character(len=:), allocatable :: content
    integer :: sz

    call title('Dash Reset Test')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], label='line', linestyle='--')
    call legend()
    call savefig(out_pdf)

    ! Read PDF file as text to search for solid dash command '[] 0 d'
    inquire(file=out_pdf, exist=exists)
    if (.not. exists) then
        print *, 'FAIL: PDF not created: ', trim(out_pdf)
        stop 1
    end if

    open(newunit=unit, file=out_pdf, access='stream', form='unformatted', status='old', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(out_pdf)
        stop 1
    end if

    inquire(unit=unit, size=sz)
    if (sz <= 0) then
        print *, 'FAIL: zero-size PDF'
        close(unit)
        stop 1
    end if

    allocate(character(len=sz) :: content)
    read(unit) content
    close(unit)

    if (index(content, '[] 0 d') == 0) then
        print *, 'FAIL: PDF stream missing solid dash reset ([] 0 d)'
        stop 1
    end if

    print *, 'PASS: PDF axes/ticks dash reset present'
end program test_pdf_dash_reset
