program test_raster_color_bytes
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_primitives, only: color_to_byte, blend_pixel
    implicit none

    integer, parameter :: channels(*) = [0, 127, 128, 178, 213, 254, 255]
    integer(1) :: pixel(3)
    integer :: i, actual

    do i = 1, size(channels)
        actual = iand(int(color_to_byte(real(channels(i), wp)/255.0_wp)), 255)
        if (actual /= channels(i)) then
            print *, 'FAIL: color channel', channels(i), 'stored as', actual
            stop 1
        end if
    end do

    pixel = -1_1
    call blend_pixel(pixel, 1, 1, 0.0_wp, 0.0_wp, 1.0_wp, &
        0.0_wp, 0.447_wp, 0.698_wp)
    if (any(iand(int(pixel), 255) /= [0, 113, 177])) then
        print *, 'FAIL: opaque blue pixel', iand(int(pixel), 255)
        stop 1
    end if

    pixel = -1_1
    call blend_pixel(pixel, 1, 1, 0.0_wp, 0.0_wp, 0.18_wp, &
        0.0_wp, 0.447_wp, 0.698_wp)
    if (any(iand(int(pixel), 255) /= [209, 229, 241])) then
        print *, 'FAIL: translucent blue pixel', iand(int(pixel), 255)
        stop 1
    end if
    print *, 'PASS: raster colors preserve unsigned channel bytes'
end program test_raster_color_bytes
