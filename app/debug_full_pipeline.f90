program debug_full_pipeline
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    real :: block(8,8)
    real :: y_val
    integer :: i, j
    
    ! Step 1: RGB to YCbCr
    print *, "=== Step 1: RGB to YCbCr ==="
    print *, "RGB(127,127,127) -> Y = 127.0"
    
    ! Step 2: Shift by 128
    print *, ""
    print *, "=== Step 2: JPEG shift ==="
    y_val = 127.0 - 128.0
    print *, "Y - 128 =", y_val
    
    ! Step 3: Create 8x8 block
    print *, ""
    print *, "=== Step 3: 8x8 block ==="
    block = y_val
    print *, "All values:", y_val
    
    ! Step 4: Expected DCT output
    print *, ""
    print *, "=== Step 4: DCT ==="
    print *, "For uniform block, DC = sum of all values"
    print *, "DC = 64 *", y_val, "=", 64.0 * y_val
    
    ! The issue might be that our gray images are being encoded
    ! with wrong DC values, making them appear black
    
    print *, ""
    print *, "If DC is negative and large, image will be dark/black"
    
end program debug_full_pipeline