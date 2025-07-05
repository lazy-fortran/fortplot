program debug_signed_unsigned
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: signed_127, signed_255
    integer :: unsigned_val
    
    signed_127 = int(127, 1)
    signed_255 = int(-1, 1)  ! This is how we represent 255 in signed int8
    
    print *, "Signed int8 values:"
    print *, "127 as int8:", signed_127
    print *, "255 as int8:", signed_255
    
    print *, ""
    print *, "Converting to unsigned:"
    print *, "iand(int(signed_127), 255) =", iand(int(signed_127), 255)
    print *, "iand(int(signed_255), 255) =", iand(int(signed_255), 255)
    
    print *, ""
    print *, "Direct int() conversion:"
    print *, "int(signed_127) =", int(signed_127)
    print *, "int(signed_255) =", int(signed_255)
    
    print *, ""
    print *, "What we need for RGB(127,127,127):"
    print *, "Each component should be 127"
    print *, "After YCbCr conversion, Y should be ~127"
    
end program debug_signed_unsigned