program debug_stb_sof0
    implicit none
    
    print *, "For 8x8 image with 2x2 subsampling:"
    print *, "- Y component: 1 full 8x8 block"
    print *, "- U component: theoretically 4x4 (subsampled), but JPEG needs 8x8 blocks"
    print *, "- V component: theoretically 4x4 (subsampled), but JPEG needs 8x8 blocks"
    print *, ""
    print *, "STB likely pads the 4x4 subsampled data to 8x8 blocks"
    print *, "This means we need to properly handle small images"
    
end program debug_stb_sof0