program debug_block_processing
    implicit none
    
    integer :: width, height
    integer :: x, y
    integer :: y_blocks, u_blocks, v_blocks, total_blocks
    logical :: subsample
    
    ! Test different sizes
    do width = 8, 64, 8
        do height = 8, 64, 8
            subsample = .true.  ! quality <= 90
            
            if (subsample) then
                ! Count blocks for subsampled mode
                y_blocks = 0
                u_blocks = 0
                v_blocks = 0
                
                do y = 1, height, 16
                    do x = 1, width, 16
                        y_blocks = y_blocks + 4  ! 4 Y blocks per MCU
                        u_blocks = u_blocks + 1  ! 1 U block per MCU
                        v_blocks = v_blocks + 1  ! 1 V block per MCU
                    end do
                end do
            else
                ! Count blocks for non-subsampled mode
                y_blocks = 0
                u_blocks = 0
                v_blocks = 0
                
                do y = 1, height, 8
                    do x = 1, width, 8
                        y_blocks = y_blocks + 1
                        u_blocks = u_blocks + 1
                        v_blocks = v_blocks + 1
                    end do
                end do
            end if
            
            total_blocks = y_blocks + u_blocks + v_blocks
            
            write(*, '("Size ", I3, "x", I3, " subsample=", L1, " blocks: Y=", I3, " U=", I3, " V=", I3, " total=", I3)') &
                  width, height, subsample, y_blocks, u_blocks, v_blocks, total_blocks
        end do
    end do
end program