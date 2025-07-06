program debug_extra_bits
    use fortplot_jpeg, only: initialize_huffman_tables, YAC_HT, UVAC_HT
    implicit none
    
    integer :: i
    
    call initialize_huffman_tables()
    
    print *, "=== Finding what '101' (3 bits) represents ==="
    print *, ""
    
    ! Check Y AC table for 3-bit codes
    print *, "Y AC Huffman codes with 3 bits:"
    do i = 1, 256
        if (YAC_HT(i,2) == 3) then
            write(*, '("Symbol ", I3, ": code=", B0.3, " (", I0, ")")') &
                  i-1, YAC_HT(i,1), YAC_HT(i,1)
        end if
    end do
    print *, ""
    
    ! Check UV AC table for 3-bit codes
    print *, "UV AC Huffman codes with 3 bits:"
    do i = 1, 256
        if (UVAC_HT(i,2) == 3) then
            write(*, '("Symbol ", I3, ": code=", B0.3, " (", I0, ")")') &
                  i-1, UVAC_HT(i,1), UVAC_HT(i,1)
        end if
    end do
    print *, ""
    
    ! The pattern suggests we might be encoding an extra symbol
    print *, "Hypothesis: We're encoding an extra AC coefficient or EOB"
    print *, "somewhere in the process."
    
end program debug_extra_bits