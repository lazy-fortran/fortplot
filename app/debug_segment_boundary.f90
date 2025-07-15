program debug_segment_boundary
    implicit none
    
    character(len=10) :: current_segment
    integer :: segment_pos, i
    
    ! Simulate the exact segment logic
    current_segment = ""
    segment_pos = 1
    
    print *, "=== Segment boundary test ==="
    
    ! Add 'A'
    current_segment(segment_pos:segment_pos) = 'A'
    segment_pos = segment_pos + 1
    print *, "After adding 'A': segment='", current_segment(1:segment_pos-1), "', pos=", segment_pos
    
    ! Add ' '
    current_segment(segment_pos:segment_pos) = ' '
    segment_pos = segment_pos + 1
    print *, "After adding ' ': segment='", current_segment(1:segment_pos-1), "', pos=", segment_pos
    
    ! Now simulate what happens when we encounter Greek letter
    if (segment_pos > 1) then
        print *, "Would output segment: '", current_segment(1:segment_pos-1), "'"
        print *, "Length of output: ", len_trim(current_segment(1:segment_pos-1))
        
        ! Check each character
        print *, "Characters in segment:"
        do i = 1, segment_pos-1
            print *, "  ", i, ": '", current_segment(i:i), "' (", iachar(current_segment(i:i)), ")"
        end do
    end if
    
end program debug_segment_boundary