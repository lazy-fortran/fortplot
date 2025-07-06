program test_jpeg_huffman_validation
    use fortplot_jpeg, only: YDC_HT, UVDC_HT, YAC_HT, UVAC_HT, initialize_huffman_tables
    implicit none
    
    ! Initialize Huffman tables first
    call initialize_huffman_tables()
    
    call test_ydc_huffman_table()
    call test_uvdc_huffman_table()
    call test_yac_huffman_table()
    call test_uvac_huffman_table()
    call test_eob_encoding()
    call test_dc_encoding_sequences()
    call test_ac_encoding_sequences()
    
    write(*,*) 'All JPEG Huffman validation tests passed!'
    
contains

    subroutine test_ydc_huffman_table()
        integer :: i
        integer, dimension(12,2) :: expected_ydc
        
        write(*,*) 'Testing YDC Huffman table against STB values...'
        
        ! Expected values from STB (0-indexed in C, 1-indexed in Fortran)
        expected_ydc(1,:) = [0, 2]    ! category 0
        expected_ydc(2,:) = [2, 3]    ! category 1
        expected_ydc(3,:) = [3, 3]    ! category 2
        expected_ydc(4,:) = [4, 3]    ! category 3
        expected_ydc(5,:) = [5, 3]    ! category 4
        expected_ydc(6,:) = [6, 3]    ! category 5
        expected_ydc(7,:) = [14, 4]   ! category 6
        expected_ydc(8,:) = [30, 5]   ! category 7
        expected_ydc(9,:) = [62, 6]   ! category 8
        expected_ydc(10,:) = [126, 7] ! category 9
        expected_ydc(11,:) = [254, 8] ! category 10
        expected_ydc(12,:) = [510, 9] ! category 11
        
        do i = 1, 12
            if (YDC_HT(i,1) /= expected_ydc(i,1) .or. YDC_HT(i,2) /= expected_ydc(i,2)) then
                write(*,*) 'YDC_HT mismatch at index', i
                write(*,*) 'Expected:', expected_ydc(i,:)
                write(*,*) 'Got:', YDC_HT(i,:)
                error stop 'YDC_HT table mismatch'
            end if
        end do
        
        write(*,*) 'YDC Huffman table validated'
    end subroutine
    
    subroutine test_uvdc_huffman_table()
        integer :: i
        integer, dimension(12,2) :: expected_uvdc
        
        write(*,*) 'Testing UVDC Huffman table against STB values...'
        
        ! Expected values from STB
        expected_uvdc(1,:) = [0, 2]     ! category 0
        expected_uvdc(2,:) = [1, 2]     ! category 1
        expected_uvdc(3,:) = [2, 2]     ! category 2
        expected_uvdc(4,:) = [6, 3]     ! category 3
        expected_uvdc(5,:) = [14, 4]    ! category 4
        expected_uvdc(6,:) = [30, 5]    ! category 5
        expected_uvdc(7,:) = [62, 6]    ! category 6
        expected_uvdc(8,:) = [126, 7]   ! category 7
        expected_uvdc(9,:) = [254, 8]   ! category 8
        expected_uvdc(10,:) = [510, 9]  ! category 9
        expected_uvdc(11,:) = [1022, 10] ! category 10
        expected_uvdc(12,:) = [2046, 11] ! category 11
        
        do i = 1, 12
            if (UVDC_HT(i,1) /= expected_uvdc(i,1) .or. UVDC_HT(i,2) /= expected_uvdc(i,2)) then
                write(*,*) 'UVDC_HT mismatch at index', i
                write(*,*) 'Expected:', expected_uvdc(i,:)
                write(*,*) 'Got:', UVDC_HT(i,:)
                error stop 'UVDC_HT table mismatch'
            end if
        end do
        
        write(*,*) 'UVDC Huffman table validated'
    end subroutine
    
    subroutine test_yac_huffman_table()
        integer :: i
        integer, dimension(17,2) :: expected_yac_first_row
        
        write(*,*) 'Testing YAC Huffman table against STB values...'
        
        ! First row of YAC table from STB (AC symbols 0x00-0x0F)
        expected_yac_first_row(1,:) = [10, 4]     ! EOB (0x00)
        expected_yac_first_row(2,:) = [0, 2]      ! 0x01
        expected_yac_first_row(3,:) = [1, 2]      ! 0x02
        expected_yac_first_row(4,:) = [4, 3]      ! 0x03
        expected_yac_first_row(5,:) = [11, 4]     ! 0x04
        expected_yac_first_row(6,:) = [26, 5]     ! 0x05
        expected_yac_first_row(7,:) = [120, 7]    ! 0x06
        expected_yac_first_row(8,:) = [248, 8]    ! 0x07
        expected_yac_first_row(9,:) = [1014, 10]  ! 0x08
        expected_yac_first_row(10,:) = [65410, 16] ! 0x09
        expected_yac_first_row(11,:) = [65411, 16] ! 0x0A
        expected_yac_first_row(12,:) = [0, 0]     ! 0x0B
        expected_yac_first_row(13,:) = [0, 0]     ! 0x0C
        expected_yac_first_row(14,:) = [0, 0]     ! 0x0D
        expected_yac_first_row(15,:) = [0, 0]     ! 0x0E
        expected_yac_first_row(16,:) = [0, 0]     ! 0x0F
        expected_yac_first_row(17,:) = [0, 0]     ! 0x10 (not used)
        
        ! Test EOB specifically
        if (YAC_HT(1,1) /= 10 .or. YAC_HT(1,2) /= 4) then
            write(*,*) 'YAC_HT EOB mismatch'
            write(*,*) 'Expected: [10, 4]'
            write(*,*) 'Got:', YAC_HT(1,:)
            error stop 'YAC_HT EOB encoding mismatch'
        end if
        
        ! Test first 11 entries
        do i = 1, 11
            if (YAC_HT(i,1) /= expected_yac_first_row(i,1) .or. &
                YAC_HT(i,2) /= expected_yac_first_row(i,2)) then
                write(*,*) 'YAC_HT mismatch at index', i
                write(*,*) 'Expected:', expected_yac_first_row(i,:)
                write(*,*) 'Got:', YAC_HT(i,:)
                error stop 'YAC_HT table mismatch'
            end if
        end do
        
        ! Test ZRL (Zero Run Length) at 0xF0 (241 in decimal, but 241 in 1-indexed)
        if (YAC_HT(241,1) /= 32707 .or. YAC_HT(241,2) /= 15) then
            write(*,*) 'YAC_HT ZRL (0xF0) mismatch'
            write(*,*) 'Expected: [32707, 15]'
            write(*,*) 'Got:', YAC_HT(241,:)
            error stop 'YAC_HT ZRL encoding mismatch'
        end if
        
        write(*,*) 'YAC Huffman table validated'
    end subroutine
    
    subroutine test_uvac_huffman_table()
        integer :: i
        integer, dimension(12,2) :: expected_uvac_first_row
        
        write(*,*) 'Testing UVAC Huffman table against STB values...'
        
        ! First row of UVAC table from STB
        expected_uvac_first_row(1,:) = [0, 2]    ! EOB (0x00)
        expected_uvac_first_row(2,:) = [1, 2]    ! 0x01
        expected_uvac_first_row(3,:) = [4, 3]    ! 0x02
        expected_uvac_first_row(4,:) = [10, 4]   ! 0x03
        expected_uvac_first_row(5,:) = [24, 5]   ! 0x04
        expected_uvac_first_row(6,:) = [25, 5]   ! 0x05
        expected_uvac_first_row(7,:) = [56, 6]   ! 0x06
        expected_uvac_first_row(8,:) = [120, 7]  ! 0x07
        expected_uvac_first_row(9,:) = [500, 9]  ! 0x08
        expected_uvac_first_row(10,:) = [1014, 10] ! 0x09
        expected_uvac_first_row(11,:) = [4084, 12] ! 0x0A
        expected_uvac_first_row(12,:) = [0, 0]   ! 0x0B
        
        ! Test EOB specifically
        if (UVAC_HT(1,1) /= 0 .or. UVAC_HT(1,2) /= 2) then
            write(*,*) 'UVAC_HT EOB mismatch'
            write(*,*) 'Expected: [0, 2]'
            write(*,*) 'Got:', UVAC_HT(1,:)
            error stop 'UVAC_HT EOB encoding mismatch'
        end if
        
        ! Test first 11 entries
        do i = 1, 11
            if (UVAC_HT(i,1) /= expected_uvac_first_row(i,1) .or. &
                UVAC_HT(i,2) /= expected_uvac_first_row(i,2)) then
                write(*,*) 'UVAC_HT mismatch at index', i
                write(*,*) 'Expected:', expected_uvac_first_row(i,:)
                write(*,*) 'Got:', UVAC_HT(i,:)
                error stop 'UVAC_HT table mismatch'
            end if
        end do
        
        write(*,*) 'UVAC Huffman table validated'
    end subroutine
    
    subroutine test_eob_encoding()
        write(*,*) 'Testing EOB encoding values...'
        
        ! EOB (End of Block) is at index 1 (0x00)
        ! YAC EOB should be [10, 4] (1010 in binary, 4 bits)
        if (YAC_HT(1,1) /= 10 .or. YAC_HT(1,2) /= 4) then
            write(*,*) 'YAC EOB encoding error'
            write(*,*) 'Expected: code=10 (0b1010), bits=4'
            write(*,*) 'Got: code=', YAC_HT(1,1), ', bits=', YAC_HT(1,2)
            error stop 'YAC EOB encoding mismatch'
        end if
        
        ! UVAC EOB should be [0, 2] (00 in binary, 2 bits)
        if (UVAC_HT(1,1) /= 0 .or. UVAC_HT(1,2) /= 2) then
            write(*,*) 'UVAC EOB encoding error'
            write(*,*) 'Expected: code=0 (0b00), bits=2'
            write(*,*) 'Got: code=', UVAC_HT(1,1), ', bits=', UVAC_HT(1,2)
            error stop 'UVAC EOB encoding mismatch'
        end if
        
        write(*,*) 'EOB encoding validated: YAC=[10,4], UVAC=[0,2]'
    end subroutine
    
    subroutine test_dc_encoding_sequences()
        write(*,*) 'Testing DC encoding sequences...'
        
        ! Test DC category 0 (value=0)
        if (YDC_HT(1,1) /= 0 .or. YDC_HT(1,2) /= 2) then
            error stop 'YDC category 0 encoding error'
        end if
        
        ! Test DC category 1 (value=±1)
        if (YDC_HT(2,1) /= 2 .or. YDC_HT(2,2) /= 3) then
            error stop 'YDC category 1 encoding error'
        end if
        
        ! Test DC category 2 (value=±2,±3)
        if (YDC_HT(3,1) /= 3 .or. YDC_HT(3,2) /= 3) then
            error stop 'YDC category 2 encoding error'
        end if
        
        write(*,*) 'DC encoding sequences validated'
    end subroutine
    
    subroutine test_ac_encoding_sequences()
        write(*,*) 'Testing AC encoding sequences...'
        
        ! Test AC symbol for run=0, size=1 (index 2, 0x01)
        if (YAC_HT(2,1) /= 0 .or. YAC_HT(2,2) /= 2) then
            error stop 'YAC symbol 0x01 encoding error'
        end if
        
        ! Test AC symbol for run=0, size=2 (index 3, 0x02)
        if (YAC_HT(3,1) /= 1 .or. YAC_HT(3,2) /= 2) then
            error stop 'YAC symbol 0x02 encoding error'
        end if
        
        ! Test ZRL (15 zeros + run=0, size=0) at 0xF0 (241)
        if (YAC_HT(241,1) /= 32707 .or. YAC_HT(241,2) /= 15) then
            error stop 'YAC ZRL encoding error'
        end if
        
        write(*,*) 'AC encoding sequences validated'
    end subroutine

end program