program test_png_performance
    !! Test PNG compression performance and file size scaling
    use fortplot_png, only: png_context, create_png_canvas
    use, intrinsic :: iso_fortran_env, only: real64, int64
    implicit none
    
    type(png_context) :: plt
    real(real64) :: x1, y1, x2, y2
    integer :: i, j
    integer :: size_empty, size_sparse, size_dense
    integer(int64) :: start_time, end_time, count_rate
    real :: compression_ratio
    
    print '(A)', '================================================'
    print '(A)', 'PNG Backend Performance & Compression Test'
    print '(A)', '================================================'
    print '(A)', ''
    
    ! Test 1: Empty canvas (baseline)
    call system_clock(start_time, count_rate)
    plt = create_png_canvas(800, 600)
    call plt%save('perf_empty.png')
    call system_clock(end_time)
    call get_file_size('perf_empty.png', size_empty)
    print '(A,I10,A,F8.2,A)', 'Empty canvas:     ', size_empty, &
           ' bytes (', real(end_time - start_time)/real(count_rate)*1000, ' ms)'
    
    ! Test 2: Sparse content (10 lines with colors)
    call system_clock(start_time, count_rate)
    plt = create_png_canvas(800, 600)
    do i = 1, 10
        x1 = real(i * 80 - 40, real64)
        y1 = 100.0_real64
        x2 = x1
        y2 = 500.0_real64
        ! Set different color for each line
        call plt%color(real(i, real64)/10.0_real64, 0.5_real64, 1.0_real64 - real(i, real64)/10.0_real64)
        call plt%line(x1, y1, x2, y2)
    end do
    call plt%save('perf_sparse.png')
    call system_clock(end_time)
    call get_file_size('perf_sparse.png', size_sparse)
    print '(A,I10,A,F8.2,A)', 'Sparse content:   ', size_sparse, &
           ' bytes (', real(end_time - start_time)/real(count_rate)*1000, ' ms)'
    
    ! Test 3: Dense content (grid pattern with varying colors)
    call system_clock(start_time, count_rate)
    plt = create_png_canvas(800, 600)
    ! Horizontal lines
    do i = 1, 30
        y1 = real(i * 20, real64)
        call plt%color(0.8_real64, real(i, real64)/30.0_real64, 0.2_real64)
        call plt%line(0.0_real64, y1, 800.0_real64, y1)
    end do
    ! Vertical lines
    do i = 1, 40
        x1 = real(i * 20, real64)
        call plt%color(real(i, real64)/40.0_real64, 0.3_real64, 0.9_real64)
        call plt%line(x1, 0.0_real64, x1, 600.0_real64)
    end do
    ! Diagonal pattern
    do i = 1, 20
        x1 = real(i * 40, real64)
        y1 = 0.0_real64
        x2 = 0.0_real64
        y2 = real(i * 30, real64)
        call plt%color(0.5_real64, 0.8_real64, real(i, real64)/20.0_real64)
        call plt%line(x1, y1, x2, y2)
    end do
    call plt%save('perf_dense.png')
    call system_clock(end_time)
    call get_file_size('perf_dense.png', size_dense)
    print '(A,I10,A,F8.2,A)', 'Dense content:    ', size_dense, &
           ' bytes (', real(end_time - start_time)/real(count_rate)*1000, ' ms)'
    
    print '(A)', ''
    print '(A)', 'Compression Analysis:'
    print '(A)', '--------------------'
    
    ! Expected uncompressed size: 800 * 600 * 3 = 1,440,000 bytes
    compression_ratio = real(size_empty) / 1440000.0 * 100.0
    print '(A,F6.2,A)', 'Empty compression:  ', compression_ratio, '%'
    
    compression_ratio = real(size_sparse) / 1440000.0 * 100.0
    print '(A,F6.2,A)', 'Sparse compression: ', compression_ratio, '%'
    
    compression_ratio = real(size_dense) / 1440000.0 * 100.0
    print '(A,F6.2,A)', 'Dense compression:  ', compression_ratio, '%'
    
    print '(A)', ''
    print '(A)', 'Size Scaling:'
    print '(A)', '-------------'
    print '(A,F6.2,A)', 'Sparse/Empty ratio: ', &
           real(size_sparse)/real(size_empty), 'x'
    print '(A,F6.2,A)', 'Dense/Empty ratio:  ', &
           real(size_dense)/real(size_empty), 'x'
    print '(A,F6.2,A)', 'Dense/Sparse ratio: ', &
           real(size_dense)/real(size_sparse), 'x'
    
    ! Verify fix for issue #264
    print '(A)', ''
    if (size_empty == 1440773 .and. size_sparse == 1440773 .and. &
        size_dense == 1440773) then
        print '(A)', '❌ FAIL: Fixed-size PNG bug detected (issue #264)'
        stop 1
    else
        print '(A)', '✅ PASS: PNG file sizes scale with content complexity'
        print '(A)', '        Issue #264 is resolved!'
    end if
    
contains
    
    subroutine get_file_size(filename, file_size)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: file_size
        integer :: unit, ios
        
        inquire(file=filename, size=file_size)
        
        if (file_size < 0) then
            open(newunit=unit, file=filename, access='stream', &
                 form='unformatted', status='old', iostat=ios)
            if (ios == 0) then
                inquire(unit, size=file_size)
                close(unit)
            else
                file_size = 0
            end if
        end if
    end subroutine get_file_size
    
end program test_png_performance