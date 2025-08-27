program test_histogram_stress
    !! Stress testing for histogram functionality
    !! Tests performance and robustness with large datasets
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    write(*,*) '=== HISTOGRAM STRESS TESTING ==='
    
    ! Test large datasets
    call test_large_datasets()
    
    ! Test extreme parameters
    call test_extreme_parameters()
    
    ! Test memory pressure
    call test_memory_usage()
    
    write(*,*) '=== STRESS TESTS COMPLETED ==='
    
contains

    subroutine test_large_datasets()
        !! Test histogram with large amounts of data
        type(figure_t) :: fig
        real(wp), allocatable :: large_data(:)
        integer :: i, n_data
        real(wp) :: start_time, end_time
        
        write(*,*) 'Testing large datasets...'
        
        ! Test with 10,000 data points
        n_data = 10000
        allocate(large_data(n_data))
        
        ! Generate synthetic data with normal-like distribution
        do i = 1, n_data
            large_data(i) = sin(real(i, wp) * 0.001_wp) * 10.0_wp + &
                           cos(real(i, wp) * 0.01_wp) * 5.0_wp + &
                           real(mod(i, 100), wp) * 0.1_wp
        end do
        
        call cpu_time(start_time)
        call fig%initialize(800, 600)
        call fig%hist(large_data, bins=50)
        call fig%set_title('Large Dataset (10K points)')
        call fig%savefig('build/stress_large_10k.png')
        call cpu_time(end_time)
        
        write(*,'(A,F8.3,A)') '  ✓ 10K points processed in ', end_time - start_time, ' seconds'
        
        deallocate(large_data)
        
        ! Test with 50,000 data points
        n_data = 50000
        allocate(large_data(n_data))
        
        do i = 1, n_data
            large_data(i) = real(i, wp) * 0.001_wp + &
                           sin(real(i, wp) * 0.0001_wp) * 100.0_wp
        end do
        
        call cpu_time(start_time)
        call fig%initialize(800, 600)
        call fig%hist(large_data, bins=100)
        call fig%set_title('Very Large Dataset (50K points)')
        call fig%savefig('build/stress_large_50k.png')
        call cpu_time(end_time)
        
        write(*,'(A,F8.3,A)') '  ✓ 50K points processed in ', end_time - start_time, ' seconds'
        
        deallocate(large_data)
        
    end subroutine test_large_datasets
    
    subroutine test_extreme_parameters()
        !! Test with extreme parameter values
        type(figure_t) :: fig
        real(wp) :: data(1000)
        integer :: i
        
        write(*,*) 'Testing extreme parameters...'
        
        ! Generate test data
        do i = 1, 1000
            data(i) = real(i, wp) * 0.01_wp
        end do
        
        ! Test with very many bins
        call fig%initialize(800, 600)
        call fig%hist(data, bins=500)
        call fig%set_title('Extreme: 500 Bins')
        call fig%savefig('build/stress_many_bins.png')
        write(*,*) '  ✓ Many bins (500) handled'
        
        ! Test with minimal bins
        call fig%initialize(800, 600)
        call fig%hist(data, bins=1)
        call fig%set_title('Extreme: 1 Bin')
        call fig%savefig('build/stress_one_bin.png')
        write(*,*) '  ✓ Single bin handled'
        
        ! Test with very wide range data
        data(1:500) = 1.0e-6_wp
        data(501:1000) = 1.0e6_wp
        
        call fig%initialize(800, 600)
        call fig%hist(data, bins=20)
        call fig%set_title('Extreme: Wide Range Data')
        call fig%savefig('build/stress_wide_range.png')
        write(*,*) '  ✓ Wide range data handled'
        
    end subroutine test_extreme_parameters
    
    subroutine test_memory_usage()
        !! Test memory usage patterns
        type(figure_t) :: fig
        real(wp), allocatable :: data(:)
        integer :: i, j, size
        
        write(*,*) 'Testing memory usage patterns...'
        
        ! Test multiple histograms in sequence (memory cleanup)
        do i = 1, 10
            size = 1000 * i
            allocate(data(size))
            
            data = real([(j, j=1,size)], wp) * 0.001_wp
            
            call fig%initialize(600, 400)
            call fig%hist(data, bins=20)
            call fig%set_title('Memory Test ' // char(48 + i))
            call fig%savefig('build/stress_memory_' // char(48 + i) // '.png')
            
            deallocate(data)
        end do
        
        write(*,*) '  ✓ Sequential memory usage patterns handled'
        
        ! Test overlapping histograms
        allocate(data(5000))
        data = real([(i, i=1,5000)], wp) * 0.01_wp
        
        call fig%initialize(800, 600)
        call fig%hist(data(1:2000), bins=30, label='First Half')
        call fig%hist(data(2500:4500), bins=30, label='Second Half')
        call fig%legend()
        call fig%set_title('Overlapping Histograms')
        call fig%savefig('build/stress_overlapping.png')
        write(*,*) '  ✓ Overlapping histograms handled'
        
        deallocate(data)
        
    end subroutine test_memory_usage

end program test_histogram_stress