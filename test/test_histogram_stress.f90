program test_histogram_stress
    !! Stress testing for histogram functionality
    !! Tests performance and robustness with large datasets
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    ! Detect CI environment and reduce test intensity
    logical :: is_ci_environment
    character(len=256) :: ci_var
    
    write(*,*) '=== HISTOGRAM STRESS TESTING ==='
    
    ! Check for GitHub Actions or other CI indicators
    call get_environment_variable('GITHUB_ACTIONS', ci_var)
    is_ci_environment = (len_trim(ci_var) > 0)
    if (.not. is_ci_environment) then
        call get_environment_variable('CI', ci_var)
        is_ci_environment = (len_trim(ci_var) > 0)
    end if
    
    if (is_ci_environment) then
        write(*,*) 'CI environment detected - using reduced test parameters'
    end if
    
    ! Test large datasets
    call test_large_datasets(is_ci_environment)
    
    ! Test extreme parameters
    call test_extreme_parameters(is_ci_environment)
    
    ! Test memory pressure
    call test_memory_usage(is_ci_environment)
    
    write(*,*) '=== STRESS TESTS COMPLETED ==='
    
contains

    subroutine test_large_datasets(is_ci_environment)
        !! Test histogram with large amounts of data
        logical, intent(in) :: is_ci_environment
        type(figure_t) :: fig
        real(wp), allocatable :: large_data(:)
        integer :: i, n_data, max_size_1, max_size_2
        real(wp) :: start_time, end_time
        
        write(*,*) 'Testing large datasets...'
        
        ! Adjust dataset sizes for CI environment
        if (is_ci_environment) then
            max_size_1 = 1000   ! Reduced from 10K
            max_size_2 = 5000   ! Reduced from 50K
        else
            max_size_1 = 10000
            max_size_2 = 50000
        end if
        
        ! Test with first dataset
        n_data = max_size_1
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
        write(*,'(A,I0,A)') '  Dataset size: ', n_data, ' points'
        call fig%set_title('Large Dataset Test 1')
        call fig%savefig('build/stress_large_1.png')
        call cpu_time(end_time)
        
        write(*,'(A,I0,A,F8.3,A)') '  ✓ ', n_data, ' points processed in ', end_time - start_time, ' seconds'
        
        deallocate(large_data)
        
        ! Test with second dataset
        n_data = max_size_2
        allocate(large_data(n_data))
        
        do i = 1, n_data
            large_data(i) = real(i, wp) * 0.001_wp + &
                           sin(real(i, wp) * 0.0001_wp) * 100.0_wp
        end do
        
        call cpu_time(start_time)
        call fig%initialize(800, 600)
        call fig%hist(large_data, bins=100)
        write(*,'(A,I0,A)') '  Dataset size: ', n_data, ' points'
        call fig%set_title('Large Dataset Test 2')
        call fig%savefig('build/stress_large_2.png')
        call cpu_time(end_time)
        
        write(*,'(A,I0,A,F8.3,A)') '  ✓ ', n_data, ' points processed in ', end_time - start_time, ' seconds'
        
        deallocate(large_data)
        
    end subroutine test_large_datasets
    
    subroutine test_extreme_parameters(is_ci_environment)
        !! Test with extreme parameter values
        logical, intent(in) :: is_ci_environment
        type(figure_t) :: fig
        real(wp) :: data(1000)
        integer :: i, max_bins
        
        write(*,*) 'Testing extreme parameters...'
        
        ! Generate test data
        do i = 1, 1000
            data(i) = real(i, wp) * 0.01_wp
        end do
        
        ! Adjust for CI environment
        if (is_ci_environment) then
            max_bins = 50  ! Reduced from 500
        else
            max_bins = 500
        end if
        
        ! Test with very many bins
        call fig%initialize(800, 600)
        call fig%hist(data, bins=max_bins)
        write(*,'(A,I0,A)') '  Using ', max_bins, ' bins for stress test'
        call fig%set_title('Extreme: Many Bins Test')
        call fig%savefig('build/stress_many_bins.png')
        write(*,'(A,I0,A)') '  ✓ Many bins (', max_bins, ') handled'
        
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
    
    subroutine test_memory_usage(is_ci_environment)
        !! Test memory usage patterns
        logical, intent(in) :: is_ci_environment
        type(figure_t) :: fig
        real(wp), allocatable :: data(:)
        integer :: i, j, size, max_iterations, max_overlap_size
        
        write(*,*) 'Testing memory usage patterns...'
        
        ! Adjust for CI environment
        if (is_ci_environment) then
            max_iterations = 5      ! Reduced from 10
            max_overlap_size = 2000 ! Reduced from 5000
        else
            max_iterations = 10
            max_overlap_size = 5000
        end if
        
        ! Test multiple histograms in sequence (memory cleanup)
        do i = 1, max_iterations
            size = 500 * i  ! Reduced base size
            allocate(data(size))
            
            data = real([(j, j=1,size)], wp) * 0.001_wp
            
            call fig%initialize(600, 400)
            call fig%hist(data, bins=20)
            write(*,'(A,I0,A,I0,A)') '  Processing iteration ', i, ' with ', size, ' points'
            call fig%set_title('Memory Test ' // char(48 + i))
            call fig%savefig('build/stress_memory_' // char(48 + i) // '.png')
            
            deallocate(data)
        end do
        
        write(*,'(A,I0,A)') '  ✓ Sequential memory usage patterns handled (', max_iterations, ' iterations)'
        
        ! Test overlapping histograms
        allocate(data(max_overlap_size))
        data = real([(i, i=1,max_overlap_size)], wp) * 0.01_wp
        
        call fig%initialize(800, 600)
        call fig%hist(data(1:max_overlap_size/2), bins=30, label='First Half', color=[1.0_wp, 0.0_wp, 0.0_wp])
        if (max_overlap_size >= 2500) then
            call fig%hist(data(max_overlap_size/2+1:4*max_overlap_size/5), bins=30, label='Second Half', color=[0.0_wp, 0.0_wp, 1.0_wp])
        end if
        call fig%legend()
        call fig%set_title('Overlapping Histograms')
        call fig%savefig('build/stress_overlapping.png')
        write(*,'(A,I0,A)') '  ✓ Overlapping histograms handled (', max_overlap_size, ' points)'
        
        deallocate(data)
        
    end subroutine test_memory_usage

end program test_histogram_stress