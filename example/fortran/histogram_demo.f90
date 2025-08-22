program histogram_demo
    !! Example demonstrating histogram plotting capabilities
    !! Shows basic histogram, custom bins, and density normalization
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    integer, parameter :: n_data = 1000
    real(wp) :: data(n_data), normal_data(n_data)
    type(figure_t) :: fig
    integer :: i
    real(wp) :: pi = 3.14159265359_wp
    
    ! Generate random-like data (simple distribution)
    do i = 1, n_data
        data(i) = real(i, wp) / 100.0_wp + sin(real(i, wp) * 0.01_wp) * 5.0_wp
    end do
    
    ! Generate normal-like data using Box-Muller transform approximation
    do i = 1, n_data
        normal_data(i) = cos(2.0_wp * pi * real(i, wp) / real(n_data, wp)) * &
                        sqrt(-2.0_wp * log(max(real(mod(i, 1000), wp) / 1000.0_wp, 0.001_wp)))
    end do
    
    ! Basic histogram
    call initialize(800, 600)
    call hist(data)
    call set_title('Basic Histogram Example')
    call set_xlabel('Value')
    call set_ylabel('Frequency')
    call savefig('output/example/fortran/histogram_demo/histogram_basic.png')
    write(*,*) 'Created histogram_basic.png'
    
    ! Custom bins histogram  
    call initialize(800, 600)
    call hist(data, bins=20)
    call set_title('Histogram with 20 Bins')
    call set_xlabel('Value')
    call set_ylabel('Frequency')
    call savefig('output/example/fortran/histogram_demo/histogram_custom_bins.png')
    write(*,*) 'Created histogram_custom_bins.png'
    
    ! Density histogram
    call initialize(800, 600)
    call hist(normal_data, bins=15, density=.true.)
    call set_title('Normalized Histogram (Density)')
    call set_xlabel('Value')
    call set_ylabel('Probability Density')
    call savefig('output/example/fortran/histogram_demo/histogram_density.png')
    write(*,*) 'Created histogram_density.png'
    
    ! Multiple histograms with labels
    call initialize(800, 600)
    call hist(data(1:500), bins=15, label='Dataset 1', color=[0.0_wp, 0.447_wp, 0.698_wp])
    call hist(normal_data(1:500), bins=15, label='Dataset 2', color=[0.835_wp, 0.369_wp, 0.0_wp])
    call legend()
    call set_title('Multiple Histograms')
    call set_xlabel('Value')
    call set_ylabel('Frequency')
    call savefig('output/example/fortran/histogram_demo/histogram_multiple.png')
    write(*,*) 'Created histogram_multiple.png'
    
    write(*,*) 'Histogram demonstration completed!'
    
end program histogram_demo