title: Advanced Scatter Plot Usage
---

# Advanced Scatter Plot Usage

## Performance Optimization

### Large Dataset Handling (10,000+ Points)

```fortran
program large_dataset_optimization
    use fortplot
    implicit none
    
    integer, parameter :: n_points = 50000
    real(wp) :: x(n_points), y(n_points), sizes(n_points), colors(n_points)
    integer :: i
    
    ! Generate large dataset efficiently
    do concurrent (i = 1:n_points)
        x(i) = real(i, wp) / real(n_points, wp) * 100.0_wp
        y(i) = sin(x(i) * 0.1_wp) + 0.01_wp * real(i, wp) / real(n_points, wp)
        sizes(i) = 3.0_wp + 15.0_wp * abs(sin(x(i) * 0.05_wp))
        colors(i) = (y(i) + 1.1_wp) / 2.2_wp
    end do
    
    ! Performance settings for large datasets
    type(figure_t) :: fig
    call fig%initialize(1200, 800)
    call fig%scatter(x, y, s=sizes, c=colors, &
                     colormap='viridis', alpha=0.5_wp, &  ! Lower alpha for overlapping
                     marker='circle', label='50K Points')
    
    call fig%set_title('Large Dataset Performance Test')
    call fig%savefig('large_performance.png')  ! Target: <200ms
end program
```

**Performance Tips:**
- Use `do concurrent` for data generation
- Lower alpha values (0.3-0.6) for dense plots
- Circle markers render fastest
- PNG backend optimized for large datasets

### Memory Management

```fortran
program memory_efficient_scatter
    use fortplot
    implicit none
    
    integer, parameter :: batch_size = 5000
    integer, parameter :: n_batches = 10
    real(wp) :: x_batch(batch_size), y_batch(batch_size)
    real(wp) :: sizes_batch(batch_size), colors_batch(batch_size)
    integer :: batch, i
    
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    
    ! Process data in batches to minimize memory usage
    do batch = 1, n_batches
        ! Generate batch data
        do i = 1, batch_size
            x_batch(i) = real((batch-1)*batch_size + i, wp) * 0.01_wp
            y_batch(i) = sin(x_batch(i)) + 0.1_wp * real(batch, wp)
            sizes_batch(i) = 10.0_wp + 5.0_wp * real(batch, wp)
            colors_batch(i) = real(batch, wp) / real(n_batches, wp)
        end do
        
        ! Add batch to plot
        call fig%scatter(x_batch, y_batch, s=sizes_batch, c=colors_batch, &
                         colormap='plasma', alpha=0.7_wp, &
                         label='Batch ' // char(48 + batch))
    end do
    
    call fig%set_title('Memory-Efficient Batch Processing')
    call fig%savefig('batch_processing.png')
end program
```

## Backend-Specific Optimization

### PNG Backend (Raster Graphics)
```fortran
! Optimized for web display and presentations
call fig%scatter(x, y, s=sizes, c=colors, &
                 colormap='viridis', alpha=0.7_wp, &
                 marker='circle')  ! Circles render fastest
call fig%savefig('web_optimized.png')
```

**PNG Characteristics:**
- Fastest rendering for large datasets
- Fixed resolution - choose size carefully
- Best for web display and presentations
- Supports transparency with alpha channel

### PDF Backend (Vector Graphics)
```fortran
! Optimized for publications and printing
call fig%scatter(x, y, s=sizes, c=colors, &
                 colormap='viridis', alpha=0.8_wp, &
                 marker='diamond')  ! Complex shapes scale perfectly
call fig%savefig('publication_quality.pdf')
```

**PDF Characteristics:**
- Perfect scaling at any zoom level
- Larger file sizes for dense datasets
- Best for publications and printing
- True vector graphics with crisp edges

### ASCII Backend (Terminal Display)
```fortran
! Character-based representation
call fig%initialize(80, 24)  ! Terminal dimensions
call fig%scatter(x, y, s=sizes, c=colors, &
                 colormap='grayscale', &
                 marker='circle')  ! Best ASCII representation
call fig%savefig('terminal_display.txt')
```

**ASCII Characteristics:**
- Character mapping: `.` → `o` → `O` → `@` for size
- Grayscale intensity for color mapping
- Perfect for debugging and terminal workflows
- Instant feedback without graphics viewer

## Advanced Colormap Usage

### Custom Color Ranges
```fortran
program custom_colormaps
    use fortplot
    implicit none
    
    real(wp) :: temperature(20), pressure(20), efficiency(20)
    integer :: i
    
    ! Scientific data with specific ranges
    do i = 1, 20
        temperature(i) = 273.0_wp + 200.0_wp * real(i-1, wp) / 19.0_wp
        pressure(i) = 1.0_wp + 9.0_wp * real(i-1, wp) / 19.0_wp
        efficiency(i) = 0.6_wp + 0.3_wp * sin(real(i, wp) * 0.3_wp)
    end do
    
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    
    ! Map efficiency to colors with specific interpretation
    call fig%scatter(temperature, pressure, c=efficiency, &
                     colormap='coolwarm', &  ! Blue = low, Red = high
                     marker='hexagon', alpha=0.8_wp, &
                     label='Thermal Efficiency')
    
    call fig%set_title('Engine Performance Map')
    call fig%set_xlabel('Temperature (K)')
    call fig%set_ylabel('Pressure (bar)')
    call fig%legend()
    call fig%savefig('engine_performance.pdf')
end program
```

### Diverging Colormaps for Scientific Data
```fortran
! Use diverging colormaps for data with meaningful center point
real(wp) :: correlation_data(30)
correlation_data = ... ! Values from -1.0 to +1.0

call fig%scatter(x, y, c=correlation_data, &
                 colormap='coolwarm', &  ! Blue = negative, Red = positive
                 marker='circle', alpha=0.8_wp)
```

## Integration with Other Plot Types

### Scatter + Line Plot Combination
```fortran
program combined_plots
    use fortplot
    implicit none
    
    real(wp) :: x_theory(100), y_theory(100)
    real(wp) :: x_data(20), y_data(20), error_bars(20)
    integer :: i
    
    ! Generate theoretical curve
    do i = 1, 100
        x_theory(i) = real(i-1, wp) * 0.1_wp
        y_theory(i) = exp(-x_theory(i)) * cos(x_theory(i) * 2.0_wp)
    end do
    
    ! Generate experimental data with errors
    do i = 1, 20
        x_data(i) = real(i-1, wp) * 0.5_wp
        y_data(i) = exp(-x_data(i)) * cos(x_data(i) * 2.0_wp) + &
                    0.1_wp * (real(i, wp) / 10.0_wp - 1.0_wp)
        error_bars(i) = 0.05_wp + 0.03_wp * abs(y_data(i))
    end do
    
    type(figure_t) :: fig
    call fig%initialize(900, 600)
    
    ! Plot theoretical curve first
    call fig%plot(x_theory, y_theory, linestyle='-', &
                  color=[0.2_wp, 0.2_wp, 0.8_wp], linewidth=2.0_wp, &
                  label='Theory')
    
    ! Add experimental data as scatter with error information
    call fig%scatter(x_data, y_data, s=error_bars*1000, &  ! Scale errors to size
                     c=error_bars, colormap='reds', &
                     marker='circle', alpha=0.7_wp, &
                     edgecolor=[0.0_wp, 0.0_wp, 0.0_wp], linewidth=1.0_wp, &
                     label='Experimental (size = error)')
    
    call fig%set_title('Theory vs Experiment')
    call fig%set_xlabel('Time (s)')
    call fig%set_ylabel('Amplitude')
    call fig%legend()
    call fig%savefig('theory_vs_experiment.png')
end program
```

### Scatter + Contour Background
```fortran
program scatter_with_background
    use fortplot
    implicit none
    
    ! Background field data
    real(wp) :: x_grid(50), y_grid(50), field_data(50, 50)
    ! Measurement points
    real(wp) :: x_points(15), y_points(15), measurements(15)
    
    ! ... generate grid and point data ...
    
    type(figure_t) :: fig
    call fig%initialize(900, 700)
    
    ! First: background field as contour
    call fig%contour_filled(x_grid, y_grid, field_data, &
                           colormap='viridis', alpha=0.3_wp)
    
    ! Second: measurement points as scatter
    call fig%scatter(x_points, y_points, c=measurements, &
                     colormap='viridis', marker='circle', &
                     s=[50.0_wp], alpha=0.9_wp, &
                     edgecolor=[0.0_wp, 0.0_wp, 0.0_wp], linewidth=2.0_wp, &
                     label='Measurements')
    
    call fig%set_title('Field Measurements on Background')
    call fig%legend()
    call fig%savefig('field_measurements.png')
end program
```

## Error Handling and Robustness

### Data Validation
```fortran
program robust_scatter
    use fortplot
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    
    real(wp) :: raw_x(100), raw_y(100), raw_colors(100)
    real(wp), allocatable :: clean_x(:), clean_y(:), clean_colors(:)
    integer :: i, n_valid
    logical :: valid_mask(100)
    
    ! ... load raw data that may contain NaN/Inf ...
    
    ! Validate data before plotting
    do i = 1, 100
        valid_mask(i) = ieee_is_finite(raw_x(i)) .and. &
                       ieee_is_finite(raw_y(i)) .and. &
                       ieee_is_finite(raw_colors(i))
    end do
    
    n_valid = count(valid_mask)
    allocate(clean_x(n_valid), clean_y(n_valid), clean_colors(n_valid))
    
    ! Pack valid data
    clean_x = pack(raw_x, valid_mask)
    clean_y = pack(raw_y, valid_mask)
    clean_colors = pack(raw_colors, valid_mask)
    
    ! Plot with clean data
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    call fig%scatter(clean_x, clean_y, c=clean_colors, &
                     colormap='plasma', marker='circle', &
                     label='Filtered Data')
    
    write(*,*) 'Plotted', n_valid, 'out of', 100, 'data points'
    call fig%savefig('robust_scatter.png')
end program
```

### Size and Color Range Normalization
```fortran
subroutine normalize_data(raw_data, normalized_data, min_val, max_val)
    real(wp), intent(in) :: raw_data(:)
    real(wp), intent(out) :: normalized_data(size(raw_data))
    real(wp), intent(in) :: min_val, max_val
    
    real(wp) :: data_min, data_max, range_data
    
    data_min = minval(raw_data)
    data_max = maxval(raw_data)
    range_data = data_max - data_min
    
    if (range_data > 0.0_wp) then
        normalized_data = min_val + (max_val - min_val) * &
                         (raw_data - data_min) / range_data
    else
        normalized_data = 0.5_wp * (min_val + max_val)  ! All same value
    endif
end subroutine
```

## Animation with Scatter Plots

### Time-Series Scatter Animation
```fortran
program animated_scatter
    use fortplot
    implicit none
    
    integer, parameter :: n_frames = 50, n_points = 100
    real(wp) :: x(n_points), y(n_points), colors(n_points)
    real(wp) :: time
    integer :: frame, i
    
    do frame = 1, n_frames
        time = real(frame-1, wp) * 0.1_wp
        
        ! Update positions and colors over time
        do i = 1, n_points
            x(i) = real(i, wp) * 0.1_wp
            y(i) = sin(x(i) + time) * exp(-x(i) * 0.1_wp)
            colors(i) = sin(time + x(i)) * 0.5_wp + 0.5_wp
        end do
        
        type(figure_t) :: fig
        call fig%initialize(800, 600)
        call fig%scatter(x, y, c=colors, colormap='plasma', &
                         marker='circle', alpha=0.8_wp, &
                         label='Wave Propagation')
        
        call fig%set_title('Wave Propagation (t=' // &
                          trim(adjustl(real_to_string(time, 2))) // ')')
        call fig%set_xlim(0.0_wp, 10.0_wp)
        call fig%set_ylim(-1.5_wp, 1.5_wp)
        
        ! Save frame
        write(frame_name, '(A,I0.3,A)') 'frame_', frame, '.png'
        call fig%savefig(frame_name)
    end do
    
    ! Combine frames into animation with ffmpeg
    ! ffmpeg -r 10 -i frame_%03d.png -c:v libx264 wave_animation.mp4
end program
```

## Publication-Quality Formatting

### High-DPI Output
```fortran
! High resolution for publications
type(figure_t) :: fig
call fig%initialize(1600, 1200)  ! 2x normal resolution
call fig%scatter(x, y, s=sizes*2, c=colors, &  ! Scale markers for high-DPI
                 colormap='viridis', marker='circle', &
                 alpha=0.8_wp, linewidth=2.0_wp)
call fig%savefig('high_res_publication.png')
```

### Multiple Figure Layouts
```fortran
program publication_layout
    use fortplot
    implicit none
    
    ! Create multiple related plots
    type(figure_t) :: fig1, fig2
    
    ! Figure 1: Raw data
    call fig1%initialize(400, 400)
    call fig1%scatter(x_raw, y_raw, c=raw_quality, &
                      colormap='reds', marker='circle', &
                      alpha=0.6_wp, label='Raw Data')
    call fig1%set_title('(a) Raw Measurements')
    call fig1%savefig('publication_fig1a.pdf')
    
    ! Figure 2: Processed data
    call fig2%initialize(400, 400)
    call fig2%scatter(x_processed, y_processed, c=processed_quality, &
                      colormap='viridis', marker='diamond', &
                      alpha=0.8_wp, label='Processed Data')
    call fig2%set_title('(b) After Processing')
    call fig2%savefig('publication_fig1b.pdf')
end program
```

## Best Practices Summary

1. **Performance**: Use circles for large datasets, lower alpha for dense plots
2. **Memory**: Process large datasets in batches when memory-constrained
3. **Backend Choice**: PNG for web/presentation, PDF for publication, ASCII for debugging
4. **Color Selection**: Use perceptually uniform colormaps for scientific data
5. **Data Validation**: Always check for NaN/Inf values before plotting
6. **Size Normalization**: Scale marker sizes to appropriate visual range
7. **Integration**: Combine scatter with other plot types for comprehensive analysis
8. **Animation**: Use consistent scales and ranges across frames
9. **Publication**: Use high-DPI settings and vector formats for print quality