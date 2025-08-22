title: Scatter Plot Tutorial
---

# Scatter Plot Tutorial: From Basic to Advanced

## Step 1: Your First Scatter Plot

```fortran
program first_scatter
    use fortplot
    implicit none
    
    ! Sample data: city population vs average temperature
    real(wp) :: temperature(5) = [15.2_wp, 22.1_wp, 8.3_wp, 28.7_wp, 18.9_wp]
    real(wp) :: population(5) = [2.1_wp, 5.3_wp, 1.2_wp, 8.7_wp, 3.4_wp]  ! millions
    
    call figure()
    call scatter(temperature, population, label='Cities')
    call title('Population vs Average Temperature')
    call xlabel('Average Temperature (°C)')
    call ylabel('Population (millions)')
    call savefig('tutorial_step1.png')
end program
```

**Key Points:**
- Basic `scatter(x, y)` syntax
- Always include axis labels and title
- Use `label` parameter for legends

## Step 2: Adding Size Information (Bubble Chart)

```fortran
program bubble_chart
    use fortplot
    implicit none
    
    real(wp) :: temperature(5) = [15.2_wp, 22.1_wp, 8.3_wp, 28.7_wp, 18.9_wp]
    real(wp) :: population(5) = [2.1_wp, 5.3_wp, 1.2_wp, 8.7_wp, 3.4_wp]
    real(wp) :: city_area(5) = [100.0_wp, 300.0_wp, 50.0_wp, 500.0_wp, 200.0_wp]  ! km²
    
    ! Size mapping: larger areas = larger markers
    real(wp) :: marker_sizes(5)
    marker_sizes = city_area * 0.2_wp  ! Scale for visibility
    
    call figure(600, 400)
    call scatter(temperature, population, s=marker_sizes, &
                 marker='circle', label='Cities by Area')
    call title('Population vs Temperature (bubble size = area)')
    call xlabel('Average Temperature (°C)')
    call ylabel('Population (millions)')
    call legend()
    call savefig('tutorial_step2.png')
end program
```

**Key Points:**
- `s` parameter for size mapping
- Scale size values for visual clarity
- Bubble size represents third dimension of data

## Step 3: Adding Color Information

```fortran
program color_scatter
    use fortplot
    implicit none
    
    real(wp) :: temperature(5) = [15.2_wp, 22.1_wp, 8.3_wp, 28.7_wp, 18.9_wp]
    real(wp) :: population(5) = [2.1_wp, 5.3_wp, 1.2_wp, 8.7_wp, 3.4_wp]
    real(wp) :: happiness_index(5) = [7.2_wp, 8.1_wp, 6.8_wp, 7.9_wp, 8.3_wp]  ! 1-10 scale
    
    call figure(800, 600)
    call scatter(temperature, population, c=happiness_index, &
                 colormap='viridis', marker='diamond', &
                 alpha=0.8_wp, label='Cities by Happiness')
    call title('Population vs Temperature (color = happiness index)')
    call xlabel('Average Temperature (°C)')
    call ylabel('Population (millions)')
    call legend()
    call savefig('tutorial_step3.png')
end program
```

**Key Points:**
- `c` parameter for color mapping
- `colormap` chooses color scheme
- `alpha` adds transparency
- Colorbar automatically generated

## Step 4: Combining Size and Color (Multi-dimensional Visualization)

```fortran
program multidimensional_scatter
    use fortplot
    implicit none
    
    real(wp) :: temperature(8) = [15.2_wp, 22.1_wp, 8.3_wp, 28.7_wp, &
                                  18.9_wp, 12.4_wp, 31.2_wp, 5.8_wp]
    real(wp) :: population(8) = [2.1_wp, 5.3_wp, 1.2_wp, 8.7_wp, &
                                 3.4_wp, 1.8_wp, 6.2_wp, 0.9_wp]
    real(wp) :: city_area(8) = [100.0_wp, 300.0_wp, 50.0_wp, 500.0_wp, &
                                200.0_wp, 80.0_wp, 400.0_wp, 30.0_wp]
    real(wp) :: gdp_per_capita(8) = [45000.0_wp, 55000.0_wp, 38000.0_wp, 62000.0_wp, &
                                     48000.0_wp, 41000.0_wp, 58000.0_wp, 35000.0_wp]
    
    ! Normalize for visualization
    real(wp) :: marker_sizes(8), gdp_normalized(8)
    marker_sizes = city_area * 0.15_wp + 10.0_wp  ! Minimum size + scaling
    gdp_normalized = (gdp_per_capita - 30000.0_wp) / 35000.0_wp  ! 0-1 range
    
    type(figure_t) :: fig
    call fig%initialize(900, 700)
    call fig%scatter(temperature, population, s=marker_sizes, c=gdp_normalized, &
                     colormap='plasma', marker='circle', alpha=0.7_wp, &
                     label='Cities (size=area, color=GDP/capita)')
    
    call fig%set_title('Multi-dimensional City Analysis')
    call fig%set_xlabel('Average Temperature (°C)')
    call fig%set_ylabel('Population (millions)')
    call fig%legend()
    call fig%savefig('tutorial_step4.png')
end program
```

**Key Points:**
- Combine `s` and `c` for 4D visualization
- Normalize color data to 0-1 range
- Choose marker size range for readability
- Object-oriented API for complex plots

## Step 5: Marker Customization and Styling

```fortran
program marker_styling
    use fortplot
    implicit none
    
    real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: sizes(3) = [50.0_wp, 60.0_wp, 55.0_wp]
    
    type(figure_t) :: fig
    call fig%initialize(600, 500)
    
    ! Custom face and edge colors
    call fig%scatter([x(1)], [y(1)], s=[sizes(1)], marker='star', &
                     facecolor=[1.0_wp, 0.8_wp, 0.0_wp], &  ! Golden
                     edgecolor=[0.0_wp, 0.0_wp, 0.0_wp], &  ! Black outline
                     linewidth=2.0_wp, label='Gold Star')
    
    call fig%scatter([x(2)], [y(2)], s=[sizes(2)], marker='hexagon', &
                     facecolor=[0.2_wp, 0.8_wp, 0.2_wp], &  ! Green
                     edgecolor=[0.1_wp, 0.4_wp, 0.1_wp], &  ! Dark green outline
                     linewidth=1.5_wp, label='Green Hex')
    
    call fig%scatter([x(3)], [y(3)], s=[sizes(3)], marker='pentagon', &
                     facecolor=[0.8_wp, 0.2_wp, 0.8_wp], &  ! Magenta
                     alpha=0.6_wp, label='Transparent Pentagon')
    
    call fig%set_title('Custom Marker Styling')
    call fig%legend()
    call fig%savefig('tutorial_step5.png')
end program
```

**Key Points:**
- `facecolor` and `edgecolor` for custom colors
- `linewidth` controls edge thickness
- Mix opaque and transparent markers
- Different marker shapes for different categories

## Step 6: Scientific Data Example

```fortran
program scientific_example
    use fortplot
    implicit none
    
    integer, parameter :: n_experiments = 50
    real(wp) :: pressure(n_experiments), yield(n_experiments), &
                temperature(n_experiments), reaction_time(n_experiments)
    integer :: i
    
    ! Generate realistic experimental data
    do i = 1, n_experiments
        ! Pressure: 1-10 bar
        pressure(i) = 1.0_wp + 9.0_wp * real(i-1, wp) / real(n_experiments-1, wp)
        
        ! Temperature: 300-400 K (with some variation)
        temperature(i) = 320.0_wp + 60.0_wp * sin(real(i, wp) * 0.2_wp) + &
                        10.0_wp * (real(i, wp) / real(n_experiments, wp) - 0.5_wp)
        
        ! Reaction time depends on pressure and temperature
        reaction_time(i) = 10.0_wp + 20.0_wp / pressure(i) + &
                          0.1_wp * (temperature(i) - 350.0_wp)
        
        ! Yield depends on all factors with some noise
        yield(i) = 60.0_wp + 20.0_wp * pressure(i) / 10.0_wp - &
                  0.1_wp * abs(temperature(i) - 360.0_wp) + &
                  real(mod(i * 7, 21) - 10, wp)
    end do
    
    type(figure_t) :: fig
    call fig%initialize(900, 600)
    call fig%scatter(pressure, yield, s=reaction_time, c=temperature, &
                     colormap='coolwarm', marker='circle', alpha=0.7_wp, &
                     label='Chemical Experiments')
    
    call fig%set_title('Chemical Reaction Analysis')
    call fig%set_xlabel('Pressure (bar)')
    call fig%set_ylabel('Product Yield (%)')
    call fig%legend()
    call fig%savefig('tutorial_scientific.png')
end program
```

**Key Points:**
- Realistic scientific data generation
- Multiple correlated variables
- Use `coolwarm` colormap for temperature data
- Size represents reaction time, color represents temperature

## Step 7: Multiple Datasets and Comparison

```fortran
program multiple_datasets
    use fortplot
    implicit none
    
    ! Dataset 1: Material A
    real(wp) :: stress_a(4) = [100.0_wp, 200.0_wp, 300.0_wp, 400.0_wp]
    real(wp) :: strain_a(4) = [0.001_wp, 0.003_wp, 0.006_wp, 0.012_wp]
    real(wp) :: temp_a(4) = [25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp]  ! Room temp
    
    ! Dataset 2: Material B
    real(wp) :: stress_b(4) = [120.0_wp, 180.0_wp, 280.0_wp, 350.0_wp]
    real(wp) :: strain_b(4) = [0.0008_wp, 0.002_wp, 0.005_wp, 0.009_wp]
    real(wp) :: temp_b(4) = [100.0_wp, 100.0_wp, 100.0_wp, 100.0_wp]  ! Elevated temp
    
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    
    ! Material A at room temperature
    call fig%scatter(strain_a, stress_a, c=temp_a, &
                     colormap='coolwarm', marker='circle', &
                     s=[30.0_wp, 30.0_wp, 30.0_wp, 30.0_wp], &
                     label='Material A (25°C)')
    
    ! Material B at elevated temperature
    call fig%scatter(strain_b, stress_b, c=temp_b, &
                     colormap='coolwarm', marker='square', &
                     s=[35.0_wp, 35.0_wp, 35.0_wp, 35.0_wp], &
                     label='Material B (100°C)')
    
    call fig%set_title('Material Comparison: Stress vs Strain')
    call fig%set_xlabel('Strain')
    call fig%set_ylabel('Stress (MPa)')
    call fig%legend()
    call fig%savefig('tutorial_comparison.png')
end program
```

**Key Points:**
- Multiple scatter series on same plot
- Different markers distinguish datasets
- Shared colormap for temperature comparison
- Clear labeling for interpretation

## Next Steps

1. **Explore Advanced Features:**
   - Backend-specific optimizations
   - Animation with changing scatter data
   - Integration with contour plots

2. **Performance Optimization:**
   - Test with 10,000+ point datasets
   - Memory-efficient data handling
   - Rendering speed optimization

3. **Scientific Applications:**
   - Error bar integration
   - Statistical overlay plots
   - Publication-quality formatting

4. **Backend Exploration:**
   - PNG for web/presentations
   - PDF for publications
   - ASCII for terminal debugging

Try modifying these examples with your own data to master scatter plot visualization!