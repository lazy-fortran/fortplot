title: Subplot Demo
---

# Subplot Demo

Demonstration of subplot functionality using the stateful API
    !! 
    !! This example demonstrates:
    !! - 2x2 subplot grid layout with sin, cos, damped oscillation, and quadratic functions
    !! - 1x3 subplot horizontal layout with linear, quadratic, and cubic functions
    !! - Independent titles and labels per subplot
    !! - Different mathematical functions visualized in each subplot
    !! 
    !! API Usage:
    !! - subplot(rows, cols, index) to create subplot grid and switch to specific subplot
    !! - plot(), title(), xlabel(), ylabel() work within current subplot context
    !! - savefig() renders entire subplot grid to output file

## Source Code

```fortran
program subplot_demo
    !! Demonstration of subplot functionality using the stateful API
    !! 
    !! This example demonstrates:
    !! - 2x2 subplot grid layout with sin, cos, damped oscillation, and quadratic functions
    !! - 1x3 subplot horizontal layout with linear, quadratic, and cubic functions
    !! - Independent titles and labels per subplot
    !! - Different mathematical functions visualized in each subplot
    !! 
    !! API Usage:
    !! - subplot(rows, cols, index) to create subplot grid and switch to specific subplot
    !! - plot(), title(), xlabel(), ylabel() work within current subplot context
    !! - savefig() renders entire subplot grid to output file

    use iso_fortran_env, only: real64, wp => real64
    use fortplot, only: figure, plot, xlabel, ylabel, title, savefig, show, subplot
    implicit none

    ! Named constants for data generation
    integer, parameter :: FINE_POINTS = 100
    integer, parameter :: COARSE_POINTS = 50
    real(wp), parameter :: FINE_STEP = 0.1_wp
    real(wp), parameter :: COARSE_STEP = 0.2_wp
    real(wp), parameter :: DAMPING_FACTOR = 0.5_wp
    real(wp), parameter :: OSCILLATION_FREQ = 2.0_wp
    real(wp), parameter :: QUADRATIC_SCALE = 50.0_wp
    
    ! Named constants for figure dimensions
    integer, parameter :: FIG_2X2_WIDTH = 800
    integer, parameter :: FIG_2X2_HEIGHT = 600
    integer, parameter :: FIG_1X3_WIDTH = 900
    integer, parameter :: FIG_1X3_HEIGHT = 300

    real(wp), allocatable :: x(:), y(:)
    real(wp), allocatable :: x2(:), y2(:)
    integer :: i

    ! Generate test data
    allocate(x(FINE_POINTS), y(FINE_POINTS))
    do i = 1, FINE_POINTS
        x(i) = real(i - 1, wp) * FINE_STEP
        y(i) = sin(x(i))
    end do

    allocate(x2(COARSE_POINTS), y2(COARSE_POINTS))
    do i = 1, COARSE_POINTS
        x2(i) = real(i - 1, wp) * COARSE_STEP
        y2(i) = exp(-x2(i) * DAMPING_FACTOR) * cos(OSCILLATION_FREQ * x2(i))
    end do

    ! Example 1: 2x2 subplot grid
    call figure(figsize=[real(FIG_2X2_WIDTH, wp), real(FIG_2X2_HEIGHT, wp)])
    
    print *, 'Creating 2x2 subplot example...'
    
    ! Top-left subplot
    call subplot(2, 2, 1)
    call plot(x, sin(x))
    call title('Sine Wave')
    call xlabel('x')
    call ylabel('sin(x)')
    
    ! Top-right subplot
    call subplot(2, 2, 2)
    call plot(x, cos(x))
    call title('Cosine Wave')
    call xlabel('x')
    call ylabel('cos(x)')
    
    ! Bottom-left subplot
    call subplot(2, 2, 3)
    call plot(x2, y2)
    call title('Damped Oscillation')
    call xlabel('x')
    call ylabel('y')
    
    ! Bottom-right subplot
    call subplot(2, 2, 4)
    call plot(x2, x2**2/QUADRATIC_SCALE)
    call title('Quadratic Function')
    call xlabel('x')
    call ylabel('x²/50')
    
    call savefig('output/example/fortran/subplot_demo/subplot_2x2_demo.png')

    ! Example 2: 1x3 subplot layout
    call figure(figsize=[real(FIG_1X3_WIDTH, wp), real(FIG_1X3_HEIGHT, wp)])
    
    print *, 'Creating 1x3 subplot example...'
    
    ! Left subplot
    call subplot(1, 3, 1)
    call plot(x2, x2)
    call title('Linear')
    call xlabel('x')
    call ylabel('x')
    
    ! Center subplot
    call subplot(1, 3, 2)
    call plot(x2, x2**2)
    call title('Quadratic')
    call xlabel('x')
    call ylabel('x²')
    
    ! Right subplot
    call subplot(1, 3, 3)
    call plot(x2, x2**3)
    call title('Cubic')
    call xlabel('x')
    call ylabel('x³')
    
    call savefig('output/example/fortran/subplot_demo/subplot_1x3_demo.png')

    print *, ''
    print *, 'Subplot demo complete!'
    print *, 'Output files:'
    print *, '  output/example/fortran/subplot_demo/subplot_2x2_demo.png'
    print *, '  output/example/fortran/subplot_demo/subplot_1x3_demo.png'
    print *, ''
    print *, 'Features demonstrated:'
    print *, '- 2x2 and 1x3 subplot grid layouts'
    print *, '- Independent subplot titles and axis labels'
    print *, '- Different mathematical functions in each subplot'
    print *, '- Proper subplot positioning and layout'

end program subplot_demo
```

## Output


---

Source: [example/fortran/subplot_demo/subplot_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/subplot_demo/subplot_demo.f90)
