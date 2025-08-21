# Subplot Demo

Demonstration of subplot functionality using the stateful API.

## Usage

```bash
make example ARGS="subplot_demo"
```

## Features Demonstrated

- **2x2 subplot grid**: Four different mathematical functions arranged in a 2×2 grid
- **1x3 subplot layout**: Three polynomial functions arranged horizontally  
- **Independent subplot titles and labels**: Each subplot has its own title and axis labels
- **Multiple plot types**: Different mathematical functions visualized in each subplot

## API Usage

```fortran
use fortplot, only: figure, plot, xlabel, ylabel, title, savefig, subplot

! Create figure and subplot grid
call figure(800, 600)
call subplot(rows, cols, index)  ! Create grid and switch to subplot

! Add content to current subplot
call plot(x, y)
call title('Subplot Title')
call xlabel('X Label')
call ylabel('Y Label')

! Save entire subplot grid
call savefig('output.png')
```

## Expected Outputs

- `output/example/fortran/subplot_demo/subplot_2x2_demo.png`: 2×2 grid showing sine, cosine, damped oscillation, and quadratic functions
- `output/example/fortran/subplot_demo/subplot_1x3_demo.png`: 1×3 layout showing linear, quadratic, and cubic functions

## Mathematical Functions

### 2x2 Grid
1. **Top-left**: sin(x) 
2. **Top-right**: cos(x)
3. **Bottom-left**: Damped oscillation e^(-x/2) * cos(2x)
4. **Bottom-right**: Quadratic function x²/50

### 1x3 Layout  
1. **Left**: Linear function x
2. **Center**: Quadratic function x²
3. **Right**: Cubic function x³