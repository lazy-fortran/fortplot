# ![fortplot logo](media/logo.jpg)

[![codecov](https://codecov.io/gh/lazy-fortran/fortplot/branch/main/graph/badge.svg)](https://codecov.io/gh/lazy-fortran/fortplot)
[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortplot/)

Fortran-native plotting inspired by Python's `matplotlib.pyplot` and https://github.com/jacobwilliams/pyplot-fortran . This library is under active development and API still subject to change. There are no external dependencies. Ironically, it has also Python interface installable via `pip` (see below) `fortplot.fortplot` that can be used as a drop-in replacement for `matplotlib.pyplot` for a limited set of features.

## Usage

```fortran
use fortplot
```

### Stateful API
```fortran
call figure()
call plot(x, y)
call title("Function Plot")
call xlabel("x")
call ylabel("y")
call xlim(0.0d0, 10.0d0)  ! Set x-axis limits
call ylim(-1.0d0, 1.0d0)   ! Set y-axis limits
call savefig("plot.png")
```

### Subplot Grids
```fortran
call figure(800, 600)
call subplot(2, 2, 1)  ! 2x2 grid, top-left
call plot(x, sin(x))
call title("Sine Wave")

call subplot(2, 2, 2)  ! Top-right
call plot(x, cos(x))
call title("Cosine Wave")
call savefig("subplots.png")
```

### Object-Oriented API
```fortran
type(figure_t) :: fig

call fig%initialize()
call fig%set_title("Function Plot")
call fig%set_xlabel("x")
call fig%set_ylabel("y")
call fig%add_plot(x, yf)
call fig%add_3d_plot(x, y, z, label="3D data")  ! 3D plotting
call fig%savefig("plot_oo.png")
```

### Advanced Examples

#### 3D plotting
```fortran
call figure(800, 600)
call add_3d_plot(x, y, z, label="3D curve")
call title("3D Line Plot")
call savefig("3d_plot.png")
```

#### Multiple plots with legend
```fortran
call figure(800, 600)
call plot(x, sin(x), label="sin(x)", linestyle="b-")
call plot(x, cos(x), label="cos(x)", linestyle="r--")
call plot(x, sin(2*x), label="sin(2x)", linestyle="g:")
call legend()
call savefig("trig_functions.pdf")
```

#### Unicode and Greek letters in scientific plots
```fortran
call figure(800, 600)
call title("Wave Functions: \psi(\omega t) = A e^{-\lambda t} sin(\omega t)")
call xlabel("Time \tau (normalized)")
call ylabel("Amplitude \Psi (V)")
call plot(t, damped_sine, label="\alpha decay")
call plot(t, damped_cosine, label="\beta oscillation")
call legend()
call savefig("unicode_demo.png")  ! Works in PNG, PDF, and ASCII
```

#### Enhanced scatter plots with size/color mapping
```fortran
! Basic scatter plot
call figure()
call scatter(x, y, label="Data Points")
call savefig("basic_scatter.png")

! Bubble chart with variable marker sizes  
type(figure_t) :: fig
call fig%initialize(600, 400)
call fig%add_scatter_2d(x, y, s=sizes, label='Bubble Chart')
call fig%savefig("bubble_chart.pdf")

! Color-mapped scatter with automatic colorbar
call fig%add_scatter_2d(x, y, c=values, colormap='viridis', &
                       show_colorbar=.true., label='Scientific Data')
call fig%savefig("scientific_scatter.png")
```

#### Surface plot with dimension validation
```fortran
use iso_fortran_env, only: wp => real64
use fortplot
implicit none

type(figure_t) :: fig
integer :: i, j
real(wp), dimension(21) :: x, y
real(wp), dimension(21,21) :: z  ! Must match: size(z,1)=size(x), size(z,2)=size(y)

! Create coordinate arrays
do i = 1, 21
    x(i) = (i-1) * 0.2_wp
    y(i) = (i-1) * 0.2_wp
end do

! Calculate surface values
do i = 1, 21
    do j = 1, 21  
        z(i,j) = x(i)**2 + y(j)**2  ! Paraboloid
    end do
end do

call fig%initialize(800, 600)
call fig%add_surface(x, y, z, label="Paraboloid")
call fig%savefig("surface.png")
```

#### Contour plot with colorbar
```fortran
call figure()
call contour_filled(x_grid, y_grid, z_data, colormap="viridis", show_colorbar=.true.)
call title("Temperature Distribution")
call xlabel("X Position")
call ylabel("Y Position")
call savefig("temperature.png")
```

#### Streamplot with arrows
```fortran
! Basic streamplot with default arrows
call figure()
call streamplot(x_grid, y_grid, u_field, v_field)
call savefig("flow_field.png")

! Streamplot with custom arrow size and style
call streamplot(x_grid, y_grid, u_field, v_field, arrowsize=1.5_real64, arrowstyle='<->')

! Streamlines without arrows
call streamplot(x_grid, y_grid, u_field, v_field, arrowsize=0.0_real64)
```

#### Error bars for scientific data
```fortran
use iso_fortran_env, only: wp => real64
use fortplot
implicit none

type(figure_t) :: fig
call fig%initialize(800, 600)
call fig%errorbar(x, y, yerr=measurement_errors, &
                  marker='o', capsize=5.0_wp, &
                  label='Experimental data')
call fig%errorbar(x, y_theory, label='Theory', linestyle='-')
call fig%legend()
call fig%savefig("scientific_plot.png")
```

#### Log scale plot
```fortran
call figure()
call plot(x, y)
call set_xscale("log")
call set_yscale("symlog", threshold=0.01d0)
call xlim(1.0d-3, 1.0d3)
call ylim(-100.0d0, 100.0d0)
call savefig("log_plot.pdf")
```

#### Text annotations with coordinate systems
```fortran
type(figure_t) :: fig
call fig%initialize(800, 600)
call fig%add_plot(x, y, label="Scientific Data")

! Basic text at data coordinates
call fig%text(2.5_wp, 1.0_wp, "Peak Region", coord_type=COORD_DATA)

! Arrow annotation pointing to data point
call fig%annotate("Maximum", xy=[x_max, y_max], &
                  xytext=[x_max+1.0_wp, y_max+0.3_wp], &
                  xy_coord_type=COORD_DATA, font_size=12.0_wp)

! Typography and positioning
call fig%text(0.5_wp, 0.95_wp, "TITLE", coord_type=COORD_FIGURE, &
              font_size=16.0_wp, alignment="center")
call fig%text(4.0_wp, 0.0_wp, "Vertical Label", rotation=90.0_wp, &
              coord_type=COORD_DATA, alignment="center")

! Mathematical expressions with Unicode
call fig%text(3.0_wp, 0.5_wp, "∂f/∂x = cos(x)e^{-x/4}", &
              coord_type=COORD_DATA, font_size=10.0_wp)

call fig%savefig("annotated_plot.png")
```

#### Animation example
```fortran
type(animation_t) :: anim
integer :: status
anim = FuncAnimation(update_func, frames=100, interval=50, fig=fig)
call anim%save("animation.mp4", fps=24, status=status)
if (status /= 0) then
    print *, "ERROR: Animation save failed. Check ffmpeg installation."
    print *, "Windows: choco install ffmpeg"
    print *, "Linux: sudo apt install ffmpeg"
end if
```

**Windows Support (Issue #189 Fixed)**: Binary pipe handling and path escaping now work correctly on Windows.

**Cross-Platform FFmpeg Setup**:
- **Windows**: `choco install ffmpeg` or download from ffmpeg.org
- **Linux**: `sudo apt install ffmpeg` (Ubuntu/Debian) or equivalent
- **macOS**: `brew install ffmpeg`

**File Validation**: Use `ffprobe -v error -show_format filename.mp4` to verify video integrity.

For more examples, see the [example directory](example) and run

```bash
make example
```

to build and run them.

## Setup

### Dependencies

**Required:**
- Modern Fortran compiler (gfortran-11 or newer)
- FPM (Fortran Package Manager) or CMake

**Optional:**
- `ffmpeg` - Required for saving animations in compressed video formats (MP4, AVI, MKV)
  - **Windows Support**: Issue #189 fixed - binary pipes and path escaping work correctly
  - **Cross-platform**: Install via `choco install ffmpeg` (Windows), `brew install ffmpeg` (macOS), or package manager (Linux)
  - **Setup Guide**: See [Windows FFmpeg Setup](doc/windows_ffmpeg_setup.md) for Windows-specific installation
  - **Validation**: FFprobe integration for format verification

### For fpm (Fortran Package Manager) projects

Add to your `fpm.toml`:
```toml
[[dependencies]]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

### For CMake projects

Add to your `CMakeLists.txt`:
```cmake
include(FetchContent)

FetchContent_Declare(
    fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG        main
)
FetchContent_MakeAvailable(fortplot)

target_link_libraries(your_target fortplot::fortplot)
```

### Security Features

**Executable Stack Protection**: fortplot prevents creation of executable stack segments which could be exploited for code injection attacks.

**Trampoline Detection**: The build system automatically detects and prevents nested functions that generate trampolines. All library code is trampoline-free for security compliance.

#### Verify Security Compliance

```bash
# Build with trampoline detection enabled
cmake -B build && cmake --build build
# Library builds successfully = trampoline-free core code

# Verify no executable stack segments
readelf -W -l build/libfortplot.a | grep -i stack
# Should return empty (no executable stack)

# Test trampoline detection (should fail on example with nested function)
fpm build --flag "-Werror=trampolines" 2>/dev/null || echo "Trampoline detection working"
# Error confirms security validation is active
```

#### Security Build Flags

```bash
# FPM: Manual security flags (as documented in fpm.toml)
fpm build --flag "-Wtrampolines -Werror=trampolines"

# CMake: Automatic security flags (see CMakeLists.txt lines 36-47)
cmake -B build && cmake --build build

# Standard development (FPM default) 
make build  # Uses fpm.toml configuration
```

### For Python projects
Install the Python package with pip:

```bash
pip install git+https://github.com/lazy-fortran/fortplot.git
```

## Features

### Plot types
- [x] Line plots (`plot`) with customizable line styles and markers
- [x] Error bars (`errorbar`) with symmetric/asymmetric X/Y errors and customization
- [x] 3D line plots (`add_3d_plot`) with automatic projection
- [x] 3D surface plots (`add_surface`) with automatic dimension validation
- [x] Contour plots (`contour`, `contourf`) with custom levels and colormaps
- [x] Pseudocolor mesh (`pcolormesh`) with color limits and edge colors
- [x] Streamplots (`streamplot`) for vector field visualization with arrows
- [x] Enhanced scatter plots (`scatter`) with size/color mapping and multiple marker shapes
- [x] Bar charts (`bar`)
- [x] Histograms (`hist`)
- [ ] Images (`imshow`)

### Backends
- [x] PNG (raster graphics)
- [x] PDF (vector graphics)
- [x] ASCII (terminal display)
- [x] Interactive display via system viewer (`show()`)

### Features
- [x] Line styles: solid (`-`), dashed (`--`), dotted (`:`), dashdot (`-.`)
- [x] Markers: circle (`o`), cross (`x`), square (`s`), diamond (`D`), plus (`+`), star (`*`), triangle, pentagon, hexagon
- [x] Format strings (`'r-o'`, `'b--'`, `'g:'`) for matplotlib compatibility
- [x] Colormaps: viridis, plasma, inferno, crest, coolwarm, jet, rocket, mako, flare
- [x] Colorbars for contour and pcolormesh plots
- [x] Legends with automatic positioning
- [x] Scales: linear, log, symlog (with configurable threshold)
- [x] Axis limits (`xlim`, `ylim`)
- [x] Subplot grids (`subplot`) for multiple plots in a single figure
- [x] Interactive display with `show()` (GUI detection for X11, Wayland, macOS, Windows)
- [x] Animation support with `FuncAnimation` (requires `ffmpeg` for video formats)
  - **5-Layer Validation**: Comprehensive framework with size, header, semantic, and external tool checks
  - **False Positive Prevention**: Multi-criteria validation framework
- [x] Unicode and LaTeX-style Greek letters (`\alpha`, `\beta`, `\gamma`, etc.) in all backends
- [x] **Security features**: Executable stack protection, trampoline detection, path validation
- [x] **Text annotations** (`text`, `annotate`) with multi-coordinate systems and typography


## Testing

### Run Tests
```bash
make test
```

### Control Warning Output
```bash
# Suppress warnings for clean test output
FORTPLOT_SUPPRESS_WARNINGS=1 make test

# Force warnings even in CI environments
FORTPLOT_FORCE_WARNINGS=1 make test
```

See [Testing Guide](doc/testing_guide.md) for complete testing documentation.

## Documentation Generation

Generate HTML documentation using FORD:

```bash
make doc
```

The documentation is generated in `build/doc/index.html` and includes:
- API reference with full procedure documentation
- Source code cross-references
- Example gallery with working code

**Browse documentation**: Open `file:///path/to/fortplot/build/doc/index.html`

## Why though?

Mostly for the lulz and to help make Fortran great again. In addition,
there is a need for high-quality high-performance plotting directly from Fortran
with the side-effect of a higher-performance limited-feature version of `matplotlib.pyplot`.

Timing comparison
```bash
time make example_matplotlib
5.13s user 0.52s system 91% cpu 6.159 total

time make example_python
1.35s user 0.17s system 97% cpu 1.562 total
```
