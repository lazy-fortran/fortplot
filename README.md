# ![fortplot logo](media/logo.jpg)

[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://lazy-fortran.github.io/fortplot/)

Fortran-native plotting inspired by Python's `matplotlib.pyplot` and https://github.com/jacobwilliams/pyplot-fortran . This library is under active development and API still subject to change. There are no external dependencies. Ironically, it has also Python interface installable via `pip` (see below) `fortplot.fortplot` that can be used as a drop-in replacement for `matplotlib.pyplot` for a limited set of features.

## Usage

```fortran
use fortplot
```

### Stateful API
```fortran
use fortplot  ! Provides wp => real64 for precision
real(wp), dimension(50) :: x, y

! Generate sample data
x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
y = sin(x)

call figure()
call plot(x, y)
call title("Function Plot")
call xlabel("x")
call ylabel("y")
call xlim(0.0_wp, 10.0_wp)  ! Set x-axis limits
call ylim(-1.0_wp, 1.0_wp)   ! Set y-axis limits
call savefig("plot.png")
```

### Note: Subplot Grids (Not Yet Implemented)
```fortran
! Note: Multiple subplots not yet implemented (Issue #XXX)
! This feature is planned for future releases
! Currently generates warning: "subplot: Multiple subplots not yet implemented"
call figure(figsize=[8.0_wp, 6.0_wp])
call subplot(2, 2, 1)  ! Will show warning
call plot(x, sin(x))
call title("Sine Wave")
call savefig("single_plot.png")  ! Only first subplot will be saved
```

### Object-Oriented API
```fortran
use fortplot  ! Imports wp => real64 for precision
type(figure_t) :: fig
real(wp), dimension(50) :: x, yf
integer :: i

! Generate test data
x = [(real(i-1, wp) * 0.1_wp, i=1, 50)]
yf = sin(x)

call fig%initialize()
call fig%set_title("Function Plot")
call fig%set_xlabel("x")
call fig%set_ylabel("y")
call fig%add_plot(x, yf)
call fig%savefig("plot_oo.png")
```

### Advanced Examples

#### 3D plotting
```fortran
call figure(figsize=[8.0_wp, 6.0_wp])
call add_3d_plot(x, y, z, label="3D curve")
call title("3D Line Plot")
call savefig("3d_plot.png")
```

#### Multiple plots with legend
```fortran
call figure(figsize=[8.0_wp, 6.0_wp])
call plot(x, sin(x), label="sin(x)", linestyle="b-")
call plot(x, cos(x), label="cos(x)", linestyle="r--")
call plot(x, sin(2*x), label="sin(2x)", linestyle="g:")
call legend()
call savefig("trig_functions.pdf")
```

#### Unicode and Greek letters in scientific plots
```fortran
call figure(figsize=[8.0_wp, 6.0_wp])
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

! Object-oriented scatter plot
type(figure_t) :: fig
real(wp), dimension(20) :: x, y, sizes
call fig%initialize()
call fig%scatter(x, y, label='Bubble Chart')
call fig%savefig("bubble_chart.pdf")
```

#### Surface plots (3D visualization)
```fortran
use fortplot
implicit none

real(wp), dimension(50) :: x, y, z
integer :: i

! Generate 3D curve data
x = [(real(i-1, wp) * 0.1_wp, i=1, 50)]
y = sin(x)
z = cos(x)

! 3D surface visualization
call figure(figsize=[8.0_wp, 6.0_wp])
call add_3d_plot(x, y, z, label="3D Surface Curve")
call title("3D Surface Plot")
call savefig("surface_3d.png")

! pcolormesh for 2D heatmaps is available and working
! Array dimensions: z(ny,nx) with x(nx+1), y(ny+1) is the
! standard scientific format and does NOT emit warnings.
! C-style z(nx,ny) is also accepted; it is transposed internally.
real(wp), dimension(10, 10) :: z_data  ! ny=10, nx=10
call pcolormesh(x_grid, y_grid, z_data, colormap="viridis")
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
use fortplot  ! Imports wp => real64 automatically
implicit none

! Use functional API for errorbar plots (currently supported)
real(wp), dimension(20) :: x, y, yerr, y_theory
call figure(figsize=[8.0_wp, 6.0_wp])
call errorbar(x, y, yerr=yerr, marker='o', label='Experimental data')
call plot(x, y_theory, label='Theory', linestyle='-')
call legend()
call savefig("scientific_plot.png")
```

#### Log scale plot
```fortran
call figure()
call plot(x, y)
call set_xscale("log")
call set_yscale("symlog", threshold=0.01_wp)
call xlim(1.0e-3_wp, 1.0e3_wp)
call ylim(-100.0_wp, 100.0_wp)
call savefig("log_plot.pdf")
```

#### Text annotations with coordinate systems
```fortran
! Text annotation system is FULLY IMPLEMENTED
use fortplot
type(figure_t) :: fig
real(wp), dimension(50) :: x, y
integer :: i

! Generate sample data
x = [(real(i-1, wp) * 0.1_wp, i=1, 50)]
y = sin(x)

call figure(figsize=[8.0_wp, 6.0_wp])
call add_plot(x, y, label="Scientific Data", linestyle="b-")
call set_title("Annotated Scientific Plot")
call set_xlabel("X Variable")
call set_ylabel("Y Variable")

! Add text annotations with different coordinate systems
call add_text_annotation("Maximum", 1.57_wp, 1.0_wp, coord_type=COORD_DATA)
call add_arrow_annotation("Peak→", 1.2_wp, 0.8_wp, 1.57_wp, 1.0_wp, coord_type=COORD_DATA)
call add_text_annotation("Title Area", 0.5_wp, 0.95_wp, coord_type=COORD_FIGURE)
call savefig("annotated_plot.png")
```

#### Animation example
```fortran
use fortplot_animation
type(figure_t) :: fig
type(animation_t) :: anim
integer :: status
real(wp), dimension(100) :: x_data, y_data

! Setup figure and initial plot
call figure(figsize=[8.0_wp, 6.0_wp])
call add_plot(x_data, y_data, label='animated data')
call title('Animation Demo')

! Create animation with update function
anim = FuncAnimation(update_frame, frames=100, interval=50, fig=fig)
call save_animation(anim, "animation.mp4", 24, status)

if (status /= 0) then
    print *, "ERROR: Animation save failed. Check ffmpeg installation."
    print *, "Windows: choco install ffmpeg"
    print *, "Linux: sudo apt install ffmpeg"
end if

contains
    subroutine update_frame(frame)
        integer, intent(in) :: frame
        ! Update plot data based on frame number
        call set_ydata(sin(x_data + real(frame, wp) * 0.1_wp))
    end subroutine update_frame
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
- [ ] Bar charts (`bar`) - Not yet implemented
- [ ] Histograms (`hist`) - Not yet implemented
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
- [ ] Subplot grids (`subplot`) - Not yet implemented (shows warning)
- [x] Interactive display with `show()` (GUI detection for X11, Wayland, macOS, Windows)
- [x] Animation support with `FuncAnimation` (requires `ffmpeg` for video formats)
  - **5-Layer Validation**: Comprehensive framework with size, header, semantic, and external tool checks
  - **False Positive Prevention**: Multi-criteria validation framework
- [x] Unicode and LaTeX-style Greek letters (`\alpha`, `\beta`, `\gamma`, etc.) in all backends
- [x] **Security features**: Executable stack protection, trampoline detection, path validation
- [x] **Text annotations** (`add_text_annotation`, `add_arrow_annotation`) with multi-coordinate systems and typography


## Module Architecture

Fortplot uses a modular architecture with focused modules under 1,000 lines each. The facade pattern maintains 100% backward compatibility:

```fortran
! Your existing code works unchanged
use fortplot_pdf, only: pdf_context, create_pdf_canvas
type(pdf_context) :: ctx
call create_pdf_canvas(ctx, "plot.pdf", 800, 600)
```

See [Module Architecture Guide](doc/module_architecture.md) for developer guidelines and refactoring details.

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

## Branch Naming & Lifecycle

- Naming: use `fix/<topic>`, `feat/<topic>`, `docs/<topic>`, `refactor/<topic>`.
- Scope: keep branches focused; one unit of change per branch.
- Lifecycle: delete branches after merge; avoid long-lived topic branches.
- Hygiene: regularly prune remote-tracking branches and local branches fully merged into `main`.

Pruning helper:

```bash
# Dry-run: see what would be pruned
make git-prune

# Apply pruning (removes merged local branches older than 30 days)
make git-prune FORCE=1
```

This keeps the repository navigable and avoids metadata bloat.

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
