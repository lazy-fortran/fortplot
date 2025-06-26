# ![fortplotlib logo](media/logo.jpg)

Fortran-native plotting inspired by Python's `matplotlib.pyplot` and https://github.com/jacobwilliams/pyplot-fortran . This library is under active development and API still subject to change.

## Requirements
zlib and freetype with development headers.

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
call savefig("plot.png")
```

### Object-Oriented API
```fortran
type(figure_t) :: fig

call fig%initialize()
call fig%set_title("Function Plot")
call fig%set_xlabel("x")
call fig%set_ylabel("y")
call fig%add_plot(x, yf)
call fig%savefig("plot_oo.png")
```

For more examples, see the [examples directory](examples) and run

```bash
make examples
```

to build and run them.

## Setup

### With fpm (Fortran Package Manager)

Add to your `fpm.toml`:
```toml
[[dependencies]]
fortplotlib = { git = "https://github.com/krystophny/fortplotlib" }
```

### With CMake

Add to your `CMakeLists.txt`:
```cmake
include(FetchContent)

FetchContent_Declare(
    fortplotlib
    GIT_REPOSITORY https://github.com/krystophny/fortplotlib
)
FetchContent_MakeAvailable(fortplotlib)

target_link_libraries(your_target fortplotlib::fortplotlib)
```

## Features

### Plot types
- [x] Line
- [x] Contour
- [ ] Image
- [ ] Surface

### Backends
- [x] PNG
- [x] PDF
- [x] ASCII
- [ ] Interactive

### Misc
- [x] Custom line styles
- [x] Markers
- [x] Legends
- [x] Logarithmic scales
- [ ] Grid
- [ ] Secondary axes
- [ ] Math text
- [ ] Animations
