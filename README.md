# ![fortplotlib logo](media/logo.jpg)

Fortran-native plotting inspired by Python's `matplotlib.pyplot` and https://github.com/jacobwilliams/pyplot-fortran . This library is under active development and API still subject to change. There are no external dependencies. Ironically, it has also Python interface installable via `pip` (see below) `fortplotlib.fortplot` that can be used as a drop-in replacement for `matplotlib.pyplot` for a limited set of features.

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

For more examples, see the [example directory](example) and run

```bash
fpm run --example
```

to build and run them.

## Setup

### For fpm (Fortran Package Manager) projects

Add to your `fpm.toml`:
```toml
[[dependencies]]
fortplotlib = { git = "https://github.com/krystophny/fortplotlib" }
```

### For CMake projects

Add to your `CMakeLists.txt`:
```cmake
include(FetchContent)

FetchContent_Declare(
    fortplotlib
    GIT_REPOSITORY https://github.com/krystophny/fortplotlib
    GIT_TAG        main
)
FetchContent_MakeAvailable(fortplotlib)

target_link_libraries(your_target fortplotlib::fortplotlib)
```

### For Python projects
Install the Python package with pip:

```bash
pip install git+https://github.com/krystophny/fortplotlib.git
```

## Features

### Plot types
- [x] Line
- [x] Contour
- [x] Streamplot
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


## Why though?

Mostly for the lulz and to help make Fortran great again. In addition,
there is a need for high-quality high-performance plotting directly from Fortran
with the side-effect of a higher-performance limited-feature version of `matplotlib.pyplot`.

Timing comparison
```bash
time make example_matplotlib

time make example_python

```
