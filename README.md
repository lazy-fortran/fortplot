# Fortplotlib
Fortran-native plotting inspired by Python's matplotlib.pyplot and https://github.com/jacobwilliams/pyplot-fortran .

## Requirements
zlib and freetype with development headers.

## Usage

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
- [ ] Contour
- [ ] Image
- [ ] Surface

### Backends
- [x] PNG
- [x] PDF
- [ ] ASCII
- [ ] Interactive

### Misc
- [ ] Custom line styles
- [ ] Logarithmic scales
- [ ] Secondary axes
- [ ] Math text
