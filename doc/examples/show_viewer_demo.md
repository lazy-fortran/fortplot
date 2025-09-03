title: Show Viewer Demo
---

# Show Viewer Demo

Source: [show_viewer_demo.f90](../../sourcefile/show_viewer_demo.f90.html)

This example demonstrates using the built-in viewer for interactive display.

## Files

- `show_viewer_demo.f90` - Source code

## Running

```bash
make example ARGS="show_viewer_demo"
```

## Features Demonstrated

- **Interactive display**: Opens plot in system viewer
- **Cross-platform**: Works on Linux, macOS, Windows
- **Auto-detection**: Finds appropriate viewer
- **Non-blocking**: Program continues after display

## Viewer Selection

The library automatically selects viewers:

### Linux
- `xdg-open` (default)
- `eog` (Eye of GNOME)
- `feh` (lightweight viewer)

### macOS
- `open` (system default)
- `Preview.app`

### Windows
- Default image viewer
- Photos app

## Usage

```fortran
! Create and display plot
call fig%initialize()
call fig%add_plot(x, y)
call fig%show()  ! Opens in viewer
```

## Benefits

- **Quick inspection**: No need to save files
- **Development**: Rapid iteration
- **Debugging**: Immediate visual feedback
- **Presentations**: Live demonstrations
