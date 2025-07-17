title: Smart Show Demo
---

# Smart Show Demo

This example demonstrates intelligent display mode selection based on environment.

## Source Files

### Fortran Source

📄 [smart_show_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/smart_show_demo/smart_show_demo.f90)

### Generated Output Files

- Various output files in PNG, PDF, and ASCII formats

## Running

```bash
make example ARGS="smart_show_demo"
```

## Features Demonstrated

- **Environment detection**: GUI vs terminal detection
- **Automatic fallback**: ASCII when no GUI available
- **Smart defaults**: Best output for context
- **User override**: Force specific backend

## Display Logic

1. **Check environment**:
   - `DISPLAY` variable (Linux/Unix)
   - Terminal capabilities
   - SSH session detection

2. **Select backend**:
   - GUI available → PNG viewer
   - Terminal only → ASCII output
   - File output → User specified

3. **Fallback chain**:
   - Try PNG viewer
   - Fall back to ASCII
   - Save to file if all else fails

## Use Cases

- **Local development**: Full GUI viewer
- **Remote SSH**: ASCII visualization
- **CI/CD pipelines**: File output only
- **Docker containers**: Depends on configuration

## Example Output

### GUI Mode
Opens in image viewer with full graphics

### Terminal Mode
```
Smart Plot Display
==================
     1.0 |    **
         |   *  *
     0.5 |  *    *
         | *      *
     0.0 |*        *
    -0.5 |          *
         |           *
    -1.0 |            **
         +---------------
         0              2π
```
