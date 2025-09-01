title: Pcolormesh Demo
---

# Pcolormesh Demo

This example demonstrates pseudocolor plots for efficient 2D data visualization.

## Files

- `pcolormesh_demo.f90` - Source code
- `pcolormesh_basic.png/pdf/txt` - Basic pcolormesh plot
- `pcolormesh_plasma.png/pdf/txt` - Pcolormesh with plasma colormap
- `pcolormesh_sinusoidal.png/pdf/txt` - Sinusoidal pattern

## Running

```bash
make example ARGS="pcolormesh_demo"
```

## Features Demonstrated

- **Grid-based visualization**: Efficient for large 2D datasets
- **Colormap support**: All standard colormaps available
- **Cell-centered data**: Each cell shows one data value
- **Automatic scaling**: Data range mapped to colors

## Array Dimensions (No-Warning Guide)

- Standard scientific layout: provide `z(ny, nx)` with `x(nx+1)` and `y(ny+1)`.
  This is the default convention and does not produce transpose warnings.
- C-style layout `z(nx, ny)` is accepted as well; data are transposed internally
  to match plotting axes. Coordinate arrays remain `x(nx+1)` and `y(ny+1)`.
/- Invalid shapes (that do not match either convention) are rejected with a clear
  error explaining the expected dimensions.

## Key Differences from Contour

- **Pcolormesh**: Shows actual data values as colored cells
- **Contour**: Interpolates and shows level curves
- **Performance**: Pcolormesh faster for large grids

## Output

### Basic Linear Gradient

![pcolormesh_basic.png](../../media/examples/pcolormesh_demo/pcolormesh_basic.png)

ASCII output preview:
```
                       Basic Pcolormesh - Linear Gradient
+--------------------------------------------------------------------------------+
|1.20                                                                            |
|        +               *              #              %               @         |
|0.90    -               =              +              *               #         |
|        .               :              -              =               +         |
|0.60    .               .               :              -               =         |
|        .               .               .              :               -         |
|0.30    .               .               .              .               :         |
|        .               .               .              .               .         |
|0.00+--------+----------+----------+----------+----------+--------+            |
     0.0     0.4        0.8        1.2        1.6        2.0                   |
+--------------------------------------------------------------------------------+
                                  X coordinate
Y coordinate
```

> **Full ASCII Output**: [Download pcolormesh_basic.txt](../../media/examples/pcolormesh_demo/pcolormesh_basic.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_basic.pdf)

### Sinusoidal Pattern

![pcolormesh_sinusoidal.png](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.png)

ASCII output preview:
```
                        Pcolormesh - Sinusoidal Pattern
+--------------------------------------------------------------------------------+
|1.20    =       =       =      =       =       =       =      =                |
|        +       +       +      +       +       +       +      +                |
|0.90    *       *       *      *       *       *       *      *                |
|        #       #       #      #       #       #       #      #                |
|0.60    %       %       %      %       %       %       %      %                |
|        @       @       @      @       @       @       @      @                |
|0.30    .       .       .      .       .       .       .      .                |
|        :       :       :      :       :       :       :      :                |
|0.00+--------+----------+----------+----------+----------+--------+            |
     0.0     0.4        0.8        1.2        1.6        2.0                   |
+--------------------------------------------------------------------------------+
                                  X coordinate
Y coordinate
```

> **Full ASCII Output**: [Download pcolormesh_sinusoidal.txt](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.pdf)

### Radial Pattern with Plasma Colormap

![pcolormesh_plasma.png](../../media/examples/pcolormesh_demo/pcolormesh_plasma.png)

ASCII output preview:
```
                      Pcolormesh - Radial Pattern (Plasma)
+--------------------------------------------------------------------------------+
|1.20    = -     =     + =      = #     =     # =       =+     =                |
|        + *     +     * +      + %     +     % +       +*     +                |
|0.80    # @     #     @ #      # =     #     = #       #@     #                |
|        % .     %     . %      % :     %     : %       %.     %                |
|0.60    @ :     @     : @      @ .     @     . @       @:     @                |
|        = -     =     - =      = +     =     + =       =-     =                |
|0.30    : +     :     + :      : *     :     * :       :+     :                |
|        . #     .     # .      . %     .     % .       .#     .                |
|0.00+--------+----------+----------+----------+----------+--------+            |
     0.0     0.4        0.8        1.2        1.6        2.0                   |
+--------------------------------------------------------------------------------+
                                  X coordinate
Y coordinate
```

> **Full ASCII Output**: [Download pcolormesh_plasma.txt](../../media/examples/pcolormesh_demo/pcolormesh_plasma.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_plasma.pdf)
