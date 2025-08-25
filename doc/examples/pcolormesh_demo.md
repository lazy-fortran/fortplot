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

## Key Differences from Contour

- **Pcolormesh**: Shows actual data values as colored cells
- **Contour**: Interpolates and shows level curves
- **Performance**: Pcolormesh faster for large grids

## Output

### Basic Linear Gradient

![pcolormesh_basic.png](../../media/examples/pcolormesh_demo/pcolormesh_basic.png)

ASCII output:
```

                       Basic Pcolormesh - Linear Gradient
+--------------------------------------------------------------------------------+
|1.20                                                                            |
| .                                                                              |
|                                                                                |
|                                                                                |
|1.00                                                                            |
|        +               *              #              %               @         |
|                                                                                |
| .                                                                              |
|                                                                                |
|.800                                                                            |
|                                                                                |
|                                                                                |
| .      -               =              +              *               #         |
|                                                                                |
|.600                                                                            |
|                                                                                |
|                                                                                |
|                                                                                |
| .      .               :              -              =               +         |
|.400                                                                            |
|                                                                                |
|                                                                                |
|                                                                                |
| .                                                                              |
|.200                                                                            |
|                                       .              :               -         |
|                                                                                |
|                                                                                |
| .       .        .       .        .       .        .       .        .        . |
|0                  .500                1.00                1.50            2.00 |
+--------------------------------------------------------------------------------+
                                  X coordinate
Y coordinate
```

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_basic.pdf)

### Sinusoidal Pattern

![pcolormesh_sinusoidal.png](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.png)

ASCII output:
```

                        Pcolormesh - Sinusoidal Pattern
+--------------------------------------------------------------------------------+
|1.20                                                                            |
| .                                                                              |
|                                                                                |
|    =       =       =      =       =       =       =      =                     |
|1.00                                                                            |
|        +               *              #              %               @         |
|                                                                                |
| .  #       :              +       %       =              :                     |
|                                                                                |
|.800                                                                            |
|    *       -       .      +       #       =       .      -                     |
|                                                                                |
| .      -               =              +              *               #         |
|    :       +       #      -       .       =       #      +                     |
|.600                                                                            |
|                                                                                |
|                                                                                |
|    .       *       %      -               =       %      *                     |
| .      .               :              -              =               +         |
|.400                                                                            |
|    =       =       =      =       =       =       =      =                     |
|                                                                                |
|                                                                                |
| .  #       :              +       @       =              :                     |
|.200                                                                            |
|                                       .              :               -         |
|                                                                                |
|    *       -       .      +       #       =       .      -                     |
| .       .        .       .        .       .        .       .        .        . |
|0                  .500                1.00                1.50            2.00 |
+--------------------------------------------------------------------------------+
                                  X coordinate
Y coordinate
```

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.pdf)

### Radial Pattern with Plasma Colormap

![pcolormesh_plasma.png](../../media/examples/pcolormesh_demo/pcolormesh_plasma.png)

ASCII output:
```

                      Pcolormesh - Radial Pattern (Plasma)
+--------------------------------------------------------------------------------+
|1.20                                                                            |
| .                                                                              |
|                                                                                |
|                                                                                |
|    = -     =     + =      = #     =     # =       =+     =                     |
|1.00                                                                            |
| .      +               *              #              %               @         |
|    #       :              +       %       =              :                     |
|                                                                                |
|                                                                                |
|.800  -           *          @           %          +                           |
|    *       -       .      +       #       =       .      -                     |
|        -               =              +              *               #         |
|                                                                                |
|    :       +       #      -       .       =       #      +                     |
|.600  :           =          +           +          =                           |
|                                                                                |
|    .       *       %      -               =       %      *                     |
|                                                                                |
|.400    .               :              -              =               +         |
|    = .     =     : =      = :     =     : =       =:     =                     |
|                                                                                |
|                                                                                |
|                                                                                |
|.200#       :              +       @       =              :                     |
|                                       .              :               -         |
|                                                                                |
|    *       -       .      +       #       =       .      -                     |
| .       .        .       .        .       .        .       .        .        . |
|0                  .500                1.00                1.50            2.00 |
+--------------------------------------------------------------------------------+
                                  X coordinate
Y coordinate
```

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_plasma.pdf)

