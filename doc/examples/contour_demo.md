title: Contour Demo
---

# Contour Demo

Source: [example/fortran/contour_demo/contour_demo.f90](../../example/fortran/contour_demo/contour_demo.f90)

This example demonstrates contour plotting capabilities, including basic contours, custom levels, and mixing contour plots with line plots.

## Files

- `contour_demo.f90` - Source code
- `contour_gaussian.png/pdf/txt` - Gaussian function contour plot
- `mixed_plot.png/pdf/txt` - Combined contour and line plot

## Running

```bash
make example ARGS="contour_demo"
```

## Features Demonstrated

- **Basic contours**: Automatic level selection
- **Custom levels**: User-defined contour levels
- **Mixed plots**: Combining contours with line plots
- **Label formatting**: Contour level labels

## Output

### Contour Gaussian

![contour_gaussian.png](../../media/examples/contour_demo/contour_gaussian.png)

ASCII output preview:
```
                              2D Gaussian Function
+--------------------------------------------------------------------------------+
|2.0                              .  .  . .  .  .                                |
|                           . ..    .  . .  .    .. .                           |
|1.5                     ..  . ...  . .. .. .  ... .  ..                        |
|                    .  .  ... ....... . . ....... ...  .  .                    |
|1.0                .   ... .. .... .... .... .... .. ...   .                   |
|                  .  .. .... ..... ##### ##### ..... ....  ..                  |
|0.5              .   ... .... ##### ##### ##### ##### .... ...                 |
|                .   . ... ### ##### @@@@@ @@@@@ ##### ### ... .                |
|0.0+--------+----------+----------+----------+----------+--------+             |
   -2.0    -1.0        0.0        1.0        2.0        3.0                    |
+--------------------------------------------------------------------------------+
                                       x
y
```

> **Full ASCII Output**: [Download contour_gaussian.txt](../../media/examples/contour_demo/contour_gaussian.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/contour_demo/contour_gaussian.pdf)

### Mixed Plot

![mixed_plot.png](../../media/examples/contour_demo/mixed_plot.png)

ASCII output preview:
```
                           Mixed Plot: Contour + Line
+--------------------------------------------------------------------------------+
|1.0        .     .         ..     #*  * *  *#     ..         .     .           |
|           .      .          *#*             *#*          .      .             |
|0.5         .      .     #*#   . .         . .   #*#     .      .              |
|   * *  * *  * *  * .                . .                . *  * * * *  * *  *   |
|0.0 ..   ..   . .    . ..         .  .  . .  .  .         .. .    . .   ..  ..|
|             ..      ..         .. .. ... .. ..         ..      ..             |
|-0.5          .        .       .   ... ### ###   .       .        .            |
|                               .   .  .###.  .   .                             |
|-1.0+--------+----------+----------+----------+----------+--------+            |
    -2.0    -1.0        0.0        1.0        2.0        3.0                   |
+--------------------------------------------------------------------------------+
                                       x
y
```

> **Full ASCII Output**: [Download mixed_plot.txt](../../media/examples/contour_demo/mixed_plot.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/contour_demo/mixed_plot.pdf)

