title: Contour Demo
---

# Contour Demo

This example demonstrates contour plotting capabilities, including basic contours, custom levels, and mixing contour plots with line plots.

## Source Files

### Fortran Source

📄 [contour_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/contour_demo/contour_demo.f90)

### Python Equivalent

🐍 [contour_demo.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/contour_demo/contour_demo.py)

### Generated Output Files

- `contour_gaussian.png/pdf/txt` - Gaussian function contours
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

![contour_gaussian.png](../../media/examples/contour_gaussian.png)

ASCII output:
```

                              2D Gaussian Function
+--------------------------------------------------------------------------------+
|                                                                                |
| .                                                                              |
|                                                                                |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                              .  .  . .  .  .                                 |
| .                         . ..    .  . .  .    .. .                            |
| .                      ..  . ...  . .. .. .  ... .  ..                         |
| .                    ..  ..  . . ..  . .  .. . .  ..  ..                       |
| .                   .  .. .. ...  .  . .  .  ... .. ..  .                      |
| .                  .  .  ... ....... . . ....... ...  .  .                     |
| .                 .   ... .. .... .... .... .... .. ...   .                    |
|                   .  . . . ... .. .       . .. ... . . .  .                    |
| .                 .   ... .. .... .... .... .... .. ...   .                    |
| .                  .  .  ... ....... . . ....... ...  .  .                     |
| .                   .  .. .. ...  .  . .  .  ... .. ..  .                      |
| .                    ..  ..  . . ..  . .  .. . .  ..  ..                       |
| .                      ..  . ...  . .. .. .  ... .  ..                         |
| .                         . ..    .  . .  .    .. .                            |
| .                              .  .  . .  .  .                                 |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .  .  .  .  .  .  .  .  .  .  .  .  .   .  .  .  .  .  .  .  .  .  .  .  .   . |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/contour_gaussian.pdf)

### Mixed Plot

![mixed_plot.png](../../media/examples/mixed_plot.png)

ASCII output:
```

                           Mixed Plot: Contour + Line
+--------------------------------------------------------------------------------+
|                                                                                |
| .   .    .                                                         .    .    . |
|  ..  . .  .. .                                                 . ..  . .  ..   |
| .  . . . ..    . .                                         . .    .. . . .  .. |
| . .   ..   . .    .  ..                               ..  .    . .   ..   .    |
| .   ..   ..    . .     .  .. .                 . ..  .     . .    ..   ..   .. |
| ...   ..   . .    . ..         .  .  . .  .  .         .. .    . .   ..   ..   |
| .   .    .    ..       . ..                       .. .       ..    .    .      |
| .    ..   ..     ...         ...             ...         ...     ..   ..       |
| .      .     .       ..           .  . .  .           ..       .     .         |
| .       .      .       . .                         . .       .      .          |
| .         .     .         ..     #*  * *  *#     ..         .     .            |
| .          .      .          *#*             *#*          .      .             |
| .           .      .     #*#   . .         . .   #*#     .      .              |
| .           .      . * *          ..     ..          * * .      .              |
| * *  * *  * .* *  * .                . .                . *  * *. *  * *  *  * |
| .           .      .              ..     ..              .      .              |
| .           .      .           . .         . .           .      .              |
| .          .      .          ..               ..          .      .             |
| .         .     .         ..                     ..         .     .            |
| .       .      .       . .                         . .       .      .          |
| .      .     .       ..           .  . .  .           ..       .     .         |
| .    ..   ..     ...         ...             ...         ...     ..   ..       |
| .   .    .    ..       . ..                       .. .       ..    .    .      |
| ...   ..   . .    . ..         .  .  . .  .  .         .. .    . .   ..   ..   |
| .   ..   ..    . .     .  .. .                 . ..  .     . .    ..   ..   .. |
| . .   ..   . .    .  ..                               ..  .    . .   ..   .    |
| .  . . . ..    . .                                         . .    .. . . .  .. |
| ........ ..... .  .  .  .  .  .  .  .   .  .  .  .  .  .  .  . ..... ....... . |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/mixed_plot.pdf)

