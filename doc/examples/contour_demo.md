title: Contour Demo
---

# Contour Demo

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

[Download PDF](../../media/examples/contour_demo/contour_gaussian.pdf)

### Mixed Plot

![mixed_plot.png](../../media/examples/contour_demo/mixed_plot.png)

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

[Download PDF](../../media/examples/contour_demo/mixed_plot.pdf)

