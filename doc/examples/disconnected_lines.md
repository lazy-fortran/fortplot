title: Disconnected Lines
---

# Disconnected Lines

Source: [disconnected_lines.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/disconnected_lines/disconnected_lines.f90)

Line plots with gaps created by NaN separators.

```bash
make example ARGS="disconnected_lines"
```

Insert NaNs into your data arrays to break a line into separate segments.

## Files

- `disconnected_lines.f90` - Source code
- Generated media in `output/example/fortran/disconnected_lines/`

## Running

```bash
make example ARGS="disconnected_lines"
```

## Output

### Disconnected Lines

![disconnected_lines.png](../../media/examples/disconnected_lines/disconnected_lines.png)

ASCII output:
```

                       Disconnected Line Segments Example
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
|                                                                                |
|                                                           o                    |
| 0.75        o - - -- o                                  -                      |
|                                                       -                        |
|            -           -                            -                          |
| 0.50                                                                           |
|          -              -                          -                           |
|                                                  o              o              |
| |       -                 -                                                    |
| 0.25  -                    -                   -                               |
|                                                                                |
| |    -                       -                -                                |
| 0.00                                                                           |
|    o                          o             -                                  |
| |                                                                              |
| -0.25                                      -                                   |
|                                                                                |
| |                                        -                                     |
|                                                                                |
| -0.50                                   o                                #     |
| |                                     -                                        |
|                                                                                |
| -0.75                               -                                          |
| |                                 -                                            |
|  # Single point                 -                                              |
|  o Disconnected segments      o                                                |
| -1.00 - - - - -  - - - - - - -  - - - - - - -  - - - - - - -  - - - - - - - -  |
|    0       1        2        3        4        5        6        7       8     |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/disconnected_lines/disconnected_lines.txt)

[Download PDF](../../media/examples/disconnected_lines/disconnected_lines.pdf)

