title: Scale Examples
---

# Scale Examples

This example demonstrates different axis scaling options including logarithmic and symmetric logarithmic (symlog) scales.

## Source Files

### Fortran Source

📄 [scale_examples.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/scale_examples/scale_examples.f90)

### Python Equivalent

🐍 [scale_examples.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/scale_examples/scale_examples.py)

### Generated Output Files

- `log_scale.png/pdf/txt` - Logarithmic scale example
- `symlog_scale.png/pdf/txt` - Symmetric logarithmic scale

## Running

```bash
make example ARGS="scale_examples"
```

## Features Demonstrated

- **Logarithmic scaling**: For exponential growth visualization
- **Symmetric log**: Handles positive and negative values with log-like behavior
- **Linear threshold**: Symlog parameter controls transition to linear near zero
- **Automatic tick generation**: Smart tick placement for non-linear scales

## Output

### Log Scale

![log_scale.png](../../media/examples/log_scale.png)

ASCII output:
```

                               Log Scale Example
+--------------------------------------------------------------------------------+
|                                                                                |
| .                                                                            * |
| .                                                                         #*## |
|                                                                        #**     |
| .                                                                   #**#       |
| .                                                                #**#          |
|                                                               *#*#             |
| .                                                           #*#                |
|                                                          #**                   |
| .                                                     #**                      |
| .                                                  *#*#                        |
|                                                 *#*#                           |
| .                                            ##*#                              |
| .                                          #**                                 |
|                                         *#*                                    |
| .                                    *#*                                       |
|                                   *#*                                          |
| .                              *#*                                             |
| .                           #*##                                               |
|                          #*#*                                                  |
| .                     #*#*                                                     |
| ....................*#*....................................................... |
|                  **#                                                           |
|               #*#                                                              |
|            #*#*                                                                |
|         #*#*                                                                   |
|      #**#                                                                      |
|    **#                                                                         |
| **#                                                                            |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/log_scale.pdf)

### Symlog Scale

![symlog_scale.png](../../media/examples/symlog_scale.png)

ASCII output:
```

                              Symlog Scale Example
+--------------------------------------------------------------------------------+
|                                                                                |
| .                                                                            * |
| .                                    *#**#*#**#**#**#*#**#**#**#*#**#**#**#*## |
| .                  #*#**#**#**#*#**#*#                                         |
| .          #*#**#**#                                                           |
| .         #*                                                                   |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .         #                                                                    |
| .        ##                                                                    |
| .        #                                                                     |
| .        #                                                                     |
| .        #                                                                     |
| .        #                                                                     |
| .        #                                                                     |
| .        #                                                                     |
| .        *                                                                     |
| .       ##                                                                     |
| .       #                                                                      |
| .       #                                                                      |
| **#**#**#                                                                      |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/symlog_scale.pdf)

