title: Scale Examples
---

# Scale Examples

This example demonstrates different axis scaling options including logarithmic and symmetric logarithmic (symlog) scales.

## Files

- `scale_examples.f90` - Source code
- `log_scale.png/pdf/txt` - Logarithmic scale example
- `symlog_scale.png/pdf/txt` - Symmetric logarithmic scale example

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

![log_scale.png](../../media/examples/scale_examples/log_scale.png)

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

[Download PDF](../../media/examples/scale_examples/log_scale.pdf)

### Symlog Scale

![symlog_scale.png](../../media/examples/scale_examples/symlog_scale.png)

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

[Download PDF](../../media/examples/scale_examples/symlog_scale.pdf)

