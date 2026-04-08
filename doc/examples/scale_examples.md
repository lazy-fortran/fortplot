title: Scale Examples
---

# Scale Examples

Source: [scale_examples.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/scale_examples/scale_examples.f90)

Linear, log, and symlog axis scales.

## Files

- `scale_examples.f90` - Source code
- Generated media in `output/example/fortran/scale_examples/`

## Running

```bash
make example ARGS="scale_examples"
```

## Output

### Log Scale

![log_scale.png](../../media/examples/scale_examples/log_scale.png)

ASCII output:
```

                               Log Scale Example
+--------------------------------------------------------------------------------+
|                                                                                |
|                                                                                |
| *                                                                        ----- |
| *                                                                    -----     |
| *                                                                 ----         |
| *                                                             -----            |
| *                                                         -----                |
| *                                                      ----                    |
| *                                                  -----                       |
| *                                              -----                           |
| *                                           ----                               |
| *                                       ----                                   |
| $10^4$                              -----                                      |
| *                               -----                                          |
| *                            ----                                              |
| *                        -----                                                 |
| *                    -----                                                     |
| *                 ----                                                         |
| *             -----                                                            |
| *          ----                                                                |
| *      ----                                                                    |
| *  -----                                                                       |
| $10^3$************************************************************************ |
|              10              20              30               40            50 |
+--------------------------------------------------------------------------------+
                                       x
exp(0.2x)
```

[Download ASCII](../../media/examples/scale_examples/log_scale.txt)

[Download PDF](../../media/examples/scale_examples/log_scale.pdf)

### Symlog Scale

![symlog_scale.png](../../media/examples/scale_examples/symlog_scale.png)

ASCII output:
```

                              Symlog Scale Example
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                              |
| *                              ----------------------------------------------- |
| $10^5$        -----------------                                                |
| *         ----                                                                 |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *         -                                                                    |
| *        --                                                                    |
| *        -                                                                     |
| *        -                                                                     |
| *        -                                                                     |
| *        -                                                                     |
| *        -                                                                     |
| *       --                                                                     |
| $10^4$  -                                                                      |
| ****************************************************************************** |
|              10              20              30               40            50 |
+--------------------------------------------------------------------------------+
                                       x
xU+00B3 - 50x
```

[Download ASCII](../../media/examples/scale_examples/symlog_scale.txt)

[Download PDF](../../media/examples/scale_examples/symlog_scale.pdf)

