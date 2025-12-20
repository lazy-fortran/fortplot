title: Fill Between Demo
---

# Fill Between Demo

Source: [fill_between_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/fill_between_demo/fill_between_demo.f90)

Create filled regions using both the stateful API and `figure_t`.

The demo highlights part of an oscillatory curve and shades a baseline response.

Generated files are written to `output/example/fortran/fill_between_demo/` in
both PNG and ASCII formats.

## Files

- `fill_between_demo.f90` - Source code
- Generated media in `output/example/fortran/fill_between_demo/`

## Running

```bash
make example ARGS="fill_between_demo"
```

## Output

### Oo Fill Between

![oo_fill_between.png](../../media/examples/fill_between_demo/oo_fill_between.png)

ASCII output:
```

                        Object API fill_between baseline
+--------------------------------------------------------------------------------+
| 0.8                                                                            |
|                        -                                                       |
| *                -------------                                                 |
| 0.6           ---=============---                                              |
|             ---==================--                                            |
| *          --======================--                                          |
| 0.4      --==========================--                                        |
|        ---=============================--                                      |
| *     --================================---                                    |
|      -====================================--                                   |
| 0.2 -=======================================--                                 |
|    -=========================================--                                |
| *--============================================-                               |
| 0.0=============================================--                             |
| -=================================================--========================== |
|                                                    ---======================== |
| *                                                    --======================= |
| -0.2                                                   --===================== |
|                                                         ---=================== |
| *                                                         ---================= |
| -0.4                                                        ---=============== |
|                                                                ----=========== |
| *  *  *  *  *   *  *  *  *  *   *  *  *  *  *   *  *  *  *  *   * -*--*--*---* |
|0           1             2            3             4            5             |
+--------------------------------------------------------------------------------+
                                      time
response
```

[Download ASCII](../../media/examples/fill_between_demo/oo_fill_between.txt)

### Stateful Fill Between

![stateful_fill_between.png](../../media/examples/fill_between_demo/stateful_fill_between.png)

ASCII output:
```

                        Stateful fill_between with mask
+--------------------------------------------------------------------------------+
|                                                                                |
| *                  -                                                           |
|              ------------                                                      |
|            ---==========---                                        -- sin(x)   |
| *         --===============--                                                  |
| 0.5     --===================--                                                |
|       ---=====================--                                               |
| *    --=========================-                                              |
|     --=======            ========--                                            |
| *  -=====                    =====--                                           |
|  --===                          ===--                                          |
| 0.0=                               =--                                         |
| *=                                   ---                                    -- |
|                                        --                                  --  |
|                                         --                                --   |
| *                                        --                              -     |
|                                           --                           --      |
| -0.5                                        -                         --       |
|                                              --                     ---        |
|                                               --                   --          |
| *                                               --               --            |
|                                                   ---          ---             |
| * *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * -*--*--*--*- *  *  *  * *  |
|0           1           2            3            4            5           6    |
+--------------------------------------------------------------------------------+
                                  angle (rad)
amplitude
```

[Download ASCII](../../media/examples/fill_between_demo/stateful_fill_between.txt)

