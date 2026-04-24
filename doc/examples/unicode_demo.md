title: Unicode Demo
---

# Unicode Demo

Source: [unicode_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/unicode_demo/unicode_demo.f90)

Unicode symbols in labels and titles.

## Files

- `unicode_demo.f90` - Source code
- Generated media in `output/example/fortran/unicode_demo/`

## Running

```bash
make example ARGS="unicode_demo"
```

## Output

### Math Examples

![math_examples.png](../../media/examples/unicode_demo/math_examples.png)

ASCII output:
```

             Common Physics: E = mc2, Delta E = hnu, F = q(E + vxB)
+--------------------------------------------------------------------------------+
|                                                                                |
| *                        #                                                     |
| 0.48                 #### #####                                                |
|            -- Gaussian: rho(xi) = e^-xi2/2sigma2/\sqrt2pisigma2                |
|            -- Modified Gamma: f(xi) = xi2 e^-xi                                |
|                  #                 ###                                         |
|                 #                     #                                        |
| 0.40           #                       ##                                      |
|               #                          ##                                    |
| ----         #                            ##                                   |
| *   --       #                              ##                                 |
| 0.32  -     #                                ##                                |
|        -   #                                   ##                              |
|         --#                                      ##                            |
|           #                                        ##                          |
|           --                                        ##                         |
| 0.24     #  -                                         ##                       |
|         #    -                                          ##                     |
|              --                                           ##                   |
| *      #       -                                            ###                |
| 0.16  #         --                                             ##              |
|       #          -                                               ###           |
|                   --                                                ###        |
|      #              --                                                 ####    |
|     #                --                                                    ### |
| 0.08                   --                                                      |
|    #                     ---                                                   |
|   #                        -----                                               |
| %# *  *  *  *  *  *  *  *  *  *--*--*---*--*--*--*--*--*--*--*--*--*--*--*---* |
|0.0       0.8       1.6        2.4        3.2       4.0        4.8        5.6   |
+--------------------------------------------------------------------------------+
                                  Parameter xi
Observable Theta
```

[Download ASCII](../../media/examples/unicode_demo/math_examples.txt)

[Download PDF](../../media/examples/unicode_demo/math_examples.pdf)

### Unicode Demo

![unicode_demo.png](../../media/examples/unicode_demo/unicode_demo.png)

ASCII output:
```

        Wave Functions: psi(omega t) = A e^-lambda t sin(omega t + phi)
+--------------------------------------------------------------------------------+
| 1.0                                                                            |
| %                                                                              |
| ##                                                                             |
| 0.8                          -- alpha damped: sin(omega t)e^-lambdatau         |
|    #                         -- beta damped: cos(omega t)e^-mutau              |
| *  #   ----                  -- gamma oscillation: sin(2omega t)               |
|       -   --                                                                   |
|     #-      -                                                                  |
| 0.5 -#       -                                                                 |
| *  -#         -         #           ####  ##                  ##               |
|    # ##       -       ####         #    ### #                # #               |
|    #   #       -     #           ##      ##  #              #   #              |
| 0.2             -         #     #          # #             #     #             |
| *#     #             #     #   #        #  -#------                      ##### |
|                  -             #       #  -  ##    --      #      #    ###     |
| #       #        -  #       # #         --   # #     --               #        |
| 0.0      #        -          #        #-      ##       -- #       # ##         |
| %        #         #        #         -         #       --         #        -# |
|           #                          -#          #       #--      ##     ---   |
|                   # -       ##      -            #          --- ##    ----  #  |
|           #          -     #       - #            ##    #      #----#--        |
| -0.2       #     #   -    #   #   --             #  #   #     ##           #   |
|            #     #    -- #     # -  #               ###    ###       #         |
|             #           #      --  #              #    #####          #   #    |
|              #  #       #--  --#   #               #  #               #  #     |
| -0.5          ##       #   --   ###                 ##                 ###     |
|               #       #                                                        |
|                #     #                                                         |
| ** ** ** ** ** *%#%%#%* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** * * |
|0           2           4            6            8            10          12   |
+--------------------------------------------------------------------------------+
                   Time tau (normalized: tau = omega t / 2pi)
Amplitude Psi (V)
```

[Download ASCII](../../media/examples/unicode_demo/unicode_demo.txt)

[Download PDF](../../media/examples/unicode_demo/unicode_demo.pdf)

