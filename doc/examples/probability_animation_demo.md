title: Probability Animation Demo
---

# Probability Animation Demo

Source: [probability_animation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/probability_animation_demo/probability_animation_demo.f90)

See source and outputs below.

## Files

- `probability_animation_demo.f90` - Source code
- Generated media in `output/example/fortran/probability_animation_demo/`

## Running

```bash
make example ARGS="probability_animation_demo"
```

## Output

### Melting Gaussian

ASCII output:
```
=== Frame 1 ===

                         Gaussian with increasing sigma
+--------------------------------------------------------------------------------+
|                                                                                |
| 0.70                                                                           |
|                                                                                |
|                                                                                |
| 0.60                                                                           |
|                                                                                |
|                                                                                |
| 0.50                                                                           |
|                                                                                |
|                                                                                |
| 0.40                                                                           |
|                                                                                |
| *                                     -                                        |
| 0.30                                 - -                                       |
|                                     -   -                                      |
|                                    -     -                                     |
|                                    -     -                                     |
| 0.20                              -       -                                    |
|                                  --       --                                   |
|                                  -         -                                   |
| 0.10                            -           -                                  |
|                               --             --                                |
| **-**-**-*-**-**-**-*-**-**-**-* ** ** ** * **-**-**-*-**-**-**-*-**-**-**-*-* |
|-6       -5        -3        -2        0         2         3         5         6|
+--------------------------------------------------------------------------------+
                                       x
density
=== Frame 2 ===

                         Gaussian with increasing sigma
+--------------------------------------------------------------------------------+
|                                                                                |
| 0.70                                                                           |
|                                                                                |
|                                                                                |
| 0.60                                                                           |
|                                                                                |
|                                                                                |
| 0.50                                                                           |
|                                                                                |
|                                                                                |
| 0.40                                                                           |
|                                                                                |
| #                                                                              |
| 0.30                                 ---                                       |
|                                     -- --                                      |
|                                    --   --                                     |
|                                    -     -                                     |
| 0.20                              -       -                                    |
|                                  --       --                                   |
|                                 --         --                                  |
| 0.10                           --           --                                 |
|                               --             --                                |
| ##-##-##-#-##-##-##-#-##-##-##-# ## ## ## # ##-##-##-#-##-##-##-#-##-##-##-#-# |
|-6       -5        -3        -2        0         2         3         5         6|
+--------------------------------------------------------------------------------+
... (truncated)
```

[Download ASCII](../../media/examples/probability_animation_demo/melting_gaussian.txt)

[Download Video](../../media/examples/probability_animation_demo/melting_gaussian.mp4)

