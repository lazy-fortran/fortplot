title: Probability Animation Demo
---

# Probability Animation Demo

Source: [probability_animation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/probability_animation_demo/probability_animation_demo.f90)

Animate a Gaussian probability distribution as its width changes over time.

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
| 0.7                                                                            |
| 0.7                                   -                                        |
|                                      - -                                       |
| 0.6                                  - -                                       |
| 0.6                                 -   -                                      |
|                                     -   -                                      |
| 0.5                                -     -                                     |
| 0.5                                                                            |
|                                    -     -                                     |
| 0.4                                -     -                                     |
| 0.4                               -       -                                    |
| |                                 -       -                                    |
| 0.3                               -       -                                    |
| 0.3                              -         -                                   |
|                                  -         -                                   |
|                                  -         -                                   |
| 0.2                             -           -                                  |
| 0.2                             -           -                                  |
|                                -             -                                 |
| 0.1                           --             --                                |
| 0.1                          --               --                               |
| -6---------4------------2----- - -- --0-- - -- ----2------------4-----------6- |
| -6        -4           -2             0            2            4           6  |
+--------------------------------------------------------------------------------+
                                       x
density
=== Frame 2 ===

                         Gaussian with increasing sigma
+--------------------------------------------------------------------------------+
|                                                                                |
| 0.7                                                                            |
| 0.7                                                                            |
|                                                                                |
| 0.6                                  ---                                       |
| 0.6                                  - -                                       |
|                                     -   -                                      |
| 0.5                                 -   -                                      |
| 0.5                                -     -                                     |
|                                    -     -                                     |
| 0.4                                -     -                                     |
| 0.4                               -       -                                    |
| |                                 -       -                                    |
| 0.3                               -       -                                    |
| 0.3                              -         -                                   |
|                                  -         -                                   |
|                                 -           -                                  |
| 0.2                             -           -                                  |
| 0.2                            --           --                                 |
|                                -             -                                 |
| 0.1                           -               -                                |
| 0.1                          --               --                               |
| -6---------4------------2----- - -- --0-- - -- ----2------------4-----------6- |
| -6        -4           -2             0            2            4           6  |
+--------------------------------------------------------------------------------+
... (truncated)
```

[Download ASCII](../../media/examples/probability_animation_demo/melting_gaussian.txt)

[Download Video](../../media/examples/probability_animation_demo/melting_gaussian.mp4)

