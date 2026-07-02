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

              Common Physics: E = mc², ΔE = hν, F = q(E + v×B)
┌────────────────────────────────────────────────────────────────────────────────┐
│                                                                                │
│ │                                                                              │
│ 0.5                                                                            │
│ │                      ■ --- Gaussian: rho(xi) = e↑(-xi↑2↗2 sigma↑2)↗sqrt(2 pi │
│ │                    ■■  --- Modified Gamma: f(xi) = xi↑2 e↑(-xi)              │
│ │                  ■■            ■■■                                           │
│ │                  ■                ■■                                         │
│ 0.4              ■■                   ■                                        │
│ │               ■                      ■■                                      │
│ │  ---          ■                        ■■                                    │
│ │     --       ■                           ■                                   │
│ │       --    ■                             ■■                                 │
│ │        --   ■                               ■■                               │
│ 0.3        - ■                                  ■                              │
│ │           ■                                    ■■                            │
│ │            --                                    ■■                          │
│ │          ■  -                                     ■■■                        │
│ │          ■   --                                      ■■                      │
│ 0.2       ■     -                                        ■■                    │
│ │        ■       --                                        ■■■                 │
│ │                  -                                          ■■               │
│ │        ■          -                                           ■■■            │
│ │       ■            -                                             ■■■         │
│ 0.1    ■              ---                                             ■■■■     │
│ │      ■                --                                                     │
│ │     ■                   ---                                                  │
│ │    ■                      -----                                              │
│ │  ■■                            -----------------------------------------     │
│ 0.0+----------+-----------+-----------+-----------+-----------+----------+---- │
│    0          1           2           3           4           5          6     │
└────────────────────────────────────────────────────────────────────────────────┘
                                  Parameter ξ
Observable Θ
```

[Download ASCII](../../media/examples/unicode_demo/math_examples.txt)

[Download PDF](../../media/examples/unicode_demo/math_examples.pdf)

### Unicode Demo

![unicode_demo.png](../../media/examples/unicode_demo/unicode_demo.png)

ASCII output:
```

             Wave Functions: ψ(ω t) = A e^{-λ t} sin(ω t + φ)
┌────────────────────────────────────────────────────────────────────────────────┐
│ 1.00                                                                           │
│ │                                                                              │
│ │                                                                              │
│ │  ■■                                      --- alpha damped: sin(omega t)e↑(-l │
│ 0.75■■                                     --- beta damped: cos(omega t)e↑(-mu │
│ │     ■                                    --- gamma oscillation: sin(2 omega  │
│ │      ■ -----                                                                 │
│ │      ■--    -                                                                │
│ 0.50   -      -                                                                │
│ │      -■      --                                                              │
│ │      ■■■              ■■■        ■■■■■■ ■■               ■■■                 │
│ │     ■  ■      -       ■ ■       ■     ■■  ■             ■   ■                │
│ 0.25 ■   ■       -     ■   ■     ■        ■■ ■           ■                     │
│ │         ■       -   ■     ■   ■       ■  ■------            ■        ■■■     │
│ │   ■              -           ■       ■ ---■■    --     ■     ■    ■■■        │
│ │   ■      ■       - ■      ■  ■        -    ■■     -              ■           │
│ 0.00       ■        -         ■       ■-      ■      ---■       ■■■            │
│ │  ■                 ■       ■        -        ■       ■-       ■■      -■     │
│ │           ■       ■-      ■        -■         ■        --   ■■ ■   ---       │
│ │            ■        -     ■ ■     -           ■■     ■   ---■ -■---   ■      │
│ -0.25        ■     ■   -   ■   ■   - ■            ■         ■■ -        ■      │
│ │             ■         -        -- ■           ■  ■■ ■   ■■■     ■            │
│ │             ■    ■    --■    ■--               ■  ■■■■■■         ■   ■       │
│ │             ■■  ■       ■    -■  ■■             ■ ■              ■  ■        │
│ -0.50          ■■■      ■■ ----  ■■               ■■■               ■■         │
│ │               ■       ■                                                      │
│ │                ■     ■                                                       │
│ │                 ■■■■■                                                        │
│ -0.75---------+----------+----------+-----------+----------+----------+------- │
│    0          2          4          6           8         10         12        │
└────────────────────────────────────────────────────────────────────────────────┘
                     Time τ (normalized: τ = ω t / 2π)
Amplitude Ψ (V)
```

[Download ASCII](../../media/examples/unicode_demo/unicode_demo.txt)

[Download PDF](../../media/examples/unicode_demo/unicode_demo.pdf)

