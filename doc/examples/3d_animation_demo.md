title: 3D Animation Demo
---

# 3D Animation Demo

Source: [3d_animation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/3d_animation_demo/3d_animation_demo.f90)

Animate a rotating 3D Lissajous curve. The same `FuncAnimation` plus
`save_animation` pipeline writes both MP4 (raster backend rendered through
ffmpeg) and a `.txt` ASCII frame stream replayable with `fortplot_play_ascii`.

fpm run --example --target 3d_animation_demo
fpm run --target fortplot_play_ascii -- output/example/fortran/3d_animation_demo/animation.txt --fps 24 --loop

## Files

- `3d_animation_demo.f90` - Source code
- Generated media in `output/example/fortran/3d_animation_demo/`

## Running

```bash
make example ARGS="3d_animation_demo"
```

## Output

### Animation

ASCII output:
```
=== Frame 1 ===

                             Rotating 3D Lissajous
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 0.9                                                                            |
| 0.9                                                                            |
| |                                                                              |
| 0.6                                                                            |
| 0.6                                                                            |
| |                                                                              |
|                                        - -- - - -                              |
| 0.3                              --- --          - - - -                       |
| |                               -          - - -- - -    - - -                 |
| 0.3                           --    -- -- -           - - - --- -              |
| |                            --  ---                           - ----          |
| 0.0-- - -                    - --                                    ----      |
| 0.0 --    - - -  - -  -      ---                                        ----   |
| |      - - -             -  ----  -                                       ---  |
|              - -  -          -       -  -  -  - -  -                       --- |
| -0.3                -  -  -  -                        -  - -  - - - - -- -- -  |
| |                            -  -  -  -                                    --  |
| -0.3                         --          -  -  -  -  -               -- ---    |
| |                            --                        -  - -  - - -           |
| -0.6                          --                                               |
| -0.6                          --                                               |
| |                                                                              |
| -0.9                                                                           |
| -0.9                                                                           |
| |                                                                              |
|                                                                                |
| |   -1.2       - -0.8  -     -0.4     -   0.0 -      - 0.4   -     0.8       - |
|     -1.2         -0.8        -0.4         0.0          0.4         0.8         |
+--------------------------------------------------------------------------------+


=== Frame 2 ===

                             Rotating 3D Lissajous
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
|                                                                                |
| 0.8                                                                            |
| |                                                                              |
| 0.8                                                                            |
|                                                                                |
| |                                                                              |
| 0.4                                 -- -- - - -                                |
| 0.4                             ----           - - - -                         |
| |                             --        -- - -- - - -  - - -                   |
|                              -   --- --               - - - ----               |
| |                            - --                               ------         |
| 0.0                          --                                       ----     |
| 0.0-- - - - -  - -  -        -                                           ----  |
| |   -- - -             -  -  -- -  -  -                                    --- |
|            - -  -  -         --          -  -  -  -  -  - -  - - - - - -- ---  |
|                      -  -  - ---                                            -  |
| |                             - --  -   -  -                             ----  |
... (truncated)
```

[Download ASCII](../../media/examples/3d_animation_demo/animation.txt)

[Download Video](../../media/examples/3d_animation_demo/animation.mp4)

