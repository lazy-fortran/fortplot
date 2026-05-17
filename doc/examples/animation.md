title: Animation
---

# Animation

Source: [save_animation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/animation/save_animation_demo.f90)

Generate an MP4 animation from a sequence of frames.

`save_animation` also accepts a `.txt` filename to emit frames as ASCII renderings
delimited by `=== Frame N ===` headers. Replay them in the terminal with the
`fortplot_play_ascii` CLI app:

fpm run --target fortplot_play_ascii -- output.txt --fps 24 --loop

## Files

- `save_animation_demo.f90` - Source code
- Run the example to populate `output/example/fortran/animation/`

## Running

```bash
make example ARGS="save_animation_demo"
```

## Output

Run this example to generate plots and other media assets.

