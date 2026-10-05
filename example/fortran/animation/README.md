title: Animation
---

Generate an MP4 animation from a sequence of frames. The demo writes
`output/example/fortran/animation/animation.mp4`.

`save_animation` also accepts a `.txt` filename to emit frames as ASCII renderings
delimited by `=== Frame N ===` headers. Replay them in the terminal with the
`fortplot_play_ascii` CLI app:

```bash
fpm run --target fortplot_play_ascii -- output.txt --fps 24 --loop
```

Pass `fig=` to `FuncAnimation` (or use `set_figure`) before saving. A missing
figure, callback or positive frame count returns a nonzero status. For a video
filename, zero status means that the requested video was produced and validated.
If encoding fails, diagnostic PNG frames may still be saved, but the video save
returns a nonzero status; the PNG fallback is not a successful MP4 save.
