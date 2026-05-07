title: Animation

Generate an MP4 animation from a sequence of frames.

`save_animation` also accepts a `.txt` filename to emit frames as ASCII renderings
delimited by `=== Frame N ===` headers. Replay them in the terminal with the
`fortplot_play_ascii` CLI app:

```bash
fpm run --target fortplot_play_ascii -- output.txt --fps 24 --loop
```
