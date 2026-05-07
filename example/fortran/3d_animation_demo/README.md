title: 3D Animation

Animate a rotating 3D Lissajous curve. The same `FuncAnimation` plus
`save_animation` pipeline writes both MP4 (raster backend rendered through
ffmpeg) and a `.txt` ASCII frame stream replayable with `fortplot_play_ascii`.

```bash
fpm run --example --target 3d_animation_demo
fpm run --target fortplot_play_ascii -- output/example/fortran/3d_animation_demo/animation.txt --fps 24 --loop
```
