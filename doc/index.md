title: Manual
---
# Background

## No external dependencies

The initial version relied on `zlib` for png compression and `freetype` for font
rendering. This was then replaced by [stb](https://github.com/nothings/stb) libraries.
The only remaining library included is `stb_truetype`.
