title: Manual
---
# Background

## No external dependencies

The initial version relied on `zlib` for png compression and `freetype` for font
rendering. This was then replaced by [stb](https://github.com/nothings/stb) libraries.
The only remaining library included is `stb_truetype`.

# Documentation

## [Examples Gallery](example/index.md)
Complete working examples with source code and generated plots.

## Developer Guides
- [Testing Guide](testing_guide.md) - Testing procedures and warning control
- [Windows CI Performance](windows_ci_performance.md) - Windows CI optimization and troubleshooting
- [Warning System](warning_system.md) - Environment variable-based warning suppression
- [Security](security.md) - Security features and compliance
- [Unicode Support](unicode_support.md) - Unicode and Greek letter support

## Plot Type Guides
- [Surface Plot Guide](surface_plot_guide.md) - 3D surface plotting
- [Scatter Plot Guide](scatter_plot_guide.md) - Basic scatter plots
- [Scatter Tutorial](scatter_tutorial.md) - Interactive scatter tutorial
- [Scatter Advanced](scatter_advanced.md) - Advanced scatter features
- [MPEG Validation](mpeg_validation.md) - Animation format validation
