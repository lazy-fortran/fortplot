title: Animation Example
---

# Animation Example

Creates animated plots and saves to video files with cross-platform support.

## Files

- `save_animation_demo.f90` - Animation saving example with error handling
- `wave_animation.mp4` - Example output video

## Quick Start

```bash
# Install FFmpeg first
# Windows: choco install ffmpeg
# Linux: sudo apt install ffmpeg  
# macOS: brew install ffmpeg

# Run example
make example ARGS="save_animation_demo"
```

## Windows Support (Issue #189 Fixed)

Windows FFmpeg animation export now works correctly with:
- Binary pipe handling for PNG data
- Automatic path escaping for special characters  
- Windows-specific error reporting

## Platform-Specific Setup

### Windows
```powershell
# Install FFmpeg
choco install ffmpeg

# Verify installation
ffmpeg -version
```

### Linux/macOS  
```bash
# Ubuntu/Debian
sudo apt install ffmpeg

# macOS
brew install ffmpeg
```

## Complete Documentation

For comprehensive documentation including:
- Cross-platform installation guides  
- Windows-specific troubleshooting
- Error handling examples
- Path and filename handling

**Windows Users**: See [Windows FFmpeg Setup Guide](../../../doc/windows_ffmpeg_setup.md) for detailed installation and troubleshooting.