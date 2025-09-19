title: Disconnected Lines
---

# Disconnected Lines

Source: [disconnected_lines.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/disconnected_lines/disconnected_lines.f90)

This example demonstrates how to create plots with disconnected line segments by using NaN (Not-a-Number) values as separators.

- **Disconnected line segments**: Use NaN values to break lines into separate segments
- **Mixed plot types**: Combine continuous lines with individual markers
- **Multiple line styles**: Different markers and line styles in the same plot
- **Multi-format output**: Generate PNG, PDF, and ASCII versions

The example generates three output files:
- `output/example/fortran/disconnected_lines/disconnected_lines.png`
- `output/example/fortran/disconnected_lines/disconnected_lines.pdf`
- `output/example/fortran/disconnected_lines/disconnected_lines.txt`

The plot shows:
1. **First segment**: Sine wave from 0 to π (connected with lines and circles)
2. **NaN separator**: Creates a break in the line
3. **Second segment**: Cosine wave from π to 2π (connected separately)
4. **NaN separator**: Another break
5. **Third segment**: Single horizontal point at y=0.5
6. **Independent point**: Red square marker at (8, -0.5)

use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
real(8) :: nan
nan = ieee_value(nan, ieee_quiet_nan)

! Add NaN to break the line
x(5) = nan
y(5) = nan

- Plot multiple data series that shouldn't be connected
- Create gaps in time series data
- Highlight missing or invalid data points
- Separate logical segments within the same dataset

- NaN values are ignored during line drawing but preserved in data
- Both X and Y coordinates should be set to NaN for clean breaks
- Markers are still drawn at valid data points before/after NaN
- Legend entries apply to entire data series including disconnected segments

## Files

- `disconnected_lines.f90` - Source code
- Run the example to populate `output/example/fortran/disconnected_lines/`

## Running

```bash
make example ARGS="disconnected_lines"
```

## Output

Run this example to generate plots and other media assets.

