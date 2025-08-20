title: Text Annotation Demonstration

# Text Annotation Demo

This example demonstrates fortplot's comprehensive text annotation system, showcasing all the essential features needed for creating publication-quality scientific figures with explanatory text and labels.

## Overview

Text annotations are essential for creating clear, informative plots that communicate scientific findings effectively. This demo shows how to use fortplot's annotation system to:

- Label important data points and features
- Add explanatory text and mathematical expressions
- Create professional figure layouts with proper typography
- Use different coordinate systems for flexible positioning

## Features Demonstrated

### Basic Text Annotations
- **Simple text placement**: Position text at specific data coordinates
- **Font size control**: Range from 8pt footnotes to 16pt titles
- **Text alignment**: Left, center, and right alignment options
- **Coordinate systems**: Data, figure, and axis coordinate positioning

### Advanced Annotation Features
- **Arrow annotations**: Point to specific data points with labeled arrows
- **Text rotation**: Vertical and angled text for space-efficient labeling
- **Background boxes**: Highlight important text with background colors
- **Mathematical expressions**: Unicode symbols for equations and formulas

### Professional Typography
- **Multiple font sizes**: Hierarchical text sizing for clear information structure
- **Alignment control**: Precise text positioning for clean layouts
- **Coordinate flexibility**: Choose the most appropriate coordinate system
- **Backend consistency**: Identical appearance across PNG, PDF, and ASCII output

## Code Structure

The demonstration follows a systematic approach:

1. **Data Generation**: Creates sample scientific data with interesting features
2. **Basic Plotting**: Sets up the main plot with multiple data series
3. **Annotation Examples**: Demonstrates each annotation feature systematically
4. **Output Generation**: Saves to all supported formats

## API Usage Examples

### Basic Text Placement
```fortran
! Simple text at data coordinates
call fig%text(x_pos, y_pos, "Label Text", coord_type=COORD_DATA)
```

### Arrow Annotations
```fortran
! Point to a specific data point
call fig%annotate("Maximum value", &
                  xy=[x_max, y_max], &          ! Arrow tip
                  xytext=[x_text, y_text], &    ! Text position
                  xy_coord_type=COORD_DATA)
```

### Typography Control
```fortran
! Customized text appearance
call fig%text(x, y, "Important Note", &
              font_size=14.0_wp, &
              alignment="center", &
              rotation=45.0_wp, &
              has_bbox=.true.)
```

### Coordinate Systems
```fortran
! Data coordinates (relative to plot data)
call fig%text(2.5, 1.0, "Data point", coord_type=COORD_DATA)

! Figure coordinates (0-1, relative to entire figure)
call fig%text(0.5, 0.95, "Figure title", coord_type=COORD_FIGURE)

! Axis coordinates (0-1, relative to plot area)
call fig%text(0.98, 0.02, "Corner label", coord_type=COORD_AXIS)
```

## Scientific Applications

This annotation system is designed for real-world scientific applications:

- **Research Papers**: Publication-quality figures with proper annotations
- **Conference Presentations**: Clear, readable plots for presentations
- **Educational Materials**: Well-labeled diagrams for teaching
- **Technical Reports**: Professional documentation with annotated data
- **Data Analysis**: Interactive exploration with on-the-fly annotations

## Performance

The annotation system is optimized for practical use:
- **Lightweight**: Minimal memory overhead per annotation
- **Fast rendering**: Efficient text positioning algorithms
- **Scalable**: Handles dozens of annotations without performance impact
- **Backend-optimized**: Each backend uses its optimal text rendering

## Files

- `annotation_demo.f90` - Complete demonstration program
- `README.md` - This documentation

## Running

```bash
# Compile and run the example
make example ARGS="annotation_demo"

# Or run directly with fpm
fpm run --example annotation_demo
```

## Output Examples

The example generates output in three formats:

- **PNG**: High-quality raster graphics with antialiased text
- **PDF**: Vector graphics for perfect scaling and printing
- **ASCII**: Terminal-friendly text representation

Each format demonstrates the same annotation features with backend-appropriate rendering optimizations.

## Integration with Other Features

Text annotations work seamlessly with all other fortplot features:
- **Legends**: Annotations complement automatic legend generation
- **Scaling**: Support for linear, logarithmic, and symmetric log scales
- **Multiple plots**: Add annotations to any subplot or plot combination
- **Styling**: Integrate with line styles, colors, and markers

The annotation system follows fortplot's design philosophy of providing a matplotlib-compatible API with robust error handling and performance optimization for scientific computing workflows.