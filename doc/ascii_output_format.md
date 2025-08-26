title: ASCII Output Format
---

# ASCII Output Format Reference

## Overview

fortplot generates ASCII text output (.txt files) for terminal-based visualization. This format is ideal for:
- Remote SSH sessions without GUI capability
- CI/CD pipeline debugging
- Terminal-based data exploration
- Documentation in text-only environments

## Character Mapping

### Line Plots
```
Symbol | Usage
-------|-------
*      | Primary data points
#      | Secondary data points
%      | Tertiary data points
.      | Grid markers
+      | Axis borders
|      | Vertical boundaries
-      | Horizontal lines
:      | Legend separators
```

### Heatmaps and 2D Plots
```
Character | Density/Intensity
----------|------------------
          | Lowest values (space)
.         | Low values
+         | Medium-low values
*         | Medium values
#         | Medium-high values
%         | High values
@         | Highest values
```

## Format Structure

Every ASCII output follows this structure:

```
                                [Plot Title]
+--------------------------------------------------------------------------------+
|                                                                                |
|                           [Plot Content Area]                                 |
|                                                                                |
+--------------------------------------------------------------------------------+
                                    [X-axis label]
[Y-axis label]
```

## Viewing Full Output

Complete ASCII outputs are available in the corresponding .txt files:

- **Example outputs**: Located in `output/example/fortran/[example_name]/`
- **File naming**: `[plot_name].txt` (matches PNG/PDF names)
- **Generation**: Run `make example ARGS="[example_name]"` to generate

## Common Patterns

### Simple Line Plot Preview
```
                                Simple Sine Wave
+--------------------------------------------------------------------------------+
| *        #                                                                     |
|        ## ##                                  #####                            |
|       ##    #                                #     #                           |
|              #                              #      #                           |
| *    #        #                                     #                          |
|     #         #                            #         #                         |
|    #                                      #                                    |
| ... [truncated - see full output in .txt file] ...                            |
+--------------------------------------------------------------------------------+
                                       x
sin(x)
```

### Multi-line Plot Preview
```
                           Sine and Cosine Functions
+--------------------------------------------------------------------------------+
| *                                                                              |
| *   ****               ***   ***               ***   ****- sin(x)       ***    |
|  *     *              *   * *   *                 * *   *- cos(x)      *   *   |
|   **    *                   *                 *    *     *            *      * |
| ... [truncated - see full output in .txt file] ...                            |
+--------------------------------------------------------------------------------+
                                       x
y
```

### Heatmap Preview
```
                       Basic Pcolormesh - Linear Gradient
+--------------------------------------------------------------------------------+
|        +               *              #              %               @         |
|        +               *              #              %               @         |
|        +               *              #              %               @         |
| ... [truncated - see full output in .txt file] ...                            |
+--------------------------------------------------------------------------------+
                                       x
```

## Backend Integration

ASCII output is automatically available for all plot types:

```fortran
! Generate all formats including ASCII
call fig%savefig('plot.png')  ! PNG
call fig%savefig('plot.pdf')  ! PDF  
call fig%savefig('plot.txt')  ! ASCII
```

## Terminal Compatibility

- **UTF-8 Support**: Modern terminals render Unicode characters correctly
- **Character Width**: Fixed-width fonts recommended for proper alignment
- **Screen Size**: Optimized for 80-character terminal width
- **Fallback**: Basic ASCII characters when Unicode unavailable