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
|0.5           #                              #      #                           |
|     #        #                                     #                          |
|    #         #                            #         #                         |
|0.0-#--------#----#----------------------------#----#-----------               |
|             #  #                              #  #                            |
|-0.5          ##                                ##                             |
|                                                                               |
|-1.0+--------+----------+----------+----------+----------+--------+           |
      0        2          4          6          8         10                   |
+--------------------------------------------------------------------------------+
                                       x
sin(x)
```

### Multi-line Plot Preview
```
                           Sine and Cosine Functions
+--------------------------------------------------------------------------------+
|1.0     *                                      *                - sin(x)       |
|      *   *                                  *   *            - cos(x)         |
|0.5  *     * o                              *     * o                          |
|     *       o   o                        *       o   o                       |
|0.0---------*----o---o----------------------------o----*-----------            |
|             *  *     o                              o  *                      |
|-0.5          **       o                              o  **                    |
|                        o                            o                         |
|-1.0+--------+----------o----------+----------+------o----+--------+           |
      0        2          4          6          8         10                   |
+--------------------------------------------------------------------------------+
                                       x
y
```

### Heatmap Preview
```
                       Basic Pcolormesh - Linear Gradient
+--------------------------------------------------------------------------------+
|1.20    @               @              @              @               @         |
|        %               %              %              %               %         |
|0.90    #               #              #              #               #         |
|        *               *              *              *               *         |
|0.60    +               +              +              +               +         |
|        -               -              -              -               -         |
|0.30    =               =              =              =               =         |
|        :               :              :              :               :         |
|0.00+--------+----------+----------+----------+----------+--------+            |
     0.0     0.4        0.8        1.2        1.6        2.0                   |
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