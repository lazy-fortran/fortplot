title: Basic Plots
---

# Basic Plots

Source: [example/fortran/basic_plots/basic_plots.f90](../../example/fortran/basic_plots/basic_plots.f90)

This example demonstrates the fundamental plotting capabilities of fortplotlib using both the simple functional API and the object-oriented interface.

## Files

- `basic_plots.f90` - Source code
- `simple_plot.png/pdf/txt` - Simple sine wave outputs
- `multi_line.png/pdf/txt` - Multi-line plot outputs

## Running

```bash
make example ARGS="basic_plots"
```

## Features Demonstrated

- **Functional API**: Simple, matplotlib-like interface with global figure management
- **Object-Oriented API**: More control through `figure_t` type
- **Multiple output formats**: PNG, PDF, and ASCII text
- **Line labeling**: Automatic legend generation
- **Axis labeling**: Clear axis titles and labels

## Output

### Simple Plot

![simple_plot.png](../../output/example/fortran/basic_plots/simple_plot.png)

ASCII output preview:
```
                                Simple Sine Wave
+--------------------------------------------------------------------------------+
|1.0     *                                      *                               |
|      *   *                                  *   *                             |
|0.5  *     *                                *     *                            |
|     *       *                            *       *                           |
|0.0---------*----*----------------------------*----*-----------                |
|             *  *                              *  *                            |
|-0.5          **                                **                             |
|                                                                               |
|-1.0+--------+----------+----------+----------+----------+--------+           |
      0        2          4          6          8         10                   |
+--------------------------------------------------------------------------------+
                                       x
sin(x)
```

> **Full ASCII Output**: [Download simple_plot.txt](../../output/example/fortran/basic_plots/simple_plot.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../output/example/fortran/basic_plots/simple_plot.pdf)

### Multi Line

![multi_line.png](../../output/example/fortran/basic_plots/multi_line.png)

ASCII output preview:
```
                           Sine and Cosine Functions
+--------------------------------------------------------------------------------+
|1.0     *                                      *                - sin(x)       |
|      *   *                                  *   *            - cos(x)         |
|0.5  *     *    o                          *     *    o                        |
|     *       *o   o                      *       *o   o                       |
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

> **Full ASCII Output**: [Download multi_line.txt](../../output/example/fortran/basic_plots/multi_line.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../output/example/fortran/basic_plots/multi_line.pdf)