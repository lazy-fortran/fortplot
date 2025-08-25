title: Colored Contours
---

# Colored Contours

This example shows filled contour plots with customizable colormaps for visualizing 2D scalar fields.

## Files

- `colored_contours.f90` - Source code
- `gaussian_default.png/pdf/txt` - Gaussian with default colormap
- `ripple_jet.png/pdf/txt` - Ripple pattern with jet colormap
- `ripple_coolwarm.png/pdf/txt` - Ripple pattern with coolwarm colormap
- `ripple_inferno.png/pdf/txt` - Ripple pattern with inferno colormap
- `saddle_plasma.png/pdf/txt` - Saddle point with plasma colormap

## Running

```bash
make example ARGS="colored_contours"
```

## Features Demonstrated

- **Filled contours**: Continuous color gradients
- **Multiple colormaps**: viridis, jet, coolwarm, inferno, plasma
- **Custom levels**: Control contour density
- **Various functions**: Gaussian, ripple, saddle point patterns

## Available Colormaps

- `viridis` - Default, perceptually uniform
- `jet` - Classic rainbow colormap
- `coolwarm` - Blue to red diverging
- `inferno` - Black to yellow sequential
- `plasma` - Purple to yellow sequential

## Output

### 2D Gaussian with Default Colorblind-Safe Colormap

![gaussian_default.png](../../media/examples/colored_contours/gaussian_default.png)

ASCII output:
```

                 2D Gaussian - Default Colorblind-Safe Colormap
+--------------------------------------------------------------------------------+
|3.00                                                                            |
| .                                                                              |
|                                                                                |
| .                                                                              |
|2.00                                                                            |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
|1.00                                                                            |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
|0.                                                                              |
|                                                                                |
| .                                                                              |
| .                                                                              |
| .                                                                              |
|-1.00                                                                           |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .                                                                              |
|-2.00                                                                           |
| .                                                                              |
| .                                                                              |
| .                                                                              |
| .  .  .  .  .  .  .  .  .  .  .  .  .   .  .  .  .  .  .  .  .  .  .  .  .   . |
|-3.00       -2.00         -1.00        0            1.00          2.00     3.00 |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download PDF](../../media/examples/colored_contours/gaussian_default.pdf)

### Saddle Function with Plasma Colormap

![saddle_plasma.png](../../media/examples/colored_contours/saddle_plasma.png)

ASCII output:
```

                       Saddle Function - Plasma Colormap
+--------------------------------------------------------------------------------+
|                                                                                |
| =  +  +  *  #   #  #  %  %  %   %  %  @  %  %   %  %  %  #  #   *  *  +  +   = |
|2.00                                                                            |
| -  =  +  +  *   *  #  #  #  %   %  %  %  %  %   %  #  #  #  *   *  +  +  =   - |
| -  -  =  +  +   *  *  *  #  #   #  #  #  #  #   #  #  *  *  *   +  +  =  -   - |
| :  -  -  =  +   +  +  *  *  *   *  *  #  *  *   *  *  *  +  +   +  =  -  -   : |
| :  :  -  -  =   =  +  +  *  *   *  *  *  *  *   *  *  +  +  =   =  -  -  :   : |
| .  :  :  -  =   =  =  +  +  +   +  *  *  *  +   +  +  +  =  =   =  -  :  :   . |
|1.00.  :  -  -   =  =  =  +  +   +  +  +  +  +   +  +  =  =  =   -  -  :  .   . |
| .  .  :  :  -   -  =  =  =  +   +  +  +  +  +   +  =  =  =  -   -  :  :  .     |
| .  .  .  :  -   -  -  =  =  =   =  =  +  =  =   =  =  =  -  -   :  :  .  .     |
|                                                                                |
| .  .  .  :  :   -  -  =  =  =   =  =  =  =  =   =  =  =  -  -   :  :  .        |
| .     .  :  :   -  -  -  =  =   =  =  =  =  =   =  =  -  -  -   :  :  .        |
|0.     .  :  :   -  -  -  =  =   =  =  =  =  =   =  =  -  -  :   :  :  .        |
|       .  .  :   :  -  -  =  =   =  =  =  =  =   =  =  -  -  :   :  .  .        |
| .     .  :  :   -  -  -  =  =   =  =  =  =  =   =  =  -  -  :   :  :  .        |
| .     .  :  :   -  -  -  =  =   =  =  =  =  =   =  =  -  -  -   :  :  .        |
| .  .  .  :  :   -  -  =  =  =   =  =  =  =  =   =  =  =  -  -   :  :  .        |
| .  .  .  :  -   -  -  =  =  =   =  =  +  =  =   =  =  =  -  -   :  :  .  .     |
|-1.00                                                                           |
| .  .  :  :  -   -  =  =  =  +   +  +  +  +  +   +  =  =  =  -   -  :  :  .     |
| .  .  :  -  -   =  =  =  +  +   +  +  +  +  +   +  +  =  =  =   -  -  :  .   . |
| .  :  :  -  =   =  =  +  +  +   +  +  *  +  +   +  +  +  =  =   =  -  :  :   . |
| .  :  -  -  =   =  +  +  +  *   *  *  *  *  *   *  +  +  +  =   =  -  -  :   . |
| :  -  -  =  +   +  +  *  *  *   *  *  #  *  *   *  *  *  +  +   +  =  -  -   : |
|-2.00  =  +  +   *  *  *  #  #   #  #  #  #  #   #  #  *  *  *   +  +  =  -   - |
| -  =  +  +  *   *  #  #  #  %   %  %  %  %  %   %  #  #  #  *   *  +  +  =   - |
| =  +  +. * .# . # .#  %  %  %.  %. %. @ .%  %  .%  %  %. #. # . * .* .+  +   = |
|       -2.00           -1.00           0               1.00            2.00     |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download PDF](../../media/examples/colored_contours/saddle_plasma.pdf)

### Ripple Function with Jet Colormap

![ripple_jet.png](../../media/examples/colored_contours/ripple_jet.png)

ASCII output:
```

                         Ripple Function - Jet Colormap
+--------------------------------------------------------------------------------+
|2.00                                                                            |
| +   *   *   *   +   =   =   -   -   :   :   -   -   =   =   +   *   *   *    + |
|                                                                                |
|1*50 *   *   +   =   -   :   .   .   .   .   .   .   :   -   =   +   *   *    * |
| *   *   +   -   :   .                                   .   :   -   +   *    * |
| .                                                                              |
| *   +   -   :   .                                           .   :   -   +    * |
|1+00 =   :   .               .   .   :   :   .   .               .   :   =    + |
| .                                                                              |
| =   -   .               .   -   =   +   +   =   -   .               .   -    = |
|.500 :               .   -   +   #   #   #   #   +   -   .               :    = |
| -   .           .   -   +   #   %   %   %   %   #   +   -   .           .    - |
|                                                                                |
| -   .           .   =   #   %   @   %   %   @   %   #   =   .           .    - |
|0:   .           :   +   #   %   %   *   *   %   %   #   +   :           .    : |
|                                                                                |
| :   .           :   +   #   %   %   *   *   %   %   #   +   :           .    : |
| -   .           .   =   #   %   @   %   %   @   %   #   =   .           .    - |
|-.500                                                                           |
| -   .           .   -   +   #   %   %   %   %   #   +   -   .           .    - |
| =   :               .   -   +   #   #   #   #   +   -   .               :    = |
| =   -   .               .   -   =   +   +   =   -   .               .   -    = |
|-1.00                                                                           |
| +   =   :   .               .   .   :   :   .   .               .   :   =    + |
| *   +   -   :   .                                           .   :   -   +    * |
|-1.50                                                                           |
| *   *   +   -   :   .                                   .   :   -   +   *    * |
| *   *   *   +   =   -   :   .   .   .   .   .   .   :   -   =   +   *   *    * |
| +   *   *.  *.  + . = . =  .-  .-   :   :   -.  -.  = . = . +  .*  .*   *    + |
|-2.00    -1.50     -1.00     -.500     0         .500      1.00      1.50  2.00 |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download PDF](../../media/examples/colored_contours/ripple_jet.pdf)

### Ripple Function with Coolwarm Colormap

![ripple_coolwarm.png](../../media/examples/colored_contours/ripple_coolwarm.png)

ASCII output:
```

                      Ripple Function - Coolwarm Colormap
+--------------------------------------------------------------------------------+
|2.00                                                                            |
| +   *   *   *   +   =   =   -   -   :   :   -   -   =   =   +   *   *   *    + |
|                                                                                |
|1*50 *   *   +   =   -   :   .   .   .   .   .   .   :   -   =   +   *   *    * |
| *   *   +   -   :   .                                   .   :   -   +   *    * |
| .                                                                              |
| *   +   -   :   .                                           .   :   -   +    * |
|1+00 =   :   .               .   .   :   :   .   .               .   :   =    + |
| .                                                                              |
| =   -   .               .   -   =   +   +   =   -   .               .   -    = |
|.500 :               .   -   +   #   #   #   #   +   -   .               :    = |
| -   .           .   -   +   #   %   %   %   %   #   +   -   .           .    - |
|                                                                                |
| -   .           .   =   #   %   @   %   %   @   %   #   =   .           .    - |
|0:   .           :   +   #   %   %   *   *   %   %   #   +   :           .    : |
|                                                                                |
| :   .           :   +   #   %   %   *   *   %   %   #   +   :           .    : |
| -   .           .   =   #   %   @   %   %   @   %   #   =   .           .    - |
|-.500                                                                           |
| -   .           .   -   +   #   %   %   %   %   #   +   -   .           .    - |
| =   :               .   -   +   #   #   #   #   +   -   .               :    = |
| =   -   .               .   -   =   +   +   =   -   .               .   -    = |
|-1.00                                                                           |
| +   =   :   .               .   .   :   :   .   .               .   :   =    + |
| *   +   -   :   .                                           .   :   -   +    * |
|-1.50                                                                           |
| *   *   +   -   :   .                                   .   :   -   +   *    * |
| *   *   *   +   =   -   :   .   .   .   .   .   .   :   -   =   +   *   *    * |
| +   *   *.  *.  + . = . =  .-  .-   :   :   -.  -.  = . = . +  .*  .*   *    + |
|-2.00    -1.50     -1.00     -.500     0         .500      1.00      1.50  2.00 |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download PDF](../../media/examples/colored_contours/ripple_coolwarm.pdf)

### Ripple Function with Inferno Colormap

![ripple_inferno.png](../../media/examples/colored_contours/ripple_inferno.png)

ASCII output:
```

                       Ripple Function - Inferno Colormap
+--------------------------------------------------------------------------------+
|2.00                                                                            |
| +   *   *   *   +   =   =   -   -   :   :   -   -   =   =   +   *   *   *    + |
|                                                                                |
|1*50 *   *   +   =   -   :   .   .   .   .   .   .   :   -   =   +   *   *    * |
| *   *   +   -   :   .                                   .   :   -   +   *    * |
| .                                                                              |
| *   +   -   :   .                                           .   :   -   +    * |
|1+00 =   :   .               .   .   :   :   .   .               .   :   =    + |
| .                                                                              |
| =   -   .               .   -   =   +   +   =   -   .               .   -    = |
|.500 :               .   -   +   #   #   #   #   +   -   .               :    = |
| -   .           .   -   +   #   %   %   %   %   #   +   -   .           .    - |
|                                                                                |
| -   .           .   =   #   %   @   %   %   @   %   #   =   .           .    - |
|0:   .           :   +   #   %   %   *   *   %   %   #   +   :           .    : |
|                                                                                |
| :   .           :   +   #   %   %   *   *   %   %   #   +   :           .    : |
| -   .           .   =   #   %   @   %   %   @   %   #   =   .           .    - |
|-.500                                                                           |
| -   .           .   -   +   #   %   %   %   %   #   +   -   .           .    - |
| =   :               .   -   +   #   #   #   #   +   -   .               :    = |
| =   -   .               .   -   =   +   +   =   -   .               .   -    = |
|-1.00                                                                           |
| +   =   :   .               .   .   :   :   .   .               .   :   =    + |
| *   +   -   :   .                                           .   :   -   +    * |
|-1.50                                                                           |
| *   *   +   -   :   .                                   .   :   -   +   *    * |
| *   *   *   +   =   -   :   .   .   .   .   .   .   :   -   =   +   *   *    * |
| +   *   *.  *.  + . = . =  .-  .-   :   :   -.  -.  = . = . +  .*  .*   *    + |
|-2.00    -1.50     -1.00     -.500     0         .500      1.00      1.50  2.00 |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download PDF](../../media/examples/colored_contours/ripple_inferno.pdf)

