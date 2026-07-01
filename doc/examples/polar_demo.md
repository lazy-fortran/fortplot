title: Polar Demo
---

# Polar Demo

Source: [polar_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/polar_demo/polar_demo.f90)

Demonstrates fortplot's polar plotting API with custom colors, linestyles, and markers.

```sh
make example ARGS="polar_demo"
```

Generated outputs are written to `output/example/fortran/polar_demo/` as PNG and text summaries.

## Files

- `polar_demo.f90` - Source code
- Generated media in `output/example/fortran/polar_demo/`

## Running

```bash
make example ARGS="polar_demo"
```

## Output

### Polar Demo

![polar_demo.png](../../media/examples/polar_demo/polar_demo.png)

ASCII output:
```

               polar_demo: custom colors, markers, and linestyles
+--------------------------------------------------------------------------------+
|                                       90deg                                    |
|                                                                                |
|                       135deg                          45deg                    |
|                               # ## ## # ## ## #           o primary rose       |
|                            # #%%%%##%### ##    # #        # secondary petals   |
|                          ##  %## #  %%%    # ##   ##                           |
|                         #  ###   ###########   ###  #                          |
|                       ## ##  %###---#####%  ###%%%## ##                        |
|                      #  #   ## -#### --- ####% ##  1.0%#                       |
|                     #  ##  # %##  #########  #0.6#  #1.2#                      |
|                    ##  #  #  ##- ##  ###-0.2--##0.8  #% ##                     |
|                 180deg#  ##%%#  #  ### ### 0.4 #- ## %#  #  0deg               |
|                    #  # %#%  #  #- #  #  #  #  #--%#% #  #                     |
|                    #  #% ##  #  #  ### ###  #  #%%##  #  #                     |
|                    ## %#  #  ##- ##  ###--##--##  #  #  ##                     |
|                     # %##  #  ##  #########  ##% #  ##  #                      |
|                      #%%#%  ## %#### --- ####  ##   #  #                       |
|                       ## ##%%%###--%#####   ###%  ## ##                        |
|                         #  ###   ###########   ###  #                          |
|                          ##   ## #    %%%  # ##%  ##                           |
|                       225deg #    ## ###%##%%%%# #    315deg                   |
|                               # ## ## # ## ## #                                |
|                                                                                |
|                                       270deg                                   |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/polar_demo/polar_demo.txt)

[Download PDF](../../media/examples/polar_demo/polar_demo.pdf)

