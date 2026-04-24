title: Polar Demo
---

# Polar Demo

Source: [polar_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/polar_demo/polar_demo.f90)

Demonstrates fortplot's polar plotting API with custom colors, linestyles, and markers.

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
|                                                                                |
|                                                                                |
|                                                                                |
|                                                           o primary rose       |
|                                                           # secondary petals   |
|                                       0 deg                                    |
|                            30 deg                330 deg                       |
|                                  . ---#--- .                                   |
|                    60 deg ..%#%#%###%### ###-#.# ..      300 deg               |
|                        ...#########################%%.                         |
|                     ..# ## ####################### ##%#%%                      |
|                 90 deg##  ##%## ############# ##-##  ##%%.  270 deg            |
|                    #  #%%#%  #  #  #  #  #  #  #  %#%%#  #                     |
|                    .%%##  ## ## ############# ##%##  ##-..                     |
|                    120 deg ####################### ## #-.240 deg               |
|                       --%%#########################----                        |
|                           .150 deg## ###%###%#%#%210 deg                       |
|                                  . .. 180 deg                                  |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/polar_demo/polar_demo.txt)

[Download PDF](../../media/examples/polar_demo/polar_demo.pdf)

