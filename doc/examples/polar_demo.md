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
| 90deg                                                                          |
| 270deg                                                                         |
|                       135deg                          45deg                    |
|                               # ## ## # ## ## #         -o- primary rose       |
|                            # # ooooo           # #      -#- secondary petals   |
|                          ##  ooo   oooo           ##                           |
|                         #    o        oo            #                          |
|                       ##     o   ###   ooo           ##                        |
|                      #       o ###  #### ooo      1.0  #                       |
|                     #        o #             0.6    1.2 #                      |
|                    ##        oo#        0.2    0.8    o ##                     |
|                 180deg    oooo ##         0.4        oo  #  0deg               |
|                    #    ooo     ##               #ooo    #                     |
|                    #  oo       ##              oooo      #                     |
|                    ## o        #        ######oo        ##                     |
|                     # o        #       ##      o        #                      |
|                      # ooooooooooooo####       o       #                       |
|                       ##         ##ooo         o     ##                        |
|                         #            oo        o    #                          |
|                          ##           oooo   ooo  ##                           |
|                       225deg #           ooooo # #    315deg                   |
|                               # ## ## # ## ## #                                |
|                                                                                |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/polar_demo/polar_demo.txt)

[Download PDF](../../media/examples/polar_demo/polar_demo.pdf)

