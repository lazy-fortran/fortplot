title: Annotation Demo
---

# Annotation Demo

Source: [annotation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/annotation_demo/annotation_demo.f90)

Add text annotations in data coordinates.

## Files

- `annotation_demo.f90` - Source code
- Generated media in `output/example/fortran/annotation_demo/`

## Running

```bash
make example ARGS="annotation_demo"
```

## Output

### Annotation Demo

![annotation_demo.png](../../media/examples/annotation_demo/annotation_demo.png)

ASCII output:
```

                     Scientific Data with Text Annotations
+--------------------------------------------------------------------------------+
|                                                                                |
| 0.6              -                                                             |
|             ----------                                                         |
| %         --          ---                   -- Damped sine: $sin(x)e^{-x/4}$ % |
| %%       --             ---                 -- Exponential: $e^{-x} - 0.5$ %%  |
| 0.4    --                  -                -- Quadratic: $0.1(x-3)^2 - 0.3$   |
| *  %  -                     --                                           %     |
|  =  %%                        --                                       %%      |
|   = - %                        -                                      %        |
| 0.2=   %%                       --                                  %%         |
| *  ==    %%                       --                              %%           |
|   -  =    %%                       --                            %%            |
|  -    =     %%                       --                        %%              |
| 0.0    =     %%%                      --                     %%%               |
| *       ==      %%                      --                 %%                  |
|           ==      %%%                     -             %%%                --- |
|             ==       %%                    ---        %%                ---    |
| -0.2         ===       %%%%                  ---  %%%%               ---       |
| *               ===        %%%%%             %%%%%-             -----          |
|                    ===         %%%%%%%%%%%%%%%     -------------               |
|                       ======                                                   |
| -0.4                       =========                                           |
| *  *  *  *  *  *  *  *  *  *  *  *  *===*==*==*==*==*==*==*==*==*==*==*==*===* |
|0           1             2            3            4             5           6 |
+--------------------------------------------------------------------------------+
                            Independent Variable (x)
Dependent Variable (y)
```

[Download ASCII](../../media/examples/annotation_demo/annotation_demo.txt)

[Download PDF](../../media/examples/annotation_demo/annotation_demo.pdf)

