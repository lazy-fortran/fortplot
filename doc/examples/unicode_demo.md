title: Unicode Demo
---

# Unicode Demo

Source: [unicode_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/unicode_demo/unicode_demo.f90)

Unicode symbols in labels and titles.

## Files

- `unicode_demo.f90` - Source code
- Generated media in `output/example/fortran/unicode_demo/`

## Running

```bash
make example ARGS="unicode_demo"
```

## Output

### Math Examples

![math_examples.png](../../media/examples/unicode_demo/math_examples.png)

ASCII output:
```

        Common Physics: E = mcU+00B2, Delta E = hnu, F = q(E + vU+00D7B)
+--------------------------------------------------------------------------------+
|                                                                                |
| 0.50                     =                                                     |
|                      ==== ======                                               |
|            -- Gaussian: \rho(\xi) = e^{-\xiU+00B2/2\sigmaU+00B2}/\sqrt{2\pi\sig|
|            -- Modified \Gamma: f(\xi) = \xiU+00B2 e^{-\xi}                     |
| 0.40            =                    ==                                        |
|               ==                       ===                                     |
| ----         =                            ==                                   |
| *  ---       =                              ==                                 |
|       -     =                                ===                               |
| 0.30   --  =                                    ==                             |
|          -=                                       ===                          |
|           =-                                        ===                        |
|          =  --                                         ==                      |
| 0.20    =    -                                           ===                   |
| *      =      --                                           ====                |
|       =         --                                             ===             |
|       =          --                                               ====         |
|      =             ---                                                ====     |
| 0.10=                --                                                   ==== |
|    =                   ---                                                     |
|   ==                      ----                                                 |
| *= *  *  *  *  *  *  *  *  *  *--*--*---*--*--*--*--*--*--*--*--*--*--*--*---* |
|0           1             2            3            4             5           6 |
+--------------------------------------------------------------------------------+
                                  Parameter xi
Observable Theta
```

[Download ASCII](../../media/examples/unicode_demo/math_examples.txt)

[Download PDF](../../media/examples/unicode_demo/math_examples.pdf)

### Unicode Demo

![unicode_demo.png](../../media/examples/unicode_demo/unicode_demo.png)

ASCII output:
```

       Wave Functions: psi(omega t) = A e^{-lambda t} sin(omega t + phi)
+--------------------------------------------------------------------------------+
| 1.0                                                                            |
| *                                                                              |
| ==                                                                             |
|   =                          -- \alpha damped: sin(\omega t)e^{-\lambda\tau}   |
| *  =   ----                  -- \beta damped: cos(\omega t)e^{-\mu\tau}        |
|    = --   --                 -- \gamma oscillation: sin(2\omega t)             |
| 0.5 =       --                                                                 |
|    - =        -                      ==                                        |
| *  %%%%       -       %%%%         ==  ===%%%                %%%               |
|   %%   %       -     %    %      ==      %=  %              %   %              |
| *      %        -    %         ==       %  ==%----         %     %          == |
|  %     =         -         %   =       %  ---=%   ---      %      %    =====   |
| %       %        -  %       % =         --   ==%    ---               =        |
| 0.0      %        -%         =        %-       =       -- %       % ==         |
| %                  -        %         %         %=       %--      =%      ---% |
|           %       % -       =%      --           %         ---- ===   ----  %  |
| *         %      %   -     =       - %            ==    %      =----%--        |
|            %          -   =   %  -- %            %  ==  %    ==      %     %   |
|             =    %     --=     %-  %              %   =%=====         %   %    |
| *           %% %%       =-- ---%%  %               %  %               %  %     |
| -0.5          %        =   --    %%                 %%                 %%%     |
|               ==     ==                                                        |
| ** ** ** ** ** **=**=** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** * * |
|0           2           4            6            8            10          12   |
+--------------------------------------------------------------------------------+
                   Time tau (normalized: tau = omega t / 2pi)
Amplitude Psi (V)
```

[Download ASCII](../../media/examples/unicode_demo/unicode_demo.txt)

[Download PDF](../../media/examples/unicode_demo/unicode_demo.pdf)

