title: Contour Demo
---

# Contour Demo

Source: [contour_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/contour_demo/contour_demo.f90)

Contour line plots from gridded data.

## Files

- `contour_demo.f90` - Source code
- Generated media in `output/example/fortran/contour_demo/`

## Running

```bash
make example ARGS="contour_demo"
```

## Output

### Contour Gaussian

![contour_gaussian.png](../../media/examples/contour_demo/contour_gaussian.png)

ASCII output:
```

                              2D Gaussian Function
+--------------------------------------------------------------------------------+
| 3                                                                              |
| *                                                                              |
| *                                                                              |
| 2                                                                              |
| *                                                                              |
| *                                                                              |
| *                                                                              |
| 1                                 -  - -  -                                    |
| *                         - -- -  -  - -  -  - -- -                            |
| *                      - - - - -  -  - -  -  - - - - -                         |
| *                     -   -  ---- - -- -- - ----  -   -                        |
| 0                    -  -  -  - -  - - - -  - -  -  -  -                       |
| *                   -   -  - - -  -       -  - - -  -   -                      |
| *                    -  -  -  - -  - - - -  - -  -  -  -                       |
| *                     -   -  ---- - -- -- - ----  -   -                        |
| -1                     - - - - -  -  - -  -  - - - - -                         |
| *                         - -- -  -  - -  -  - -- -                            |
| *                                 -  - -  -                                    |
| *                                                                              |
| -2                                                                             |
| *                                                                              |
| *                                                                              |
| *  *  *  *  *  *  *  *  *  *  *  *  *   *  *  *  *  *  *  *  *  *  *  *  *   * |
|-3          -2            -1           0            1             2           3 |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/contour_demo/contour_gaussian.txt)

[Download PDF](../../media/examples/contour_demo/contour_gaussian.pdf)

### Mixed Plot

![mixed_plot.png](../../media/examples/contour_demo/mixed_plot.png)

ASCII output:
```

                           Mixed Plot: Contour + Line
+--------------------------------------------------------------------------------+
| 3                                                                              |
| *   -    -                                                         -    -    - |
| *--  - -  -- -                                                 - --  - -  --   |
| 2  - --  ---   - --                                       -- -   ---  -- -  -- |
| * - -- - --  - - -   ---  --                     --  ---   - - -  -- - -- - -- |
| *--   --   - --   - --       - -  -  - -  -  - -       -- -   -- -   --   --   |
| *   --   --    - -     - --  -                 -  -- -     - -    --   --      |
| 1     --   - -    -- -        --  -  - -  -  --        - --    - -   --        |
| *      --      -      -- -                         - --      -      --         |
| *         --    - -       -- - = ==  = =  == = - --       - -    --            |
| *           -     --       = ==-             -== =       --     -              |
| 0           -      - = = ==      ---     ---      == = = -      -              |
| * =  = =  = -= =  = -                - -                - =  = =- =  = =  =  = |
| *           -      -             ---     ---             -      -              |
| *           -     --          --             --          --     -              |
| -1        --    - -       -- -                 - --       - -    --            |
| *      --      -      -- -                         - --      -      --         |
| *     --   - -    -- -        --  -  - -  -  --        - --    - -   --        |
| *   --   --    - -     - --  -                 -  -- -     - -    --   --      |
| -2-   --   - --   - --       - -  -  - -  -  - -       -- -   -- -   --   --   |
| * - -- - --  - - -   ---  --                     --  ---   - - -  -- - -- - -- |
| *  - --  ---   - --                                       -- -   ---  -- -  -- |
| *--*--*- *--*- *  *  *  *  *  *  *  *   *  *  *  *  *  *  *  * -*--* -*--*-- * |
|-3          -2            -1           0            1             2           3 |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/contour_demo/mixed_plot.txt)

[Download PDF](../../media/examples/contour_demo/mixed_plot.pdf)

