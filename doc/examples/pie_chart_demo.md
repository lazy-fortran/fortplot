title: Pie Chart Demo
---

# Pie Chart Demo

Source: [pie_chart_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/pie_chart_demo/pie_chart_demo.f90)

Build pie charts using both the stateful API and `figure_t` (exploded wedges, `autopct`, and start angles).

Outputs are written in PNG, PDF, and ASCII formats.

Run it with:

Generated media is written to `output/example/fortran/pie_chart_demo/`.

## Files

- `pie_chart_demo.f90` - Source code
- Generated media in `output/example/fortran/pie_chart_demo/`

## Running

```bash
make example ARGS="pie_chart_demo"
```

## Output

### Oo Energy

![oo_energy.png](../../media/examples/pie_chart_demo/oo_energy.png)

ASCII output:
```

                           Clean energy capacity mix
+--------------------------------------------------------------------------------+
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                    - Solar     |
|                                 ----------###      Storage         = Wind      |
|                             ------------- ######                   % Hydro     |
|                          ---Solar--------#########                 # Storage   |
|                         ---------------- ######12%##                           |
|                        -----------42%---###########%%                          |
|                       ----------------- ########%%%%%                          |
|                      ------------------######%%%%%%%%%                         |
|                      ----------------- ###%%%%%%%%18%%   Hydro                 |
|                      -----------------=%%%%%%%%%%%%%%%%                        |
|                      --------------  =====%%%%%%%%%%%%                         |
|                       -----------  ==========%%%%%%%%%                         |
|                       ---------  ==========28%==%%%%%                          |
|                        ------  =====================%                          |
|                          --  ======================                            |
|                            =======================                             |
|                               ==============Wind                               |
|                                   =========                                    |
|                                                                                |
|                                                                                |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/pie_chart_demo/oo_energy.txt)

[Download PDF](../../media/examples/pie_chart_demo/oo_energy.pdf)

### Stateful Sales

![stateful_sales.png](../../media/examples/pie_chart_demo/stateful_sales.png)

ASCII output:
```

                             Regional revenue share
+--------------------------------------------------------------------------------+
|                                                                                |
|                                                                                |
|                                                                                |
|                                                  Online             - North    |
|                                   -----@@@@                         = East     |
|                              ----------@@@@@@@@@                    % South    |
|                            ---North----@@@@@@@@@@@                  # West     |
|                          --------------@@@@@@@15.0%@#               @ Online   |
|                         -----------30.0%@@@@@@@@@ ####                         |
|                        ----------------@@@@@@@ ########   West                 |
|                        ----------------@@@@@ ######15.0%                       |
|                       -----------------@@ ##############                       |
|                       -----------------#################                       |
|                       ---------  ======%%%%%%% #########                       |
|                        -    ===========%%%%%%%%18.0%%%##                       |
|                        ==============22.0%%%%%%%%%%%%%                         |
|                         ================%%%%%%%%%%%%%                          |
|                         ================%%%%%%%%%%% South                      |
|                           ========East==%%%%%%%%%%                             |
|                             ============%%%%%%%                                |
|                                 =========%%%                                   |
|                                                                                |
|                                                                                |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/pie_chart_demo/stateful_sales.txt)

[Download PDF](../../media/examples/pie_chart_demo/stateful_sales.pdf)

