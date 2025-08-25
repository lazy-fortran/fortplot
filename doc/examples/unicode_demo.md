title: Unicode Demo
---

# Unicode Demo

This example demonstrates mathematical symbols and Unicode support in plots.

## Files

- `unicode_demo.f90` - Source code
- `unicode_demo.png/pdf/txt` - Greek letters and math symbols
- `math_examples.png/pdf/txt` - Mathematical expressions

## Running

```bash
make example ARGS="unicode_demo"
```

## Features Demonstrated

- **Greek letters**: α, β, γ, δ, π, θ, φ, ψ, ω
- **Mathematical symbols**: ∞, ∑, ∏, ∫, √, ∂
- **Subscripts/Superscripts**: Via LaTeX-like syntax
- **Special characters**: °, ±, ≤, ≥, ≠

## Unicode Support

### Direct Unicode
- Use actual Unicode characters in strings
- Full UTF-8 support in PNG and PDF backends
- ASCII backend shows approximations

### LaTeX-like Commands
- `\alpha` → α
- `\beta` → β
- `\pi` → π
- `\infty` → ∞
- `\sum` → ∑

## Output

### Unicode Demo

![unicode_demo.png](../../media/examples/unicode_demo.png)

ASCII output:
```

             Wave Functions: ψ(ω t) = A e^{-λ t} sin(ω t + φ)
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                              |
| **                    -- α damped: si                                         |
|   *                   -- β damped: cos                                        |
|    *                  -- γ oscillatio                                         |
| .  *   ****                                                                    |
|       *   **                                                                   |
|     **      *                                                                  |
|     **       *                                                                 |
| .  **         *         *           ****  **                  **               |
|    * **       *       ****         *    *** *                * *               |
|    *   *       *     *           **      **  *              *   *              |
|   *             *         *     *          * *             *     *             |
| .*     *             *     *   *        *  ********                      ***** |
|                  *             *       *  *  **    **      *      *    ***     |
| *       *        *  *       * *         **   * *     **               *        |
|          *        *          *        **      **       ** *       * **         |
| *        *         *        *         *         *       **         *        ** |
|           *                          **          *       ***      **     ***   |
|                   * *       **      *            *          *** **    ****  *  |
|           *          *     *       * *            **    *      ********        |
| .          *     *   *    *   *   **             *  *   *     **           *   |
|            *     *    ** *     * *  *               ***    ***       *         |
|             *           *      **  *              *    *****          *   *    |
|              *  *       ***  ***   *               *  *               *  *     |
| .             **       *   **   ***                 **                 ***     |
|               *       *                                                        |
|                *     *                                                         |
| .. .. .. .. .. .******. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . . |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/unicode_demo.pdf)

### Unicode Demo
![Unicode Demo](../../media/examples/unicode_demo.png)

### Math Examples
![Math Examples](../../media/examples/math_examples.png)
