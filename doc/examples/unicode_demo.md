title: Unicode Demo
---

# Unicode Demo

Source: [example/fortran/unicode_demo/unicode_demo.f90](../../example/fortran/unicode_demo/unicode_demo.f90)

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

![unicode_demo.png](../../media/examples/unicode_demo/unicode_demo.png)

ASCII output preview:
```
             Wave Functions: ψ(ω t) = A e^{-λ t} sin(ω t + φ)
+--------------------------------------------------------------------------------+
|1.0                                                                             |
|     *                     -- α damped: sin                                    |
|   *   *                   -- β damped: cos                                    |
|0.5 *     *                -- γ oscillation                                   |
|   *       *   ****                                                            |
|0.0*--------*-------****---*----*--***-*-------*-*------***---*----------     |
|             * *     *  *      *      *     * *         *   * *               |
|-0.5           *       *                * *               *                    |
|                                                                               |
|-1.0+--------+----------+----------+----------+----------+--------+           |
     0        2          4          6          8         10                    |
+--------------------------------------------------------------------------------+
                                  ωt (radians)
Amplitude
```

> **Full ASCII Output**: [Download unicode_demo.txt](../../media/examples/unicode_demo/unicode_demo.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/unicode_demo/unicode_demo.pdf)

### Unicode Demo
![Unicode Demo](../../media/examples/unicode_demo/unicode_demo.png)

### Math Examples
![Math Examples](../../media/examples/unicode_demo/math_examples.png)
