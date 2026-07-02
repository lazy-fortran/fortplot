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
<pre><code>

              Common Physics: E = mc², ΔE = hν, F = q(E + v×B)
┌────────────────────────────────────────────────────────────────────────────────┐
│                                                                                │
│ │                                                                              │
│ 0.5                                                                            │
│ │                      <span style="color:#d62728">■</span>                                                       │
│ │                    <span style="color:#d62728">■■</span>                                                        │
│ │                  <span style="color:#d62728">■■</span>            <span style="color:#d62728">■■■</span>                                           │
│ │                  <span style="color:#d62728">■</span>                <span style="color:#d62728">■■</span>                                         │
│ 0.4              <span style="color:#d62728">■■</span>                   <span style="color:#d62728">■</span>                                        │
│ │               <span style="color:#d62728">■</span>                      <span style="color:#d62728">■■</span>                                      │
│ │  <span style="color:#1f77b4">---</span>          <span style="color:#d62728">■</span>                        <span style="color:#d62728">■■</span>                                    │
│ │     <span style="color:#1f77b4">--</span>       <span style="color:#d62728">■</span>                           <span style="color:#d62728">■</span>                                   │
│ │       <span style="color:#1f77b4">--</span>    <span style="color:#d62728">■</span>                             <span style="color:#d62728">■■</span>                                 │
│ │        <span style="color:#1f77b4">--</span>   <span style="color:#d62728">■</span>                               <span style="color:#d62728">■■</span>                               │
│ 0.3        <span style="color:#1f77b4">-</span> <span style="color:#d62728">■</span>                                  <span style="color:#d62728">■</span>                              │
│ │           <span style="color:#d62728">■</span>                                    <span style="color:#d62728">■■</span>                            │
│ │            <span style="color:#1f77b4">--</span>                                    <span style="color:#d62728">■■</span>                          │
│ │          <span style="color:#d62728">■</span>  <span style="color:#1f77b4">-</span>                                     <span style="color:#d62728">■■■</span>                        │
│ │          <span style="color:#d62728">■</span>   <span style="color:#1f77b4">--</span>                                      <span style="color:#d62728">■■</span>                      │
│ 0.2       <span style="color:#d62728">■</span>     <span style="color:#1f77b4">-</span>                                        <span style="color:#d62728">■■</span>                    │
│ │        <span style="color:#d62728">■</span>       <span style="color:#1f77b4">--</span>                                        <span style="color:#d62728">■■■</span>                 │
│ │                  <span style="color:#1f77b4">-</span>                                          <span style="color:#d62728">■■</span>               │
│ │        <span style="color:#d62728">■</span>          <span style="color:#1f77b4">-</span>                                           <span style="color:#d62728">■■■</span>            │
│ │       <span style="color:#d62728">■</span>            <span style="color:#1f77b4">-</span>                                             <span style="color:#d62728">■■■</span>         │
│ 0.1    <span style="color:#d62728">■</span>              <span style="color:#1f77b4">---</span>                                             <span style="color:#d62728">■■■■</span>     │
│ │      <span style="color:#d62728">■</span>                <span style="color:#1f77b4">--</span>                                                     │
│ │     <span style="color:#d62728">■</span>                   <span style="color:#1f77b4">---</span>                                                  │
│ │    <span style="color:#d62728">■</span>                      <span style="color:#1f77b4">-----</span>                                              │
│ │  <span style="color:#d62728">■■</span>                            <span style="color:#1f77b4">-----------------------------------------</span>     │
│ 0.0+----------+-----------+-----------+-----------+-----------+----------+---- │
│    0          1           2           3           4           5          6     │
└────────────────────────────────────────────────────────────────────────────────┘
                                  Parameter ξ
Observable Θ

Legend:
--- Gaussian: ρ(ξ) = e^{-ξ²/2σ²}/√(2πσ²)
--- Modified Γ: f(ξ) = ξ² e^{-ξ}
</code></pre>

[Download ASCII](../../media/examples/unicode_demo/math_examples.txt)

[Download PDF](../../media/examples/unicode_demo/math_examples.pdf)

### Unicode Demo

![unicode_demo.png](../../media/examples/unicode_demo/unicode_demo.png)

ASCII output:
<pre><code>

             Wave Functions: ψ(ω t) = A e^{-λ t} sin(ω t + φ)
┌────────────────────────────────────────────────────────────────────────────────┐
│ 1.00                                                                           │
│ │                                                                              │
│ │                                                                              │
│ │  <span style="color:#d62728">■■</span>                                                                          │
│ 0.75<span style="color:#d62728">■■</span>                                                                         │
│ │     <span style="color:#d62728">■</span>                                                                        │
│ │      <span style="color:#d62728">■</span> <span style="color:#1f77b4">-----</span>                                                                 │
│ │      <span style="color:#d62728">■</span><span style="color:#1f77b4">--</span>    <span style="color:#1f77b4">-</span>                                                                │
│ 0.50   <span style="color:#1f77b4">-</span>      <span style="color:#1f77b4">-</span>                                                                │
│ │      <span style="color:#1f77b4">-</span><span style="color:#d62728">■</span>      <span style="color:#1f77b4">--</span>                                                              │
│ │      <span style="color:#2ca02c">■■</span><span style="color:#d62728">■</span>              <span style="color:#2ca02c">■■■</span>        <span style="color:#d62728">■■■■■■</span> <span style="color:#2ca02c">■■</span>               <span style="color:#2ca02c">■■■</span>                 │
│ │     <span style="color:#2ca02c">■</span>  <span style="color:#2ca02c">■</span>      <span style="color:#1f77b4">-</span>       <span style="color:#2ca02c">■</span> <span style="color:#2ca02c">■</span>       <span style="color:#d62728">■</span>     <span style="color:#2ca02c">■■</span>  <span style="color:#2ca02c">■</span>             <span style="color:#2ca02c">■</span>   <span style="color:#2ca02c">■</span>                │
│ 0.25 <span style="color:#2ca02c">■</span>   <span style="color:#2ca02c">■</span>       <span style="color:#1f77b4">-</span>     <span style="color:#2ca02c">■</span>   <span style="color:#2ca02c">■</span>     <span style="color:#d62728">■</span>        <span style="color:#d62728">■■</span> <span style="color:#2ca02c">■</span>           <span style="color:#2ca02c">■</span>                     │
│ │         <span style="color:#2ca02c">■</span>       <span style="color:#1f77b4">-</span>   <span style="color:#2ca02c">■</span>     <span style="color:#2ca02c">■</span>   <span style="color:#d62728">■</span>       <span style="color:#2ca02c">■</span>  <span style="color:#d62728">■</span><span style="color:#1f77b4">------</span>            <span style="color:#2ca02c">■</span>        <span style="color:#d62728">■■■</span>     │
│ │   <span style="color:#2ca02c">■</span>              <span style="color:#1f77b4">-</span>           <span style="color:#d62728">■</span>       <span style="color:#2ca02c">■</span> <span style="color:#1f77b4">---</span><span style="color:#d62728">■</span><span style="color:#2ca02c">■</span>    <span style="color:#1f77b4">--</span>     <span style="color:#2ca02c">■</span>     <span style="color:#2ca02c">■</span>    <span style="color:#d62728">■■■</span>        │
│ │   <span style="color:#2ca02c">■</span>      <span style="color:#2ca02c">■</span>       <span style="color:#1f77b4">-</span> <span style="color:#2ca02c">■</span>      <span style="color:#2ca02c">■</span>  <span style="color:#d62728">■</span>        <span style="color:#1f77b4">-</span>    <span style="color:#d62728">■</span><span style="color:#2ca02c">■</span>     <span style="color:#1f77b4">-</span>              <span style="color:#d62728">■</span>           │
│ 0.00       <span style="color:#2ca02c">■</span>        <span style="color:#1f77b4">-</span>         <span style="color:#d62728">■</span>       <span style="color:#2ca02c">■</span><span style="color:#1f77b4">-</span>      <span style="color:#d62728">■</span>      <span style="color:#1f77b4">---</span><span style="color:#2ca02c">■</span>       <span style="color:#2ca02c">■</span><span style="color:#d62728">■■</span>            │
│ │  <span style="color:#2ca02c">■</span>                 <span style="color:#2ca02c">■</span>       <span style="color:#2ca02c">■</span>        <span style="color:#1f77b4">-</span>        <span style="color:#2ca02c">■</span>       <span style="color:#2ca02c">■</span><span style="color:#1f77b4">-</span>       <span style="color:#d62728">■■</span>      <span style="color:#1f77b4">-</span><span style="color:#2ca02c">■</span>     │
│ │           <span style="color:#2ca02c">■</span>       <span style="color:#2ca02c">■</span><span style="color:#1f77b4">-</span>      <span style="color:#d62728">■</span>        <span style="color:#1f77b4">-</span><span style="color:#2ca02c">■</span>         <span style="color:#d62728">■</span>        <span style="color:#1f77b4">--</span>   <span style="color:#d62728">■■</span> <span style="color:#2ca02c">■</span>   <span style="color:#1f77b4">---</span>       │
│ │            <span style="color:#d62728">■</span>        <span style="color:#1f77b4">-</span>     <span style="color:#d62728">■</span> <span style="color:#2ca02c">■</span>     <span style="color:#1f77b4">-</span>           <span style="color:#2ca02c">■</span><span style="color:#d62728">■</span>     <span style="color:#2ca02c">■</span>   <span style="color:#1f77b4">---</span><span style="color:#d62728">■</span> <span style="color:#1f77b4">-</span><span style="color:#2ca02c">■</span><span style="color:#1f77b4">---</span>   <span style="color:#2ca02c">■</span>      │
│ -0.25        <span style="color:#2ca02c">■</span>     <span style="color:#2ca02c">■</span>   <span style="color:#1f77b4">-</span>   <span style="color:#d62728">■</span>   <span style="color:#2ca02c">■</span>   <span style="color:#1f77b4">-</span> <span style="color:#2ca02c">■</span>            <span style="color:#d62728">■</span>         <span style="color:#d62728">■■</span> <span style="color:#1f77b4">-</span>        <span style="color:#2ca02c">■</span>      │
│ │             <span style="color:#2ca02c">■</span>         <span style="color:#1f77b4">-</span>        <span style="color:#1f77b4">--</span> <span style="color:#2ca02c">■</span>           <span style="color:#2ca02c">■</span>  <span style="color:#d62728">■■</span> <span style="color:#2ca02c">■</span>   <span style="color:#d62728">■■■</span>     <span style="color:#2ca02c">■</span>            │
│ │             <span style="color:#d62728">■</span>    <span style="color:#2ca02c">■</span>    <span style="color:#1f77b4">--</span><span style="color:#d62728">■</span>    <span style="color:#2ca02c">■</span><span style="color:#1f77b4">--</span>               <span style="color:#2ca02c">■</span>  <span style="color:#d62728">■</span><span style="color:#2ca02c">■</span><span style="color:#d62728">■■■■</span>         <span style="color:#2ca02c">■</span>   <span style="color:#2ca02c">■</span>       │
│ │             <span style="color:#2ca02c">■</span><span style="color:#d62728">■</span>  <span style="color:#2ca02c">■</span>       <span style="color:#d62728">■</span>    <span style="color:#1f77b4">-</span><span style="color:#2ca02c">■</span>  <span style="color:#2ca02c">■■</span>             <span style="color:#2ca02c">■</span> <span style="color:#2ca02c">■</span>              <span style="color:#2ca02c">■</span>  <span style="color:#2ca02c">■</span>        │
│ -0.50          <span style="color:#2ca02c">■■■</span>      <span style="color:#d62728">■■</span> <span style="color:#1f77b4">----</span>  <span style="color:#2ca02c">■■</span>               <span style="color:#2ca02c">■■■</span>               <span style="color:#2ca02c">■■</span>         │
│ │               <span style="color:#d62728">■</span>       <span style="color:#d62728">■</span>                                                      │
│ │                <span style="color:#d62728">■</span>     <span style="color:#d62728">■</span>                                                       │
│ │                 <span style="color:#d62728">■■■■■</span>                                                        │
│ -0.75---------+----------+----------+-----------+----------+----------+------- │
│    0          2          4          6           8         10         12        │
└────────────────────────────────────────────────────────────────────────────────┘
                     Time τ (normalized: τ = ω t / 2π)
Amplitude Ψ (V)

Legend:
--- α damped: sin(ω t)e^{-λτ}
--- β damped: cos(ω t)e^{-μτ}
--- γ oscillation: sin(2ω t)
</code></pre>

[Download ASCII](../../media/examples/unicode_demo/unicode_demo.txt)

[Download PDF](../../media/examples/unicode_demo/unicode_demo.pdf)

