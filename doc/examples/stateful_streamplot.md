title: Stateful Streamplot
---

# Stateful Streamplot

This example shows time-evolving vector field animations using streamplots.

## Source Files

### Fortran Source

📄 [stateful_streamplot.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/stateful_streamplot/stateful_streamplot.f90)

### Generated Output Files

- Various output files in PNG, PDF, and ASCII formats

## Running

```bash
make example ARGS="stateful_streamplot"
```

## Features Demonstrated

- **Time-dependent fields**: Vector fields that change over time
- **State preservation**: Maintain streamline continuity
- **Smooth transitions**: Interpolate between states
- **Physical simulations**: Fluid flow, electromagnetic fields

## Applications

- **Fluid dynamics**: Visualize flow evolution
- **Weather patterns**: Show wind field changes
- **Electromagnetic fields**: Time-varying E/B fields
- **Traffic flow**: Vehicle movement patterns

## Implementation Notes

- **Stateful integration**: Remember previous streamline positions
- **Adaptive refinement**: Add/remove streamlines as needed
- **Performance**: Optimized for real-time updates
- **Memory efficient**: Only store necessary state

## Output Example

![Stateful Streamplot](../../media/examples/stateful_streamplot.png)
