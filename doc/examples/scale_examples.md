title: Scale Examples
---

# Scale Examples

This example demonstrates different axis scaling options including logarithmic and symmetric logarithmic (symlog) scales.

## Files

- `scale_examples.f90` - Source code
- `log_scale.png/pdf/txt` - Logarithmic scale example
- `symlog_scale.png/pdf/txt` - Symmetric logarithmic scale example

## Running

```bash
make example ARGS="scale_examples"
```

## Features Demonstrated

- **Logarithmic scaling**: For exponential growth visualization
- **Symmetric log**: Handles positive and negative values with log-like behavior
- **Linear threshold**: Symlog parameter controls transition to linear near zero
- **Automatic tick generation**: Smart tick placement for non-linear scales

## Output

### Log Scale

![log_scale.png](../../media/examples/scale_examples/log_scale.png)

ASCII output preview:
```
Log Scale Example
+--------------------------------------------------------------------------------+
| ... [representative scale visualization] ...                                  |
| ... [truncated - see full output in log_scale.txt] ...                          |
+--------------------------------------------------------------------------------+
```

> **Full ASCII Output**: [Download log_scale.txt](../../media/examples/scale_examples/log_scale.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/scale_examples/log_scale.pdf)

### Symlog Scale

![symlog_scale.png](../../media/examples/scale_examples/symlog_scale.png)

ASCII output preview:
```
Symlog Scale Example
+--------------------------------------------------------------------------------+
| ... [representative scale visualization] ...                                  |
| ... [truncated - see full output in symlog_scale.txt] ...                          |
+--------------------------------------------------------------------------------+
```

> **Full ASCII Output**: [Download symlog_scale.txt](../../media/examples/scale_examples/symlog_scale.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/scale_examples/symlog_scale.pdf)

