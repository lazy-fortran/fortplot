title: Legend Box Demo
---

# Legend Box Demo

This example shows advanced legend styling with boxes and frames.

## Source Files

### Fortran Source

📄 [legend_box_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/legend_box_demo/legend_box_demo.f90)

### Generated Output Files

- Various output files in PNG, PDF, and ASCII formats

## Running

```bash
make example ARGS="legend_box_demo"
```

## Features Demonstrated

- **Box frames**: Border around legend
- **Background**: Semi-transparent background
- **Padding**: Proper spacing inside box
- **Shadow effects**: Optional drop shadow

## Legend Box Properties

- **Frame**: Black border line
- **Background**: White with alpha transparency
- **Padding**: Automatic based on content
- **Position**: Respects location parameter

## Output

### Default Legend Box
![Default Legend Box](../../media/examples/legend_box_demo_default.png)

### Upper Left Position
![Upper Left](../../media/examples/legend_box_demo_upper_left.png)
