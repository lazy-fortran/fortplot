title: Display Demo
---

# Display Demo

Source: [display_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/display_demo/display_demo.f90)

Demonstrates the two plot display methods: `show_viewer()` always opens in the system PDF viewer, while `show()` uses intelligent dispatch (viewer if a GUI is available, ASCII output otherwise).

## Files

- `display_demo.f90` - Source code
- Generated media in `output/example/fortran/display_demo/`

## Running

```bash
make example ARGS="display_demo"
```

## Output

### show_viewer() Demo

![show_viewer_demo.png](../../media/examples/display_demo/show_viewer_demo.png)

[Download PDF](../../media/examples/display_demo/show_viewer_demo.pdf)

### show() Demo

![smart_show_demo.png](../../media/examples/display_demo/smart_show_demo.png)

[Download PDF](../../media/examples/display_demo/smart_show_demo.pdf)

