# Using fortplotlib with FPM

This example shows how to use fortplotlib as an FPM dependency.

## Usage

```bash
fpm run
```

This will:
1. Download fortplotlib as a dependency
2. Build the example program  
3. Run it to create `example.png`

## fpm.toml

The key is adding fortplotlib as a dependency:

```toml
[dependencies]
fortplotlib = { git = "https://github.com/krystophny/fortplotlib.git" }
```