# FPM Project Template for Fortplot Users

This is a complete, working template for creating your own Fortran projects that use the fortplot library with FPM (Fortran Package Manager).

## Quick Start

1. **Copy this template** to your project directory:
   ```bash
   cp -r fpm_project_template my_project
   cd my_project
   ```

2. **Customize the project**:
   - Edit `fmp.toml` with your project name and details
   - Modify `app/main.f90` with your plotting code

3. **Build and run**:
   ```bash
   fpm build
   fpm run
   ```

## Project Structure

```
my_project/
├── fpm.toml          # FPM configuration with fortplot dependency
├── app/
│   └── main.f90      # Your main program
├── src/              # Additional modules (optional)
└── README.md         # This file
```

## Customizing Your Project

### Basic Modification

Replace the content in `app/main.f90` with your own plotting code:

```fortran
program my_analysis
    use fortplot
    implicit none
    
    ! Your data and plotting code here
    call figure()
    ! ... your plots ...
    call savefig("my_results.png")
end program my_analysis
```

### Advanced Project Structure

For larger projects, you can add modules in the `src/` directory:

```
my_project/
├── fpm.toml
├── app/
│   └── main.f90      # Main program
├── src/
│   ├── data_processing.f90    # Your data processing module
│   ├── analysis_tools.f90     # Analysis utilities
│   └── plot_utilities.f90     # Custom plotting functions
└── test/
    └── test_analysis.f90       # Unit tests
```

### Adding Dependencies

To add more dependencies to your project, edit `fpm.toml`:

```toml
[[dependencies]]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }

[[dependencies]]
some_other_lib = { git = "https://github.com/user/library" }
```

## Testing Your Setup

The template includes a working example that tests:
- Basic fortplot functionality
- Data visualization
- Multiple output formats (PNG, PDF)

If the template runs successfully, your fortplot setup is working correctly.

## Common Issues and Solutions

### Build Fails
- Ensure you have FPM installed: `pip install fpm`
- Check internet connection for git dependencies
- Verify gfortran is available: `gfortran --version`

### Runtime Errors
- Check file permissions in output directory
- Verify data array sizes match your loops
- Use `fpm run --profile debug` for more detailed error messages

## Next Steps

1. **Read the documentation**: See `/doc/user_compilation_guide.md` for comprehensive guidance
2. **Explore examples**: Check `/example/fortran/` for more advanced plotting examples
3. **Join the community**: Submit issues or contributions to the fortplot repository

Happy plotting!