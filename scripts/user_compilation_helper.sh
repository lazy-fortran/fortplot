#!/bin/bash

# Fortplot User Compilation Helper Script
# Provides enhanced error messages and compilation assistance for users

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script information
SCRIPT_NAME="Fortplot User Compilation Helper"
VERSION="1.0.0"

# Function to print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_header() {
    echo -e "${BLUE}=== $1 ===${NC}"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to display usage
show_usage() {
    echo "Usage: $0 [OPTIONS] FORTRAN_FILE"
    echo ""
    echo "Compile a Fortran program that uses fortplot with enhanced error messages"
    echo "and compilation assistance."
    echo ""
    echo "OPTIONS:"
    echo "  -h, --help           Show this help message"
    echo "  -v, --verbose        Enable verbose output"
    echo "  -d, --debug          Compile with debug flags"
    echo "  -o, --output NAME    Specify output executable name"
    echo "  --fpm                Use FPM for compilation (if available)"
    echo "  --cmake              Use CMake for compilation (if available)"
    echo "  --check-deps         Check system dependencies only"
    echo "  --test-compilation   Run compilation test only"
    echo ""
    echo "EXAMPLES:"
    echo "  $0 my_program.f90                  # Basic compilation"
    echo "  $0 -d -o my_debug my_program.f90   # Debug compilation"  
    echo "  $0 --fpm                           # Use FPM if available"
    echo "  $0 --check-deps                    # Check dependencies"
    echo ""
}

# Function to check system dependencies
check_dependencies() {
    print_header "System Dependencies Check"
    
    local all_good=true
    
    # Check Fortran compiler
    if command_exists gfortran; then
        local version=$(gfortran --version | head -n1)
        print_success "Fortran compiler found: $version"
        
        # Check version (need gfortran 8+)
        local major_version=$(gfortran -dumpversion | cut -d. -f1)
        if [ "$major_version" -ge 8 ]; then
            print_success "Compiler version is compatible (>= 8.0)"
        else
            print_warning "Compiler version is old (< 8.0). Consider upgrading."
        fi
    else
        print_error "gfortran not found. Please install a modern Fortran compiler."
        print_info "Ubuntu/Debian: sudo apt install gfortran"
        print_info "macOS: brew install gcc"
        print_info "Windows: Install MinGW-w64 or MSYS2"
        all_good=false
    fi
    
    # Check FPM
    if command_exists fpm; then
        local fmp_version=$(fpm --version 2>&1 | head -n1)
        print_success "FPM found: $fpm_version"
    else
        print_warning "FPM not found. Install for easier project management:"
        print_info "pip install fpm"
        print_info "Or download from: https://github.com/fortran-lang/fpm"
    fi
    
    # Check CMake
    if command_exists cmake; then
        local cmake_version=$(cmake --version | head -n1)
        print_success "CMake found: $cmake_version"
    else
        print_warning "CMake not found. Useful for large projects:"
        print_info "Ubuntu/Debian: sudo apt install cmake"
        print_info "macOS: brew install cmake"  
        print_info "Windows: Download from cmake.org"
    fi
    
    # Check Git
    if command_exists git; then
        local git_version=$(git --version)
        print_success "Git found: $git_version"
    else
        print_warning "Git not found. Required for dependency management:"
        print_info "Install git from your package manager or git-scm.com"
        all_good=false
    fi
    
    # Check internet connectivity for git operations
    if ping -c 1 github.com >/dev/null 2>&1; then
        print_success "Internet connectivity: OK (can access GitHub)"
    else
        print_warning "Internet connectivity issues detected"
        print_info "Some dependency downloads may fail"
    fi
    
    if $all_good; then
        print_success "All critical dependencies are available"
        return 0
    else
        print_error "Some critical dependencies are missing"
        return 1
    fi
}

# Function to find fortplot installation
find_fortplot() {
    print_header "Locating Fortplot Installation"
    
    # Check current directory first
    if [ -f "build/gfortran"*"/fortplot/"*"/src/fortplot.mod" ] 2>/dev/null; then
        local mod_file=$(find build -name "fortplot.mod" -type f 2>/dev/null | head -1)
        local lib_file=$(find build -name "libfortplot.a" -type f 2>/dev/null | head -1)
        
        if [ -n "$mod_file" ] && [ -n "$lib_file" ]; then
            FORTPLOT_MOD_DIR=$(dirname "$mod_file")
            FORTPLOT_LIB_FILE="$lib_file"
            print_success "Found fortplot in current directory:"
            print_info "Module directory: $FORTPLOT_MOD_DIR"
            print_info "Library file: $FORTPLOT_LIB_FILE"
            return 0
        fi
    fi
    
    # Check common installation locations
    local common_locations=(
        "$HOME/.local/lib/fortplot"
        "/usr/local/lib/fortplot"
        "/opt/fortplot"
    )
    
    for location in "${common_locations[@]}"; do
        if [ -d "$location" ]; then
            local mod_file=$(find "$location" -name "fortplot.mod" -type f 2>/dev/null | head -1)
            local lib_file=$(find "$location" -name "libfortplot.a" -type f 2>/dev/null | head -1)
            
            if [ -n "$mod_file" ] && [ -n "$lib_file" ]; then
                FORTPLOT_MOD_DIR=$(dirname "$mod_file")
                FORTPLOT_LIB_FILE="$lib_file"
                print_success "Found fortplot installation:"
                print_info "Module directory: $FORTPLOT_MOD_DIR"
                print_info "Library file: $FORTPLOT_LIB_FILE"
                return 0
            fi
        fi
    done
    
    print_error "Fortplot installation not found"
    print_info "Please ensure fortplot is built:"
    print_info "  1. Clone: git clone https://github.com/lazy-fortran/fortplot"
    print_info "  2. Build: cd fortplot && make build"
    print_info "  3. Or use FPM/CMake dependency management"
    return 1
}

# Function to compile with detailed error reporting
compile_program() {
    local source_file="$1"
    local output_name="$2"
    local debug_mode="$3"
    local verbose="$4"
    
    print_header "Compiling User Program"
    
    if [ ! -f "$source_file" ]; then
        print_error "Source file not found: $source_file"
        return 1
    fi
    
    print_info "Source file: $source_file"
    print_info "Output executable: $output_name"
    
    # Build compiler command
    local compile_cmd="gfortran"
    local compile_flags=""
    
    # Add debug flags if requested
    if [ "$debug_mode" = "true" ]; then
        compile_flags="$compile_flags -g -Wall -Wextra -fcheck=all -fbacktrace"
        print_info "Debug mode enabled"
    else
        compile_flags="$compile_flags -O2"
    fi
    
    # Add fortplot-specific flags
    if [ -n "$FORTPLOT_MOD_DIR" ] && [ -n "$FORTPLOT_LIB_FILE" ]; then
        compile_flags="$compile_flags -I \"$FORTPLOT_MOD_DIR\""
        local link_flags="\"$FORTPLOT_LIB_FILE\" -lm"
        
        # Build full command
        local full_command="$compile_cmd $compile_flags -o \"$output_name\" \"$source_file\" $link_flags"
        
        if [ "$verbose" = "true" ]; then
            print_info "Compilation command:"
            echo "  $full_command"
            echo ""
        fi
        
        print_info "Starting compilation..."
        
        # Execute compilation with error capture
        if eval $full_command 2>&1; then
            print_success "Compilation completed successfully!"
            print_info "Executable created: $output_name"
            
            # Check if executable was actually created
            if [ -f "$output_name" ]; then
                print_success "Output file verified: $output_name"
                return 0
            else
                print_error "Compilation reported success but output file not found"
                return 1
            fi
        else
            print_error "Compilation failed"
            print_info "Common solutions:"
            print_info "  1. Check source code syntax"
            print_info "  2. Verify all 'use' statements are correct"
            print_info "  3. Ensure array dimensions are properly defined"
            print_info "  4. Try debug mode: $0 -d $source_file"
            return 1
        fi
    else
        print_error "Fortplot installation not properly configured"
        print_info "Run: $0 --check-deps"
        return 1
    fi
}

# Function to test compilation with a simple program
test_compilation() {
    print_header "Testing Fortplot Compilation"
    
    local test_file="fortplot_compile_test.f90"
    local test_exe="fortplot_compile_test"
    
    # Create test program
    cat > "$test_file" << 'EOF'
program fortplot_compile_test
    use fortplot
    implicit none
    
    real, dimension(10) :: x, y
    integer :: i
    
    print *, "Testing fortplot compilation and basic functionality..."
    
    ! Simple test data
    do i = 1, 10
        x(i) = real(i)
        y(i) = real(i)**2
    end do
    
    ! Test basic plotting
    call figure()
    call plot(x, y)
    call title("Compilation Test")
    call savefig("compilation_test.png")
    
    print *, "SUCCESS: Compilation test completed!"
    print *, "Check compilation_test.png to verify plot generation."
end program fortplot_compile_test
EOF
    
    print_info "Created test program: $test_file"
    
    # Compile test program
    if compile_program "$test_file" "$test_exe" false false; then
        print_success "Test program compiled successfully!"
        
        # Try to run it
        print_info "Running test program..."
        if ./"$test_exe"; then
            print_success "Test program executed successfully!"
            
            # Check for output file
            if [ -f "compilation_test.png" ]; then
                print_success "Plot file generated: compilation_test.png"
                print_success "PASS: Fortplot compilation and functionality test PASSED"
            else
                print_warning "Test compiled and ran, but no plot file found"
                print_info "This may indicate a runtime issue with file generation"
            fi
        else
            print_error "Test program compiled but failed to run"
            print_info "This indicates a runtime or linking issue"
        fi
        
        # Clean up
        print_info "Cleaning up test files..."
        rm -f "$test_file" "$test_exe" "compilation_test.png"
        
        return 0
    else
        print_error "FAIL: Fortplot compilation test FAILED"
        rm -f "$test_file"
        return 1
    fi
}

# Main script logic
main() {
    local source_file=""
    local output_name=""
    local debug_mode=false
    local verbose=false
    local use_fpm=false
    local use_cmake=false
    local check_deps_only=false
    local test_compilation_only=false
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -v|--verbose)
                verbose=true
                shift
                ;;
            -d|--debug)
                debug_mode=true
                shift
                ;;
            -o|--output)
                output_name="$2"
                shift 2
                ;;
            --fpm)
                use_fpm=true
                shift
                ;;
            --cmake)
                use_cmake=true
                shift
                ;;
            --check-deps)
                check_deps_only=true
                shift
                ;;
            --test-compilation)
                test_compilation_only=true
                shift
                ;;
            -*)
                print_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
            *)
                if [ -z "$source_file" ]; then
                    source_file="$1"
                else
                    print_error "Multiple source files not supported"
                    exit 1
                fi
                shift
                ;;
        esac
    done
    
    # Print header
    print_header "$SCRIPT_NAME v$VERSION"
    
    # Check dependencies first
    if ! check_dependencies; then
        exit 1
    fi
    
    # If only checking dependencies, exit here
    if [ "$check_deps_only" = "true" ]; then
        print_success "Dependency check completed"
        exit 0
    fi
    
    # If only testing compilation, do that
    if [ "$test_compilation_only" = "true" ]; then
        if find_fortplot && test_compilation; then
            exit 0
        else
            exit 1
        fi
    fi
    
    # Handle FPM case
    if [ "$use_fpm" = "true" ]; then
        if command_exists fpm; then
            print_info "Using FPM for build..."
            if [ -f "fpm.toml" ]; then
                print_info "Found fpm.toml, building project..."
                fpm build
                print_success "FPM build completed"
            else
                print_error "No fmp.toml found. Create FPM project first."
                print_info "See example templates in example/user_compilation_examples/"
            fi
        else
            print_error "FPM not available but requested"
        fi
        exit $?
    fi
    
    # Handle CMake case
    if [ "$use_cmake" = "true" ]; then
        if command_exists cmake; then
            print_info "Using CMake for build..."
            if [ -f "CMakeLists.txt" ]; then
                print_info "Found CMakeLists.txt, building project..."
                cmake -B build
                cmake --build build
                print_success "CMake build completed"
            else
                print_error "No CMakeLists.txt found. Create CMake project first."
                print_info "See example templates in example/user_compilation_examples/"
            fi
        else
            print_error "CMake not available but requested"
        fi
        exit $?
    fi
    
    # Manual compilation case
    if [ -z "$source_file" ]; then
        print_error "No source file specified"
        show_usage
        exit 1
    fi
    
    # Set default output name
    if [ -z "$output_name" ]; then
        output_name=$(basename "$source_file" .f90)
    fi
    
    # Find fortplot
    if ! find_fortplot; then
        exit 1
    fi
    
    # Compile the program
    if compile_program "$source_file" "$output_name" "$debug_mode" "$verbose"; then
        print_success "Compilation completed successfully!"
        print_info "Run your program: ./$output_name"
        exit 0
    else
        print_error "Compilation failed"
        print_info "For help, run: $0 --help"
        exit 1
    fi
}

# Global variables for fortplot paths
FORTPLOT_MOD_DIR=""
FORTPLOT_LIB_FILE=""

# Run main function
main "$@"
