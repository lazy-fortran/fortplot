# fortplot Library Design Architecture

## Project Overview

**fortplot** is a modern Fortran plotting library providing scientific visualization with PNG, PDF, ASCII, GLTF, and animation backends. The library follows scientific computing best practices with a clean API inspired by matplotlib.

## Build System Architecture

### Primary Build System: FPM
- **Fortran Package Manager (FPM)** is the primary build system
- All development workflows use `make` wrapper commands
- Automatic discovery of sources, tests, examples, and dependencies
- Matrix testing across gfortran-11, 12, 13, 14

### Secondary Build System: CMake
- **CMake integration** for library consumption via FetchContent
- Located in `doc/cmake_example/` for demonstration
- Currently **BROKEN** - missing CMakeLists.txt in project root

## Current Infrastructure Issues (Issue #73)

### Problem 1: Missing CMake Export Configuration
**Root Cause**: No main CMakeLists.txt in project root
- Example CMake project expects `fortplot::fortplot` target to be available
- FetchContent cannot find CMake configuration
- No export targets defined for external consumption

**Impact**: 
- CI build-cmake job fails
- External projects cannot consume fortplot via CMake
- Inconsistency between FPM and CMake build systems

### Problem 2: Missing ffmpeg Dependency for Animation CI
**Root Cause**: Animation examples require ffmpeg for video generation
- CI installs ffmpeg but animation tests may not be properly isolated
- Python animation backend depends on ffmpeg availability
- No graceful degradation when ffmpeg unavailable

**Impact**:
- Animation-related CI tests fail
- Inconsistent behavior between development and CI environments

## Solution Architecture

### CMake Export Target Solution
**Foundation Layer Impact**: This fix enables external project consumption and maintains build system consistency across the ecosystem.

**Required Files**:
1. **Root CMakeLists.txt**: Define library targets and export configuration
2. **cmake/fortplotConfig.cmake**: Package configuration for find_package()
3. **cmake/fortplotTargets.cmake**: Export target definitions

**Target Structure**:
```cmake
# Library target: fortplot
# Exported as: fortplot::fortplot (namespaced alias)
add_library(fortplot STATIC ${FORTPLOT_SOURCES})
add_library(fortplot::fortplot ALIAS fortplot)
```

**Export Pattern**:
```cmake
export(TARGETS fortplot 
       NAMESPACE fortplot::
       FILE "${CMAKE_CURRENT_BINARY_DIR}/fortplotTargets.cmake")
```

### Animation Dependency Solution
**Graceful Degradation Strategy**:
1. **Runtime Detection**: Check ffmpeg availability before animation tests
2. **Conditional Testing**: Skip animation tests when ffmpeg unavailable
3. **Clear Error Messages**: Inform users about missing dependencies

## Implementation Plan

### Phase 1: CMake Infrastructure (High Priority)
1. **Create root CMakeLists.txt**
   - Define fortplot library target with all source files
   - Set up proper include directories and compiler flags
   - Create fortplot::fortplot alias target
   - Configure export targets

2. **Add CMake configuration files**
   - `cmake/fortplotConfig.cmake.in` template
   - Export target configuration
   - Version compatibility checks

3. **Update CMake example**
   - Verify FetchContent integration works
   - Test target linking and compilation

### Phase 2: Animation Dependency Management (Medium Priority)
1. **Add ffmpeg detection logic**
   - Runtime availability check
   - Graceful test skipping
   - Clear user messaging

2. **Update CI configuration**
   - Ensure ffmpeg properly installed
   - Add conditional animation testing

### Phase 3: Integration Testing (Medium Priority)
1. **Verify both build systems work**
   - FPM primary workflow unchanged
   - CMake secondary workflow functional
   - Cross-system compatibility maintained

## Risk Assessment

### Technical Risks
- **CMake Complexity**: CMake export configuration can be complex
  - *Mitigation*: Use proven FetchContent patterns from research
  - *Mitigation*: Start with minimal working configuration

- **Build System Divergence**: Maintaining two build systems
  - *Mitigation*: Keep CMake configuration minimal and delegate to FPM where possible
  - *Mitigation*: Automated testing of both systems in CI

### Schedule Risks
- **Unknown CMake Export Issues**: Potential integration problems
  - *Mitigation*: Research existing FetchContent patterns
  - *Mitigation*: Incremental implementation and testing

### Quality Risks
- **Build System Inconsistency**: Different behavior between FPM and CMake
  - *Mitigation*: Comprehensive integration testing
  - *Mitigation*: Clear documentation of supported workflows

## Opportunity Analysis

### Performance Opportunities
- **Foundation Layer Optimization**: Fixing CI enables parallel development workflows
- **Build Efficiency**: Proper CMake configuration enables better caching

### Innovation Opportunities
- **Multi-Build-System Template**: Demonstrate best practices for FPM+CMake integration
- **Dependency Management**: Showcase graceful degradation patterns

### Efficiency Opportunities
- **Development Workflow**: Unblocked CI enables faster iteration
- **External Adoption**: CMake support enables broader ecosystem integration

## Architecture Principles Applied

### SOLID Principles
- **Single Responsibility**: CMake configuration only handles library export
- **Open/Closed**: CMake support extends library without modifying core
- **Dependency Inversion**: Build systems depend on source abstractions

### KISS Principle
- **Minimal CMake Configuration**: Only essential export functionality
- **Clear Separation**: CMake handles export, FPM handles development

### Foundation Layer Focus
This infrastructure work provides maximum strategic impact by:
- Enabling external project consumption
- Maintaining build system consistency
- Unblocking future development workflows
- Supporting broader ecosystem integration

## Success Criteria

### Phase 1 Success
- ✅ CMake example builds successfully
- ✅ FetchContent properly resolves fortplot::fortplot target
- ✅ CI build-cmake job passes

### Phase 2 Success
- ✅ Animation tests skip gracefully when ffmpeg unavailable
- ✅ Clear error messaging for missing dependencies
- ✅ CI animation tests pass consistently

### Phase 3 Success
- ✅ Both FPM and CMake workflows tested in CI
- ✅ No regression in existing FPM functionality
- ✅ Documentation updated for both build systems

## Error Bar Plotting Architecture (Issue #52)

### Overview
**Issue #52**: Add comprehensive error bar plotting support for scientific data visualization
- **Status**: IMPLEMENTATION COMPLETE - Architecture documentation for batch mode continuation
- **Context**: Error bar functionality implemented with symmetric/asymmetric support
- **Current Phase**: Architecture documentation and implementation planning review

### Error Bar System Architecture

#### Core Data Structures
**Error Bar Data Container** (`plot_data_t` extensions):
```fortran
type :: plot_data_t
    ! Error bar specific fields
    real(wp), allocatable :: xerr(:), yerr(:)           ! Symmetric errors
    real(wp), allocatable :: xerr_lower(:), xerr_upper(:) ! Asymmetric X errors
    real(wp), allocatable :: yerr_lower(:), yerr_upper(:) ! Asymmetric Y errors
    real(wp) :: capsize = 5.0_wp                       ! Cap size for error bars
    real(wp) :: elinewidth = 1.0_wp                    ! Error bar line width
    logical :: has_xerr = .false., has_yerr = .false.  ! Error presence flags
    logical :: asymmetric_xerr = .false., asymmetric_yerr = .false.
end type
```

#### API Design Patterns
**Matplotlib-Compatible Interface**:
- **Symmetric errors**: `yerr=error_values` or `xerr=x_errors`
- **Asymmetric errors**: `yerr_lower=lower`, `yerr_upper=upper`
- **Combined errors**: Both X and Y error bars simultaneously
- **Styling integration**: Full integration with line/marker customization

**Error Bar API Signatures**:
```fortran
! Primary error bar interface
subroutine errorbar(self, x, y, xerr, yerr, xerr_lower, xerr_upper, &
                   yerr_lower, yerr_upper, capsize, elinewidth, &
                   label, linestyle, marker, color)

! Global convenience interface
subroutine errorbar(x, y, xerr, yerr, xerr_lower, xerr_upper, &
                   yerr_lower, yerr_upper, capsize, elinewidth, &
                   label, linestyle, marker, color)
```

#### Backend Rendering Architecture

**PNG/PDF Backend Rendering**:
- **Error bar geometry**: Vertical/horizontal lines with perpendicular caps
- **Cap rendering**: Configurable cap size and line width
- **Integration**: Seamless integration with existing line/marker rendering
- **Performance**: Optimized rendering for large datasets (10^4+ points)

**ASCII Backend Strategy**:
- **Character representation**: Creative ASCII art for error bars
- **Simplified caps**: ASCII-appropriate cap visualization
- **Layout integration**: Error bars within ASCII plot boundaries

**Animation Backend Support**:
- **Simplified rendering**: Basic error bar representation in animations
- **Performance focus**: Optimized for frame-by-frame rendering
- **Validation**: Error bar data validation for animation contexts

#### Error Handling and Data Validation

**Input Validation Strategy**:
- **Array size consistency**: x, y, and error arrays must match dimensions
- **NaN handling**: Graceful handling of NaN values in error data
- **Boundary conditions**: Zero errors, negative values, very large errors
- **Memory management**: Proper allocation/deallocation of error arrays

**Error Propagation Patterns**:
- **Validation errors**: Clear error messages for input mismatches
- **Rendering errors**: Backend-specific error handling
- **Memory errors**: RAII pattern for automatic cleanup

### Implementation Status Assessment

#### Completed Components ✅
1. **Core API Implementation**: Complete error bar interface in `fortplot_figure_core.f90`
2. **Data Structure Design**: Error bar fields integrated into `plot_data_t`
3. **Public Interface**: Error bar functions exported in main `fortplot` module
4. **Test Infrastructure**: Comprehensive test suite in `test_errorbar.f90`
5. **Example Implementation**: Working demo in `example/fortran/errorbar_demo.f90`
6. **Animation Integration**: Basic error bar support in animation backend

#### Implementation Quality Analysis

**Architectural Strengths**:
- ✅ **SOLID Compliance**: Single responsibility for error bar data handling
- ✅ **API Consistency**: Follows matplotlib patterns for user familiarity
- ✅ **Memory Safety**: Proper allocatable array management
- ✅ **Backend Integration**: Consistent interface across all backends
- ✅ **Performance Design**: Efficient data structures for large datasets

**Current Implementation Assessment**:
- **API Completeness**: Full symmetric/asymmetric error bar support
- **Integration Quality**: Seamless integration with existing plotting system
- **Test Coverage**: Comprehensive test scenarios covering edge cases
- **Error Handling**: Robust input validation and memory management
- **Documentation**: Example code demonstrates all key features

### Performance Characteristics

**Target Performance Metrics**:
- **Large datasets**: Support for 10^4+ data points with error bars
- **Memory efficiency**: O(n) memory usage for error data
- **Rendering speed**: Comparable to line plots with minimal overhead
- **Backend performance**: Optimized rendering across PNG/PDF/ASCII

**Optimization Strategies**:
- **Batch rendering**: Group error bar rendering operations
- **Memory layout**: Contiguous array storage for cache efficiency
- **Conditional rendering**: Skip error bars outside plot boundaries
- **Backend specialization**: Optimized algorithms per backend type

### Integration with Existing Systems

**Plotting System Integration**:
- **Plot type enumeration**: `PLOT_TYPE_ERRORBAR = 4` for type identification
- **Rendering pipeline**: Integration with existing backend rendering
- **Styling system**: Full compatibility with colors, line styles, markers
- **Legend integration**: Error bar plots included in legend generation

**Memory Management Integration**:
- **RAII patterns**: Automatic cleanup of error bar data arrays
- **Allocation strategy**: Efficient memory allocation for variable-size arrays
- **Copy semantics**: Proper handling of error data in plot operations

### Risk Assessment for Future Development

#### Technical Risks (Low - Implementation Complete)
- **Backend compatibility**: All backends support error bars ✅
- **Performance scalability**: Tested with large datasets ✅
- **Memory management**: RAII patterns implemented ✅

#### Integration Risks (Minimal)
- **API stability**: Mature API design following matplotlib patterns
- **Backward compatibility**: No breaking changes to existing functionality
- **Test coverage**: Comprehensive test suite validates all scenarios

### Opportunity Analysis

#### Scientific Visualization Enhancement
- **Research applications**: Enhanced scientific plotting capabilities
- **Publication quality**: Professional error bar rendering for papers
- **Data analysis**: Improved uncertainty visualization tools

#### Performance Advantages
- **Native implementation**: No external dependencies for error bars
- **Optimized rendering**: Backend-specific optimization opportunities
- **Memory efficiency**: Direct integration with plot data structures

#### Extensibility Opportunities
- **Uncertainty quantification**: Foundation for advanced error analysis
- **Statistical visualization**: Base for confidence intervals, bands
- **Error propagation**: Potential for error calculation utilities

### Architecture Validation Summary

**Design Principles Applied**:
- ✅ **SOLID**: Single responsibility, interface segregation
- ✅ **KISS**: Simple, clear API matching user expectations
- ✅ **DRY**: Reuse of existing rendering and styling infrastructure
- ✅ **Performance-first**: Optimized data structures and algorithms

**Quality Standards Met**:
- ✅ **Test coverage**: Comprehensive test scenarios
- ✅ **Documentation**: Clear examples and usage patterns
- ✅ **Integration**: Seamless backend compatibility
- ✅ **Error handling**: Robust input validation and error management

**Strategic Impact**:
Error bar implementation provides critical scientific visualization capabilities while maintaining architectural consistency and performance standards. The implementation demonstrates mature design patterns suitable for complex scientific plotting requirements.

## Dependencies and Constraints

### External Dependencies
- CMake 3.20+ (already required by example)
- FPM 0.12.0+ (already required)
- ffmpeg (for animation features)

### Internal Constraints
- Must not break existing FPM workflow
- Must maintain API compatibility
- Must follow established project conventions

## Next Steps

1. **Immediate**: Create root CMakeLists.txt with minimal export configuration
2. **Short-term**: Add ffmpeg detection and graceful degradation
3. **Medium-term**: Comprehensive integration testing and documentation