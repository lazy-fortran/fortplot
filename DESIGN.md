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