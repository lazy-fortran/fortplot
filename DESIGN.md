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

## Enhanced Scatter Plot Architecture (Issue #56)

### Overview
**Issue #56**: Enhanced scatter plot with comprehensive marker symbols, size/color mapping for multi-dimensional data visualization
- **Status**: ACTIVE DEVELOPMENT - Architecture documentation for batch mode execution
- **Context**: Comprehensive scatter plot functionality with bubble charts and color mapping
- **Scope**: Full matplotlib-compatible scatter plot API with all backends support

### Strategic Foundation Assessment
**Infrastructure Readiness** ✅:
- **Build System**: Complete CMake/FPM integration enables robust development
- **Plotting Framework**: Mature fortplot architecture ready for scatter enhancement
- **Backend Pipeline**: Established rendering system supports new visual elements
- **Quality Framework**: Comprehensive testing and CI/CD infrastructure in place

### Core Scatter Plot System Architecture

#### Enhanced Data Structures
**Scatter Plot Data Container** (`plot_data_t` extensions):
```fortran
type :: plot_data_t
    ! Existing fields...
    
    ! Scatter-specific enhancements
    real(wp), allocatable :: marker_sizes(:)       ! Size mapping values
    real(wp), allocatable :: marker_colors(:)      ! Color mapping values
    character(len=32) :: marker_shape = 'circle'   ! Marker shape type
    character(len=32) :: colormap = 'viridis'      ! Color mapping scheme
    real(wp) :: marker_scale = 1.0_wp              ! Global size scaling
    real(wp) :: alpha = 1.0_wp                     ! Transparency level
    
    ! Edge and face color control
    real(wp) :: edge_color(3) = [0.0_wp, 0.0_wp, 0.0_wp]   ! Edge RGB
    real(wp) :: face_color(3) = [0.5_wp, 0.5_wp, 0.5_wp]   ! Face RGB
    real(wp) :: edge_linewidth = 1.0_wp            ! Edge line thickness
    logical :: has_edge_color = .false.            ! Custom edge color flag
    logical :: has_face_color = .false.            ! Custom face color flag
    
    ! Size and color mapping flags
    logical :: has_size_mapping = .false.          ! Variable marker sizes
    logical :: has_color_mapping = .false.         ! Variable marker colors
    logical :: show_colorbar = .true.              ! Colorbar display control
end type
```

#### Marker Symbol System Architecture

**Marker Shape Enumeration**:
```fortran
! Comprehensive marker shape support
integer, parameter :: MARKER_CIRCLE    = 1   ! 'o', 'circle'
integer, parameter :: MARKER_SQUARE    = 2   ! 's', 'square'  
integer, parameter :: MARKER_TRIANGLE  = 3   ! '^', 'triangle_up'
integer, parameter :: MARKER_DIAMOND   = 4   ! 'D', 'diamond'
integer, parameter :: MARKER_STAR      = 5   ! '*', 'star'
integer, parameter :: MARKER_PLUS      = 6   ! '+', 'plus'
integer, parameter :: MARKER_CROSS     = 7   ! 'x', 'cross'
integer, parameter :: MARKER_PENTAGON  = 8   ! 'p', 'pentagon'
integer, parameter :: MARKER_HEXAGON   = 9   ! 'h', 'hexagon'
integer, parameter :: MARKER_OCTAGON   = 10  ! '8', 'octagon'

! Shape conversion utilities
integer function marker_name_to_id(name) result(id)
character(len=*), intent(in) :: name
```

**Marker Rendering Geometry**:
- **Circle**: Standard filled circle with configurable radius
- **Square**: Axis-aligned square with edge/fill color support
- **Triangle**: Equilateral triangle pointing up/down/left/right variants
- **Diamond**: 45-degree rotated square (rhombus)
- **Star**: 5-pointed star with inner/outer radius control
- **Plus/Cross**: Line-based markers with thickness control
- **Polygons**: Regular n-sided polygons (pentagon, hexagon, octagon)

#### Color Mapping System Architecture

**Colormap Engine**:
```fortran
type :: colormap_t
    character(len=32) :: name              ! Colormap identifier
    real(wp), allocatable :: color_data(:,:) ! RGB values [0,1] (n_colors, 3)
    integer :: n_colors                    ! Number of discrete colors
    real(wp) :: value_min, value_max       ! Data range for mapping
contains
    procedure :: map_value => colormap_map_value
    procedure :: get_color => colormap_get_color
    procedure :: set_range => colormap_set_range
end type

! Standard colormaps
integer, parameter :: CMAP_VIRIDIS = 1    ! Perceptually uniform
integer, parameter :: CMAP_PLASMA  = 2    ! Purple-pink-yellow
integer, parameter :: CMAP_INFERNO = 3    ! Black-red-yellow  
integer, parameter :: CMAP_MAGMA   = 4    ! Black-purple-white
integer, parameter :: CMAP_JET     = 5    ! Blue-cyan-yellow-red (traditional)
integer, parameter :: CMAP_RAINBOW = 6    ! Full spectrum
integer, parameter :: CMAP_GRAYSCALE = 7  ! Black to white
```

**Color Interpolation Algorithm**:
- **Linear interpolation**: Between discrete colormap points
- **Value normalization**: Map data range to [0,1] colormap range
- **Edge handling**: Clamp out-of-range values to colormap extremes
- **NaN handling**: Default color for invalid data values

#### Size Mapping System Architecture

**Size Scaling Strategy**:
```fortran
! Size mapping configuration
type :: size_mapping_t
    real(wp) :: min_size = 10.0_wp         ! Minimum marker size (points)
    real(wp) :: max_size = 100.0_wp        ! Maximum marker size (points)
    real(wp) :: scale_factor = 1.0_wp      ! Global scaling multiplier
    logical :: linear_scaling = .true.     ! Linear vs sqrt scaling
    real(wp) :: value_min, value_max       ! Data range for size mapping
contains
    procedure :: map_size => size_mapping_map_size
    procedure :: set_range => size_mapping_set_range
end type
```

**Size Calculation Algorithm**:
- **Linear scaling**: `size = min_size + (value - min_val) / (max_val - min_val) * (max_size - min_size)`
- **Area-based scaling**: `size = sqrt(area_factor * normalized_value)` for bubble charts
- **Bounds checking**: Ensure size values remain within reasonable rendering limits
- **Performance optimization**: Pre-compute scaling factors for large datasets

### API Design Architecture

#### Primary Scatter Plot Interface
```fortran
! Comprehensive scatter plot API
subroutine scatter(self, x, y, s, c, marker, colormap, alpha, &
                   edgecolor, facecolor, linewidth, label, show_colorbar)
    class(figure_t), intent(inout) :: self
    real(wp), intent(in) :: x(:), y(:)          ! Position data
    real(wp), intent(in), optional :: s(:)      ! Size mapping data
    real(wp), intent(in), optional :: c(:)      ! Color mapping data
    character(len=*), intent(in), optional :: marker    ! Marker shape
    character(len=*), intent(in), optional :: colormap  ! Color scheme
    real(wp), intent(in), optional :: alpha             ! Transparency
    real(wp), intent(in), optional :: edgecolor(3)      ! Edge RGB
    real(wp), intent(in), optional :: facecolor(3)      ! Face RGB
    real(wp), intent(in), optional :: linewidth         ! Edge thickness
    character(len=*), intent(in), optional :: label     ! Legend label
    logical, intent(in), optional :: show_colorbar      ! Colorbar control
end subroutine

! Global convenience interface
subroutine scatter(x, y, s, c, marker, colormap, alpha, &
                   edgecolor, facecolor, linewidth, label, show_colorbar)
```

#### Colorbar Integration API
```fortran
! Colorbar management for color-mapped scatter plots
subroutine add_colorbar(self, position, size, label, ticks, format)
    class(figure_t), intent(inout) :: self
    character(len=*), intent(in), optional :: position  ! 'right', 'bottom', etc.
    real(wp), intent(in), optional :: size(2)          ! Width, height fractions
    character(len=*), intent(in), optional :: label     ! Colorbar title
    real(wp), intent(in), optional :: ticks(:)         ! Custom tick positions
    character(len=*), intent(in), optional :: format    ! Tick label format
end subroutine
```

### Backend Rendering Architecture

#### PNG/PDF Backend Implementation
**High-Resolution Marker Rendering**:
- **Vector graphics**: True geometric shapes for perfect scaling
- **Antialiasing**: Smooth edges for publication-quality output
- **Color accuracy**: Full RGB color space support with alpha blending
- **Performance optimization**: Batch rendering for large datasets
- **Memory efficiency**: Streaming geometry generation for 10^4+ markers

**Marker Rendering Pipeline**:
1. **Geometry generation**: Calculate marker vertices/curves based on shape
2. **Size scaling**: Apply size mapping to base marker geometry
3. **Color mapping**: Apply colormap to determine marker colors
4. **Alpha blending**: Combine edge/face colors with transparency
5. **Batch submission**: Group similar markers for efficient rendering

#### ASCII Backend Strategy
**Character-Based Marker Representation**:
```fortran
! ASCII marker character mapping
character(len=1), parameter :: ASCII_MARKERS(10) = &
    ['o', 's', '^', 'D', '*', '+', 'x', 'p', 'h', '8']
    
! Size representation through character repetition/spacing
character(len=3), parameter :: ASCII_SIZES(3) = ['.', 'o', 'O']
```

**ASCII Color Representation**:
- **ANSI color codes**: Terminal color support where available
- **Character intensity**: Different symbols for different color ranges
- **Fallback mode**: Monochrome representation for basic terminals
- **Colorbar simulation**: ASCII art colorbar with character gradients

**ASCII Layout Optimization**:
- **Marker overlap resolution**: Priority-based character placement
- **Grid alignment**: Snap markers to character grid positions
- **Density adaptation**: Adjust marker density for readable output
- **Legend integration**: ASCII-compatible legend for marker meanings

#### Animation Backend Enhancement
**Simplified Scatter Animation**:
- **Static markers**: Focus on position animation rather than complex marker rendering
- **Performance priority**: Optimized frame generation for smooth animation
- **Basic color support**: Limited color mapping for animation efficiency
- **Size variation**: Simple size changes for bubble chart animations

### Performance Optimization Architecture

#### Large Dataset Optimization (10^4+ Points)
**Rendering Performance Strategies**:
- **Culling optimization**: Skip markers completely outside plot boundaries
- **Level-of-detail**: Reduce marker complexity at small sizes
- **Batch processing**: Group identical markers for efficient rendering
- **Memory streaming**: Process marker data in chunks to control memory usage
- **Spatial indexing**: Optimize marker overlap detection and resolution

**Memory Efficiency Patterns**:
- **Lazy evaluation**: Generate marker geometry on-demand
- **Data compression**: Compress repeated size/color values
- **Cache optimization**: Reuse computed geometry for identical markers
- **Memory pooling**: Reuse allocation buffers for marker rendering

#### Backend-Specific Optimizations
**PNG/PDF Optimizations**:
- **Vector reuse**: Cache common marker shapes as reusable objects
- **Path optimization**: Combine similar markers into compound paths
- **Color grouping**: Batch markers by color to minimize state changes
- **Transparency optimization**: Optimize alpha blending operations

**ASCII Optimizations**:
- **Character buffer**: Pre-allocate full plot character matrix
- **Collision detection**: Efficient marker overlap resolution algorithms
- **Color minimization**: Reduce color palette for performance
- **Layout caching**: Cache character positioning calculations

### Error Handling and Validation Architecture

#### Input Validation Framework
**Data Consistency Checks**:
```fortran
! Comprehensive input validation
subroutine validate_scatter_inputs(x, y, s, c, success, error_msg)
    real(wp), intent(in) :: x(:), y(:)
    real(wp), intent(in), optional :: s(:), c(:)
    logical, intent(out) :: success
    character(len=:), allocatable, intent(out) :: error_msg
    
    ! Array dimension consistency
    ! NaN/Inf value detection
    ! Size/color data range validation
    ! Memory allocation verification
end subroutine
```

**Runtime Error Handling**:
- **Graceful degradation**: Fall back to basic markers on rendering failure
- **Memory safety**: Proper cleanup on allocation failures
- **Backend fallback**: Switch to simpler backend if advanced features fail
- **User feedback**: Clear error messages for common user mistakes

#### Edge Case Management
**Boundary Conditions**:
- **Empty data**: Handle zero-length arrays gracefully
- **Single point**: Ensure scatter plot works with single data point
- **Identical values**: Handle datasets with no size/color variation
- **Extreme ranges**: Manage very large or very small size/color ranges
- **Missing data**: Handle NaN values in size/color mapping arrays

### Integration Architecture

#### Plotting System Integration
**Plot Type Classification**:
```fortran
integer, parameter :: PLOT_TYPE_SCATTER = 5    ! New scatter plot type
```

**Rendering Pipeline Integration**:
- **Backend dispatch**: Route scatter plots to appropriate backend renderer
- **Legend integration**: Add scatter markers to plot legends
- **Colorbar coordination**: Integrate colorbar with main plot layout
- **Layout management**: Handle colorbar space allocation

#### Memory Management Integration
**RAII Pattern Application**:
- **Automatic cleanup**: Scatter data structures with proper finalizers
- **Exception safety**: Ensure cleanup on error conditions
- **Resource management**: Proper handling of colormap and geometry data
- **Copy semantics**: Safe deep copying of scatter plot data

### Implementation Plan

#### Phase 1: Core Scatter Infrastructure (1-2 days)
**Foundation Layer**:
1. **Data structure enhancement**: Extend `plot_data_t` with scatter fields
2. **Basic API implementation**: Core `scatter()` subroutine in `fortplot_figure_core.f90`
3. **Marker shape enumeration**: Define marker types and conversion utilities
4. **Input validation**: Comprehensive parameter checking and error handling

**Deliverables**:
- Enhanced plot data structures
- Basic scatter plot API (no size/color mapping yet)
- Marker shape identification system
- Input validation framework

#### Phase 2: Size and Color Mapping (2-3 days)  
**Mapping Systems**:
1. **Size mapping engine**: Size scaling algorithms and configuration
2. **Colormap implementation**: Standard colormap definitions and interpolation
3. **Color mapping engine**: Value-to-color conversion with range handling
4. **API enhancement**: Full scatter API with size/color support

**Deliverables**:
- Complete size mapping system
- Comprehensive colormap engine
- Full scatter plot API implementation
- Size and color validation systems

#### Phase 3: Backend Rendering Implementation (3-4 days)
**Multi-Backend Support**:
1. **PNG/PDF rendering**: High-quality marker geometry rendering
2. **ASCII backend**: Creative character-based marker representation
3. **Animation support**: Basic scatter plot animation capabilities
4. **Performance optimization**: Large dataset handling optimization

**Deliverables**:
- Complete PNG/PDF marker rendering
- ASCII scatter plot representation
- Animation backend integration
- Performance benchmarks for 10^4+ points

#### Phase 4: Colorbar and Advanced Features (2-3 days)
**Enhanced Visualization**:
1. **Colorbar implementation**: Automatic colorbar generation and layout
2. **Advanced marker features**: Edge/face color control, transparency
3. **Layout integration**: Colorbar space management and positioning
4. **User experience polish**: Intuitive defaults and error messages

**Deliverables**:
- Complete colorbar system
- Advanced marker customization
- Integrated layout management
- Polished user experience

#### Phase 5: Testing and Documentation (2-3 days)
**Quality Assurance**:
1. **Comprehensive test suite**: All scatter features and edge cases
2. **Performance testing**: Large dataset benchmarks and optimization
3. **Example implementation**: Complete scatter plot demonstration
4. **Documentation updates**: API documentation and usage guides

**Deliverables**:
- Complete test coverage (>90%)
- Performance validation
- Working example (`scatter_demo.f90`)
- Updated documentation

### Risk Assessment

#### Technical Risks
**Rendering Complexity**: Multi-backend marker rendering with consistent appearance
- **Mitigation**: Start with basic shapes, incrementally add complexity
- **Mitigation**: Comprehensive backend testing for visual consistency

**Performance Scalability**: Large dataset rendering (10^4+ points)
- **Mitigation**: Implement culling and level-of-detail optimizations early
- **Mitigation**: Performance benchmarking throughout development

**ASCII Backend Limitations**: Complex marker shapes in character mode
- **Mitigation**: Focus on creative character usage and fallback strategies
- **Mitigation**: Accept ASCII limitations while maintaining functionality

#### Integration Risks
**API Complexity**: Comprehensive scatter API with many optional parameters
- **Mitigation**: Provide sensible defaults and clear documentation
- **Mitigation**: Incremental API development with user feedback integration

**Memory Management**: Efficient handling of size/color mapping arrays
- **Mitigation**: Implement RAII patterns consistently
- **Mitigation**: Comprehensive memory testing with large datasets

#### Schedule Risks
**Feature Scope**: Comprehensive scatter plot implementation is substantial
- **Mitigation**: Phase-based development with deliverable milestones
- **Mitigation**: Focus on MVP functionality first, enhance incrementally

### Opportunity Analysis

#### Scientific Visualization Enhancement
**Multi-dimensional Data Analysis**:
- **Bubble charts**: Size mapping enables three-dimensional data visualization
- **Correlation analysis**: Color mapping reveals additional variable relationships
- **Statistical visualization**: Enhanced capability for research and analysis

**Publication Quality Output**:
- **Professional appearance**: High-quality marker rendering for papers
- **Colormap standards**: Scientific colormap support (viridis, plasma)
- **Customization control**: Precise marker styling for publication requirements

#### Performance Advantages
**Native Implementation**: 
- **No external dependencies**: Pure Fortran implementation
- **Optimized rendering**: Backend-specific optimization opportunities
- **Memory efficiency**: Direct integration with existing plot data structures

#### Extensibility Foundation
**Advanced Visualization Features**:
- **3D scatter plots**: Foundation for three-dimensional marker rendering
- **Interactive features**: Base for hover/click interactions in future GUI backends
- **Statistical overlays**: Foundation for confidence ellipses, regression lines

### Success Criteria

#### Phase 1 Success Metrics
- ✅ Basic scatter plot API compiles and executes
- ✅ Marker shape enumeration system functional
- ✅ Input validation catches common user errors
- ✅ Integration with existing plotting system seamless

#### Phase 2 Success Metrics  
- ✅ Size mapping produces intuitive bubble charts
- ✅ Color mapping with standard colormaps functional
- ✅ Combined size/color mapping works correctly
- ✅ Edge cases handled gracefully (NaN, extremes)

#### Phase 3 Success Metrics
- ✅ PNG/PDF backend produces publication-quality markers
- ✅ ASCII backend provides usable character-based representation
- ✅ Performance acceptable for 10^4+ data points
- ✅ All backends produce consistent visual output

#### Phase 4 Success Metrics
- ✅ Colorbar integration functional and aesthetically pleasing
- ✅ Advanced marker features (edge/face colors, alpha) working
- ✅ Layout management handles colorbar space allocation
- ✅ User experience intuitive with good defaults

#### Phase 5 Success Metrics
- ✅ Test coverage >90% with comprehensive edge case testing
- ✅ Performance benchmarks meet targets
- ✅ Example demonstrates all key features clearly
- ✅ Documentation complete and user-friendly

### Architecture Principles Validation

**SOLID Principles Applied**:
- **Single Responsibility**: Scatter system focused solely on marker-based visualization
- **Open/Closed**: Extensible for new marker shapes and colormaps
- **Liskov Substitution**: Scatter plots integrate seamlessly with existing plot types
- **Interface Segregation**: Clear separation between size mapping, color mapping, rendering
- **Dependency Inversion**: Abstract interfaces for colormaps and marker renderers

**Performance-First Design**:
- **Optimized data structures**: Efficient storage for large datasets
- **Backend specialization**: Rendering optimized for each output format
- **Memory efficiency**: RAII patterns and careful allocation management
- **Algorithmic efficiency**: O(n) complexity for all core operations

**Strategic Impact Assessment**:
Enhanced scatter plot implementation significantly expands fortplot's scientific visualization capabilities while maintaining architectural consistency. The comprehensive feature set positions fortplot as a mature alternative to matplotlib for Fortran-based scientific applications, with performance advantages for large datasets and native integration with computational workflows.

## Next Steps

1. **Immediate**: Create root CMakeLists.txt with minimal export configuration
2. **Short-term**: Add ffmpeg detection and graceful degradation
3. **Medium-term**: Comprehensive integration testing and documentation