# Styling and Appearance Implementation Design

## Overview

This document describes the matplotlib-compatible styling and appearance system in fortplot, covering color management, typography, line styles, markers, and theme support. The implementation follows matplotlib's rcParams system and Artist property hierarchy while maintaining Fortran's type safety and performance characteristics.

## Problem Statement

The current styling implementation has several limitations:
1. **No centralized configuration** - Missing rcParams-style parameter management
2. **Limited color support** - Basic scientific colormaps without professional palettes
3. **No style sheets** - Missing theme and style management system
4. **Basic typography** - Limited font control without size/weight/style options
5. **Simple line styles** - No dash pattern support or style customization
6. **Missing property hierarchy** - No systematic styling inheritance
7. **No transparency support** - Limited alpha blending capabilities

## Solution: Matplotlib-Compatible Styling System

### Architecture

The solution consists of seven main components:

1. **Configuration system** - rcParams-style centralized parameter management
2. **Color management** - Complete color cycles, palettes, and colormaps
3. **Typography system** - Font families, sizes, weights, and text rendering
4. **Line and marker styles** - Comprehensive pattern and symbol support
5. **Style sheets** - Theme management and style application
6. **Artist properties** - Hierarchical property inheritance system
7. **Transparency and blending** - Alpha channel and compositing support

### Key Components

#### 1. Configuration System

Following matplotlib's rcParams in `rcsetup.py` (lines 1-100):

```fortran
module fortplot_rc
    implicit none
    private
    
    ! Parameter registry (matplotlib rcParams pattern)
    type :: rc_param_t
        character(len=50) :: key
        character(len=100) :: value
        character(len=20) :: param_type      ! 'real', 'integer', 'string', 'logical'
        character(len=200) :: valid_values   ! Comma-separated valid options
        character(len=200) :: description
    end type
    
    type :: rc_params_t
        type(rc_param_t), allocatable :: params(:)
        integer :: n_params = 0
    contains
        procedure :: set_param
        procedure :: get_param
        procedure :: validate_param
        procedure :: load_defaults
        procedure :: save_to_file
    end type
    
    type(rc_params_t), save :: rcParams
    
    ! Default parameter categories (matplotlib pattern)
contains
    subroutine initialize_default_rcparams()
        ! FIGURE parameters
        call rcParams%set_param('figure.figsize', '[6.4, 4.8]', 'Figure size in inches')
        call rcParams%set_param('figure.dpi', '100', 'Figure resolution')
        call rcParams%set_param('figure.facecolor', 'white', 'Figure background color')
        call rcParams%set_param('figure.edgecolor', 'white', 'Figure border color')
        
        ! FONT parameters
        call rcParams%set_param('font.family', 'sans-serif', 'Font family')
        call rcParams%set_param('font.size', '10.0', 'Default font size in points')
        call rcParams%set_param('font.weight', 'normal', 'Font weight')
        call rcParams%set_param('font.style', 'normal', 'Font style')
        
        ! LINES parameters
        call rcParams%set_param('lines.linewidth', '1.5', 'Line width in points')
        call rcParams%set_param('lines.linestyle', '-', 'Default line style')
        call rcParams%set_param('lines.color', 'C0', 'Default line color')
        call rcParams%set_param('lines.marker', 'None', 'Default marker')
        call rcParams%set_param('lines.markersize', '6.0', 'Marker size in points')
        
        ! AXES parameters
        call rcParams%set_param('axes.facecolor', 'white', 'Axes background color')
        call rcParams%set_param('axes.edgecolor', 'black', 'Axes spine color')
        call rcParams%set_param('axes.linewidth', '0.8', 'Axes spine line width')
        call rcParams%set_param('axes.prop_cycle', 'cycler("color", ["1f77b4", "ff7f0e", ...])', 'Color cycle')
        
        ! GRID parameters
        call rcParams%set_param('grid.color', 'b0b0b0', 'Grid color')
        call rcParams%set_param('grid.linestyle', '-', 'Grid line style')
        call rcParams%set_param('grid.linewidth', '0.8', 'Grid line width')
        call rcParams%set_param('grid.alpha', '1.0', 'Grid transparency')
    end subroutine
end module
```

#### 2. Color Management System

Following matplotlib's color system in `colors.py` and `_color_data.py`:

```fortran
type :: color_t
    real(wp) :: r, g, b, a = 1.0            ! RGBA components [0-1]
    character(len=20) :: name = ''           ! Optional color name
contains
    procedure :: from_hex                    ! Parse hex color '#1f77b4'
    procedure :: from_name                   ! Parse named color 'red'
    procedure :: from_cycle                  ! Parse cycle color 'C0'
    procedure :: to_hex, to_rgb
end type

type :: color_cycle_t
    type(color_t), allocatable :: colors(:)
    integer :: current_index = 1
    character(len=50) :: name = ''
contains
    procedure :: next_color
    procedure :: reset_cycle
    procedure :: set_colors
end type

! Professional color palettes (matplotlib defaults)
type(color_cycle_t), parameter :: TAB10_CYCLE = color_cycle_t( &
    colors = [ &
        color_t(0.122, 0.467, 0.706, 1.0), &  ! tab:blue
        color_t(1.000, 0.498, 0.055, 1.0), &  ! tab:orange
        color_t(0.173, 0.627, 0.173, 1.0), &  ! tab:green
        color_t(0.839, 0.153, 0.157, 1.0), &  ! tab:red
        color_t(0.580, 0.404, 0.741, 1.0), &  ! tab:purple
        color_t(0.549, 0.337, 0.294, 1.0), &  ! tab:brown
        color_t(0.890, 0.467, 0.761, 1.0), &  ! tab:pink
        color_t(0.498, 0.498, 0.498, 1.0), &  ! tab:gray
        color_t(0.737, 0.741, 0.133, 1.0), &  ! tab:olive
        color_t(0.090, 0.745, 0.812, 1.0)  ], &  ! tab:cyan
    name = 'tab10')

! Named color registry (matplotlib XKCD + BASE colors)
type :: color_registry_t
    type(color_t), allocatable :: colors(:)
    character(len=20), allocatable :: names(:)
contains
    procedure :: add_color
    procedure :: get_color_by_name
    procedure :: load_standard_colors
end type

subroutine load_standard_colors(registry)
    ! BASE_COLORS (single letter codes)
    call registry%add_color('b', color_t(0.0, 0.0, 1.0, 1.0))  ! blue
    call registry%add_color('g', color_t(0.0, 0.5, 0.0, 1.0))  ! green
    call registry%add_color('r', color_t(1.0, 0.0, 0.0, 1.0))  ! red
    call registry%add_color('c', color_t(0.0, 0.75, 0.75, 1.0)) ! cyan
    call registry%add_color('m', color_t(0.75, 0.0, 0.75, 1.0)) ! magenta
    call registry%add_color('y', color_t(1.0, 1.0, 0.0, 1.0))  ! yellow
    call registry%add_color('k', color_t(0.0, 0.0, 0.0, 1.0))  ! black
    call registry%add_color('w', color_t(1.0, 1.0, 1.0, 1.0))  ! white
    
    ! TABLEAU_COLORS (professional palette)
    call registry%add_color('tab:blue', color_t(0.122, 0.467, 0.706, 1.0))
    call registry%add_color('tab:orange', color_t(1.000, 0.498, 0.055, 1.0))
    ! ... more colors
end subroutine
```

#### 3. Typography System

Following matplotlib's font handling:

```fortran
type :: font_properties_t
    character(len=50) :: family = 'sans-serif'  ! Font family
    real(wp) :: size = 10.0                     ! Size in points
    character(len=20) :: weight = 'normal'      ! 'normal', 'bold', 'light'
    character(len=20) :: style = 'normal'       ! 'normal', 'italic', 'oblique'
    character(len=20) :: variant = 'normal'     ! 'normal', 'small-caps'
    character(len=20) :: stretch = 'normal'     ! 'normal', 'condensed', 'expanded'
contains
    procedure :: copy
    procedure :: set_size_relative              ! Relative sizing
end type

type :: font_manager_t
    character(len=200), allocatable :: font_paths(:)
    character(len=50), allocatable :: font_families(:)
    type(font_properties_t) :: default_font
contains
    procedure :: find_system_fonts
    procedure :: get_best_font                  ! Font matching algorithm
    procedure :: load_font_file
end type

! Font family fallback lists (matplotlib pattern)
character(len=*), parameter :: SANS_SERIF_FONTS(*) = [ &
    'DejaVu Sans   ', 'Bitstream Vera Sans', 'Computer Modern Sans Serif', &
    'Lucida Grande ', 'Verdana       ', 'Geneva        ', 'Lucid         ', &
    'Arial         ', 'Helvetica     ', 'Avant Garde   ', 'sans-serif    ' ]

character(len=*), parameter :: SERIF_FONTS(*) = [ &
    'DejaVu Serif  ', 'Bitstream Vera Serif', 'Computer Modern Roman', &
    'New Century Schoolbook', 'Century Schoolbook L', 'Utopia        ', &
    'ITC Bookman   ', 'Bookman       ', 'Nimbus Roman No9 L', &
    'Times New Roman', 'Times         ', 'Palatino      ', 'Charter       ', &
    'serif         ' ]
```

#### 4. Line and Marker Styles

Following matplotlib's line style patterns in `lines.py` (lines 33-96):

```fortran
type :: line_style_t
    character(len=20) :: name = '-'             ! Style name
    real(wp), allocatable :: dash_pattern(:)   ! Dash pattern in points
    real(wp) :: dash_offset = 0.0              ! Pattern offset
contains
    procedure :: scale_with_width               ! Scale pattern with line width
    procedure :: get_pattern_length
end type

! Matplotlib standard line styles
type(line_style_t), parameter :: LINE_STYLES(*) = [ &
    line_style_t('-', []),                     & ! solid
    line_style_t('--', [6.0, 6.0]),          & ! dashed  
    line_style_t('-.', [6.0, 3.0, 1.0, 3.0]), & ! dashdot
    line_style_t(':', [1.0, 3.0]),           & ! dotted
    line_style_t('None', [])                   ] ! no line

type :: marker_style_t
    character(len=10) :: symbol = 'o'          ! Marker symbol
    real(wp) :: size = 6.0                     ! Size in points
    type(color_t) :: facecolor                 ! Fill color
    type(color_t) :: edgecolor                 ! Edge color
    real(wp) :: edgewidth = 1.0               ! Edge line width
    character(len=20) :: fillstyle = 'full'    ! 'full', 'left', 'right', 'top', 'bottom', 'none'
contains
    procedure :: get_marker_path               ! Vector path for marker
    procedure :: get_marker_transform          ! Size/rotation transform
end type

! Complete matplotlib marker set
character(len=*), parameter :: MARKER_SYMBOLS(*) = [ &
    'o  ', 's  ', '^  ', 'v  ', '<  ', '>  ', 'D  ', 'd  ', &  ! Basic shapes
    'p  ', 'h  ', 'H  ', '*  ', '+  ', 'x  ', 'X  ', '|  ', &  ! Star/cross
    '_  ', '.  ', ',  ', '1  ', '2  ', '3  ', '4  '          ] ! Lines/tri
```

#### 5. Style Sheet System

Following matplotlib's style system in `style/core.py`:

```fortran
type :: style_sheet_t
    character(len=50) :: name = ''
    type(rc_param_t), allocatable :: parameters(:)
    logical :: is_context_style = .false.      ! Temporary vs permanent
contains
    procedure :: apply_style
    procedure :: save_current_state
    procedure :: restore_state
end type

type :: style_manager_t
    type(style_sheet_t), allocatable :: styles(:)
    type(style_sheet_t) :: saved_state         ! For context management
contains
    procedure :: load_style_from_file
    procedure :: register_style
    procedure :: apply_style_context
end type

! Built-in styles (matplotlib equivalents)
subroutine create_seaborn_style(style)
    call style%add_param('axes.facecolor', 'white')
    call style%add_param('axes.edgecolor', 'black')
    call style%add_param('axes.linewidth', '1.25')
    call style%add_param('axes.grid', 'True')
    call style%add_param('axes.axisbelow', 'True')
    call style%add_param('grid.linewidth', '1.0')
    call style%add_param('grid.color', 'b0b0b0')
    call style%add_param('axes.prop_cycle', 'cycler("color", ["4C72B0", "55A868", "C44E52", ...])')
    call style%add_param('font.size', '11.0')
    call style%add_param('legend.frameon', 'False')
end subroutine

! Style context manager (matplotlib pattern)
type :: style_context_t
    type(style_manager_t), pointer :: manager
    type(style_sheet_t) :: original_state
contains
    procedure :: enter_context => style_enter
    procedure :: exit_context => style_exit
end type
```

#### 6. Artist Property System

Following matplotlib's Artist hierarchy in `artist.py`:

```fortran
type, abstract :: artist_t
    logical :: visible = .true.
    real(wp) :: alpha = 1.0
    character(len=50) :: label = ''
    integer :: zorder = 0                       ! Drawing order
    type(color_t) :: color
    logical :: clip_on = .true.
    type(bbox_t) :: clip_box                    ! Clipping bounds
contains
    procedure(draw_interface), deferred :: draw
    procedure :: set_alpha, get_alpha
    procedure :: set_visible, get_visible
    procedure :: set_zorder, get_zorder
    procedure :: update_properties              ! Batch property update
end type

type, extends(artist_t) :: line_artist_t
    type(line_style_t) :: linestyle
    real(wp) :: linewidth = 1.0
    type(marker_style_t) :: markerstyle
    real(wp), allocatable :: xdata(:), ydata(:)
contains
    procedure :: draw => draw_line
    procedure :: set_linewidth, set_linestyle
    procedure :: set_marker, set_markersize
end type

type, extends(artist_t) :: text_artist_t
    character(len=200) :: text = ''
    type(font_properties_t) :: fontproperties
    real(wp) :: x = 0.0, y = 0.0               ! Position
    character(len=20) :: ha = 'left'           ! Horizontal alignment
    character(len=20) :: va = 'baseline'       ! Vertical alignment
    real(wp) :: rotation = 0.0                 ! Text rotation
contains
    procedure :: draw => draw_text
    procedure :: set_text, set_fontsize
    procedure :: set_position, set_rotation
end type
```

#### 7. Transparency and Blending

Following matplotlib's alpha compositing:

```fortran
type :: blending_t
    character(len=20) :: mode = 'normal'        ! 'normal', 'multiply', 'screen'
    real(wp) :: alpha = 1.0                     ! Global alpha
contains
    procedure :: composite_over                 ! Alpha blending
    procedure :: composite_colors               ! Color blending
end type

subroutine composite_over(fg_color, bg_color, alpha, result_color)
    ! Standard alpha compositing: C = αA + (1-α)B
    result_color%r = alpha * fg_color%r + (1.0 - alpha) * bg_color%r
    result_color%g = alpha * fg_color%g + (1.0 - alpha) * bg_color%g
    result_color%b = alpha * fg_color%b + (1.0 - alpha) * bg_color%b
    result_color%a = alpha + bg_color%a * (1.0 - alpha)
end subroutine
```

### Implementation Details

#### Style Application Pipeline

Following matplotlib's property update mechanism:

```fortran
subroutine apply_style_to_figure(fig, style_name)
    ! 1. Load style sheet
    style = global_style_manager%get_style(style_name)
    
    ! 2. Save current state for restoration
    call save_current_rcparams(saved_state)
    
    ! 3. Apply style parameters
    do i = 1, size(style%parameters)
        call rcParams%set_param(style%parameters(i)%key, style%parameters(i)%value)
    end do
    
    ! 4. Update all artists in figure
    call update_figure_artists(fig)
end subroutine

subroutine update_figure_artists(fig)
    ! Update all artists to reflect new rcParams
    do i = 1, size(fig%axes)
        call update_axes_artist_properties(fig%axes(i))
    end do
end subroutine
```

#### Color Cycle Management

Following matplotlib's cycler integration:

```fortran
subroutine setup_axes_color_cycle(axes, cycle_spec)
    ! Parse cycler specification
    call parse_cycler(cycle_spec, property_cycle)
    
    ! Apply to axes
    axes%prop_cycle = property_cycle
    axes%color_index = 1
end subroutine

function get_next_plot_properties(axes) result(props)
    ! Get next set of properties from cycle
    props = axes%prop_cycle%get_next()
    
    ! Update axes state
    axes%color_index = axes%color_index + 1
    if (axes%color_index > axes%prop_cycle%length) then
        axes%color_index = 1
    end if
end function
```

## Verification

### Test Cases

1. **rcParams system** - Verify parameter validation and persistence
2. **Color management** - Test color parsing, cycles, and conversion
3. **Typography** - Test font loading, sizing, and rendering
4. **Style sheets** - Verify style application and context management
5. **Artist properties** - Test property inheritance and updates
6. **Performance** - Benchmark styling operations and memory usage

### Expected Results

**Before (current implementation):**
- Basic scientific colormaps only
- Limited marker and line style support
- Simple font rendering without typography control
- No centralized configuration or style management
- Fixed appearance without customization

**After (matplotlib-compatible):**
- Complete color system with professional palettes and cycles
- Comprehensive line and marker style support
- Advanced typography with font families and properties
- rcParams configuration system with validation
- Style sheet support for themes and contexts
- Hierarchical artist property system with inheritance

## Performance Considerations

### Computational Complexity

- **Color lookup**: O(1) - hash table for named colors
- **Style application**: O(n_artists) - linear in number of artists
- **Font rendering**: O(text_length) - proportional to text complexity
- **Property updates**: O(property_count) - constant for typical usage

### Memory Usage

- **Color registry**: O(n_colors) - constant for standard palette
- **Font cache**: O(n_fonts × font_size) - cached font rasterization
- **Style sheets**: O(n_styles × n_params) - style parameter storage
- **Artist properties**: O(n_artists × n_properties) - property storage

### Optimization Opportunities

1. **Color caching** - Cache color conversions and lookups
2. **Font atlas** - Pre-render common character sets
3. **Style batching** - Batch style updates to minimize redraws
4. **Property compression** - Compact property storage for large scenes

## Future Enhancements

### Matplotlib Feature Parity

1. **Animation styling** - Style interpolation for animations
2. **3D styling** - Colors and materials for 3D plots
3. **Interactive styling** - Real-time style updates with GUI
4. **Style validation** - Advanced parameter validation and hints

### Advanced Features

1. **Perceptual colormaps** - Scientifically accurate color spaces
2. **Accessibility** - Colorblind-safe palettes and high contrast
3. **Brand styling** - Corporate style templates and guidelines
4. **Export optimization** - Vector graphics with embedded fonts

## References

1. **rcParams system**: `thirdparty/matplotlib/lib/matplotlib/rcsetup.py` (lines 1-100)
2. **Color management**: `thirdparty/matplotlib/lib/matplotlib/colors.py` (lines 1-150)
3. **Color data**: `thirdparty/matplotlib/lib/matplotlib/_color_data.py` (lines 1-100)
4. **Line styles**: `thirdparty/matplotlib/lib/matplotlib/lines.py` (lines 33-96)
5. **Style system**: `thirdparty/matplotlib/lib/matplotlib/style/core.py` (lines 1-100)
6. **Artist hierarchy**: `thirdparty/matplotlib/lib/matplotlib/artist.py` (lines 116-150)
7. **pyplot-fortran**: `thirdparty/pyplot-fortran/src/pyplot_module.F90` (styling patterns)

## Implementation Files

- **`src/fortplot_rc.f90`** - rcParams configuration system
- **`src/fortplot_colors.f90`** - Color management and palettes
- **`src/fortplot_typography.f90`** - Font and text styling system
- **`src/fortplot_line_styles.f90`** - Line and marker style definitions
- **`src/fortplot_style_sheets.f90`** - Style sheet management
- **`src/fortplot_artists.f90`** - Artist property hierarchy
- **`src/fortplot_blending.f90`** - Transparency and compositing
- **`python/fortplot/fortplot.py`** - Enhanced Python styling interface
- **`test/test_styling.f90`** - Comprehensive styling system tests
- **`test/test_color_management.f90`** - Color system verification

This implementation ensures fortplot provides matplotlib-compatible styling and appearance management with professional color palettes, advanced typography, comprehensive style sheets, and flexible artist property systems for high-quality scientific visualization.