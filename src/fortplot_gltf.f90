module fortplot_gltf
    !! GLTF backend for 3D plot export
    !! Following SRP - handles only GLTF file generation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_gltf_base
    use fortplot_gltf_writer
    use fortplot_gltf_geometry
    use fortplot_gltf_buffer
    implicit none
    
    private
    public :: gltf_context, create_gltf_canvas
    
    type, extends(plot_context) :: gltf_context
        !! GLTF context for 3D plot export
        character(len=:), allocatable :: filename
    contains
        procedure :: save => save_gltf
        procedure :: line => gltf_line
        procedure :: color => gltf_color
        procedure :: text => gltf_text
        procedure :: set_line_width => gltf_set_line_width
        procedure :: draw_marker => gltf_draw_marker
        procedure :: set_marker_colors => gltf_set_marker_colors
        procedure :: set_marker_colors_with_alpha => gltf_set_marker_colors_with_alpha
    end type gltf_context
    
contains

    function create_gltf_canvas(width, height) result(ctx)
        !! Create GLTF context for 3D export
        !! Following KISS - minimal initialization
        integer, intent(in) :: width, height
        type(gltf_context) :: ctx
        
        call setup_canvas(ctx, width, height)
    end function create_gltf_canvas

    subroutine save_gltf(this, filename)
        !! Save 3D plot data as GLTF file
        !! Following KISS - simple file writing
        class(gltf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: unit, iostat
        character(len=:), allocatable :: json
        
        this%filename = filename
        
        ! Generate GLTF JSON
        json = write_gltf_header()
        
        ! Write to file
        open(newunit=unit, file=filename, status='replace', &
             action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') json
            close(unit)
            print *, "GLTF file '" // trim(filename) // "' created successfully!"
        else
            print *, "Error: Failed to create GLTF file"
        end if
        
    end subroutine save_gltf
    
    ! Placeholder implementations for abstract methods
    subroutine gltf_line(this, x1, y1, x2, y2)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        ! Not used for GLTF
    end subroutine gltf_line
    
    subroutine gltf_color(this, r, g, b)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        ! Store for material generation
    end subroutine gltf_color
    
    subroutine gltf_text(this, x, y, text)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        ! Not used for GLTF
    end subroutine gltf_text
    
    subroutine gltf_set_line_width(this, width)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: width
        ! Not used for GLTF
    end subroutine gltf_set_line_width
    
    subroutine gltf_draw_marker(this, x, y, style)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        ! Not used for GLTF
    end subroutine gltf_draw_marker
    
    subroutine gltf_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        ! Store for material generation
    end subroutine gltf_set_marker_colors
    
    subroutine gltf_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                           face_r, face_g, face_b, face_alpha)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        ! Store for material generation
    end subroutine gltf_set_marker_colors_with_alpha

end module fortplot_gltf