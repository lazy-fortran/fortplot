module fortplot_ascii_elements
    !! ASCII terminal plotting backend - Drawing Elements (Re-export Module)
    !!
    !! This module re-exports functionality from focused ASCII backend modules
    !! to maintain backward compatibility while improving code organization.
    !!
    !! Author: fortplot contributors

    ! Re-export from specialized modules
    use fortplot_ascii_drawing, only: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    use fortplot_ascii_legend, only: render_ascii_legend_specialized, calculate_ascii_legend_dimensions
    use fortplot_ascii_legend, only: set_ascii_legend_border_width, calculate_ascii_legend_position
    use fortplot_ascii_legend, only: reset_ascii_legend_lines_helper, append_ascii_legend_line_helper
    use fortplot_ascii_legend, only: register_legend_entry_helper, assign_pending_autopct_helper
    use fortplot_ascii_text, only: draw_ascii_axes_and_labels, ascii_draw_text_helper
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    public :: render_ascii_legend_specialized, calculate_ascii_legend_dimensions
    public :: set_ascii_legend_border_width, calculate_ascii_legend_position
    public :: draw_ascii_axes_and_labels
    public :: reset_ascii_legend_lines_helper, append_ascii_legend_line_helper
    public :: register_legend_entry_helper, assign_pending_autopct_helper
    public :: ascii_draw_text_helper

contains
    ! No-op subroutines to maintain compatibility
    ! All functionality is re-exported from the specialized modules
end module fortplot_ascii_elements