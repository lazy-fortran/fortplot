module fortplot_ascii_backend_ops
    !! ASCII terminal plotting backend - Backend Operations
    !!
    !! Extracted implementation procedures for polymorphic backend operations.
    !!
    !! Author: fortplot contributors

   use fortplot_context, only: plot_context
   use fortplot_plot_data, only: plot_data_t
   use fortplot_ascii_utils, only: text_element_t
   use fortplot_ascii_elements, only: draw_ascii_axes_and_labels
   use fortplot_margins, only: plot_area_t
   use, intrinsic :: iso_fortran_env, only: wp => real64
   implicit none

   private
   public :: ascii_extract_rgb_impl, ascii_get_png_impl, ascii_prepare_3d_impl
   public :: ascii_render_ylabel_impl, ascii_draw_axes_impl
   public :: ascii_save_coord_impl, ascii_set_coord_impl, ascii_render_axes_impl

contains

   subroutine ascii_extract_rgb_impl(width, height, rgb_data)
      integer, intent(in) :: width, height
      real(wp), intent(out) :: rgb_data(width, height, 3)

      rgb_data = 0.0_wp
   end subroutine ascii_extract_rgb_impl

   subroutine ascii_get_png_impl(width, height, png_data, status)
      integer, intent(in) :: width, height
      integer(1), allocatable, intent(out) :: png_data(:)
      integer, intent(out) :: status

      allocate (png_data(0))
      status = -1
   end subroutine ascii_get_png_impl

   subroutine ascii_prepare_3d_impl(plots)
      type(plot_data_t), intent(in) :: plots(:)

      associate (unused_n => size(plots)); end associate
   end subroutine ascii_prepare_3d_impl

   subroutine ascii_render_ylabel_impl(ylabel)
      character(len=*), intent(in) :: ylabel

      associate (unused_l => len_trim(ylabel)); end associate
   end subroutine ascii_render_ylabel_impl

   subroutine ascii_draw_axes_impl(canvas, xscale, yscale, symlog_threshold, &
                                   x_min, x_max, y_min, y_max, &
                                   title, xlabel, ylabel, &
                                   x_date_format, y_date_format, &
                                   z_min, z_max, has_3d_plots, &
                                   current_r, current_g, current_b, &
                                   plot_area, &
                                   plot_width, plot_height, &
                                   title_text, xlabel_text, ylabel_text, &
                                   text_elements, num_text_elements, &
                          has_custom_ticks, custom_xtick_positions, custom_xtick_labels)
      character(len=1), intent(inout) :: canvas(:, :)
      character(len=*), intent(in) :: xscale, yscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
      character(len=*), intent(in), optional :: x_date_format, y_date_format
      real(wp), intent(in), optional :: z_min, z_max
      logical, intent(in) :: has_3d_plots
      real(wp), intent(in) :: current_r, current_g, current_b
      type(plot_area_t), intent(in) :: plot_area
      integer, intent(in) :: plot_width, plot_height
    character(len=:), allocatable, intent(inout) :: title_text, xlabel_text, ylabel_text
      type(text_element_t), intent(inout) :: text_elements(:)
      integer, intent(inout) :: num_text_elements
      logical, intent(in) :: has_custom_ticks
      real(wp), intent(in), optional :: custom_xtick_positions(:)
      character(len=*), intent(in), optional :: custom_xtick_labels(:)

      if (has_custom_ticks) then
         call draw_ascii_axes_and_labels(canvas, xscale, yscale, &
                                         symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title, xlabel, ylabel, &
                                         x_date_format, y_date_format, &
                                         z_min, z_max, has_3d_plots, &
                                         current_r, current_g, current_b, &
                                         plot_area, &
                                         plot_width, plot_height, &
                                         title_text, xlabel_text, ylabel_text, &
                                         text_elements, num_text_elements, &
                                         custom_xticks=custom_xtick_positions, &
                                         custom_xtick_labels=custom_xtick_labels)
      else
         call draw_ascii_axes_and_labels(canvas, xscale, yscale, &
                                         symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title, xlabel, ylabel, &
                                         x_date_format, y_date_format, &
                                         z_min, z_max, has_3d_plots, &
                                         current_r, current_g, current_b, &
                                         plot_area, &
                                         plot_width, plot_height, &
                                         title_text, xlabel_text, ylabel_text, &
                                         text_elements, num_text_elements)
      end if
   end subroutine ascii_draw_axes_impl

   subroutine ascii_save_coord_impl(x_min_in, x_max_in, y_min_in, y_max_in, &
                                    has_stored_y_range, stored_y_min, stored_y_max, &
                                    x_min_out, x_max_out, y_min_out, y_max_out)
      real(wp), intent(in) :: x_min_in, x_max_in, y_min_in, y_max_in
      logical, intent(in) :: has_stored_y_range
      real(wp), intent(in) :: stored_y_min, stored_y_max
      real(wp), intent(out) :: x_min_out, x_max_out, y_min_out, y_max_out

      x_min_out = x_min_in
      x_max_out = x_max_in
      if (has_stored_y_range) then
         y_min_out = stored_y_min
         y_max_out = stored_y_max
      else
         y_min_out = y_min_in
         y_max_out = y_max_in
      end if
   end subroutine ascii_save_coord_impl

   subroutine ascii_set_coord_impl(x_min, x_max, y_min, y_max, &
                                   x_min_out, x_max_out, y_min_out, y_max_out, &
                             stored_y_min_out, stored_y_max_out, has_stored_y_range_out)
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      real(wp), intent(out) :: x_min_out, x_max_out, y_min_out, y_max_out
      real(wp), intent(out) :: stored_y_min_out, stored_y_max_out
      logical, intent(out) :: has_stored_y_range_out

      x_min_out = x_min
      x_max_out = x_max
      y_min_out = y_min
      y_max_out = y_max
      stored_y_min_out = y_min
      stored_y_max_out = y_max
      has_stored_y_range_out = .true.
   end subroutine ascii_set_coord_impl

   subroutine ascii_render_axes_impl(x_min, x_max, y_min, y_max, &
                                     has_stored_y_range, stored_y_min, stored_y_max, &
                                     last_xscale, last_yscale, last_symlog_threshold, &
                                     canvas, plot_area, plot_width, plot_height, &
                                     title_text, xlabel_text, ylabel_text, &
                                     text_elements, num_text_elements, &
                                     custom_xtick_positions, custom_xtick_labels)
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      logical, intent(in) :: has_stored_y_range
      real(wp), intent(in) :: stored_y_min, stored_y_max
      character(len=*), intent(in) :: last_xscale, last_yscale
      real(wp), intent(in) :: last_symlog_threshold
      character(len=1), intent(inout) :: canvas(:, :)
      type(plot_area_t), intent(in) :: plot_area
      integer, intent(in) :: plot_width, plot_height
    character(len=:), allocatable, intent(inout) :: title_text, xlabel_text, ylabel_text
      type(text_element_t), intent(inout) :: text_elements(:)
      integer, intent(inout) :: num_text_elements
      real(wp), intent(in), optional :: custom_xtick_positions(:)
      character(len=*), intent(in), optional :: custom_xtick_labels(:)

      real(wp) :: sx_min, sx_max, sy_min, sy_max

      sx_min = x_min
      sx_max = x_max
      if (has_stored_y_range) then
         sy_min = stored_y_min
         sy_max = stored_y_max
      else
         sy_min = y_min
         sy_max = y_max
      end if

      call ascii_draw_axes_impl(canvas, last_xscale, last_yscale, &
                                last_symlog_threshold, &
                                sx_min, sx_max, sy_min, sy_max, &
                                has_3d_plots=.false., &
                                current_r=0.0_wp, current_g=0.0_wp, current_b=0.0_wp, &
                                plot_area=plot_area, &
                                plot_width=plot_width, plot_height=plot_height, &
                                title_text=title_text, xlabel_text=xlabel_text, &
                                ylabel_text=ylabel_text, &
                                text_elements=text_elements, &
                                num_text_elements=num_text_elements, &
                                has_custom_ticks=.false.)
   end subroutine ascii_render_axes_impl

end module fortplot_ascii_backend_ops
