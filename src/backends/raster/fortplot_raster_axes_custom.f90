module fortplot_raster_axes_custom
    !! Custom tick label and axis label wrapper procedures for raster backend
    !! Extracted from fortplot_raster_axes to maintain file size compliance
   use fortplot_margins, only: plot_area_t
   use fortplot_raster_core, only: raster_image_t
   use fortplot_raster_ticks, only: raster_draw_x_axis_tick_labels_only, &
                                    raster_draw_y_axis_tick_labels_only, &
                                    raster_draw_x_axis_ticks, &
                                    raster_draw_y_axis_ticks, &
                                    raster_draw_x_axis_tick_marks_only, &
                                    raster_draw_y_axis_tick_marks_only
   use fortplot_raster_labels, only: raster_draw_axis_labels
   use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
   use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                        format_tick_value_consistent
   use, intrinsic :: iso_fortran_env, only: wp => real64
   implicit none

   private
   public :: raster_draw_x_axis_tick_labels_only_custom
   public :: raster_draw_y_axis_tick_labels_only_custom
   public :: raster_draw_axis_labels_wrapper
   public :: raster_draw_x_axis_ticks_wrapper
   public :: raster_draw_y_axis_ticks_wrapper
   public :: raster_draw_x_axis_tick_marks_only_wrapper
   public :: raster_draw_y_axis_tick_marks_only_wrapper
   public :: raster_draw_x_axis_tick_labels_only_wrapper
   public :: raster_draw_y_axis_tick_labels_only_wrapper

contains

   subroutine raster_draw_x_axis_tick_labels_only_custom(raster, width, height, &
                                                         plot_area, xscale, &
                                                         symlog_threshold, &
                                                         x_min, x_max, &
                                                         positions, labels)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: xscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max
      real(wp), contiguous, intent(in) :: positions(:)
      character(len=*), intent(in) :: labels(:)

      character(len=50) :: tick_labels(size(positions))
      integer :: i, n

      n = size(positions)
      if (n == 0) return
      if (size(labels) /= n) return

      do i = 1, n
         tick_labels(i) = trim(labels(i))
      end do

      call raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
                                               xscale, symlog_threshold, &
                                               positions, tick_labels, &
                                               x_min, x_max)
   end subroutine raster_draw_x_axis_tick_labels_only_custom

   subroutine raster_draw_y_axis_tick_labels_only_custom(raster, width, height, &
                                                         plot_area, yscale, &
                                                         symlog_threshold, &
                                                         y_min, y_max, &
                                                         positions, labels)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: yscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: y_min, y_max
      real(wp), contiguous, intent(in) :: positions(:)
      character(len=*), intent(in) :: labels(:)

      character(len=50) :: tick_labels(size(positions))
      integer :: i, n

      n = size(positions)
      if (n == 0) return
      if (size(labels) /= n) return

      do i = 1, n
         tick_labels(i) = trim(labels(i))
      end do

      call raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, &
                                               yscale, symlog_threshold, &
                                               positions, tick_labels, &
                                               y_min, y_max)
   end subroutine raster_draw_y_axis_tick_labels_only_custom

   subroutine raster_draw_axis_labels_wrapper(raster, width, height, plot_area, &
                                              title, xlabel, ylabel)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
      character(len=:), allocatable :: title_str, xlabel_str, ylabel_str

      title_str = ""
      xlabel_str = ""
      ylabel_str = ""

      if (present(title)) then
         if (allocated(title)) title_str = title
      end if

      if (present(xlabel)) then
         if (allocated(xlabel)) xlabel_str = xlabel
      end if

      if (present(ylabel)) then
         if (allocated(ylabel)) ylabel_str = ylabel
      end if

      call raster_draw_axis_labels(raster, width, height, plot_area, title_str, &
                                   xlabel_str, ylabel_str)
   end subroutine raster_draw_axis_labels_wrapper

   subroutine raster_draw_x_axis_ticks_wrapper(raster, width, height, plot_area, &
                                               xscale, symlog_threshold, &
                                               x_min, x_max, &
                                               y_min, y_max, date_format)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: xscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      character(len=*), intent(in), optional :: date_format
      real(wp) :: x_tick_positions(MAX_TICKS)
      character(len=50) :: tick_labels(MAX_TICKS)
      integer :: tick_colors(3, MAX_TICKS)
      integer :: num_x_ticks, i, decimals

      call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                               x_tick_positions, num_x_ticks)

      if (num_x_ticks > 0) then
         decimals = 0
         if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
            decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
         end if

         do i = 1, num_x_ticks
            if (trim(xscale) == 'linear') then
               tick_labels(i) = &
                  format_tick_value_consistent(x_tick_positions(i), decimals)
            else
               tick_labels(i) = format_tick_label(x_tick_positions(i), xscale, &
                                                  date_format=date_format, &
                                                  data_min=x_min, data_max=x_max)
            end if
            tick_colors(:, i) = (/0, 0, 0/)
         end do

         call raster_draw_x_axis_ticks(raster, width, height, plot_area, xscale, &
                                       symlog_threshold, &
                                       x_tick_positions(1:num_x_ticks), &
                                       tick_labels(1:num_x_ticks), &
                                       tick_colors(:, 1:num_x_ticks), x_min, x_max)
      end if
   end subroutine raster_draw_x_axis_ticks_wrapper

   subroutine raster_draw_y_axis_ticks_wrapper(raster, width, height, plot_area, &
                                               yscale, symlog_threshold, &
                                               x_min, x_max, &
                                               y_min, y_max, date_format)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: yscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      character(len=*), intent(in), optional :: date_format
      real(wp) :: y_tick_positions(MAX_TICKS)
      character(len=50) :: tick_labels(MAX_TICKS)
      integer :: tick_colors(3, MAX_TICKS)
      integer :: num_y_ticks, i, decimals

      call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                               y_tick_positions, num_y_ticks)

      if (num_y_ticks > 0) then
         decimals = 0
         if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
            decimals = determine_decimals_from_ticks(y_tick_positions, num_y_ticks)
         end if

         do i = 1, num_y_ticks
            if (trim(yscale) == 'linear') then
               tick_labels(i) = &
                  format_tick_value_consistent(y_tick_positions(i), decimals)
            else
               tick_labels(i) = format_tick_label(y_tick_positions(i), yscale, &
                                                  date_format=date_format, &
                                                  data_min=y_min, data_max=y_max)
            end if
            tick_colors(:, i) = (/0, 0, 0/)
         end do

         call raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, &
                                       symlog_threshold, &
                                       y_tick_positions(1:num_y_ticks), &
                                       tick_labels(1:num_y_ticks), &
                                       tick_colors(:, 1:num_y_ticks), y_min, y_max)
      end if
   end subroutine raster_draw_y_axis_ticks_wrapper

   subroutine raster_draw_x_axis_tick_marks_only_wrapper(raster, width, height, &
                                                         plot_area, &
                                                         xscale, &
                                                         symlog_threshold, &
                                                         x_min, &
                                                         x_max, y_min, y_max)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: xscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      real(wp) :: x_tick_positions(MAX_TICKS)
      integer :: tick_colors(3, MAX_TICKS)
      integer :: num_x_ticks, i

      call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                               x_tick_positions, num_x_ticks)

      if (num_x_ticks > 0) then
         do i = 1, num_x_ticks
            tick_colors(:, i) = (/0, 0, 0/)
         end do
         call raster_draw_x_axis_tick_marks_only(raster, width, height, plot_area, &
                                                 xscale, symlog_threshold, &
                                                 x_tick_positions(1:num_x_ticks), &
                                                 tick_colors(:, &
                                                             1:num_x_ticks), &
                                                 x_min, x_max)
      end if
   end subroutine raster_draw_x_axis_tick_marks_only_wrapper

   subroutine raster_draw_y_axis_tick_marks_only_wrapper(raster, width, height, &
                                                         plot_area, &
                                                         yscale, &
                                                         symlog_threshold, &
                                                         x_min, &
                                                         x_max, y_min, y_max)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: yscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      real(wp) :: y_tick_positions(MAX_TICKS)
      integer :: tick_colors(3, MAX_TICKS)
      integer :: num_y_ticks, i

      call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                               y_tick_positions, num_y_ticks)

      if (num_y_ticks > 0) then
         do i = 1, num_y_ticks
            tick_colors(:, i) = (/0, 0, 0/)
         end do
         call raster_draw_y_axis_tick_marks_only(raster, width, height, plot_area, &
                                                 yscale, symlog_threshold, &
                                                 y_tick_positions(1:num_y_ticks), &
                                                 tick_colors(:, &
                                                             1:num_y_ticks), &
                                                 y_min, y_max)
      end if
   end subroutine raster_draw_y_axis_tick_marks_only_wrapper

   subroutine raster_draw_x_axis_tick_labels_only_wrapper(raster, width, height, &
                                                          plot_area, &
                                                          xscale, &
                                                          symlog_threshold, &
                                                          x_min, &
                                                          x_max, y_min, y_max, &
                                                          date_format)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: xscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      character(len=*), intent(in), optional :: date_format
      real(wp) :: x_tick_positions(MAX_TICKS)
      character(len=50) :: tick_labels(MAX_TICKS)
      integer :: num_x_ticks, i, decimals

      call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                               x_tick_positions, num_x_ticks)

      if (num_x_ticks > 0) then
         decimals = 0
         if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
            decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
         end if

         do i = 1, num_x_ticks
            if (trim(xscale) == 'linear') then
               tick_labels(i) = &
                  format_tick_value_consistent(x_tick_positions(i), decimals)
            else
               tick_labels(i) = format_tick_label(x_tick_positions(i), xscale, &
                                                  date_format=date_format, &
                                                  data_min=x_min, data_max=x_max)
            end if
         end do

         call raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
                                                  xscale, symlog_threshold, &
                                                  x_tick_positions(1:num_x_ticks), &
                                                  tick_labels(1:num_x_ticks), &
                                                  x_min, x_max)
      end if
   end subroutine raster_draw_x_axis_tick_labels_only_wrapper

   subroutine raster_draw_y_axis_tick_labels_only_wrapper(raster, width, height, &
                                                          plot_area, &
                                                          yscale, &
                                                          symlog_threshold, &
                                                          x_min, &
                                                          x_max, y_min, y_max, &
                                                          date_format)
      type(raster_image_t), intent(inout) :: raster
      integer, intent(in) :: width, height
      type(plot_area_t), intent(in) :: plot_area
      character(len=*), intent(in) :: yscale
      real(wp), intent(in) :: symlog_threshold
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      character(len=*), intent(in), optional :: date_format
      real(wp) :: y_tick_positions(MAX_TICKS)
      character(len=50) :: tick_labels(MAX_TICKS)
      integer :: num_y_ticks, i, decimals

      call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                               y_tick_positions, num_y_ticks)

      if (num_y_ticks > 0) then
         decimals = 0
         if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
            decimals = determine_decimals_from_ticks(y_tick_positions, num_y_ticks)
         end if

         do i = 1, num_y_ticks
            if (trim(yscale) == 'linear') then
               tick_labels(i) = &
                  format_tick_value_consistent(y_tick_positions(i), decimals)
            else
               tick_labels(i) = format_tick_label(y_tick_positions(i), yscale, &
                                                  date_format=date_format, &
                                                  data_min=y_min, data_max=y_max)
            end if
         end do

         call raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, &
                                                  yscale, symlog_threshold, &
                                                  y_tick_positions(1:num_y_ticks), &
                                                  tick_labels(1:num_y_ticks), &
                                                  y_min, y_max)
      end if
   end subroutine raster_draw_y_axis_tick_labels_only_wrapper

end module fortplot_raster_axes_custom
