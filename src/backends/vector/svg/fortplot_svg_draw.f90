module fortplot_svg_draw
    !! Standalone SVG drawing primitives: line, arrow, quad, heatmap, file I/O

   use, intrinsic :: iso_fortran_env, only: wp => real64
   use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
   use fortplot_colormap, only: colormap_value_to_color
   use fortplot_logging, only: log_error, log_info
   implicit none

   private
   public :: svg_draw_line_impl, svg_draw_arrow_impl, svg_fill_quad_impl, &
             svg_fill_heatmap_impl, svg_write_file_impl, svg_add_to_stream

contains

   subroutine svg_add_to_stream(stream, content)
      character(len=:), allocatable, intent(inout) :: stream
      character(len=*), intent(in) :: content

      stream = stream//content//new_line('a')
   end subroutine svg_add_to_stream

   subroutine svg_draw_line_impl(x1, y1, x2, y2, &
                                 pa_left, pa_bottom, pa_width, pa_height, &
                                 x_min, x_max, y_min, y_max, &
                                 r, g, b, line_width, dash_pattern, &
                                 svg_content)
      real(wp), intent(in) :: x1, y1, x2, y2
      real(wp), intent(in) :: pa_left, pa_bottom, pa_width, pa_height
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      real(wp), intent(in) :: r, g, b, line_width
      character(len=*), intent(in) :: dash_pattern
      character(len=:), allocatable, intent(inout) :: svg_content
      real(wp) :: sx1, sy1, sx2, sy2
      real(wp) :: x_range, y_range
      character(len=1024) :: line_elem
      character(len=128) :: stroke_dasharray

      if (ieee_is_nan(x1) .or. ieee_is_nan(y1) .or. &
          ieee_is_nan(x2) .or. ieee_is_nan(y2)) return

      x_range = x_max - x_min
      y_range = y_max - y_min
      if (abs(x_range) < 1.0e-12_wp) x_range = 1.0_wp
      if (abs(y_range) < 1.0e-12_wp) y_range = 1.0_wp

      sx1 = pa_left + (x1 - x_min)/x_range*pa_width
      sy1 = pa_bottom + pa_height - (y1 - y_min)/y_range*pa_height
      sx2 = pa_left + (x2 - x_min)/x_range*pa_width
      sy2 = pa_bottom + pa_height - (y2 - y_min)/y_range*pa_height

      stroke_dasharray = ''
      if (len_trim(dash_pattern) > 0) then
         stroke_dasharray = ' stroke-dasharray="'//trim(dash_pattern)//'"'
      end if

      write (line_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A,F0.1,A,F0.1,A, &
&           F0.3,A)') &
           '<line x1="', sx1, '" y1="', sy1, '" x2="', sx2, '" y2="', sy2, &
           '" stroke="rgb(', r*255.0_wp, ',', g*255.0_wp, ',', b*255.0_wp, &
           ')" stroke-width="', line_width, &
           '"'//trim(stroke_dasharray)//'/>'
      call svg_add_to_stream(svg_content, trim(line_elem))
   end subroutine svg_draw_line_impl

   subroutine svg_draw_arrow_impl(x, y, dx, dy, size, style, &
                                  pa_left, pa_bottom, pa_width, pa_height, &
                                  x_min, x_max, y_min, y_max, &
                                  r, g, b, svg_content)
      real(wp), intent(in) :: x, y, dx, dy, size
      character(len=*), intent(in) :: style
      real(wp), intent(in) :: pa_left, pa_bottom, pa_width, pa_height
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      real(wp), intent(in) :: r, g, b
      character(len=:), allocatable, intent(inout) :: svg_content
      real(wp) :: sx, sy, mag, nx, ny, px, py
      real(wp) :: arrow_len, arrow_w, base_x, base_y
      real(wp) :: left_x, left_y, right_x, right_y
      real(wp) :: x_range, y_range
      character(len=1024) :: elem

      x_range = x_max - x_min
      y_range = y_max - y_min
      if (abs(x_range) < 1.0e-12_wp) x_range = 1.0_wp
      if (abs(y_range) < 1.0e-12_wp) y_range = 1.0_wp

      sx = pa_left + (x - x_min)/x_range*pa_width
      sy = pa_bottom + pa_height - (y - y_min)/y_range*pa_height

      mag = sqrt(dx*dx + dy*dy)
      if (mag < 1.0e-12_wp) return
      nx = dx/mag
      ny = -dy/mag
      px = -ny
      py = nx

      arrow_len = max(4.0_wp, 1.5_wp*size)
      arrow_w = 0.55_wp*arrow_len
      base_x = sx - arrow_len*nx
      base_y = sy - arrow_len*ny
      left_x = base_x + arrow_w*px
      left_y = base_y + arrow_w*py
      right_x = base_x - arrow_w*px
      right_y = base_y - arrow_w*py

      if (index(style, '>') > 0 .or. index(style, '<') > 0 .or. &
          style == 'filled' .or. style == 'open') then
        write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A,F0.1,A,F0.1,A)') &
             '<line x1="', sx - mag*nx, '" y1="', sy - mag*ny, '" x2="', sx, &
             '" y2="', sy, '" stroke="rgb(', &
             r*255.0_wp, ',', g*255.0_wp, ',', b*255.0_wp, &
             ')"/>'
         call svg_add_to_stream(svg_content, trim(elem))

       write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A, &
&               F0.1,A,F0.1,A)') &
              '<polygon points="', sx, ',', sy, ' ', left_x, ',', left_y, &
              ' ', right_x, ',', right_y, '" fill="rgb(', &
              r*255.0_wp, ',', g*255.0_wp, ',', b*255.0_wp, ')"/>'
         call svg_add_to_stream(svg_content, trim(elem))
      end if
   end subroutine svg_draw_arrow_impl

   subroutine svg_fill_quad_impl(x_quad, y_quad, &
                                 pa_left, pa_bottom, pa_width, pa_height, &
                                 x_min, x_max, y_min, y_max, &
                                 r, g, b, svg_content)
      real(wp), intent(in) :: x_quad(4), y_quad(4)
      real(wp), intent(in) :: pa_left, pa_bottom, pa_width, pa_height
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      real(wp), intent(in) :: r, g, b
      character(len=:), allocatable, intent(inout) :: svg_content
      real(wp) :: sx(4), sy(4)
      real(wp) :: x_range, y_range
      character(len=1024) :: elem
      integer :: i

      x_range = x_max - x_min
      y_range = y_max - y_min
      if (abs(x_range) < 1.0e-12_wp) x_range = 1.0_wp
      if (abs(y_range) < 1.0e-12_wp) y_range = 1.0_wp

      do i = 1, 4
         sx(i) = pa_left + (x_quad(i) - x_min)/x_range*pa_width
         sy(i) = pa_bottom + pa_height - (y_quad(i) - y_min)/y_range*pa_height
      end do

     write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3, &
&           A,F0.1,A,F0.1,A,F0.1,A)') &
           '<polygon points="', sx(1), ',', sy(1), ' ', sx(2), ',', sy(2), &
           ' ', sx(3), ',', sy(3), ' ', sx(4), ',', sy(4), &
           '" fill="rgb(', r*255.0_wp, ',', g*255.0_wp, ',', b*255.0_wp, &
           ')" stroke="none"/>'
      call svg_add_to_stream(svg_content, trim(elem))
   end subroutine svg_fill_quad_impl

   subroutine svg_fill_heatmap_impl(x_grid, y_grid, z_grid, z_min, z_max, &
                                    colormap_name, &
                                    pa_left, pa_bottom, pa_width, pa_height, &
                                    x_min, x_max, y_min, y_max, &
                                    svg_content)
      real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
      real(wp), intent(in) :: z_min, z_max
      real(wp), intent(in) :: pa_left, pa_bottom, pa_width, pa_height
      real(wp), intent(in) :: x_min, x_max, y_min, y_max
      character(len=*), intent(in), optional :: colormap_name
      character(len=:), allocatable, intent(inout) :: svg_content
      integer :: i, j, nx, ny
      real(wp) :: x_quad(4), y_quad(4), value
      real(wp), dimension(3) :: clr
      character(len=20) :: cmap
      real(wp) :: cur_r, cur_g, cur_b

      cmap = 'viridis'
      if (present(colormap_name)) cmap = trim(colormap_name)

      nx = size(x_grid)
      ny = size(y_grid)
      if (nx < 2 .or. ny < 2) return
      if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return

      do j = 1, ny - 1
         do i = 1, nx - 1
            value = z_grid(j, i)
            call colormap_value_to_color(value, z_min, z_max, cmap, clr)
            cur_r = clr(1)
            cur_g = clr(2)
            cur_b = clr(3)

            x_quad(1) = x_grid(i)
            x_quad(2) = x_grid(i + 1)
            x_quad(3) = x_grid(i + 1)
            x_quad(4) = x_grid(i)
            y_quad(1) = y_grid(j)
            y_quad(2) = y_grid(j)
            y_quad(3) = y_grid(j + 1)
            y_quad(4) = y_grid(j + 1)
            call svg_fill_quad_impl(x_quad, y_quad, &
                                    pa_left, pa_bottom, pa_width, pa_height, &
                                    x_min, x_max, y_min, y_max, &
                                    cur_r, cur_g, cur_b, svg_content)
         end do
      end do
   end subroutine svg_fill_heatmap_impl

   subroutine svg_write_file_impl(filename, content_stream, width, height, ios)
      use fortplot_system_viewer, only: launch_system_viewer, &
                                        has_graphical_session, &
                                        get_temp_filename
      character(len=*), intent(in) :: filename
      character(len=:), allocatable, intent(in) :: content_stream
      integer, intent(in) :: width, height
      integer, intent(out) :: ios
      integer :: unit
      character(len=1024) :: actual_filename
      logical :: viewer_success

      ios = 0

      if (trim(filename) == 'terminal') then
         if (has_graphical_session()) then
            call get_temp_filename('.svg', actual_filename)
         else
            call log_info("No graphical session detected for SVG display")
            return
         end if
      else
         actual_filename = filename
      end if

      open (newunit=unit, file=trim(actual_filename), status='replace', &
            form='formatted', action='write', iostat=ios)
      if (ios /= 0) then
         call log_error('SVG: failed to open file: '//trim(actual_filename))
         return
      end if

      write (unit, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
      write (unit, '(A,I0,A,I0,A)') &
         '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', &
         width, ' ', height, '">'

      write (unit, '(A,I0,A,I0,A)') &
         '<rect width="', width, '" height="', height, &
         '" fill="white"/>'

      if (allocated(content_stream)) then
         write (unit, '(A)') trim(content_stream)
      end if

      write (unit, '(A)') '</svg>'
      close (unit)

      if (trim(filename) == 'terminal' .and. has_graphical_session()) then
         call launch_system_viewer(actual_filename, viewer_success)
         if (.not. viewer_success) then
            call log_error("Failed to launch SVG viewer")
         end if
      end if
   end subroutine svg_write_file_impl

end module fortplot_svg_draw
