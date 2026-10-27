module fortplot_spec_json_field_plot
    !! JSON serialization helpers for field_plot_t

   use, intrinsic :: iso_fortran_env, only: wp => real64
   use fortplot_spec_types, only: field_plot_t
   use fortplot_spec_json_reader, only: skip_ws, expect_char, read_string, &
                                       read_real, read_int, read_bool, skip_value
   use fortplot_spec_json_channels, only: parse_real_array
   implicit none

   private
   public :: parse_field_plot
   public :: serialize_field_plot, append_field_real_array, &
             append_field_string, append_field_bool, &
             append_field_int, append_field_real

   character(len=*), parameter :: NL = new_line('a')
   character(len=*), parameter :: Q = '"'

contains

   function serialize_field_plot(field, indent) result(json)
      type(field_plot_t), intent(in) :: field
      integer, intent(in) :: indent
      character(len=:), allocatable :: json, pad
      logical :: first

      pad = repeat(' ', indent)
      json = pad//'"fortplotField": {'//NL
      first = .true.

      call append_field_real_array(json, 'x', field%x, indent + 2, first)
      call append_field_real_array(json, 'y', field%y, indent + 2, first)
      call append_field_real_array(json, 'z', field%z, indent + 2, first)
      call append_field_real_array(json, 'u', field%u, indent + 2, first)
      call append_field_real_array(json, 'v', field%v, indent + 2, first)
      call append_field_real_array(json, 'levels', field%levels, indent + 2, first)
      call append_field_int(json, 'nrows', field%nrows, indent + 2, first)
      call append_field_int(json, 'ncols', field%ncols, indent + 2, first)
      call append_field_string(json, 'colormap', field%colormap, indent + 2, first)
      if (field%show_colorbar_set) then
         call append_field_bool(json, 'showColorbar', field%show_colorbar, &
                                indent + 2, first)
      end if
      if (field%density >= 0.0_wp) then
         call append_field_real(json, 'density', field%density, indent + 2, first)
      end if
      if (field%vmin_set) then
         call append_field_real(json, 'vmin', field%vmin, indent + 2, first)
      end if
      if (field%vmax_set) then
         call append_field_real(json, 'vmax', field%vmax, indent + 2, first)
      end if
      if (field%linewidths >= 0.0_wp) then
         call append_field_real(json, 'linewidths', field%linewidths, indent + 2, first)
      end if

      json = json//NL//pad//'}'
   end function serialize_field_plot

   subroutine append_field_real_array(json, name, values, indent, first)
      character(len=:), allocatable, intent(inout) :: json
      character(len=*), intent(in) :: name
      real(wp), allocatable, intent(in) :: values(:)
      integer, intent(in) :: indent
      logical, intent(inout) :: first
      integer :: i
      character(len=:), allocatable :: pad

      if (.not. allocated(values)) return
      pad = repeat(' ', indent)
      if (.not. first) json = json//','//NL
      first = .false.
      json = json//pad//Q//name//Q//': ['
      do i = 1, size(values)
         if (i > 1) json = json//', '
         json = json//real_to_str(values(i))
      end do
      json = json//']'
   end subroutine append_field_real_array

   subroutine append_field_string(json, name, value, indent, first)
      character(len=:), allocatable, intent(inout) :: json
      character(len=*), intent(in) :: name
      character(len=:), allocatable, intent(in) :: value
      integer, intent(in) :: indent
      logical, intent(inout) :: first
      character(len=:), allocatable :: pad

      if (.not. allocated(value)) return
      pad = repeat(' ', indent)
      if (.not. first) json = json//','//NL
      first = .false.
      json = json//pad//Q//name//Q//': '//Q//escape_json_string(value)//Q
   end subroutine append_field_string

   subroutine append_field_bool(json, name, value, indent, first)
      character(len=:), allocatable, intent(inout) :: json
      character(len=*), intent(in) :: name
      logical, intent(in) :: value
      integer, intent(in) :: indent
      logical, intent(inout) :: first
      character(len=:), allocatable :: pad

      pad = repeat(' ', indent)
      if (.not. first) json = json//','//NL
      first = .false.
      json = json//pad//Q//name//Q//': '
      if (value) then
         json = json//'true'
      else
         json = json//'false'
      end if
   end subroutine append_field_bool

   subroutine append_field_int(json, name, value, indent, first)
      character(len=:), allocatable, intent(inout) :: json
      character(len=*), intent(in) :: name
      integer, intent(in) :: value
      integer, intent(in) :: indent
      logical, intent(inout) :: first
      character(len=:), allocatable :: pad

      if (value <= 0) return
      pad = repeat(' ', indent)
      if (.not. first) json = json//','//NL
      first = .false.
      json = json//pad//Q//name//Q//': '//int_to_str(value)
   end subroutine append_field_int

   subroutine append_field_real(json, name, value, indent, first)
      character(len=:), allocatable, intent(inout) :: json
      character(len=*), intent(in) :: name
      real(wp), intent(in) :: value
      integer, intent(in) :: indent
      logical, intent(inout) :: first
      character(len=:), allocatable :: pad

      pad = repeat(' ', indent)
      if (.not. first) json = json//','//NL
      first = .false.
      json = json//pad//Q//name//Q//': '//real_to_str(value)
   end subroutine append_field_real

   pure function escape_json_string(s) result(escaped)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: escaped
      integer :: i
      character(len=1) :: ch

      escaped = ''
      do i = 1, len(s)
         ch = s(i:i)
         select case (ch)
         case ('"')
            escaped = escaped//'\"'
         case ('\')
            escaped = escaped//'\\'
         case (char(8))
            escaped = escaped//'\b'
         case (char(9))
            escaped = escaped//'\t'
         case (char(10))
            escaped = escaped//'\n'
         case (char(12))
            escaped = escaped//'\f'
         case (char(13))
            escaped = escaped//'\r'
         case default
            escaped = escaped//ch
         end select
      end do
   end function escape_json_string

   pure function int_to_str(n) result(s)
      integer, intent(in) :: n
      character(len=:), allocatable :: s
      character(len=20) :: buf

      write (buf, '(i0)') n
      s = trim(buf)
   end function int_to_str

   pure function real_to_str(x) result(s)
      real(wp), intent(in) :: x
      character(len=:), allocatable :: s
      character(len=30) :: buf
      integer :: i
      logical :: is_integer

      if (x /= x) then
         s = 'null'
         return
      end if
      if (abs(x) > huge(x)) then
         s = 'null'
         return
      end if

      is_integer = .false.
      if (abs(x) <= real(huge(1), wp)) then
         is_integer = abs(x - nint(x)) < 1.0d-10
      end if

      if (is_integer) then
         write (buf, '(i0)') nint(x)
      else
         write (buf, '(es17.10)') x
         buf = adjustl(buf)
         i = len_trim(buf)
         do
            if (i <= 1) exit
            if (buf(i:i) /= '0') exit
            if (buf(i - 1:i - 1) == '.') exit
            i = i - 1
         end do
         buf(i + 1:) = ' '
      end if
      s = trim(adjustl(buf))
   end function real_to_str

    subroutine parse_field_plot(json, pos, field, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(field_plot_t), intent(out) :: field
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        field%defined = .true.
        if (.not. expect_char(json, pos, '{')) then
            status = 95
            return
        end if

        do
            call skip_ws(json, pos)
            if (pos > len(json)) then
                status = 77
                return
            end if
            if (json(pos:pos) == ',') pos = pos + 1
            call skip_ws(json, pos)
            if (pos > len(json)) then
                status = 77
                return
            end if
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if

            call read_string(json, pos, key, status)
            if (status /= 0) return
            call skip_ws(json, pos)
            if (.not. expect_char(json, pos, ':')) then
                status = 96
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('x')
                call parse_real_array(json, pos, field%x, status)
            case ('y')
                call parse_real_array(json, pos, field%y, status)
            case ('z')
                call parse_real_array(json, pos, field%z, status)
            case ('u')
                call parse_real_array(json, pos, field%u, status)
            case ('v')
                call parse_real_array(json, pos, field%v, status)
            case ('levels')
                call parse_real_array(json, pos, field%levels, status)
            case ('nrows')
                call read_int(json, pos, field%nrows, status)
            case ('ncols')
                call read_int(json, pos, field%ncols, status)
            case ('colormap')
                call read_string(json, pos, field%colormap, status)
            case ('showColorbar')
                call read_bool(json, pos, field%show_colorbar, status)
                field%show_colorbar_set = .true.
            case ('density')
                call read_real(json, pos, field%density, status)
            case ('vmin')
                call read_real(json, pos, field%vmin, status)
                field%vmin_set = .true.
            case ('vmax')
                call read_real(json, pos, field%vmax, status)
                field%vmax_set = .true.
            case ('linewidths')
                call read_real(json, pos, field%linewidths, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_field_plot

end module fortplot_spec_json_field_plot
