module fortplot_font_profiles
    implicit none
    private
    public :: find_profile_font, find_nixos_config_font
contains
    logical function find_profile_font(font_name, font_path, profile_list) result(found)
        character(len=*), intent(in) :: font_name
        character(len=*), optional, intent(in) :: profile_list
        character(len=*), intent(out) :: font_path
        character(len=:), allocatable :: profiles, home, data, user
        integer :: start, finish

        found = .false.
        font_path = ''
        profiles = environment('NIX_PROFILES')
        if (present(profile_list)) profiles = profile_list
        start = 1
        do while (start <= len(profiles))
            do while (start <= len(profiles))
                if (profiles(start:start) /= ' ') exit
                start = start + 1
            end do
            if (start > len(profiles)) exit
            finish = index(profiles(start:), ' ')
            if (finish == 0) finish = len(profiles) - start + 2
            found = search_profile(profiles(start:start + finish - 2), &
                font_name, font_path)
            if (found) return
            start = start + finish
        end do
        home = environment('HOME')
        if (len(home) > 0) then
            found = search_profile(home//'/.nix-profile', font_name, font_path)
            if (found) return
            data = environment('XDG_STATE_HOME')
            if (len(data) == 0) data = home//'/.local/state'
            found = search_profile(data//'/nix/profile', font_name, font_path)
            if (found) return
            data = environment('XDG_DATA_HOME')
            if (len(data) == 0) data = home//'/.local/share'
            found = search_font_root(data//'/fonts', font_name, font_path)
            if (found) return
            found = search_font_root(home//'/.fonts', font_name, font_path)
            if (found) return
        end if
        user = environment('USER')
        if (len(user) > 0) then
            found = search_profile('/etc/profiles/per-user/'//user, font_name, font_path)
            if (found) return
        end if
        found = search_profile('/run/current-system/sw', font_name, font_path)
        if (found) return
        found = search_profile('/nix/var/nix/profiles/default', font_name, font_path)
        if (found) return
        found = find_nixos_config_font(font_name, font_path, &
            '/etc/fonts/conf.d/00-nixos-cache.conf')
    end function find_profile_font

    logical function find_nixos_config_font(name, path, config) result(found)
        character(len=*), intent(in) :: name, config
        character(len=*), intent(out) :: path
        character(len=4096) :: line
        character(len=:), allocatable :: root
        integer :: unit, status, first, last, cursor

        ! NixOS generates concrete package directories here, even when its
        ! optional /run/current-system/sw/share/X11/fonts link is disabled.
        found = .false.
        path = ''
        open (newunit=unit, file=config, status='old', action='read', iostat=status)
        if (status /= 0) return
        do
            read (unit, '(a)', iostat=status) line
            if (status /= 0) exit
            cursor = 1
            do
                first = index(line(cursor:), '<dir>')
                if (first == 0) exit
                first = cursor + first + 4
                last = index(line(first:), '</dir>')
                if (last == 0) exit
                last = first + last - 2
                root = line(first:last)
                found = search_profile(root, name, path)
                if (.not. found) found = search_font_root(root, name, path)
                if (found) exit
                cursor = last + 7
                if (cursor > len_trim(line)) exit
            end do
            if (found) exit
        end do
        close (unit)
    end function find_nixos_config_font

    function environment(name) result(value)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: value
        integer :: length, status
        call get_environment_variable(name, length=length, status=status)
        value = ''
        if (status /= 0 .or. length == 0) return
        deallocate (value)
        allocate (character(len=length) :: value)
        call get_environment_variable(name, value, status=status)
        if (status /= 0) value = ''
    end function environment

    logical function search_profile(root, name, path) result(found)
        character(len=*), intent(in) :: root, name
        character(len=*), intent(out) :: path
        found = search_font_root(root//'/share/fonts', name, path)
        if (found) return
        found = search_font_root(root//'/share/X11/fonts', name, path)
    end function search_profile

    logical function search_font_root(root, name, path) result(found)
        character(len=*), intent(in) :: root, name
        character(len=*), intent(out) :: path
        character(len=64) :: filename
        character(len=32), parameter :: dirs(*) = [character(len=32) :: &
            '', '/truetype', '/TTF', '/truetype/dejavu', '/dejavu', &
            '/truetype/liberation', '/liberation', '/liberation-fonts', &
            '/truetype/msttcorefonts', '/truetype/msttcore', '/arial']
        character(len=:), allocatable :: candidate
        integer :: i

        found = .false.
        path = ''
        select case (name)
        case ('Helvetica')
            filename = 'Helvetica.ttf'
        case ('Liberation Sans')
            filename = 'LiberationSans-Regular.ttf'
        case ('Arial')
            filename = 'Arial.ttf'
        case ('DejaVu Sans')
            filename = 'DejaVuSans.ttf'
        case default
            return
        end select
        do i = 1, size(dirs)
            candidate = root//trim(dirs(i))//'/'//trim(filename)
            if (len(candidate) > len(path)) cycle
            inquire (file=candidate, exist=found)
            if (.not. found .and. name == 'Arial') then
                candidate = root//trim(dirs(i))//'/arial.ttf'
                inquire (file=candidate, exist=found)
            end if
            if (.not. found) cycle
            path = candidate
            return
        end do
    end function search_font_root
end module fortplot_font_profiles
