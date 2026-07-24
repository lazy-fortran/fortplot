program test_example_manifest_matches_tree
    !! The example manifest must come from example/fortran/, not from a file the
    !! documentation generator also writes.
    !!
    !! It used to scrape the generated gallery page for '- [Name](...)' bullets.
    !! Rendering that gallery as cards instead silently matched nothing, and the
    !! empty result fell through to a hardcoded fallback list naming examples
    !! deleted long ago -- which the site then published. This checks the
    !! manifest against the directory listing, so a change to how the gallery is
    !! presented can never again decide which examples exist.

    use fortplot_documentation, only: get_example_count, get_example_name, &
                                      PATH_MAX_LEN
    use fortplot_directory_listing, only: list_directory_entries
    implicit none

    integer, parameter :: MAX_ENTRIES = 256
    character(len=256), allocatable :: entries(:)
    character(len=PATH_MAX_LEN) :: name
    integer :: entry_count, status, i, j, n_dirs, n_manifest
    logical :: found, exists
    character(len=256) :: dirs(MAX_ENTRIES)

    allocate (entries(MAX_ENTRIES))
    entries = ''
    call list_directory_entries('example/fortran', entries, entry_count, status)
    if (status /= 0) then
        print *, 'FAIL: cannot list example/fortran'
        stop 1
    end if

    ! Every directory holding Fortran source is an example.
    n_dirs = 0
    do i = 1, min(entry_count, MAX_ENTRIES)
        if (len_trim(entries(i)) == 0) cycle
        inquire (file='example/fortran/'//trim(entries(i))//'/.', exist=exists)
        if (.not. exists) cycle
        if (.not. has_source(trim(entries(i)))) cycle
        n_dirs = n_dirs + 1
        dirs(n_dirs) = entries(i)
    end do

    if (n_dirs == 0) then
        print *, 'FAIL: no example directories discovered'
        stop 1
    end if

    n_manifest = get_example_count()
    if (n_manifest /= n_dirs) then
        print *, 'FAIL: manifest has', n_manifest, 'examples, tree has', n_dirs
        stop 1
    end if

    ! Every manifest entry must be a real directory, so a stale name can never
    ! reach the generated pages.
    do i = 1, n_manifest
        call get_example_name(i, name)
        found = .false.
        do j = 1, n_dirs
            if (trim(name) == trim(dirs(j))) then
                found = .true.
                exit
            end if
        end do
        if (.not. found) then
            print *, 'FAIL: manifest names "', trim(name), &
                '" which is not a directory under example/fortran'
            stop 1
        end if
    end do

    print *, 'PASS: example manifest matches the source tree (', n_dirs, &
        'examples)'

contains

    logical function has_source(name) result(ok)
        character(len=*), intent(in) :: name
        character(len=256), allocatable :: files(:)
        character(len=:), allocatable :: candidate
        integer :: count, st, k

        ok = .false.
        allocate (files(MAX_ENTRIES))
        files = ''
        call list_directory_entries('example/fortran/'//name, files, count, st)
        if (st /= 0) return
        do k = 1, min(count, MAX_ENTRIES)
            candidate = trim(files(k))
            if (len(candidate) < 5) cycle
            if (candidate(len(candidate) - 3:) == '.f90') then
                ok = .true.
                return
            end if
        end do
    end function has_source

end program test_example_manifest_matches_tree
