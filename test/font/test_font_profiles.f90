program test_font_profiles
    use, intrinsic :: iso_fortran_env, only: int8
    use fortplot_font_profiles, only: find_profile_font, find_nixos_config_font
    use fortplot_text_fonts, only: find_any_available_font
    use fortplot_truetype, only: truetype_font_t
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none
    type(truetype_font_t) :: font
    character(len=:), allocatable :: output, profile, target
    character(len=256) :: source, actual
    integer :: unit, input, length
    integer(int8), allocatable :: bytes(:)
    character(len=:), allocatable :: font_dir

    if (.not. find_any_available_font(source)) error stop 'No public test font available'
    call ensure_test_output_dir('font_profiles', output)
    profile = output//'nix-profile'
    target = profile//'/share/fonts/truetype/DejaVuSans.ttf'
    call ensure_test_output_dir('font_profiles/nix-profile/share/fonts/truetype', font_dir)
    inquire (file=trim(source), size=length)
    allocate (bytes(length))
    open (newunit=input, file=trim(source), status='old', access='stream', &
        form='unformatted', action='read')
    read (input) bytes
    close (input)
    open (newunit=unit, file=target, status='replace', access='stream', &
        form='unformatted', action='write')
    write (unit) bytes
    close (unit)
    if (.not. find_profile_font('DejaVu Sans', actual, ' /nonexistent '//profile//' ')) &
        error stop 'Nix profile font was not found'
    if (trim(actual) /= target) error stop 'Wrong Nix profile font selected'
    if (.not. font%init(trim(actual))) error stop 'Discovered font cannot initialize'
    call font%cleanup()
    open (newunit=unit, file=output//'00-nixos-cache.conf', status='replace')
    write (unit, '(a)') '<fontconfig><dir>/nonexistent</dir><dir>'//profile// &
        '</dir></fontconfig>'
    close (unit)
    if (.not. find_nixos_config_font('DejaVu Sans', actual, &
        output//'00-nixos-cache.conf')) &
        error stop 'NixOS configured package font was not found'
    if (trim(actual) /= target) error stop 'Wrong configured font selected'
    if (find_profile_font('Unknown font', actual)) error stop 'Unknown family accepted'
    print *, 'PASS: synthetic Nix profile discovery initializes a real TrueType font'
end program test_font_profiles
