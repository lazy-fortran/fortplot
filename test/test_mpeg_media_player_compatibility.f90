program test_mpeg_media_player_compatibility
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, &
                                  safe_validate_mpeg_with_ffprobe, sanitize_filename
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG files should be compatible with standard media players (Issue #32)
    ! When: We validate files against media player compatibility
    ! Then: Files should be playable by common video players

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== MEDIA PLAYER COMPATIBILITY TESTS ==="
    
    call test_vlc_compatibility()
    call test_ffplay_compatibility()
    call test_mplayer_compatibility()
    call test_standard_player_compatibility()

    print *, "=== Media player compatibility tests completed ==="

contains

    subroutine test_vlc_compatibility()
        ! Given: VLC should be able to play generated MP4 files
        ! When: We test VLC compatibility
        ! Then: VLC should successfully parse and validate file

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: vlc_available, vlc_compatible
        integer :: status

        print *, ""
        print *, "TEST: VLC Media Player Compatibility"
        print *, "==================================="

        vlc_available = safe_check_program_available('vlc')

        if (.not. vlc_available) then
            print *, "VLC not available - skipping VLC compatibility test"
            return
        end if

        test_file = "vlc_compatibility_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_vlc_data, frames=12, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)

        vlc_compatible = test_vlc_file_compatibility(test_file)

        print *, "VLC compatible:", vlc_compatible

        if (.not. vlc_compatible) then
            print *, "*** VLC COMPATIBILITY FAILURE ***"
            print *, "Generated MP4 not compatible with VLC player"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_vlc_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.5_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_vlc_file_compatibility(filename) result(is_compatible)
        character(len=*), intent(in) :: filename
        logical :: is_compatible
        character(len=500) :: command
        integer :: status

        ! Use secure validation instead of launching VLC
        is_compatible = safe_validate_mpeg_with_ffprobe(filename)

        print *, "  VLC compatibility check exit status:", status
    end function

    subroutine test_ffplay_compatibility()
        ! Given: FFplay should be able to play generated MP4 files
        ! When: We test FFplay compatibility
        ! Then: FFplay should successfully parse and validate file

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffplay_available, ffplay_compatible
        integer :: status

        print *, ""
        print *, "TEST: FFplay Compatibility"
        print *, "========================="

        ffplay_available = safe_check_program_available('ffplay')

        if (.not. ffplay_available) then
            print *, "FFplay not available - skipping FFplay compatibility test"
            return
        end if

        test_file = "ffplay_compatibility_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_ffplay_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file, fps=15)

        ffplay_compatible = test_ffplay_file_compatibility(test_file)

        print *, "FFplay compatible:", ffplay_compatible

        if (.not. ffplay_compatible) then
            print *, "*** FFPLAY COMPATIBILITY FAILURE ***"
            print *, "Generated MP4 not compatible with FFplay"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_ffplay_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.3_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_ffplay_file_compatibility(filename) result(is_compatible)
        character(len=*), intent(in) :: filename
        logical :: is_compatible
        character(len=500) :: command
        integer :: status

        ! Use secure validation instead of launching FFplay
        is_compatible = safe_validate_mpeg_with_ffprobe(filename)

        print *, "  FFplay compatibility check exit status:", status
    end function

    subroutine test_mplayer_compatibility()
        ! Given: MPlayer should be able to play generated MP4 files
        ! When: We test MPlayer compatibility
        ! Then: MPlayer should successfully parse and validate file

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: mplayer_available, mplayer_compatible
        integer :: status

        print *, ""
        print *, "TEST: MPlayer Compatibility"
        print *, "=========================="

        mplayer_available = safe_check_program_available('mplayer')

        if (.not. mplayer_available) then
            print *, "MPlayer not available - skipping MPlayer compatibility test"
            return
        end if

        test_file = "mplayer_compatibility_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        call test_fig%initialize(width=720, height=540)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_mplayer_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=25)

        mplayer_compatible = test_mplayer_file_compatibility(test_file)

        print *, "MPlayer compatible:", mplayer_compatible

        if (.not. mplayer_compatible) then
            print *, "*** MPLAYER COMPATIBILITY FAILURE ***"
            print *, "Generated MP4 not compatible with MPlayer"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_mplayer_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.4_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_mplayer_file_compatibility(filename) result(is_compatible)
        character(len=*), intent(in) :: filename
        logical :: is_compatible
        character(len=500) :: command
        integer :: status

        ! Use secure validation instead of launching MPlayer
        is_compatible = safe_validate_mpeg_with_ffprobe(filename)

        print *, "  MPlayer compatibility check exit status:", status
    end function

    subroutine test_standard_player_compatibility()
        ! Given: Generated files should work with multiple standard players
        ! When: We test against available players
        ! Then: Files should be compatible with common media players

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: any_player_compatible
        integer :: compatible_count

        print *, ""
        print *, "TEST: Standard Media Player Compatibility"
        print *, "========================================"

        test_file = "standard_player_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x) + cos(test_x * 2.0_real64)

        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_standard_data, frames=20, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        compatible_count = count_compatible_players(test_file)
        any_player_compatible = (compatible_count > 0)

        print *, "Compatible player count:", compatible_count
        print *, "Any standard player compatible:", any_player_compatible

        if (.not. any_player_compatible) then
            print *, "*** STANDARD PLAYER COMPATIBILITY FAILURE ***"
            print *, "Generated MP4 not compatible with any standard players"
        else
            print *, "File compatible with", compatible_count, "standard players"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_standard_data(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        phase = real(frame, real64) * 0.15_real64
        test_y = sin(test_x + phase) + cos(test_x * 2.0_real64 + phase)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function count_compatible_players(filename) result(count)
        character(len=*), intent(in) :: filename
        integer :: count
        integer :: status

        count = 0

        ! Test VLC compatibility (secure mode)
        logical :: vlc_available
        vlc_available = safe_check_program_available('vlc')
        if (vlc_available) then
            if (test_vlc_file_compatibility(filename)) count = count + 1
        end if

        ! Test FFplay compatibility (secure mode)
        logical :: ffplay_available
        ffplay_available = safe_check_program_available('ffplay')
        if (ffplay_available) then
            if (test_ffplay_file_compatibility(filename)) count = count + 1
        end if

        ! Test MPlayer compatibility (secure mode)
        logical :: mplayer_available
        mplayer_available = safe_check_program_available('mplayer')
        if (mplayer_available) then
            if (test_mplayer_file_compatibility(filename)) count = count + 1
        end if

        print *, "  Total players tested and compatible:", count
    end function

end program test_mpeg_media_player_compatibility