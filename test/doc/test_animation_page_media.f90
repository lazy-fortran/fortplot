program test_animation_page_media
    !! The animation example writes an MP4 into its page output directory, so the
    !! generated Animation page must embed a download link for that video. A page
    !! whose media lands in a mismatched directory must emit no video link.
    use fortplot_documentation, only: write_generated_outputs, PATH_MAX_LEN
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    character(len=*), parameter :: ANIM_DIR = 'output/example/fortran/animation'
    character(len=*), parameter :: DEMO_DIR = &
        'output/example/fortran/save_animation_demo'
    character(len=*), parameter :: VIDEO_LINK = &
        '[Download Video](../../media/examples/animation/animation.mp4)'

    call ensure_dir('build/test/output')

    call assert_video_link_emitted_when_media_present()
    call assert_no_video_link_when_media_mismatched()

    print *, 'Animation page media tests passed'

contains

    subroutine assert_video_link_emitted_when_media_present()
        character(len=PATH_MAX_LEN) :: out_file
        integer :: unit_out, ios

        call ensure_dir(ANIM_DIR)
        call touch_file(ANIM_DIR // '/animation.mp4')

        out_file = 'build/test/output/test_animation_page_present.md'
        open(newunit=unit_out, file=trim(out_file), status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot create ', trim(out_file)
            stop 1
        end if
        call write_generated_outputs(unit_out, 'example/fortran/animation', &
            'animation')
        close(unit_out)

        call assert_file_contains(out_file, VIDEO_LINK)
    end subroutine assert_video_link_emitted_when_media_present

    subroutine assert_no_video_link_when_media_mismatched()
        character(len=PATH_MAX_LEN) :: out_file
        integer :: unit_out, ios

        call remove_file(ANIM_DIR // '/animation.mp4')
        call ensure_dir(DEMO_DIR)
        call touch_file(DEMO_DIR // '/animation.mp4')

        out_file = 'build/test/output/test_animation_page_mismatch.md'
        open(newunit=unit_out, file=trim(out_file), status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot create ', trim(out_file)
            stop 1
        end if
        call write_generated_outputs(unit_out, 'example/fortran/animation', &
            'animation')
        close(unit_out)

        call assert_file_lacks(out_file, 'Download Video')
    end subroutine assert_no_video_link_when_media_mismatched

    subroutine ensure_dir(path)
        character(len=*), intent(in) :: path
        logical :: ok
        call create_directory_runtime(path, ok)
        if (.not. ok) then
            print *, 'FAIL: cannot create directory: ', trim(path)
            stop 1
        end if
    end subroutine ensure_dir

    subroutine touch_file(path)
        character(len=*), intent(in) :: path
        integer :: u, ios
        open(newunit=u, file=trim(path), status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot create file: ', trim(path)
            stop 1
        end if
        close(u)
    end subroutine touch_file

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: u, ios
        logical :: exists
        inquire(file=trim(path), exist=exists)
        if (.not. exists) return
        open(newunit=u, file=trim(path), status='old', action='readwrite', &
            iostat=ios)
        if (ios /= 0) return
        close(u, status='delete')
    end subroutine remove_file

    subroutine assert_file_contains(path, needle)
        character(len=*), intent(in) :: path, needle
        if (.not. file_has(path, needle)) then
            print *, 'FAIL: file missing expected content:'
            print *, '  file  :', trim(path)
            print *, '  needle:', trim(needle)
            stop 1
        end if
    end subroutine assert_file_contains

    subroutine assert_file_lacks(path, needle)
        character(len=*), intent(in) :: path, needle
        if (file_has(path, needle)) then
            print *, 'FAIL: file contains forbidden content:'
            print *, '  file  :', trim(path)
            print *, '  needle:', trim(needle)
            stop 1
        end if
    end subroutine assert_file_lacks

    logical function file_has(path, needle) result(found)
        character(len=*), intent(in) :: path, needle
        character(len=1024) :: line
        integer :: u, ios
        found = .false.
        open(newunit=u, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open file: ', trim(path)
            stop 1
        end if
        do
            read(u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, trim(needle)) > 0) then
                found = .true.
                exit
            end if
        end do
        close(u)
    end function file_has

end program test_animation_page_media
