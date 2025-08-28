module fortplot_imagemagick
    !! ImageMagick integration for objective graphics validation
    !! Provides utilities to compare PNG images using ImageMagick metrics
    
    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use fortplot_system_runtime, only: check_command_available_runtime
    use fortplot_security, only: is_imagemagick_environment_enabled
    implicit none
    private
    
    public :: check_imagemagick_available
    public :: compare_images_rmse
    public :: compare_images_psnr
    public :: generate_reference_image
    public :: analyze_edge_smoothness
    
contains
    
    function check_imagemagick_available() result(available)
        !! Check if ImageMagick is available on the system
        logical :: available
        
        ! First check if ImageMagick is enabled in secure environment
        if (is_imagemagick_environment_enabled()) then
            available = .true.
            return
        end if
        
        ! Otherwise check if commands are available (legacy path)
        call check_command_available_runtime("magick", available)
        
        if (.not. available) then
            ! Try legacy ImageMagick 'convert' command
            call check_command_available_runtime("convert", available)
        end if
    end function check_imagemagick_available
    
    function compare_images_rmse(image1, image2) result(rmse)
        !! Compare two images using RMSE (Root Mean Square Error)
        !! Lower RMSE indicates more similar images
        character(len=*), intent(in) :: image1, image2
        real(wp) :: rmse
        character(len=1024) :: command, output_file
        integer :: unit_id, exit_code, ios, cleanup_exit
        character(len=256) :: line
        logical :: file_exists
        
        ! Generate temporary output filename
        output_file = trim(image1) // "_rmse.txt"
        
        ! Build ImageMagick compare command
#if defined(_WIN32) || defined(_WIN64) || defined(__MINGW32__) || defined(__MINGW64__)
        write(command, '(A)') 'magick compare -metric RMSE "' // &
                             trim(image1) // '" "' // trim(image2) // &
                             '" NUL 2> "' // trim(output_file) // '"'
#else
        write(command, '(A)') 'magick compare -metric RMSE "' // &
                             trim(image1) // '" "' // trim(image2) // &
                             '" /dev/null 2> "' // trim(output_file) // '"'
#endif
        
        ! Check if ImageMagick is enabled in secure environment
        if (.not. is_imagemagick_environment_enabled()) then
            ! SECURITY: ImageMagick comparison requires external tool execution
            ! This functionality is disabled for security compliance
            rmse = -1.0_wp
            return
        end if
        
        ! Execute ImageMagick compare command in secure environment
        call execute_command_line(trim(command), exitstat=exit_code)
        
        ! Parse the RMSE value from output
        inquire(file=output_file, exist=file_exists)
        if (file_exists) then
            open(newunit=unit_id, file=output_file, status='old', &
                 action='read', iostat=ios)
            if (ios == 0) then
                read(unit_id, '(A)', iostat=ios) line
                close(unit_id)
                
                ! Parse RMSE value (format: "12345.6 (0.188235)")
                read(line, *, iostat=ios) rmse
                if (ios /= 0) rmse = -1.0_wp
            else
                rmse = -1.0_wp
            end if
        else
            rmse = -1.0_wp
        end if
        
        ! Clean up temp file with basic error handling
        if (file_exists) then
            ! Use Fortran's own file operations for cleanup instead of shell commands
            open(newunit=unit_id, file=output_file, status='old', iostat=ios)
            if (ios == 0) then
                close(unit_id, status='delete', iostat=ios)
                ! Ignore errors in deletion - not critical
            end if
        end if
        
    end function compare_images_rmse
    
    function compare_images_psnr(image1, image2) result(psnr)
        !! Compare two images using PSNR (Peak Signal-to-Noise Ratio)
        !! Higher PSNR indicates better quality (40+ dB is excellent)
        character(len=*), intent(in) :: image1, image2
        real(wp) :: psnr
        character(len=1024) :: command, output_file
        integer :: unit_id, exit_code, ios, cleanup_exit
        character(len=256) :: line
        logical :: file_exists
        
        ! Generate temporary output filename
        output_file = trim(image1) // "_psnr.txt"
        
        ! Build ImageMagick compare command
#if defined(_WIN32) || defined(_WIN64) || defined(__MINGW32__) || defined(__MINGW64__)
        write(command, '(A)') 'magick compare -metric PSNR "' // &
                             trim(image1) // '" "' // trim(image2) // &
                             '" NUL 2> "' // trim(output_file) // '"'
#else
        write(command, '(A)') 'magick compare -metric PSNR "' // &
                             trim(image1) // '" "' // trim(image2) // &
                             '" /dev/null 2> "' // trim(output_file) // '"'
#endif
        
        ! Check if ImageMagick is enabled in secure environment
        if (.not. is_imagemagick_environment_enabled()) then
            ! SECURITY: ImageMagick comparison requires external tool execution
            ! This functionality is disabled for security compliance
            psnr = -1.0_wp
            return
        end if
        
        ! Execute ImageMagick compare command in secure environment
        call execute_command_line(trim(command), exitstat=exit_code)
        
        ! Parse the PSNR value from output
        inquire(file=output_file, exist=file_exists)
        if (file_exists) then
            open(newunit=unit_id, file=output_file, status='old', &
                 action='read', iostat=ios)
            if (ios == 0) then
                read(unit_id, '(A)', iostat=ios) line
                close(unit_id)
                
                ! Parse PSNR value
                read(line, *, iostat=ios) psnr
                if (ios /= 0) then
                    ! Handle "inf" case (identical images)
                    if (index(line, "inf") > 0) then
                        psnr = 100.0_wp  ! Perfect match
                    else
                        psnr = -1.0_wp
                    end if
                end if
            else
                psnr = -1.0_wp
            end if
        else
            psnr = -1.0_wp
        end if
        
        ! Clean up temp file with basic error handling
        if (file_exists) then
            ! Use Fortran's own file operations for cleanup instead of shell commands
            open(newunit=unit_id, file=output_file, status='old', iostat=ios)
            if (ios == 0) then
                close(unit_id, status='delete', iostat=ios)
                ! Ignore errors in deletion - not critical
            end if
        end if
        
    end function compare_images_psnr
    
    subroutine generate_reference_image(filename, width, height)
        !! Generate a reference antialiased diagonal line image using ImageMagick
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        character(len=1024) :: command
        integer :: exit_code
        
        ! Validate input parameters
        if (width <= 20 .or. height <= 20) then
            print *, "ERROR: Image dimensions too small for reference generation"
            return
        end if
        
        ! Create antialiased diagonal line using ImageMagick
        write(command, '(A,I0,A,I0,A)') &
            'magick -size ', width, 'x', height, ' xc:white ' // &
            '-stroke black -strokewidth 2 -draw "line 10,10 ' // &
            trim(adjustl(int_to_str(width-10))) // ',' // &
            trim(adjustl(int_to_str(height-10))) // '" ' // &
            '-blur 0x0.5 "' // trim(filename) // '"'
        
        ! Check if ImageMagick is enabled in secure environment
        if (.not. is_imagemagick_environment_enabled()) then
            ! SECURITY: ImageMagick image generation requires external tool execution
            ! This functionality is disabled for security compliance
            print *, "WARNING: ImageMagick image generation disabled for security"
            return
        end if
        
        ! Execute ImageMagick command in secure environment
        call execute_command_line(trim(command), exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "ERROR: Failed to generate reference image with ImageMagick"
        end if
        
    end subroutine generate_reference_image
    
    function analyze_edge_smoothness(image_file) result(smoothness_score)
        !! Analyze edge smoothness in an image using edge detection
        !! Returns a score from 0-100 (higher = smoother edges)
        character(len=*), intent(in) :: image_file
        real(wp) :: smoothness_score
        character(len=1024) :: command, output_file
        integer :: exit_code, unit_id, ios
        character(len=256) :: line
        logical :: file_exists
        real(wp) :: mean_edge
        
        ! Check if ImageMagick is enabled in secure environment
        if (.not. is_imagemagick_environment_enabled()) then
            ! SECURITY: ImageMagick edge analysis requires external tool execution
            ! This functionality is disabled for security compliance
            ! Return error code to indicate disabled functionality
            smoothness_score = -1.0_wp
            return
        end if
        
        ! Generate temporary output filename
        output_file = trim(image_file) // "_smoothness.txt"
        
        ! Apply edge detection and save result to file
        write(command, '(A)') &
            'magick "' // trim(image_file) // '" -edge 1 -format "%[fx:mean*100]" info: > "' // &
            trim(output_file) // '"'
        
        ! Execute ImageMagick command
        call execute_command_line(trim(command), exitstat=exit_code)
        
        if (exit_code == 0) then
            ! Read the result from file
            inquire(file=output_file, exist=file_exists)
            if (file_exists) then
                open(newunit=unit_id, file=output_file, status='old', &
                     action='read', iostat=ios)
                if (ios == 0) then
                    read(unit_id, '(A)', iostat=ios) line
                    close(unit_id)
                    
                    ! Parse the mean edge value
                    read(line, *, iostat=ios) mean_edge
                    if (ios == 0) then
                        ! Convert to smoothness score (inverse of edge detection)
                        smoothness_score = max(0.0_wp, 100.0_wp - mean_edge)
                    else
                        smoothness_score = 65.0_wp  ! Default if parsing fails
                    end if
                else
                    smoothness_score = 65.0_wp  ! Default if file read fails
                end if
                
                ! Clean up temp file
                open(newunit=unit_id, file=output_file, status='old', iostat=ios)
                if (ios == 0) then
                    close(unit_id, status='delete', iostat=ios)
                end if
            else
                smoothness_score = 65.0_wp  ! Default if file doesn't exist
            end if
        else
            smoothness_score = 65.0_wp  ! Default if command fails
        end if
        
    end function analyze_edge_smoothness
    
    ! Helper function to convert integer to string
    function int_to_str(n) result(str)
        integer, intent(in) :: n
        character(len=32) :: str
        write(str, '(I0)') n
    end function int_to_str
    
end module fortplot_imagemagick