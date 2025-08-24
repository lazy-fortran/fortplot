module fortplot_imagemagick
    !! ImageMagick integration for objective graphics validation
    !! Provides utilities to compare PNG images using ImageMagick metrics
    
    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
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
        integer :: exit_code
        
        call execute_command_line("magick -version > /dev/null 2>&1", &
                                 exitstat=exit_code)
        available = (exit_code == 0)
        
        if (.not. available) then
            ! Try legacy ImageMagick command
            call execute_command_line("convert -version > /dev/null 2>&1", &
                                     exitstat=exit_code)
            available = (exit_code == 0)
        end if
    end function check_imagemagick_available
    
    function compare_images_rmse(image1, image2) result(rmse)
        !! Compare two images using RMSE (Root Mean Square Error)
        !! Lower RMSE indicates more similar images
        character(len=*), intent(in) :: image1, image2
        real(wp) :: rmse
        character(len=1024) :: command, output_file
        integer :: unit_id, exit_code, ios
        character(len=256) :: line
        
        ! Generate temporary output filename
        output_file = trim(image1) // "_rmse.txt"
        
        ! Build ImageMagick compare command
        write(command, '(A)') 'magick compare -metric RMSE "' // &
                             trim(image1) // '" "' // trim(image2) // &
                             '" /dev/null 2> "' // trim(output_file) // '"'
        
        ! Execute comparison
        call execute_command_line(trim(command), exitstat=exit_code)
        
        ! Parse the RMSE value from output
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
        
        ! Clean up temp file
        call execute_command_line('rm -f "' // trim(output_file) // '"')
        
    end function compare_images_rmse
    
    function compare_images_psnr(image1, image2) result(psnr)
        !! Compare two images using PSNR (Peak Signal-to-Noise Ratio)
        !! Higher PSNR indicates better quality (40+ dB is excellent)
        character(len=*), intent(in) :: image1, image2
        real(wp) :: psnr
        character(len=1024) :: command, output_file
        integer :: unit_id, exit_code, ios
        character(len=256) :: line
        
        ! Generate temporary output filename
        output_file = trim(image1) // "_psnr.txt"
        
        ! Build ImageMagick compare command
        write(command, '(A)') 'magick compare -metric PSNR "' // &
                             trim(image1) // '" "' // trim(image2) // &
                             '" /dev/null 2> "' // trim(output_file) // '"'
        
        ! Execute comparison
        call execute_command_line(trim(command), exitstat=exit_code)
        
        ! Parse the PSNR value from output
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
        
        ! Clean up temp file
        call execute_command_line('rm -f "' // trim(output_file) // '"')
        
    end function compare_images_psnr
    
    subroutine generate_reference_image(filename, width, height)
        !! Generate a reference antialiased diagonal line image using ImageMagick
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        character(len=1024) :: command
        integer :: exit_code
        
        ! Create antialiased diagonal line using ImageMagick
        write(command, '(A,I0,A,I0,A)') &
            'magick -size ', width, 'x', height, ' xc:white ' // &
            '-stroke black -strokewidth 2 -draw "line 10,10 ' // &
            trim(adjustl(int_to_str(width-10))) // ',' // &
            trim(adjustl(int_to_str(height-10))) // '" ' // &
            '-blur 0x0.5 "' // trim(filename) // '"'
        
        call execute_command_line(trim(command), exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "ERROR: Failed to generate reference image"
        end if
        
    end subroutine generate_reference_image
    
    function analyze_edge_smoothness(image_file) result(smoothness_score)
        !! Analyze edge smoothness in an image using edge detection
        !! Returns a score from 0-100 (higher = smoother edges)
        character(len=*), intent(in) :: image_file
        real(wp) :: smoothness_score
        character(len=1024) :: command, stats_file
        integer :: unit_id, exit_code, ios
        character(len=256) :: line
        real(wp) :: mean_edge, std_edge
        
        ! Generate temporary stats filename
        stats_file = trim(image_file) // "_edge_stats.txt"
        
        ! Apply edge detection and get statistics
        write(command, '(A)') &
            'magick "' // trim(image_file) // '" -edge 1 ' // &
            '-format "%[fx:mean*100] %[fx:standard_deviation*100]" ' // &
            'info: > "' // trim(stats_file) // '"'
        
        call execute_command_line(trim(command), exitstat=exit_code)
        
        ! Parse statistics
        open(newunit=unit_id, file=stats_file, status='old', &
             action='read', iostat=ios)
        if (ios == 0) then
            read(unit_id, *, iostat=ios) mean_edge, std_edge
            close(unit_id)
            
            if (ios == 0) then
                ! Lower edge values indicate smoother antialiasing
                ! Convert to 0-100 score (100 = smoothest)
                smoothness_score = max(0.0_wp, 100.0_wp - mean_edge * 10.0_wp)
            else
                smoothness_score = -1.0_wp
            end if
        else
            smoothness_score = -1.0_wp
        end if
        
        ! Clean up temp file
        call execute_command_line('rm -f "' // trim(stats_file) // '"')
        
    end function analyze_edge_smoothness
    
    ! Helper function to convert integer to string
    function int_to_str(n) result(str)
        integer, intent(in) :: n
        character(len=32) :: str
        write(str, '(I0)') n
    end function int_to_str
    
end module fortplot_imagemagick