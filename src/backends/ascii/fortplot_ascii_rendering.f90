module fortplot_ascii_rendering
    !! ASCII terminal plotting backend - Core Rendering Logic
    !!
    !! This module contains the core rendering functionality for ASCII plotting,
    !! including canvas output, file writing, and terminal display.
    !!
    !! Author: fortplot contributors
    
    use fortplot_logging, only: log_info, log_error
    use fortplot_ascii_utils, only: text_element_t, render_text_elements_to_canvas
    use fortplot_ascii_utils, only: print_centered_title, write_centered_title
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: output_to_terminal, output_to_file
    public :: ascii_finalize, ascii_get_output
    
contains

    subroutine ascii_finalize(canvas, text_elements, num_text_elements, &
                             plot_width, plot_height, title_text, xlabel_text, ylabel_text, &
                             filename)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        character(len=*), intent(in) :: filename
        
        integer :: unit, ios
        character(len=512) :: error_msg
        
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            call output_to_terminal(canvas, text_elements, num_text_elements, &
                                  plot_width, plot_height, title_text, xlabel_text, ylabel_text)
        else
            open(newunit=unit, file=filename, status='replace', iostat=ios, iomsg=error_msg)
            
            if (ios /= 0) then
                call log_error("Cannot save ASCII file '" // trim(filename) // "': " // trim(error_msg))
                ! Fall back to terminal output
                call log_info("Falling back to terminal output due to file error")
                call output_to_terminal(canvas, text_elements, num_text_elements, &
                                      plot_width, plot_height, title_text, xlabel_text, ylabel_text)
                return
            end if
            
            call output_to_file(canvas, text_elements, num_text_elements, &
                              plot_width, plot_height, title_text, xlabel_text, ylabel_text, unit)
            close(unit)
            call log_info("Unicode plot saved to '" // trim(filename) // "'")
        end if
    end subroutine ascii_finalize
    
    subroutine output_to_terminal(canvas, text_elements, num_text_elements, &
                                 plot_width, plot_height, title_text, xlabel_text, ylabel_text)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        integer :: i, j
        
        ! Render text elements to canvas before output
        call render_text_elements_to_canvas(canvas, text_elements, &
                                            num_text_elements, &
                                            plot_width, plot_height)
        
        if (allocated(title_text)) then
            print '(A)', ''  ! Empty line before title
            call print_centered_title(title_text, plot_width)
        end if
        
        print '(A)', '+' // repeat('-', plot_width) // '+'
        do i = 1, plot_height
            write(*, '(A)', advance='no') '|'
            do j = 1, plot_width
                write(*, '(A)', advance='no') canvas(i, j)
            end do
            print '(A)', '|'
        end do
        
        print '(A)', '+' // repeat('-', plot_width) // '+'
        
        ! Print xlabel below the plot if present
        if (allocated(xlabel_text)) then
            call print_centered_title(xlabel_text, plot_width)
        end if
        
        ! Print ylabel (simple horizontal placement for now)
        if (allocated(ylabel_text)) then
            print '(A)', ylabel_text
        end if
    end subroutine output_to_terminal
    
    subroutine output_to_file(canvas, text_elements, num_text_elements, &
                            plot_width, plot_height, title_text, xlabel_text, ylabel_text, unit)
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(in) :: title_text, xlabel_text, ylabel_text
        integer, intent(in) :: unit
        integer :: i, j
        
        ! Render text elements to canvas before output
        call render_text_elements_to_canvas(canvas, text_elements, &
                                            num_text_elements, &
                                            plot_width, plot_height)
        
        if (allocated(title_text)) then
            write(unit, '(A)') ''  ! Empty line before title
            call write_centered_title(unit, title_text, plot_width)
        end if
        
        write(unit, '(A)') '+' // repeat('-', plot_width) // '+'
        do i = 1, plot_height
            write(unit, '(A)', advance='no') '|'
            do j = 1, plot_width
                write(unit, '(A)', advance='no') canvas(i, j)
            end do
            write(unit, '(A)') '|'
        end do
        
        write(unit, '(A)') '+' // repeat('-', plot_width) // '+'
        
        ! Write xlabel below the plot if present
        if (allocated(xlabel_text)) then
            call write_centered_title(unit, xlabel_text, plot_width)
        end if
        
        ! Write ylabel to the left side of the plot if present
        if (allocated(ylabel_text)) then
            write(unit, '(A)') ylabel_text
        end if
    end subroutine output_to_file

    function ascii_get_output(canvas, width, height) result(output)
        !! Get the complete ASCII canvas as a string
        character(len=1), intent(in) :: canvas(:,:)
        integer, intent(in) :: width, height
        character(len=:), allocatable :: output
        character(len=1000) :: line_buffer
        integer :: i, j, total_len, line_len
        
        ! Calculate total length needed
        total_len = height * (width + 1)  ! +1 for newline per row
        allocate(character(len=total_len) :: output)
        
        output = ""
        do i = 1, height
            line_buffer = ""
            do j = 1, width
                line_buffer(j:j) = canvas(i, j)
            end do
            line_len = len_trim(line_buffer(:width))
            if (line_len == 0) line_len = 1  ! Ensure at least one character
            output = output // line_buffer(1:line_len) // new_line('a')
        end do
    end function ascii_get_output

end module fortplot_ascii_rendering