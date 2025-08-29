module fortplot_doc_output
    !! Output generation for documentation
    use fortplot_doc_constants, only: FILENAME_MAX_LEN, LINE_MAX_LEN, VIDEO_WIDTH, VIDEO_HEIGHT
    use fortplot_doc_files, only: get_file_extension, replace_extension, copy_file_content
    use fortplot_doc_text, only: get_output_title
    use fortplot_doc_media, only: scan_directory_for_media
    implicit none
    private
    
    ! Public interface
    public :: write_generated_outputs
    
contains

    subroutine write_generated_outputs(unit_out, example_dir, example_name)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_dir, example_name
        
        character(len=FILENAME_MAX_LEN) :: media_files(10)
        integer :: n_media, j
        
        call find_output_files(example_dir, example_name, media_files, n_media)
        
        do j = 1, n_media
            call write_media_output(unit_out, example_name, media_files(j))
            write(unit_out, '(A)') ''
        end do
    end subroutine write_generated_outputs
    
    subroutine find_output_files(example_dir, example_name, media_files, n_media)
        character(len=*), intent(in) :: example_dir, example_name
        character(len=*), intent(out) :: media_files(:)
        integer, intent(out) :: n_media
        
        character(len=FILENAME_MAX_LEN) :: output_dir
        
        output_dir = 'output/example/fortran/' // trim(example_name)
        call scan_directory_for_media(output_dir, media_files, n_media)
    end subroutine find_output_files
    
    subroutine write_media_output(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        character(len=10) :: extension
        
        extension = get_file_extension(media_file)
        
        call write_output_title(unit_out, media_file)
        
        if (extension == 'mp4') then
            call write_video_output(unit_out, example_name, media_file)
        else
            call write_image_output(unit_out, example_name, media_file)
        end if
    end subroutine write_media_output
    
    subroutine write_output_title(unit_out, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: media_file
        
        write(unit_out, '(A)') '### ' // trim(get_output_title(media_file))
        write(unit_out, '(A)') ''
    end subroutine write_output_title
    
    subroutine write_video_output(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        call write_video_html(unit_out, example_name, media_file)
        call write_video_download_link(unit_out, example_name, media_file)
    end subroutine write_video_output
    
    subroutine write_video_html(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A,I0,A,I0,A)') '<video width="', VIDEO_WIDTH, &
                                       '" height="', VIDEO_HEIGHT, '" controls>'
        write(unit_out, '(A)') '  <source src="../../media/examples/' // &
                              trim(example_name) // '/' // trim(media_file) // &
                              '" type="video/mp4">'
        write(unit_out, '(A)') '  Your browser does not support the video tag.'
        write(unit_out, '(A)') '</video>'
        write(unit_out, '(A)') ''
    end subroutine write_video_html
    
    subroutine write_video_download_link(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A)') '[Download MP4](../../media/examples/' // &
                              trim(example_name) // '/' // trim(media_file) // ')'
    end subroutine write_video_download_link
    
    subroutine write_image_output(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        call write_image_link(unit_out, example_name, media_file)
        call write_ascii_output_if_exists(unit_out, example_name, media_file)
        call write_pdf_download_link(unit_out, example_name, media_file)
    end subroutine write_image_output
    
    subroutine write_image_link(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A)') '![' // trim(media_file) // &
                              '](../../media/examples/' // trim(example_name) // &
                              '/' // trim(media_file) // ')'
        write(unit_out, '(A)') ''
    end subroutine write_image_link
    
    subroutine write_ascii_output_if_exists(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        character(len=FILENAME_MAX_LEN) :: ascii_file
        logical :: file_exists
        
        call build_ascii_file_path(example_name, media_file, ascii_file)
        inquire(file=trim(ascii_file), exist=file_exists)
        
        if (file_exists) then
            call write_ascii_content(unit_out, ascii_file)
        end if
    end subroutine write_ascii_output_if_exists
    
    subroutine build_ascii_file_path(example_name, media_file, ascii_file)
        character(len=*), intent(in) :: example_name, media_file
        character(len=FILENAME_MAX_LEN), intent(out) :: ascii_file
        
        ascii_file = 'output/example/fortran/' // trim(example_name) // '/' // &
                    replace_extension(media_file, 'txt')
    end subroutine build_ascii_file_path
    
    subroutine write_ascii_content(unit_out, ascii_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: ascii_file
        
        write(unit_out, '(A)') 'ASCII output:'
        write(unit_out, '(A)') '```'
        call copy_file_content(ascii_file, unit_out)
        write(unit_out, '(A)') '```'
        write(unit_out, '(A)') ''
    end subroutine write_ascii_content
    
    subroutine write_pdf_download_link(unit_out, example_name, media_file)
        integer, intent(in) :: unit_out
        character(len=*), intent(in) :: example_name, media_file
        
        write(unit_out, '(A)') '[Download PDF](../../media/examples/' // &
                              trim(example_name) // '/' // &
                              replace_extension(media_file, 'pdf') // ')'
    end subroutine write_pdf_download_link

end module fortplot_doc_output