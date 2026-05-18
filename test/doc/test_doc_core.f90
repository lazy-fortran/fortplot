program test_doc_core
    use fortplot_documentation, only: &
        title_case, get_output_title, get_file_extension, replace_extension, &
        build_fortran_url, build_local_fortran_path, get_fortran_filename, &
        get_example_run_target, PATH_MAX_LEN
    implicit none

    character(len=PATH_MAX_LEN) :: got, want
    character(len=PATH_MAX_LEN) :: url

    ! title_case
    call assert_eq('title_case basic_plots', &
                   title_case('basic_plots'), 'Basic Plots')
    call assert_eq('title_case single', &
                   title_case('animation'), 'Animation')
    call assert_eq('title_case hyphen', &
                   title_case('multi-axis_plot'), 'Multi Axis Plot')
    call assert_eq('title_case digits', &
                   title_case('3d_rotation'), '3D Rotation')

    ! get_output_title
    call assert_eq('get_output_title png', &
                   get_output_title('simple_plot.png'), 'Simple Plot')
    call assert_eq('get_output_title noext', &
                   get_output_title('multi_line'), 'Multi Line')

    ! extension helpers
    call assert_eq('get_file_extension', get_file_extension('file.mp4'), 'mp4')
    call assert_eq('replace_extension', &
                   replace_extension('image.png', 'pdf'), 'image.pdf')

    call assert_example_source('basic_plots', 'basic_plots.f90')
    call assert_example_source('animation', 'save_animation_demo.f90')
    call assert_example_source('ascii_heatmap', 'ascii_heatmap_demo.f90')

    print *, 'All doc core tests passed!'

contains

    subroutine assert_eq(name, a, b)
        character(len=*), intent(in) :: name, a, b
        if (trim(a) /= trim(b)) then
            print *, 'FAIL:', trim(name)
            print *, '  got :', trim(a)
            print *, '  want:', trim(b)
            stop 1
        end if
    end subroutine assert_eq

    subroutine assert_example_source(example, source_file)
        character(len=*), intent(in) :: example, source_file
        character(len=PATH_MAX_LEN) :: actual, source_path, run_target
        character(len=:), allocatable :: expected_path, expected_target
        integer :: dot_pos

        call get_fortran_filename(example, actual)
        call assert_eq(trim(example)//' filename', trim(actual), source_file)

        call build_fortran_url(example, url)
        if (index(url, '/example/fortran/'//trim(example)//'/'//source_file) == 0) then
            print *, 'FAIL: build_fortran_url suffix mismatch:', trim(url)
            stop 1
        end if

        call build_local_fortran_path(example, source_path)
        expected_path = 'example/fortran/'//trim(example)//'/'//source_file
        call assert_eq(trim(example)//' local path', trim(source_path), expected_path)

        dot_pos = index(source_file, '.', back=.true.)
        if (dot_pos > 1) then
            expected_target = source_file(1:dot_pos - 1)
        else
            expected_target = source_file
        end if
        call get_example_run_target(example, run_target)
        call assert_eq(trim(example)//' run target', trim(run_target), &
                       expected_target)
    end subroutine assert_example_source

end program test_doc_core
