program test_doc_core
    use fortplot_documentation, only: &
        title_case, get_output_title, build_python_path, &
        get_file_extension, replace_extension, build_fortran_url, PATH_MAX_LEN
    implicit none

    character(len=PATH_MAX_LEN) :: got, want
    character(len=PATH_MAX_LEN) :: url

    ! title_case
    call assert_eq('title_case basic_plots', title_case('basic_plots'), 'Basic Plots')
    call assert_eq('title_case single', title_case('animation'), 'Animation')

    ! get_output_title
    call assert_eq('get_output_title png', get_output_title('simple_plot.png'), 'Simple Plot')
    call assert_eq('get_output_title noext', get_output_title('multi_line'), 'Multi Line')

    ! build_python_path
    got = build_python_path('basic_plots')
    want = 'example/python/basic_plots/basic_plots.py'
    call assert_eq('build_python_path', got, want)

    ! extension helpers
    call assert_eq('get_file_extension', get_file_extension('file.mp4'), 'mp4')
    call assert_eq('replace_extension', replace_extension('image.png','pdf'), 'image.pdf')

    ! build_fortran_url suffix check
    url = build_fortran_url('marker_demo', 'marker_demo.f90')
    if (index(url, '/example/fortran/marker_demo/marker_demo.f90') == 0) then
        print *, 'FAIL: build_fortran_url suffix mismatch:', trim(url)
        stop 1
    end if

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

end program test_doc_core

