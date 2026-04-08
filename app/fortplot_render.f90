program fortplot_render
    !! Render Vega-Lite JSON specs to PNG/PDF/SVG.
    !!
    !! Usage:
    !!   echo '{"mark":"line",...}' | fortplot_render -o output.png
    !!   fortplot_render -i plot.vl.json -o output.png
    !!   fortplot_render -i plot.vl.json -o plot.svg
    !!
    !! Reads a Vega-Lite JSON spec from stdin (default) or a file
    !! (-i flag), parses it into spec_t, and renders to the output
    !! file. Output format is determined by file extension.

    use fortplot_spec_types, only: spec_t
    use fortplot_spec_builder, only: spec_savefig
    use fortplot_spec_json_parse, only: json_to_spec, &
                                        read_stdin, read_file
    implicit none

    character(len=256) :: input_file, output_file, arg
    character(len=:), allocatable :: json_content
    type(spec_t) :: spec
    integer :: status, argc, i
    logical :: has_input, has_output

    has_input = .false.
    has_output = .false.
    input_file = ''
    output_file = ''

    argc = command_argument_count()
    i = 1
    do while (i <= argc)
        call get_command_argument(i, arg)
        select case (trim(adjustl(arg)))
        case ('-i', '--input')
            if (i + 1 > argc) then
                write (*, '(a)') 'Error: -i requires a filename'
                stop 1
            end if
            i = i + 1
            call get_command_argument(i, input_file)
            has_input = .true.
        case ('-o', '--output')
            if (i + 1 > argc) then
                write (*, '(a)') 'Error: -o requires a filename'
                stop 1
            end if
            i = i + 1
            call get_command_argument(i, output_file)
            has_output = .true.
        case ('-h', '--help')
            call print_help()
            stop
        case default
            write (*, '(a,a)') 'Unknown option: ', trim(arg)
            call print_help()
            stop 1
        end select
        i = i + 1
    end do

    if (.not. has_output) then
        write (*, '(a)') 'Error: -o <output> is required'
        call print_help()
        stop 1
    end if

    if (has_input) then
        call read_file(trim(input_file), json_content, status)
        if (status /= 0) then
            write (*, '(a,a)') 'Error reading file: ', &
                trim(input_file)
            stop 1
        end if
    else
        call read_stdin(json_content, status)
        if (status /= 0) then
            write (*, '(a)') 'Error reading stdin'
            stop 1
        end if
    end if

    if (len(json_content) == 0) then
        write (*, '(a)') 'Error: empty input'
        stop 1
    end if

    call json_to_spec(json_content, spec, status)
    if (status /= 0) then
        write (*, '(a,i0)') 'Error parsing JSON, code: ', status
        stop 1
    end if

    call spec_savefig(spec, trim(output_file), status)
    if (status /= 0) then
        write (*, '(a,a)') 'Error rendering to: ', &
            trim(output_file)
        stop 1
    end if

contains

    subroutine print_help()
        write (*, '(a)') 'fortplot_render - render Vega-Lite ' &
                          //'JSON to image'
        write (*, '(a)') ''
        write (*, '(a)') 'Usage:'
        write (*, '(a)') '  fortplot_render -o output.png ' &
                          //'[-i input.vl.json]'
        write (*, '(a)') ''
        write (*, '(a)') 'Options:'
        write (*, '(a)') '  -i, --input   Input JSON file ' &
                          //'(default: stdin)'
        write (*, '(a)') '  -o, --output  Output file ' &
                          //'(required, format from extension)'
        write (*, '(a)') '  -h, --help    Show this help'
        write (*, '(a)') ''
        write (*, '(a)') 'Examples:'
        write (*, '(a)') '  echo ''{"mark":"line",...}'' | ' &
                          //'fortplot_render -o plot.png'
        write (*, '(a)') '  fortplot_render -i spec.vl.json ' &
                          //'-o plot.svg'
    end subroutine print_help

end program fortplot_render
