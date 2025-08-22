program test_xlabel_rendering_regression
    !! Test xlabel rendering regression - Issue #190
    !! 
    !! GIVEN: A figure with xlabel set
    !! WHEN: Figure is rendered to output file
    !! THEN: xlabel text should appear in the actual output content
    !!
    !! This test MUST FAIL initially due to stub implementations in
    !! render_axis_framework() and render_axis_labels()

    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit, error_unit
    use fortplot_figure_base, only: figure_t
    use fortplot_plotting, only: add_plot
    use fortplot_rendering, only: savefig
    implicit none

    integer, parameter :: n_points = 10
    real(wp) :: x(n_points), y(n_points)
    type(figure_t) :: fig
    integer :: i, iostat, file_unit
    character(len=1000) :: line_buffer
    logical :: xlabel_found
    character(len=*), parameter :: expected_xlabel = "Test X-axis Label"
    character(len=*), parameter :: test_filename = "test_xlabel_output.txt"

    write(output_unit, '(A)') "=== Testing xlabel rendering in ASCII output ==="

    ! GIVEN: Create test data and figure with xlabel
    do i = 1, n_points
        x(i) = real(i - 1, wp)
        y(i) = sin(x(i) * 0.5_wp)
    end do

    call fig%initialize(400, 300)
    call add_plot(fig, x, y, label="test_line")
    call fig%set_xlabel(expected_xlabel)

    write(output_unit, '(A)') "GIVEN: Created figure with xlabel = '" // expected_xlabel // "'"

    ! WHEN: Save figure to ASCII file
    call savefig(fig, test_filename)
    write(output_unit, '(A)') "WHEN: Figure saved to " // test_filename

    ! THEN: xlabel should appear in the output file content
    xlabel_found = .false.
    open(newunit=file_unit, file=test_filename, status='old', action='read', iostat=iostat)
    
    if (iostat /= 0) then
        write(error_unit, '(A,I0)') "CRITICAL: Cannot open output file. iostat = ", iostat
        call exit(1)
    end if

    ! Scan file content for xlabel text
    do
        read(file_unit, '(A)', iostat=iostat) line_buffer
        if (iostat /= 0) exit
        
        if (index(line_buffer, expected_xlabel) > 0) then
            xlabel_found = .true.
            write(output_unit, '(A)') "Found xlabel in line: " // trim(line_buffer)
            exit
        end if
    end do
    
    close(file_unit)

    ! ASSERTION: xlabel must be present in output
    if (.not. xlabel_found) then
        write(error_unit, '(A)') "FAIL: xlabel '" // expected_xlabel // "' NOT FOUND in output file"
        write(error_unit, '(A)') "This indicates render_axis_labels() is a stub - Issue #190"
        call exit(1)
    else
        write(output_unit, '(A)') "PASS: xlabel found in output file"
    end if

    write(output_unit, '(A)') "=== Testing xlabel with special characters ==="

    ! Test with special characters
    call test_xlabel_special_chars()

    write(output_unit, '(A)') "=== Testing xlabel positioning validation ==="

    ! Test xlabel positioning
    call test_xlabel_positioning()

    write(output_unit, '(A)') "All xlabel rendering tests passed"

contains

    subroutine test_xlabel_special_chars()
        !! GIVEN: xlabel with special characters
        !! WHEN: Figure rendered
        !! THEN: Special characters should appear in output
        
        type(figure_t) :: fig_special
        character(len=*), parameter :: special_xlabel = "Force [N] vs Time \\tau [s]"
        character(len=*), parameter :: special_filename = "test_xlabel_special.txt"
        logical :: special_found
        integer :: unit_special, io_stat
        character(len=1000) :: buffer
        
        call fig_special%initialize(400, 300)
        call add_plot(fig_special, [1.0_wp, 2.0_wp], [1.0_wp, 4.0_wp])
        call fig_special%set_xlabel(special_xlabel)
        
        call savefig(fig_special, special_filename)
        
        special_found = .false.
        open(newunit=unit_special, file=special_filename, status='old', action='read', iostat=io_stat)
        
        if (io_stat == 0) then
            do
                read(unit_special, '(A)', iostat=io_stat) buffer
                if (io_stat /= 0) exit
                
                if (index(buffer, "Force") > 0 .or. index(buffer, "tau") > 0) then
                    special_found = .true.
                    exit
                end if
            end do
            close(unit_special)
        end if
        
        if (.not. special_found) then
            write(error_unit, '(A)') "FAIL: Special character xlabel not rendered"
            call exit(1)
        end if
        
        write(output_unit, '(A)') "PASS: Special character xlabel rendering"
    end subroutine test_xlabel_special_chars

    subroutine test_xlabel_positioning()
        !! GIVEN: xlabel set on figure
        !! WHEN: Multiple plots added
        !! THEN: xlabel should appear in consistent position
        
        type(figure_t) :: fig_pos
        character(len=*), parameter :: pos_xlabel = "Consistent Position Label"
        character(len=*), parameter :: pos_filename = "test_xlabel_position.txt"
        integer :: label_line_count, unit_pos, io_stat, line_num
        character(len=1000) :: buffer
        
        call fig_pos%initialize(400, 300)
        call add_plot(fig_pos, [1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp], label="data1")
        call add_plot(fig_pos, [1.0_wp, 2.0_wp, 3.0_wp], [2.0_wp, 3.0_wp, 4.0_wp], label="data2")
        call fig_pos%set_xlabel(pos_xlabel)
        
        call savefig(fig_pos, pos_filename)
        
        label_line_count = 0
        line_num = 0
        open(newunit=unit_pos, file=pos_filename, status='old', action='read', iostat=io_stat)
        
        if (io_stat == 0) then
            do
                read(unit_pos, '(A)', iostat=io_stat) buffer
                if (io_stat /= 0) exit
                line_num = line_num + 1
                
                if (index(buffer, pos_xlabel) > 0) then
                    label_line_count = label_line_count + 1
                    write(output_unit, '(A,I0,A)') "xlabel found at line ", line_num, ": " // trim(buffer)
                end if
            end do
            close(unit_pos)
        end if
        
        if (label_line_count == 0) then
            write(error_unit, '(A)') "FAIL: xlabel not found in positioned output"
            call exit(1)
        else if (label_line_count > 1) then
            write(error_unit, '(A,I0)') "FAIL: xlabel appears multiple times: ", label_line_count
            call exit(1)
        end if
        
        write(output_unit, '(A)') "PASS: xlabel positioning validation"
    end subroutine test_xlabel_positioning

end program test_xlabel_rendering_regression