program test_axis_rendering_pipeline
    !! Test axis rendering pipeline validation - Issue #190
    !! 
    !! GIVEN: Figure with xlabel and rendering pipeline
    !! WHEN: render_figure() is called
    !! THEN: Backend draw_axes_and_labels_backend() should be invoked
    !!
    !! This test exposes the gap where stubs never call backend methods

    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit, error_unit
    use fortplot_figure_core
    use fortplot_rendering, only: render_figure
    implicit none

    type(figure_t) :: fig
    real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp), parameter :: y_data(3) = [1.0_wp, 4.0_wp, 9.0_wp]
    character(len=*), parameter :: test_xlabel = "Pipeline Test Label"

    write(output_unit, '(A)') "=== Testing axis rendering pipeline ==="

    ! GIVEN: Create figure with xlabel
    call fig%initialize(400, 300)
    call fig%add_plot(x_data, y_data)
    call fig%set_xlabel(test_xlabel)
    
    write(output_unit, '(A)') "GIVEN: Created figure with xlabel = '" // test_xlabel // "'"

    ! WHEN: Explicitly call render_figure to test pipeline
    call render_figure(fig)
    write(output_unit, '(A)') "WHEN: render_figure() called"

    ! THEN: Check if rendering actually occurred
    call test_rendering_pipeline_execution(fig)

    ! Test stub method detection
    call test_stub_method_detection()

    ! Test backend method invocation
    call test_backend_method_invocation()

    write(output_unit, '(A)') "Pipeline tests completed"

contains

    subroutine test_rendering_pipeline_execution(test_fig)
        !! GIVEN: Figure after render_figure() call
        !! WHEN: Checking rendered flag and output
        !! THEN: Figure should be marked as rendered
        
        type(figure_t), intent(in) :: test_fig
        
        if (.not. test_fig%rendered) then
            write(error_unit, '(A)') "FAIL: Figure not marked as rendered after render_figure()"
            call exit(1)
        end if
        
        write(output_unit, '(A)') "PASS: Figure marked as rendered"
        
        ! Verify rendering occurred by testing show() output
        call test_show_output_contains_xlabel()
        
    end subroutine test_rendering_pipeline_execution

    subroutine test_show_output_contains_xlabel()
        !! GIVEN: Rendered figure with xlabel
        !! WHEN: show() method called (saves to fortplot_output.txt)
        !! THEN: Output should contain xlabel
        
        type(figure_t) :: show_fig
        character(len=*), parameter :: show_xlabel = "Show Method Label"
        character(len=*), parameter :: show_output = "fortplot_output.txt"
        logical :: xlabel_in_show
        integer :: show_unit, io_stat
        character(len=1000) :: buffer
        
        call show_fig%initialize(400, 300)
        call show_fig%add_plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
        call show_fig%set_xlabel(show_xlabel)
        
        ! Call show() which saves to fortplot_output.txt
        call show_fig%show()
        
        xlabel_in_show = .false.
        open(newunit=show_unit, file=show_output, status='old', action='read', iostat=io_stat)
        
        if (io_stat == 0) then
            do
                read(show_unit, '(A)', iostat=io_stat) buffer
                if (io_stat /= 0) exit
                
                if (index(buffer, show_xlabel) > 0) then
                    xlabel_in_show = .true.
                    exit
                end if
            end do
            close(show_unit)
        end if
        
        if (.not. xlabel_in_show) then
            write(error_unit, '(A)') "FAIL: xlabel not found in show() output"
            write(error_unit, '(A)') "This indicates render_axis_labels() stub is not calling backend"
            call exit(1)
        end if
        
        write(output_unit, '(A)') "PASS: xlabel found in show() output"
    end subroutine test_show_output_contains_xlabel

    subroutine test_stub_method_detection()
        !! GIVEN: Current implementation with stubs
        !! WHEN: Testing axis rendering functionality
        !! THEN: Should detect that stubs are not calling backend methods
        
        type(figure_t) :: stub_fig
        character(len=*), parameter :: stub_xlabel = "Stub Detection Label"
        character(len=*), parameter :: stub_output = "test_stub_detection.txt"
        logical :: backend_called
        integer :: stub_unit, io_stat
        character(len=1000) :: buffer
        integer :: content_lines
        
        write(output_unit, '(A)') "--- Testing stub method detection ---"
        
        call stub_fig%initialize(400, 300)
        call stub_fig%add_plot([1.0_wp, 2.0_wp], [2.0_wp, 4.0_wp])
        call stub_fig%set_xlabel(stub_xlabel)
        
        call stub_fig%save(stub_output)
        
        ! Count non-empty content lines to detect if backend was called
        backend_called = .false.
        content_lines = 0
        
        open(newunit=stub_unit, file=stub_output, status='old', action='read', iostat=io_stat)
        
        if (io_stat == 0) then
            do
                read(stub_unit, '(A)', iostat=io_stat) buffer
                if (io_stat /= 0) exit
                
                if (len_trim(buffer) > 0) then
                    content_lines = content_lines + 1
                end if
                
                ! Look for any xlabel content
                if (index(buffer, stub_xlabel) > 0) then
                    backend_called = .true.
                end if
            end do
            close(stub_unit)
        end if
        
        write(output_unit, '(A,I0)') "Content lines found: ", content_lines
        
        if (.not. backend_called .and. content_lines > 0) then
            write(output_unit, '(A)') "EXPECTED FAIL: Backend called but xlabel missing (stub issue)"
            write(output_unit, '(A)') "This confirms render_axis_labels() is a stub"
        else if (.not. backend_called .and. content_lines == 0) then
            write(error_unit, '(A)') "CRITICAL: Backend not called at all - deeper pipeline issue"
            call exit(1)
        else
            write(output_unit, '(A)') "Backend appears to be working (unexpected)"
        end if
    end subroutine test_stub_method_detection

    subroutine test_backend_method_invocation()
        !! GIVEN: Multiple backend types
        !! WHEN: Testing xlabel rendering across backends  
        !! THEN: All backends should show missing xlabel issue
        
        write(output_unit, '(A)') "--- Testing cross-backend xlabel rendering ---"
        
        ! Test ASCII backend
        call test_single_backend_xlabel("test_ascii_xlabel.txt")
        
        ! Test other format extensions to trigger backend switching
        call test_single_backend_xlabel("test_png_xlabel.png")
        call test_single_backend_xlabel("test_pdf_xlabel.pdf")
        
        write(output_unit, '(A)') "Cross-backend testing completed"
    end subroutine test_backend_method_invocation

    subroutine test_single_backend_xlabel(filename)
        !! Test xlabel rendering for specific backend
        character(len=*), intent(in) :: filename
        type(figure_t) :: backend_fig
        character(len=*), parameter :: backend_xlabel = "Backend Test Label"
        logical :: file_exists
        
        call backend_fig%initialize(400, 300)
        call backend_fig%add_plot([0.0_wp, 1.0_wp, 2.0_wp], [0.0_wp, 1.0_wp, 4.0_wp])
        call backend_fig%set_xlabel(backend_xlabel)
        
        call backend_fig%save(filename)
        
        inquire(file=filename, exist=file_exists)
        
        if (file_exists) then
            write(output_unit, '(A)') "PASS: File created for " // filename
            ! Note: Content validation would require format-specific parsing
            ! This test focuses on confirming backend switching works
        else
            write(error_unit, '(A)') "FAIL: File not created for " // filename
            call exit(1)
        end if
    end subroutine test_single_backend_xlabel

end program test_axis_rendering_pipeline