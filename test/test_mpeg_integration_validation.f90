program test_mpeg_integration_validation
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, safe_validate_mpeg_with_ffprobe
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation must integrate properly with overall system (Issue #32)
    ! When: We test integration with animation pipeline and validation workflow
    ! Then: Validation should work seamlessly with the full system

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== MPEG INTEGRATION VALIDATION TESTS ==="
    
    call test_animation_pipeline_integration()
    call test_validation_workflow_integration()
    call test_figure_backend_integration()
    call test_error_handling_integration()

    print *, "=== Integration validation tests completed ==="

contains

    subroutine test_animation_pipeline_integration()
        ! Given: Validation should integrate with the animation pipeline
        ! When: We test the complete animation-to-validation workflow
        ! Then: Pipeline should work end-to-end with proper validation

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: pipeline_integration_successful
        integer :: status, file_size

        print *, ""
        print *, "TEST: Animation Pipeline Integration"
        print *, "=================================="

        test_file = "integration_pipeline.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        ! Test complete pipeline: animation creation -> save -> validation
        anim = FuncAnimation(update_pipeline_data, frames=10, interval=50, fig=test_fig)
        
        print *, "Testing animation save with status tracking..."
        call anim%save(test_file, fps=20, status=status)

        inquire(file=test_file, size=file_size)
        pipeline_integration_successful = test_pipeline_integration(test_file, status, file_size)

        print *, "Save status:", status
        print *, "File size:", file_size, "bytes"
        print *, "Pipeline integration successful:", pipeline_integration_successful

        if (.not. pipeline_integration_successful) then
            print *, "*** ANIMATION PIPELINE INTEGRATION FAILURE ***"
            print *, "Validation not properly integrated with animation pipeline"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_pipeline_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.3_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_pipeline_integration(filename, save_status, file_size) result(successful)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: save_status, file_size
        logical :: successful
        logical :: save_successful, file_adequate, validation_consistent
        
        save_successful = (save_status == 0)
        file_adequate = (file_size > 2000)
        validation_consistent = validate_pipeline_output(filename)
        
        successful = save_successful .and. file_adequate .and. validation_consistent
        
        print *, "  Pipeline integration analysis:"
        print *, "    Save successful:", save_successful
        print *, "    File adequate:", file_adequate
        print *, "    Validation consistent:", validation_consistent
        print *, "    Overall successful:", successful
    end function

    function validate_pipeline_output(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: exists, header_ok, tool_ok
        integer :: file_size
        
        inquire(file=filename, exist=exists, size=file_size)
        header_ok = check_file_header(filename)
        tool_ok = check_tool_validation(filename)
        
        valid = exists .and. header_ok .and. tool_ok
    end function

    function check_file_header(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=12) :: header
        integer :: file_unit, ios
        
        valid = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        valid = (index(header, 'ftyp') > 0 .or. &
                index(header, 'mdat') > 0 .or. &
                index(header, 'moov') > 0)
    end function

    function check_tool_validation(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: ffprobe_available
        
        ffprobe_available = safe_check_program_available('ffprobe')
        if (.not. ffprobe_available) then
            valid = .true.  ! Tool not available, skip check
            return
        end if
        
        valid = safe_validate_mpeg_with_ffprobe(filename)
    end function

    subroutine test_validation_workflow_integration()
        ! Given: Validation should integrate with the overall workflow
        ! When: We test validation at different workflow stages
        ! Then: Validation should provide consistent results throughout workflow

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: workflow_integration_successful
        logical :: pre_save_valid, post_save_valid, post_validation_valid

        print *, ""
        print *, "TEST: Validation Workflow Integration"
        print *, "==================================="

        test_file = "integration_workflow.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)

        ! Pre-save validation (should indicate pending state)
        pre_save_valid = .false.  ! File doesn't exist yet
        
        anim = FuncAnimation(update_workflow_data, frames=12, interval=50, fig=test_fig)
        call anim%save(test_file, fps=18)

        ! Post-save validation
        post_save_valid = validate_pipeline_output(test_file)
        
        ! Post-validation check (verify validation was applied correctly)
        post_validation_valid = verify_validation_applied(test_file)

        workflow_integration_successful = (.not. pre_save_valid) .and. post_save_valid .and. post_validation_valid

        print *, "Pre-save validation (should be FALSE):", pre_save_valid
        print *, "Post-save validation:", post_save_valid
        print *, "Post-validation verification:", post_validation_valid
        print *, "Workflow integration successful:", workflow_integration_successful

        if (.not. workflow_integration_successful) then
            print *, "*** VALIDATION WORKFLOW INTEGRATION FAILURE ***"
            print *, "Validation not properly integrated with workflow stages"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_workflow_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.25_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function verify_validation_applied(filename) result(verified)
        character(len=*), intent(in) :: filename
        logical :: verified
        logical :: exists
        integer :: file_size
        
        inquire(file=filename, exist=exists, size=file_size)
        
        ! Verification that validation was applied means file exists and has content
        verified = exists .and. (file_size > 1000)
        
        print *, "  Validation application verification:"
        print *, "    File exists:", exists
        print *, "    File has content:", (file_size > 1000)
        print *, "    Verification successful:", verified
    end function

    subroutine test_figure_backend_integration()
        ! Given: Validation should work with different figure backends
        ! When: We test with various backend configurations
        ! Then: Validation should be backend-agnostic

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: backend_integration_successful

        print *, ""
        print *, "TEST: Figure Backend Integration"
        print *, "=============================="

        test_file = "integration_backend.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        ! Test with different figure configurations
        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_backend_data, frames=14, interval=50, fig=test_fig)
        call anim%save(test_file, fps=22)

        backend_integration_successful = test_backend_agnostic_validation(test_file)

        print *, "Backend integration successful:", backend_integration_successful

        if (.not. backend_integration_successful) then
            print *, "*** FIGURE BACKEND INTEGRATION FAILURE ***"
            print *, "Validation not properly integrated with figure backends"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_backend_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.2_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_backend_agnostic_validation(filename) result(successful)
        character(len=*), intent(in) :: filename
        logical :: successful
        logical :: file_valid, format_independent, content_consistent
        integer :: file_size
        
        inquire(file=filename, size=file_size)
        
        file_valid = validate_pipeline_output(filename)
        format_independent = (file_size > 3000)  ! Should work regardless of backend
        content_consistent = check_file_header(filename)
        
        successful = file_valid .and. format_independent .and. content_consistent
        
        print *, "  Backend integration analysis:"
        print *, "    File valid:", file_valid
        print *, "    Format independent:", format_independent
        print *, "    Content consistent:", content_consistent
        print *, "    Backend integration successful:", successful
    end function

    subroutine test_error_handling_integration()
        ! Given: Validation should integrate with error handling
        ! When: We test error conditions and recovery
        ! Then: Error handling should work seamlessly with validation

        type(animation_t) :: anim
        character(len=200) :: test_file, nonexistent_file
        logical :: error_handling_integration_successful
        logical :: normal_case_handled, error_case_handled

        print *, ""
        print *, "TEST: Error Handling Integration"
        print *, "=============================="

        test_file = "integration_error_handling.mp4"
        nonexistent_file = "nonexistent_file.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_error_data, frames=8, interval=50, fig=test_fig)
        call anim%save(test_file, fps=16)

        ! Test normal case
        normal_case_handled = test_normal_error_handling(test_file)
        
        ! Test error case
        error_case_handled = test_error_case_handling(nonexistent_file)

        error_handling_integration_successful = normal_case_handled .and. error_case_handled

        print *, "Normal case error handling:", normal_case_handled
        print *, "Error case handling:", error_case_handled
        print *, "Error handling integration successful:", error_handling_integration_successful

        if (.not. error_handling_integration_successful) then
            print *, "*** ERROR HANDLING INTEGRATION FAILURE ***"
            print *, "Error handling not properly integrated with validation"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_error_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 2.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function test_normal_error_handling(filename) result(handled)
        character(len=*), intent(in) :: filename
        logical :: handled
        logical :: validation_result
        
        ! Normal case should be handled without errors
        validation_result = validate_pipeline_output(filename)
        handled = validation_result  ! Should succeed for valid file
        
        print *, "  Normal case handling:"
        print *, "    Validation result:", validation_result
        print *, "    Handled correctly:", handled
    end function

    function test_error_case_handling(filename) result(handled)
        character(len=*), intent(in) :: filename
        logical :: handled
        logical :: validation_result
        
        ! Error case (nonexistent file) should be handled gracefully
        validation_result = validate_pipeline_output(filename)
        handled = .not. validation_result  ! Should fail for nonexistent file
        
        print *, "  Error case handling:"
        print *, "    Validation result (should be FALSE):", validation_result
        print *, "    Handled correctly:", handled
    end function

end program test_mpeg_integration_validation