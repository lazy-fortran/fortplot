! test_rendering_comparison.f90 - Comprehensive tests for rendering comparison framework
program test_rendering_comparison
    !! Test suite for enhanced rendering comparison framework
    !! 
    !! Tests all comparison modes, performance optimizations, and regression detection
    !! capabilities to ensure comprehensive coverage of the comparison framework.
    
    use fortplot_rendering_comparison
    use fortplot_validation, only: validation_result_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Rendering Comparison Framework Tests ==="
    print *, ""
    
    ! Core comparison functionality tests
    call test_pixel_diff_comparison(all_tests_passed)
    call test_statistical_comparison(all_tests_passed) 
    call test_histogram_comparison(all_tests_passed)
    call test_perceptual_comparison(all_tests_passed)
    
    ! File validation and error handling tests
    call test_missing_file_handling(all_tests_passed)
    call test_invalid_format_handling(all_tests_passed)
    call test_pdf_comparison(all_tests_passed)
    
    ! Threshold-based comparison tests
    call test_threshold_comparison(all_tests_passed)
    call test_similarity_metrics_calculation(all_tests_passed)
    
    ! Performance optimization tests
    call test_large_image_optimization(all_tests_passed)
    call test_performance_mode_selection(all_tests_passed)
    
    ! Regression suite tests
    call test_regression_suite_execution(all_tests_passed)
    call test_batch_comparison_results(all_tests_passed)
    
    ! Difference visualization tests
    call test_diff_image_generation(all_tests_passed)
    
    print *, ""
    if (all_tests_passed) then
        print *, "✓ All rendering comparison tests PASSED!"
    else
        print *, "✗ Some rendering comparison tests FAILED!"
        error stop 1
    end if
    
contains
    
    subroutine test_pixel_diff_comparison(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: ref_file, test_file
        logical :: test_passed
        
        print *, "Testing pixel difference comparison..."
        
        ! Create test files for comparison
        ref_file = "test_reference.png"
        test_file = "test_current.png"
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        ! Test pixel difference mode
        result = compare_png_images(ref_file, test_file, comparison_mode_t(1))
        
        test_passed = .true.
        
        ! Verify result structure is populated
        if (result%similarity_score < 0.0_wp .or. result%similarity_score > 1.0_wp) then
            print *, "✗ Invalid similarity score range"
            test_passed = .false.
        end if
        
        if (result%total_pixels <= 0) then
            print *, "✗ Invalid total pixel count"
            test_passed = .false.
        end if
        
        if (result%different_pixels < 0) then
            print *, "✗ Invalid different pixel count" 
            test_passed = .false.
        end if
        
        if (result%mode_used%mode /= 1) then
            print *, "✗ Incorrect comparison mode recorded"
            test_passed = .false.
        end if
        
        ! Clean up test files
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        
        if (test_passed) then
            print *, "✓ Pixel difference comparison test passed"
        else
            print *, "✗ Pixel difference comparison test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_statistical_comparison(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: ref_file, test_file
        logical :: test_passed
        
        print *, "Testing statistical comparison..."
        
        ref_file = "test_ref_stat.png"
        test_file = "test_cur_stat.png"
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        result = compare_png_images(ref_file, test_file, comparison_mode_t(2))
        
        test_passed = .true.
        
        ! Verify statistical metrics are calculated
        if (result%mse_value < 0.0_wp) then
            print *, "✗ Invalid MSE value"
            test_passed = .false.
        end if
        
        if (result%ssim_value < 0.0_wp .or. result%ssim_value > 1.0_wp) then
            print *, "✗ Invalid SSIM value range"
            test_passed = .false.
        end if
        
        if (len_trim(result%message) == 0) then
            print *, "✗ Empty result message"
            test_passed = .false.
        end if
        
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        
        if (test_passed) then
            print *, "✓ Statistical comparison test passed"
        else
            print *, "✗ Statistical comparison test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_histogram_comparison(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: ref_file, test_file
        logical :: test_passed
        
        print *, "Testing histogram comparison..."
        
        ref_file = "test_ref_hist.png"
        test_file = "test_cur_hist.png"
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        result = compare_png_images(ref_file, test_file, comparison_mode_t(3))
        
        test_passed = .true.
        
        ! Verify histogram correlation is calculated
        if (result%histogram_correlation < -1.0_wp .or. result%histogram_correlation > 1.0_wp) then
            print *, "✗ Invalid histogram correlation range"
            test_passed = .false.
        end if
        
        if (result%mode_used%mode /= 3) then
            print *, "✗ Incorrect mode recorded for histogram comparison"
            test_passed = .false.
        end if
        
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        
        if (test_passed) then
            print *, "✓ Histogram comparison test passed"
        else
            print *, "✗ Histogram comparison test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_perceptual_comparison(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: ref_file, test_file
        logical :: test_passed
        
        print *, "Testing perceptual comparison..."
        
        ref_file = "test_ref_percept.png"
        test_file = "test_cur_percept.png"
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        result = compare_png_images(ref_file, test_file, comparison_mode_t(4))
        
        test_passed = .true.
        
        ! Verify perceptual similarity is calculated
        if (result%similarity_score < 0.0_wp .or. result%similarity_score > 1.0_wp) then
            print *, "✗ Invalid perceptual similarity score"
            test_passed = .false.
        end if
        
        if (result%mode_used%mode /= 4) then
            print *, "✗ Incorrect mode recorded for perceptual comparison"
            test_passed = .false.
        end if
        
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        
        if (test_passed) then
            print *, "✓ Perceptual comparison test passed"
        else
            print *, "✗ Perceptual comparison test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_missing_file_handling(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        logical :: test_passed
        
        print *, "Testing missing file error handling..."
        
        test_passed = .true.
        
        ! Test missing reference file
        result = compare_png_images("nonexistent_ref.png", "nonexistent_cur.png", comparison_mode_t(1))
        
        if (result%passed) then
            print *, "✗ Should fail with missing reference file"
            test_passed = .false.
        end if
        
        if (.not. index(result%message, "missing") > 0) then
            print *, "✗ Error message should mention missing file"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "✓ Missing file handling test passed"
        else
            print *, "✗ Missing file handling test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_invalid_format_handling(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: invalid_file
        logical :: test_passed
        
        print *, "Testing invalid format error handling..."
        
        test_passed = .true.
        invalid_file = "invalid_format.png"
        
        ! Create file with invalid PNG signature
        call create_invalid_png_file(invalid_file)
        
        result = compare_png_images(invalid_file, invalid_file, comparison_mode_t(1))
        
        if (result%passed) then
            print *, "✗ Should fail with invalid PNG format"
            test_passed = .false.
        end if
        
        if (.not. index(result%message, "signature") > 0) then
            print *, "✗ Error message should mention signature"
            test_passed = .false.
        end if
        
        call delete_test_file(invalid_file)
        
        if (test_passed) then
            print *, "✓ Invalid format handling test passed"
        else
            print *, "✗ Invalid format handling test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_pdf_comparison(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: ref_pdf, test_pdf
        logical :: test_passed
        
        print *, "Testing PDF file comparison..."
        
        test_passed = .true.
        ref_pdf = "test_reference.pdf"
        test_pdf = "test_current.pdf"
        
        call create_test_pdf_file(ref_pdf)
        call create_test_pdf_file(test_pdf)
        
        result = compare_pdf_files(ref_pdf, test_pdf)
        
        ! Verify PDF comparison functionality
        if (result%similarity_score < 0.0_wp .or. result%similarity_score > 1.0_wp) then
            print *, "✗ Invalid PDF similarity score"
            test_passed = .false.
        end if
        
        if (len_trim(result%message) == 0) then
            print *, "✗ Empty PDF comparison message"
            test_passed = .false.
        end if
        
        call delete_test_file(ref_pdf)
        call delete_test_file(test_pdf)
        
        if (test_passed) then
            print *, "✓ PDF comparison test passed"
        else
            print *, "✗ PDF comparison test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_threshold_comparison(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: result
        character(len=256) :: ref_file, test_file
        real(wp) :: high_threshold, low_threshold
        logical :: test_passed
        
        print *, "Testing threshold-based comparison..."
        
        test_passed = .true.
        ref_file = "test_ref_thresh.png"
        test_file = "test_cur_thresh.png"
        
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        high_threshold = 0.99_wp
        low_threshold = 0.50_wp
        
        ! Test with high threshold (should likely fail)
        result = compare_with_threshold(ref_file, test_file, high_threshold)
        
        if (result%similarity_score > high_threshold .and. .not. result%passed) then
            print *, "✗ Threshold logic error: high similarity should pass"
            test_passed = .false.
        end if
        
        ! Test with low threshold (should likely pass)
        result = compare_with_threshold(ref_file, test_file, low_threshold)
        
        if (result%similarity_score > low_threshold .and. .not. result%passed) then
            print *, "✗ Threshold logic error: should pass low threshold"
            test_passed = .false.
        end if
        
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        
        if (test_passed) then
            print *, "✓ Threshold comparison test passed"
        else
            print *, "✗ Threshold comparison test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_similarity_metrics_calculation(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t) :: metrics
        character(len=256) :: ref_file, test_file
        logical :: test_passed
        
        print *, "Testing detailed similarity metrics calculation..."
        
        test_passed = .true.
        ref_file = "test_ref_metrics.png"
        test_file = "test_cur_metrics.png"
        
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        metrics = calculate_similarity_metrics(ref_file, test_file)
        
        ! Verify comprehensive metrics are provided
        if (metrics%similarity_score < 0.0_wp .or. metrics%similarity_score > 1.0_wp) then
            print *, "✗ Invalid similarity score in metrics"
            test_passed = .false.
        end if
        
        if (len_trim(metrics%message) == 0) then
            print *, "✗ Empty metrics message"
            test_passed = .false.
        end if
        
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        
        if (test_passed) then
            print *, "✓ Similarity metrics test passed"
        else
            print *, "✗ Similarity metrics test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_large_image_optimization(all_passed)
        logical, intent(inout) :: all_passed
        logical :: should_optimize
        logical :: test_passed
        
        print *, "Testing performance optimization for large images..."
        
        test_passed = .true.
        
        ! Test small image (should not optimize)
        should_optimize = optimize_comparison_performance(800, 600, comparison_mode_t(2))
        if (should_optimize) then
            print *, "✗ Small image should not require optimization"
            test_passed = .false.
        end if
        
        ! Test large image (should optimize)
        should_optimize = optimize_comparison_performance(4000, 3000, comparison_mode_t(2))
        if (.not. should_optimize) then
            print *, "✗ Large image should require optimization"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "✓ Large image optimization test passed"
        else
            print *, "✗ Large image optimization test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_performance_mode_selection(all_passed)
        logical, intent(inout) :: all_passed
        logical :: optimize_pixel, optimize_stat, optimize_hist
        logical :: test_passed
        
        print *, "Testing performance mode selection..."
        
        test_passed = .true.
        
        ! Test different modes with same large image size
        optimize_pixel = optimize_comparison_performance(3000, 2000, comparison_mode_t(1))
        optimize_stat = optimize_comparison_performance(3000, 2000, comparison_mode_t(2))
        optimize_hist = optimize_comparison_performance(3000, 2000, comparison_mode_t(3))
        
        ! All should suggest optimization for large images
        if (.not. (optimize_pixel .and. optimize_stat .and. optimize_hist)) then
            print *, "✗ All modes should suggest optimization for large images"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "✓ Performance mode selection test passed"
        else
            print *, "✗ Performance mode selection test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_regression_suite_execution(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t), allocatable :: results(:)
        character(len=256) :: ref_dir, cur_dir
        logical :: test_passed
        
        print *, "Testing regression suite execution..."
        
        test_passed = .true.
        ref_dir = "test_reference_dir"
        cur_dir = "test_current_dir"
        
        ! Create test directories
        call create_test_directory(ref_dir)
        call create_test_directory(cur_dir)
        
        call run_regression_suite(ref_dir, cur_dir, results)
        
        ! Verify results structure
        if (.not. allocated(results)) then
            print *, "✗ Regression suite should return allocated results"
            test_passed = .false.
        else
            if (size(results) == 0) then
                print *, "✗ Regression suite should return non-empty results"
                test_passed = .false.
            end if
            
            if (len_trim(results(1)%message) == 0) then
                print *, "✗ Regression suite should provide summary message"
                test_passed = .false.
            end if
        end if
        
        ! Clean up test directories
        call cleanup_test_directory(ref_dir)
        call cleanup_test_directory(cur_dir)
        
        if (test_passed) then
            print *, "✓ Regression suite execution test passed"
        else
            print *, "✗ Regression suite execution test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_batch_comparison_results(all_passed)
        logical, intent(inout) :: all_passed
        type(comparison_result_t), allocatable :: results(:)
        character(len=256) :: ref_dir, cur_dir
        integer :: i, passed_count
        logical :: test_passed
        
        print *, "Testing batch comparison result aggregation..."
        
        test_passed = .true.
        ref_dir = "test_batch_ref"
        cur_dir = "test_batch_cur"
        
        call create_test_directory(ref_dir)
        call create_test_directory(cur_dir)
        
        call run_regression_suite(ref_dir, cur_dir, results)
        
        if (allocated(results) .and. size(results) > 1) then
            ! Count passed tests
            passed_count = 0
            do i = 1, size(results)
                if (results(i)%passed) passed_count = passed_count + 1
            end do
            
            ! Verify summary includes pass/fail counts
            if (.not. index(results(1)%message, "passed") > 0) then
                print *, "✗ Summary should include pass count"
                test_passed = .false.
            end if
        end if
        
        call cleanup_test_directory(ref_dir)
        call cleanup_test_directory(cur_dir)
        
        if (test_passed) then
            print *, "✓ Batch comparison results test passed"
        else
            print *, "✗ Batch comparison results test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    subroutine test_diff_image_generation(all_passed)
        logical, intent(inout) :: all_passed
        character(len=256) :: ref_file, test_file, diff_file
        logical :: diff_exists, test_passed
        
        print *, "Testing difference image generation..."
        
        test_passed = .true.
        ref_file = "test_ref_diff.png"
        test_file = "test_cur_diff.png"
        diff_file = "test_diff_output.png"
        
        call create_test_png_file(ref_file)
        call create_test_png_file(test_file)
        
        call generate_diff_image(ref_file, test_file, diff_file)
        
        ! Verify diff image was created
        inquire(file=diff_file, exist=diff_exists)
        if (.not. diff_exists) then
            print *, "✗ Difference image was not generated"
            test_passed = .false.
        end if
        
        ! Clean up test files
        call delete_test_file(ref_file)
        call delete_test_file(test_file)
        call delete_test_file(diff_file)
        
        if (test_passed) then
            print *, "✓ Difference image generation test passed"
        else
            print *, "✗ Difference image generation test failed"
            all_passed = .false.
        end if
        
    end subroutine
    
    ! Helper subroutines for test setup and teardown
    
    subroutine create_test_png_file(filename)
        character(len=*), intent(in) :: filename
        character(len=8), parameter :: PNG_HEADER = achar(137) // "PNG" // achar(13) // achar(10) // achar(26) // achar(10)
        integer :: file_unit
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted')
        write(file_unit) PNG_HEADER
        write(file_unit) "PLACEHOLDER_PNG_DATA"
        close(file_unit)
    end subroutine
    
    subroutine create_invalid_png_file(filename)
        character(len=*), intent(in) :: filename
        integer :: file_unit
        
        open(newunit=file_unit, file=filename)
        write(file_unit, '(a)') "INVALID_PNG_CONTENT"
        close(file_unit)
    end subroutine
    
    subroutine create_test_pdf_file(filename)
        character(len=*), intent(in) :: filename
        integer :: file_unit
        
        open(newunit=file_unit, file=filename)
        write(file_unit, '(a)') "%PDF-1.4"
        write(file_unit, '(a)') "1 0 obj"
        write(file_unit, '(a)') "<< /Type /Catalog /Pages 2 0 R >>"
        write(file_unit, '(a)') "endobj"
        write(file_unit, '(a)') "%%EOF"
        close(file_unit)
    end subroutine
    
    subroutine delete_test_file(filename)
        character(len=*), intent(in) :: filename
        logical :: file_exists
        integer :: file_unit, ios
        
        inquire(file=filename, exist=file_exists)
        if (file_exists) then
            open(newunit=file_unit, file=filename, iostat=ios)
            if (ios == 0) then
                close(file_unit, status='delete')
            end if
        end if
    end subroutine
    
    subroutine create_test_directory(dirname)
        character(len=*), intent(in) :: dirname
        integer :: exit_code
        
        ! Create directory using system command
        call execute_command_line("mkdir -p " // trim(dirname), exitstat=exit_code)
        
        ! If mkdir fails, skip creating test files for now
        if (exit_code /= 0) then
            print *, "Warning: Could not create test directory: ", trim(dirname)
        end if
    end subroutine
    
    subroutine cleanup_test_directory(dirname)
        character(len=*), intent(in) :: dirname
        integer :: exit_code
        
        ! Clean up test directory
        call execute_command_line("rm -rf " // trim(dirname), exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "Warning: Could not clean up test directory: ", trim(dirname)
        end if
    end subroutine
    
end program test_rendering_comparison