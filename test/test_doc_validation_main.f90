program test_doc_validation_main
    !! Documentation validation test - test all examples from windows_ci_performance.md
    use fortplot_windows_performance
    use fortplot_ci_performance_monitor
    use fortplot_memory_backend
    use iso_fortran_env, only: real64
    implicit none
    
    type(ci_performance_monitor_t), pointer :: monitor
    type(memory_backend_t), pointer :: backend
    type(performance_config_t) :: config
    logical :: in_ci
    integer :: buffer_count
    real(real64) :: total_memory
    
    print *, "=== DOCUMENTATION VALIDATION TEST ==="
    
    ! Test Example 1: Check performance status (from Quick Start section)
    if (should_use_memory_backend()) then
        print *, "Memory backend optimization: ACTIVE"
    else
        print *, "Memory backend optimization: INACTIVE"
    end if
    
    ! Test Example 2: Get CI monitor and generate report
    monitor => get_ci_monitor()
    call monitor%initialize()
    call monitor%generate_performance_report()
    
    ! Test Example 3: Check if running in CI
    in_ci = is_ci_environment()
    print *, "Running in CI:", in_ci
    
    ! Test Example 4: Get optimal performance configuration
    config = get_performance_config()
    print *, "Performance config - Use memory backend:", config%use_memory_backend
    print *, "Performance config - Batch IO:", config%batch_file_operations
    
    ! Test Example 5: Manual memory backend control
    backend => get_memory_backend()
    call backend%initialize(max_buffers=500)
    
    ! Test Example 6: Get memory usage statistics
    call backend%get_stats(buffer_count, total_memory)
    print *, "Buffers: ", buffer_count, ", Memory: ", total_memory/1024/1024, " MB"
    
    ! Test Example 7: Performance monitoring test
    call monitor%start_test("test_doc_validation")
    ! Simulate some work with a small loop
    real(real64) :: dummy_work
    integer :: i
    dummy_work = 0.0_real64
    do i = 1, 1000
        dummy_work = dummy_work + real(i, real64)
    end do
    call monitor%end_test("test_doc_validation") 
    
    ! Test Example 8: Check performance regression
    logical :: regression_detected
    regression_detected = monitor%check_performance_regression("test_doc_validation")
    print *, "Regression detected:", regression_detected
    
    ! Test Example 9: Get execution time
    real(real64) :: test_time
    test_time = monitor%get_test_time("test_doc_validation")
    print *, "Test execution time: ", test_time, " seconds"
    
    print *, "=== DOCUMENTATION VALIDATION COMPLETE ==="
    
end program test_doc_validation_main