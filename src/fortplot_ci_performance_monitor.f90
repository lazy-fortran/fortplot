module fortplot_ci_performance_monitor
    !! CI Performance Monitoring and Regression Detection
    !!
    !! This module tracks CI execution times and detects performance regressions
    !! to ensure Windows CI tests complete within acceptable time limits (Issue #188).
    
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private
    
    public :: ci_performance_monitor_t
    public :: performance_metric_t
    public :: get_ci_monitor
    
    ! Performance metric for individual tests
    type :: performance_metric_t
        character(len=256) :: test_name = ""
        real(real64) :: execution_time = 0.0_real64
        real(real64) :: baseline_time = 0.0_real64
        logical :: regression_detected = .false.
        real(real64) :: timestamp = 0.0_real64
    end type performance_metric_t
    
    ! CI Performance Monitor
    type :: ci_performance_monitor_t
        type(performance_metric_t), dimension(:), allocatable :: metrics
        integer :: metric_count = 0
        integer :: max_metrics = 1000
        real(real64) :: total_execution_time = 0.0_real64
        real(real64) :: performance_threshold = 1.5_real64  ! 50% degradation triggers alert
        logical :: monitoring_enabled = .false.
        character(len=256) :: baseline_file = "ci_performance_baseline.dat"
    contains
        procedure :: initialize => monitor_initialize
        procedure :: start_test => monitor_start_test
        procedure :: end_test => monitor_end_test
        procedure :: check_performance_regression => monitor_check_regression
        procedure :: generate_performance_report => monitor_generate_report
        procedure :: save_baseline => monitor_save_baseline
        procedure :: load_baseline => monitor_load_baseline
        procedure :: get_test_time => monitor_get_test_time
        procedure :: get_total_time => monitor_get_total_time
    end type ci_performance_monitor_t
    
    ! Global monitor instance
    type(ci_performance_monitor_t), save, target :: global_monitor
    logical, save :: monitor_initialized = .false.
    
    ! Active test tracking
    character(len=256), save :: active_test_name = ""
    real(real64), save :: active_test_start = 0.0_real64
    
contains
    
    subroutine monitor_initialize(this, enable_monitoring, threshold)
        !! Initialize performance monitor
        class(ci_performance_monitor_t), intent(inout) :: this
        logical, intent(in), optional :: enable_monitoring
        real(real64), intent(in), optional :: threshold
        
        character(len=256) :: env_value
        integer :: stat
        
        ! Check environment for monitoring settings
        call get_environment_variable("CI_PERF_MONITORING", env_value, status=stat)
        if (stat == 0 .and. (trim(env_value) == "1" .or. trim(env_value) == "true")) then
            this%monitoring_enabled = .true.
        else if (present(enable_monitoring)) then
            this%monitoring_enabled = enable_monitoring
        end if
        
        if (present(threshold)) then
            this%performance_threshold = threshold
        end if
        
        if (allocated(this%metrics)) deallocate(this%metrics)
        allocate(this%metrics(this%max_metrics))
        
        this%metric_count = 0
        this%total_execution_time = 0.0_real64
        
        ! Try to load baseline if it exists
        call this%load_baseline()
        
    end subroutine monitor_initialize
    
    subroutine monitor_start_test(this, test_name)
        !! Start timing a test
        class(ci_performance_monitor_t), intent(inout) :: this
        character(len=*), intent(in) :: test_name
        
        if (.not. this%monitoring_enabled) return
        
        active_test_name = test_name
        call cpu_time(active_test_start)
        
    end subroutine monitor_start_test
    
    subroutine monitor_end_test(this, test_name)
        !! End timing a test and record metric
        class(ci_performance_monitor_t), intent(inout) :: this
        character(len=*), intent(in), optional :: test_name
        
        real(real64) :: end_time, execution_time
        character(len=256) :: actual_test_name
        integer :: idx
        
        if (.not. this%monitoring_enabled) return
        
        if (present(test_name)) then
            actual_test_name = test_name
        else
            actual_test_name = active_test_name
        end if
        
        if (len_trim(actual_test_name) == 0) return
        
        call cpu_time(end_time)
        execution_time = end_time - active_test_start
        
        ! Find or create metric entry
        idx = 0
        do idx = 1, this%metric_count
            if (this%metrics(idx)%test_name == actual_test_name) exit
        end do
        
        if (idx > this%metric_count) then
            if (this%metric_count >= this%max_metrics) return
            this%metric_count = this%metric_count + 1
            idx = this%metric_count
            this%metrics(idx)%test_name = actual_test_name
            this%metrics(idx)%baseline_time = execution_time  ! First run becomes baseline
        end if
        
        this%metrics(idx)%execution_time = execution_time
        call cpu_time(this%metrics(idx)%timestamp)
        
        ! Check for regression
        if (this%metrics(idx)%baseline_time > 0.0_real64) then
            if (execution_time > this%metrics(idx)%baseline_time * this%performance_threshold) then
                this%metrics(idx)%regression_detected = .true.
            else
                this%metrics(idx)%regression_detected = .false.
            end if
        end if
        
        this%total_execution_time = this%total_execution_time + execution_time
        
        ! Clear active test
        active_test_name = ""
        active_test_start = 0.0_real64
        
    end subroutine monitor_end_test
    
    function monitor_check_regression(this, test_name) result(regression_detected)
        !! Check if a specific test has performance regression
        class(ci_performance_monitor_t), intent(in) :: this
        character(len=*), intent(in), optional :: test_name
        logical :: regression_detected
        
        integer :: i
        
        regression_detected = .false.
        
        if (.not. this%monitoring_enabled) return
        
        if (present(test_name)) then
            ! Check specific test
            do i = 1, this%metric_count
                if (this%metrics(i)%test_name == test_name) then
                    regression_detected = this%metrics(i)%regression_detected
                    return
                end if
            end do
        else
            ! Check any test
            do i = 1, this%metric_count
                if (this%metrics(i)%regression_detected) then
                    regression_detected = .true.
                    return
                end if
            end do
        end if
        
    end function monitor_check_regression
    
    subroutine monitor_generate_report(this, unit)
        !! Generate performance report
        class(ci_performance_monitor_t), intent(in) :: this
        integer, intent(in), optional :: unit
        
        integer :: output_unit, i
        real(real64) :: speedup
        
        if (.not. this%monitoring_enabled) return
        
        if (present(unit)) then
            output_unit = unit
        else
            output_unit = 6  ! stdout
        end if
        
        write(output_unit, '(A)') "=== CI PERFORMANCE REPORT ==="
        write(output_unit, '(A, F10.3, A)') "Total execution time: ", &
            this%total_execution_time, " seconds"
        write(output_unit, '(A, I0)') "Tests monitored: ", this%metric_count
        write(output_unit, '(A, F5.1, A)') "Regression threshold: ", &
            (this%performance_threshold - 1.0_real64) * 100.0_real64, "%"
        write(output_unit, '(A)') ""
        
        if (this%metric_count > 0) then
            write(output_unit, '(A)') "Test Performance Details:"
            write(output_unit, '(A)') "----------------------------------------"
            
            do i = 1, this%metric_count
                write(output_unit, '(A, A)') "Test: ", trim(this%metrics(i)%test_name)
                write(output_unit, '(A, F10.3, A)') "  Execution time: ", &
                    this%metrics(i)%execution_time, " seconds"
                
                if (this%metrics(i)%baseline_time > 0.0_real64) then
                    write(output_unit, '(A, F10.3, A)') "  Baseline time: ", &
                        this%metrics(i)%baseline_time, " seconds"
                    
                    speedup = this%metrics(i)%execution_time / this%metrics(i)%baseline_time
                    if (speedup < 1.0_real64) then
                        write(output_unit, '(A, F5.2, A)') "  Performance: ", &
                            1.0_real64 / speedup, "x faster"
                    else
                        write(output_unit, '(A, F5.2, A)') "  Performance: ", &
                            speedup, "x slower"
                    end if
                    
                    if (this%metrics(i)%regression_detected) then
                        write(output_unit, '(A)') "  ** REGRESSION DETECTED **"
                    end if
                end if
                write(output_unit, '(A)') ""
            end do
        end if
        
        write(output_unit, '(A)') "=============================="
        
    end subroutine monitor_generate_report
    
    subroutine monitor_save_baseline(this)
        !! Save current metrics as baseline
        class(ci_performance_monitor_t), intent(in) :: this
        
        integer :: unit, i, iostat
        
        if (.not. this%monitoring_enabled) return
        if (this%metric_count == 0) return
        
        open(newunit=unit, file=this%baseline_file, status='replace', &
             action='write', iostat=iostat)
        if (iostat /= 0) return
        
        write(unit, *) this%metric_count
        do i = 1, this%metric_count
            write(unit, '(A, F20.6)') trim(this%metrics(i)%test_name), &
                this%metrics(i)%execution_time
        end do
        
        close(unit)
        
    end subroutine monitor_save_baseline
    
    subroutine monitor_load_baseline(this)
        !! Load baseline metrics
        class(ci_performance_monitor_t), intent(inout) :: this
        
        integer :: unit, i, iostat, count
        character(len=256) :: test_name
        real(real64) :: baseline_time
        logical :: exists
        
        if (.not. this%monitoring_enabled) return
        
        inquire(file=this%baseline_file, exist=exists)
        if (.not. exists) return
        
        open(newunit=unit, file=this%baseline_file, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) return
        
        read(unit, *, iostat=iostat) count
        if (iostat /= 0) then
            close(unit)
            return
        end if
        
        do i = 1, count
            read(unit, '(A, F20.6)', iostat=iostat) test_name, baseline_time
            if (iostat /= 0) exit
            
            ! Store baseline for matching tests
            if (this%metric_count < this%max_metrics) then
                this%metric_count = this%metric_count + 1
                this%metrics(this%metric_count)%test_name = test_name
                this%metrics(this%metric_count)%baseline_time = baseline_time
            end if
        end do
        
        close(unit)
        
    end subroutine monitor_load_baseline
    
    function monitor_get_test_time(this, test_name) result(time)
        !! Get execution time for a specific test
        class(ci_performance_monitor_t), intent(in) :: this
        character(len=*), intent(in) :: test_name
        real(real64) :: time
        
        integer :: i
        
        time = -1.0_real64
        
        do i = 1, this%metric_count
            if (this%metrics(i)%test_name == test_name) then
                time = this%metrics(i)%execution_time
                return
            end if
        end do
        
    end function monitor_get_test_time
    
    function monitor_get_total_time(this) result(time)
        !! Get total execution time
        class(ci_performance_monitor_t), intent(in) :: this
        real(real64) :: time
        
        time = this%total_execution_time
        
    end function monitor_get_total_time
    
    function get_ci_monitor() result(monitor)
        !! Get global CI performance monitor
        type(ci_performance_monitor_t), pointer :: monitor
        
        if (.not. monitor_initialized) then
            call global_monitor%initialize()
            monitor_initialized = .true.
        end if
        
        monitor => global_monitor
        
    end function get_ci_monitor
    
end module fortplot_ci_performance_monitor