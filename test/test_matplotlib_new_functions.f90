program test_matplotlib_new_functions
    !! Test the new matplotlib-compatible functions
    use fortplot
    use fortplot_plot_data, only: AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_matplotlib_session, only: get_global_figure
    use iso_fortran_env, only: real64
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(real64), allocatable :: x(:), y(:), z(:,:)
    real(real64), allocatable :: theta(:), r(:)
    real(real64), allocatable :: values(:)
    real(real64), allocatable :: y_secondary(:), y_top(:)
    class(figure_t), pointer :: fig_ptr
    logical, allocatable :: mask(:)
    character(len=:), allocatable :: output_dir
    integer :: i, j

    call ensure_test_output_dir('matplotlib_new_functions', output_dir)

    print *, "Testing new matplotlib-compatible functions..."
    
    ! Test imshow
    allocate(z(50, 50))
    do i = 1, 50
        do j = 1, 50
            z(i, j) = sin(real(i, real64) / 5.0) * cos(real(j, real64) / 5.0)
        end do
    end do
    
    call figure()
    call imshow(z)
    call title("imshow() test - 2D heatmap")
    call savefig(trim(output_dir)//'test_imshow.png')
    print *, "✓ imshow() function works"
    
    ! Test pie chart
    allocate(values(5))
    values = [30.0_real64, 25.0_real64, 20.0_real64, 15.0_real64, 10.0_real64]
    
    call figure()
    call pie(values)
    call title("pie() test - Pie chart")
    call savefig(trim(output_dir)//'test_pie.png')
    print *, "✓ pie() function works"
    
    ! Test polar plot
    allocate(theta(100), r(100))
    do i = 1, 100
        theta(i) = 2.0_real64 * 3.14159_real64 * real(i-1, real64) / 99.0_real64
        r(i) = 1.0_real64 + 0.5_real64 * sin(5.0_real64 * theta(i))
    end do
    
    call figure()
    call polar(theta, r)
    call title("polar() test - Polar plot")
    call savefig(trim(output_dir)//'test_polar.png')
    print *, "✓ polar() function works"
    
    ! Test step plot
    allocate(x(20), y(20))
    do i = 1, 20
        x(i) = real(i, real64)
        y(i) = sin(real(i, real64) / 3.0_real64)
    end do
    
    call figure()
    call step(x, y)
    call title("step() test - Step plot")
    call xlabel("x")
    call ylabel("y")
    call savefig(trim(output_dir)//'test_step.png')
    print *, "✓ step() function works"
    
    ! Test stem plot
    call figure()
    call stem(x, y)
    call title("stem() test - Stem plot")
    call xlabel("x")
    call ylabel("y")
    call savefig(trim(output_dir)//'test_stem.png')
    print *, "✓ stem() function works"
    
    ! Test fill
    call figure()
    call plot(x, y)
    call fill(x, y)
    call title("fill() test - Area under curve")
    call xlabel("x")
    call ylabel("y")
    call savefig(trim(output_dir)//'test_fill.png')
    print *, "✓ fill() function works"
    
    ! Test fill_between
    deallocate(x, y)
    allocate(x(50), y(50))
    do i = 1, 50
        x(i) = real(i-25, real64) / 5.0_real64
        y(i) = x(i)**2
    end do
    
    call figure()
    call fill_between(x, y1=y, y2=y*0.5_real64)
    call title("fill_between() test - Area between curves")
    call xlabel("x")
    call ylabel("y")
    call savefig(trim(output_dir)//'test_fill_between.png')
    print *, "✓ fill_between() function works"

    allocate(mask(size(x)))
    mask = .false.
    mask(10:30) = .true.

    call figure()
    call plot(x, y)
    call fill_between(x, y1=y, y2=y*0.2_real64, where=mask)
    call title("fill_between() mask test - Partial area fill")
    call savefig(trim(output_dir)//'test_fill_between_masked.png')
    print *, "✓ fill_between() accepts where masks"

    deallocate(mask)
    
    ! Test twinx and twiny with independent scaling and labels
    allocate(y_secondary(size(x)), y_top(size(x)))
    do i = 1, size(x)
        y_secondary(i) = 12.0_real64 + 0.4_real64 * x(i)
        y_top(i) = log(1.0_real64 + real(i, real64))
    end do

    call figure()
    call plot(x, y, label="primary axis")
    call ylabel("Primary amplitude")

    call twinx()
    call plot(x, y_secondary, label="secondary axis")
    call ylabel("Secondary amplitude")
    call set_yscale("log")

    call twiny()
    call plot(y_top, y, label="top axis")
    call set_xscale("log")
    call xlabel("Log-scaled index")

    call use_axis("primary")
    call xlabel("Sample index")
    call title("Twin axis verification")
    call legend()
    call savefig(trim(output_dir)//'test_twin_axes.png')

    fig_ptr => get_global_figure()
    if (.not. associated(fig_ptr)) then
        print *, "ERROR: global figure pointer not associated for twin axis test"
        stop 1
    end if
    if (fig_ptr%plot_count /= 3) then
        print *, "ERROR: expected 3 plots for twin axis test, got", fig_ptr%plot_count
        stop 1
    end if
    if (.not. fig_ptr%state%has_twinx) then
        print *, "ERROR: twinx() did not update state%%has_twinx"
        stop 1
    end if
    if (.not. fig_ptr%state%has_twiny) then
        print *, "ERROR: twiny() did not update state%%has_twiny"
        stop 1
    end if
    if (fig_ptr%plots(1)%axis /= AXIS_PRIMARY) then
        print *, "ERROR: primary plot not tagged with AXIS_PRIMARY"
        stop 1
    end if
    if (fig_ptr%plots(2)%axis /= AXIS_TWINX) then
        print *, "ERROR: second plot not tagged with AXIS_TWINX"
        stop 1
    end if
    if (fig_ptr%plots(3)%axis /= AXIS_TWINY) then
        print *, "ERROR: third plot not tagged with AXIS_TWINY"
        stop 1
    end if
    if (trim(fig_ptr%state%yscale) /= 'linear') then
        print *, "ERROR: primary axis scale modified unexpectedly"
        stop 1
    end if
    if (trim(fig_ptr%state%twinx_yscale) /= 'log') then
        print *, "ERROR: twinx axis scale not captured as log"
        stop 1
    end if
    if (trim(fig_ptr%state%twiny_xscale) /= 'log') then
        print *, "ERROR: twiny axis scale not captured as log"
        stop 1
    end if
    if (.not. allocated(fig_ptr%state%twinx_ylabel)) then
        print *, "ERROR: twinx ylabel not stored"
        stop 1
    end if
    if (.not. allocated(fig_ptr%state%twiny_xlabel)) then
        print *, "ERROR: twiny xlabel not stored"
        stop 1
    end if
    print *, "✓ twinx() and twiny() render independent axes"
    
    print *, ""
    print *, "All new matplotlib-compatible functions are accessible!"
    print *, "Test images saved under ", trim(output_dir)
    
    deallocate(x, y, z, theta, r, values, y_secondary, y_top)
    
end program test_matplotlib_new_functions
