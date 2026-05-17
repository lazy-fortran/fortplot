program test_categorical_axis
    !! Test categorical x-axis labels for bar charts (Issue #1458)
    use fortplot
    use fortplot_plotting_advanced, only: bar_impl
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), dimension(5) :: x, heights
    character(len=20), dimension(5) :: categories
    character(len=:), allocatable :: output_dir
    type(figure_t) :: fig
    integer :: i

    call ensure_test_output_dir('categorical_axis', output_dir)

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    heights = [23.0_wp, 45.0_wp, 56.0_wp, 32.0_wp, 18.0_wp]
    categories = ['Apples              ', 'Oranges             ', &
                  'Bananas             ', 'Grapes              ', 'Mangoes             ']

    ! Test 1: OO API - set_xticks with labels
    call fig%initialize()
    call bar_impl(fig, x, heights)
    call fig%set_xticks(x, categories)
    call fig%set_title("Categorical Bar Chart - set_xticks")
    call fig%set_xlabel("Fruit")
    call fig%set_ylabel("Sales")
    call fig%savefig(trim(output_dir)//'categorical_bar_xticks.png')
    print *, "Test 1: set_xticks with labels - PASS"

    ! Test 2: OO API - set_xtick_labels only (uses default 1..N positions)
    call fig%initialize()
    call bar_impl(fig, x, heights)
    call fig%set_xtick_labels(categories)
    call fig%set_title("Categorical Bar Chart - set_xtick_labels")
    call fig%set_xlabel("Fruit")
    call fig%set_ylabel("Sales")
    call fig%savefig(trim(output_dir)//'categorical_bar_labels_only.png')
    print *, "Test 2: set_xtick_labels only - PASS"

    ! Test 3: OO API - set_yticks with labels
    call fig%initialize()
    call bar_impl(fig, x, heights)
    call fig%set_yticks([0.0_wp, 20.0_wp, 40.0_wp, 60.0_wp], &
                        ['Zero  ', 'Low   ', 'Medium', 'High  '])
    call fig%set_title("Bar Chart - Custom Y-axis Labels")
    call fig%set_xlabel("Category")
    call fig%set_ylabel("Level")
    call fig%savefig(trim(output_dir)//'categorical_bar_yticks.png')
    print *, "Test 3: set_yticks with labels - PASS"

    ! Test 4: Stateful API equivalent using OO behind the scenes
    call fig%initialize()
    call bar_impl(fig, x, heights)
    call fig%set_xticks(x, categories)
    call fig%set_title("Fruit Sales by Category")
    call fig%set_xlabel("Fruit Type")
    call fig%set_ylabel("Units Sold")
    call fig%savefig(trim(output_dir)//'categorical_bar_full.png')
    print *, "Test 4: Full categorical bar chart - PASS"

    ! Test 5: More categories with longer labels
    block
        real(wp) :: pos(7), vals(7)
        character(len=15) :: days(7)

        pos = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp]
        vals = [120.0_wp, 95.0_wp, 140.0_wp, 155.0_wp, 180.0_wp, 150.0_wp, 100.0_wp]
        days = ['Monday         ', 'Tuesday        ', 'Wednesday      ', &
                'Thursday       ', 'Friday         ', 'Saturday       ', 'Sunday         ']

        call fig%initialize()
        call bar_impl(fig, pos, vals)
        call fig%set_xticks(pos, days)
        call fig%set_title("Weekly Sales Data")
        call fig%set_xlabel("Day of Week")
        call fig%set_ylabel("Sales ($)")
        call fig%savefig(trim(output_dir)//'categorical_weekly_sales.png')
        print *, "Test 5: Weekly sales with days - PASS"
    end block

    print *, "All categorical axis tests completed successfully"

end program test_categorical_axis
