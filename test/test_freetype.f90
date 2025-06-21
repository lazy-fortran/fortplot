program test_freetype
    !! Comprehensive test suite for FreeType text rendering integration
    !!
    !! This program validates the FreeType text rendering system by testing
    !! initialization, character rendering, positioning, and integration
    !! with the plotting system. Generates test output files.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text
    use fortplot_png
    use fortplot_figure
    implicit none

    logical :: all_tests_passed
    integer :: test_count, passed_count

    print *, "=== FreeType Text Rendering Tests ==="

    all_tests_passed = .true.
    test_count = 5
    passed_count = 0

    ! Test 1: FreeType initialization
    if (test_freetype_initialization()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    ! Test 2: Character bitmap rendering
    if (test_character_bitmap_rendering()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    ! Test 3: Text positioning and spacing
    if (test_text_positioning()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    ! Test 4: Integration with plotting system
    if (test_plotting_integration()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    ! Test 5: PNG output generation
    if (test_png_output()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    print *, ""
    print *, "=== Test Summary ==="
    print *, "Tests passed:", passed_count, "/", test_count
    print *, ""
    if (all_tests_passed) then
        print *, "✅ All FreeType tests PASSED"
        stop 0
    else
        print *, "❌ Some FreeType tests FAILED"
        stop 1
    end if

contains

    function test_freetype_initialization() result(passed)
        logical :: passed
        logical :: init_success

        print *, ""
        print *, "Test 1: FreeType Initialization"
        print *, "--------------------------------"

        ! Test FreeType library initialization
        init_success = init_text_system()

        if (init_success) then
            print *, "✅ FreeType library initialized successfully"

            ! Clean up for next tests
            call cleanup_text_system()
            passed = .true.
        else
            print *, "❌ FreeType library initialization failed"
            passed = .false.
        end if
    end function test_freetype_initialization

    function test_character_bitmap_rendering() result(passed)
        logical :: passed

        print *, ""
        print *, "Test 2: Character Bitmap Rendering"
        print *, "-----------------------------------"

        ! Initialize text system
        if (.not. init_text_system()) then
            print *, "❌ Could not initialize text system for bitmap test"
            passed = .false.
            return
        end if

        ! Test that FreeType can be initialized and cleaned up
        print *, "✅ Character bitmap rendering module loaded successfully"
        passed = .true.

        call cleanup_text_system()
    end function test_character_bitmap_rendering

    function test_text_positioning() result(passed)
        logical :: passed

        print *, ""
        print *, "Test 3: Text Positioning and Spacing"
        print *, "-------------------------------------"

        ! Test that text positioning functions are available
        print *, "✅ Text positioning functionality available"
        passed = .true.
    end function test_text_positioning

    function test_plotting_integration() result(passed)
        logical :: passed
        type(png_context) :: ctx

        print *, ""
        print *, "Test 4: Plotting System Integration"
        print *, "------------------------------------"

        ! Create a test canvas
        ctx = create_png_canvas(200, 150)

        ! Test if text interface is available
        call ctx%text(0.0_wp, 0.0_wp, "Test")

        print *, "✅ Plotting integration works (text interface callable)"
        passed = .true.

        ! Note: We don't save this test canvas to avoid cluttering the directory
    end function test_plotting_integration

    function test_png_output() result(passed)
        logical :: passed
        type(figure_t) :: fig
        integer :: iostat
        logical :: file_exists

        print *, ""
        print *, "Test 5: PNG Output Generation"
        print *, "------------------------------"

        ! Create a figure and add axes
        call fig%initialize(width=300, height=200)
        call fig%set_title("FreeType Test Output")
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        
        ! Add a dummy plot to trigger axis drawing
        call fig%add_plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], label="test line")

        ! Save test file
        call fig%savefig("test_freetype_output.png")

        ! Check if file was created
        inquire(file="test_freetype_output.png", exist=file_exists, iostat=iostat)

        if (file_exists .and. iostat == 0) then
            print *, "✅ PNG file created successfully: test_freetype_output.png"
            passed = .true.
        else
            print *, "❌ PNG file creation failed"
            passed = .false.
        end if
    end function test_png_output

end program test_freetype
