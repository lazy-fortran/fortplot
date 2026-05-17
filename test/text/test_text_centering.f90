program test_text_centering
    !! Test text centering calculations
    use fortplot_text, only: calculate_text_width, init_text_system
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: success
    
    ! Initialize text system first
    success = init_text_system()
    if (.not. success) then
        print *, "FAIL: Could not initialize text system"
        error stop 1
    end if
    
    call test_title_width_calculation()
    print *, "Text centering tests passed!"
    
contains

    subroutine test_title_width_calculation()
        !! Test text width calculation for centering
        character(len=*), parameter :: title = "Simple Sine Wave"
        character(len=*), parameter :: xlabel = "x"
        integer :: title_width, xlabel_width
        real(wp) :: canvas_width, centered_x
        
        canvas_width = 640.0_wp
        
        ! Test text width calculation
        title_width = calculate_text_width(title)
        xlabel_width = calculate_text_width(xlabel)
        
        print *, "Title '", title, "' width:", title_width, "pixels"
        print *, "Xlabel '", xlabel, "' width:", xlabel_width, "pixels"
        
        ! Test centering calculation
        centered_x = canvas_width / 2.0_wp - real(title_width, wp) / 2.0_wp
        print *, "Canvas center:", canvas_width / 2.0_wp
        print *, "Title centered position:", centered_x
        print *, "Should move title", real(title_width, wp) / 2.0_wp, "pixels left from center"
        
        ! Verify reasonable values
        if (title_width < 50 .or. title_width > 200) then
            print *, "WARN: Title width", title_width, "seems unreasonable"
        end if
        
        if (xlabel_width < 5 .or. xlabel_width > 20) then
            print *, "WARN: Xlabel width", xlabel_width, "seems unreasonable"
        end if
        
        print *, "PASS: Text width calculations completed"
    end subroutine test_title_width_calculation

end program test_text_centering