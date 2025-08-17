program test_legend
    !! Unit tests for legend functionality following TDD approach
    !! Tests both API design and rendering across backends
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_legend_api()
    call test_legend_positioning()
    call test_legend_rendering()
    print *, "All legend tests passed!"
    
contains

    subroutine test_legend_api()
        !! Test legend API design and data storage
        type(figure_t) :: fig
        real(wp), dimension(10) :: x, y1, y2
        integer :: i
        
        ! Generate test data
        x = [(real(i, wp), i=1, 10)]
        y1 = sin(x)
        y2 = cos(x)
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y1, label="sin(x)")
        call fig%add_plot(x, y2, label="cos(x)")
        
        ! Test legend API - this should exist and work
        call fig%legend()
        
        ! Test legend with custom location
        call fig%legend(location="upper right")
        
        ! Save to test rendering
        call fig%savefig('output/test/test_legend/test_legend_basic.png')
        call fig%savefig('/tmp/test/test_legend_basic.png')
        
        print *, "PASS: Legend API tests completed"
    end subroutine test_legend_api

    subroutine test_legend_positioning()
        !! Test legend positioning options
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        x = [(real(i, wp), i=1, 5)]
        y = x**2
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y, label="x²")
        
        ! Test different legend positions
        call fig%legend(location="upper left")
        call fig%savefig('output/test/test_legend/test_legend_upper_left.png')
        
        call fig%legend(location="lower right")  
        call fig%savefig('output/test/test_legend/test_legend_lower_right.png')
        call fig%savefig('/tmp/test/test_legend_upper_left.png')
        
        call fig%legend(location="lower right")  
        call fig%savefig('/tmp/test/test_legend_lower_right.png')
        
        print *, "PASS: Legend positioning tests completed"
    end subroutine test_legend_positioning
    
    subroutine test_legend_rendering()
        !! Test legend rendering across all backends
        type(figure_t) :: fig
        real(wp), dimension(8) :: x, y1, y2, y3
        integer :: i
        
        x = [(real(i, wp) * 0.5_wp, i=1, 8)]
        y1 = exp(-x)
        y2 = x * 0.5_wp
        y3 = sin(x * 2.0_wp)
        
        call fig%initialize(640, 480)
        call fig%set_title("Legend Rendering Test")
        call fig%add_plot(x, y1, label="exp(-x)")
        call fig%add_plot(x, y2, label="0.5x")  
        call fig%add_plot(x, y3, label="sin(2x)")
        
        call fig%legend()
        
        ! Test all backends render legends correctly
        call fig%savefig('output/test/test_legend/test_legend_render.png')
        call fig%savefig('output/test/test_legend/test_legend_render.pdf')
        call fig%savefig('output/test/test_legend/test_legend_render.txt')
        call fig%savefig('/tmp/test/test_legend_render.png')
        call fig%savefig('/tmp/test/test_legend_render.pdf')
        call fig%savefig('/tmp/test/test_legend_render.txt')
        
        print *, "PASS: Legend rendering tests completed"
    end subroutine test_legend_rendering

end program test_legend