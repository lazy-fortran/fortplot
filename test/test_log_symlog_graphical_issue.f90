program test_log_symlog_graphical_issue
    !! Test to reproduce Issue #156: log and symlog giving only horizontal lines in PNG/PDF
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== Testing Log and Symlog Scale Issue ==="
    
    call test_log_scale_graphical()
    call test_symlog_scale_graphical()
    
    print *, "Log/Symlog scale test completed!"
    print *, "CHECK: Generated PNG/PDF files should show proper curves, not horizontal lines"
    
contains

    subroutine test_log_scale_graphical()
        !! Test logarithmic scale in PNG and PDF outputs
        type(figure_t) :: fig
        real(wp), dimension(20) :: x, y
        integer :: i
        
        print *, "Testing log scale graphical output..."
        
        ! Generate exponential data - should show as straight line on log scale
        do i = 1, 20
            x(i) = real(i, wp)
            y(i) = 10.0_wp * exp(x(i) * 0.2_wp)
        end do
        
        ! Test PNG output
        call fig%initialize(800, 600)
        call fig%add_plot(x, y, label="10*exp(0.2x)")
        call fig%set_yscale('log')
        call fig%set_title("Log Scale Test (PNG) - Should show straight line")
        call fig%set_xlabel("x")
        call fig%set_ylabel("10*exp(0.2x)")
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('build/test/log_scale_test.png'))
        
        ! Test PDF output  
        call fig%initialize(800, 600)
        call fig%add_plot(x, y, label="10*exp(0.2x)")
        call fig%set_yscale('log')
        call fig%set_title("Log Scale Test (PDF) - Should show straight line")
        call fig%set_xlabel("x")
        call fig%set_ylabel("10*exp(0.2x)")
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('build/test/log_scale_test.pdf'))
        
        print *, "Created: log_scale_test.png/pdf"
    end subroutine test_log_scale_graphical

    subroutine test_symlog_scale_graphical()
        !! Test symmetric log scale in PNG and PDF outputs
        type(figure_t) :: fig
        real(wp), dimension(40) :: x, y
        integer :: i
        
        print *, "Testing symlog scale graphical output..."
        
        ! Generate data that goes through zero with large dynamic range
        do i = 1, 40
            x(i) = real(i - 20, wp)  ! Range: -19 to 20
            y(i) = sign(x(i)**2, x(i)) * 100.0_wp  ! x² with sign of x
        end do
        
        ! Test PNG output
        call fig%initialize(800, 600)  
        call fig%add_plot(x, y, label="sign(x)*x²*100")
        call fig%set_yscale('symlog', 10.0_wp)
        call fig%set_title("Symlog Scale Test (PNG) - Should show S-curve")
        call fig%set_xlabel("x")
        call fig%set_ylabel("sign(x)*x²*100")
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('build/test/symlog_scale_test.png'))
        
        ! Test PDF output
        call fig%initialize(800, 600)
        call fig%add_plot(x, y, label="sign(x)*x²*100")
        call fig%set_yscale('symlog', 10.0_wp)
        call fig%set_title("Symlog Scale Test (PDF) - Should show S-curve")
        call fig%set_xlabel("x")
        call fig%set_ylabel("sign(x)*x²*100")
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('build/test/symlog_scale_test.pdf'))
        
        print *, "Created: symlog_scale_test.png/pdf"
    end subroutine test_symlog_scale_graphical

end program test_log_symlog_graphical_issue