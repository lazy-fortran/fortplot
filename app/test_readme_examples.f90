program test_readme_examples
    ! Test examples from README to verify user experience
    use fortplot
    implicit none
    
    ! Test basic README example
    call test_stateful_api()
    
    ! Test Object-oriented API example
    call test_oo_api()
    
    ! Test convenience methods
    call test_convenience()

contains

    subroutine test_stateful_api()
        real(wp), dimension(50) :: x, y
        integer :: i
        
        print *, "Testing stateful API from README..."
        
        ! Generate sample data exactly as in README
        x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
        y = sin(x)

        call figure()
        call plot(x, y)
        call title("Function Plot")
        call xlabel("x")
        call ylabel("y")
        call xlim(0.0_wp, 10.0_wp)  
        call ylim(-1.0_wp, 1.0_wp)   
        call savefig("readme_stateful.png")
        
        print *, "Stateful API test completed"
    end subroutine

    subroutine test_oo_api()
        type(figure_t) :: fig
        real(wp), dimension(50) :: x, yf
        integer :: i
        
        print *, "Testing OO API from README..."

        ! Generate test data exactly as in README
        x = [(real(i-1, wp) * 0.1_wp, i=1, 50)]
        yf = sin(x)

        call fig%initialize()
        call fig%set_title("Function Plot")
        call fig%set_xlabel("x") 
        call fig%set_ylabel("y")
        call fig%add_plot(x, yf)
        call fig%savefig("readme_oo.png")
        
        print *, "OO API test completed"
    end subroutine

    subroutine test_convenience()
        real(wp), dimension(10) :: x, y, yerr
        integer :: i
        
        print *, "Testing convenience methods..."
        
        x = [(real(i, wp), i=1, 10)]
        y = x**2
        yerr = 0.1_wp * y
        
        ! Test errorbar (claimed to be working in README)  
        call figure(figsize=[800.0_wp, 600.0_wp])
        call errorbar(x, y, yerr=yerr, marker='o', label='Test data')
        call legend()
        call savefig("readme_errorbar.png")
        
        print *, "Convenience methods test completed"
    end subroutine

end program test_readme_examples