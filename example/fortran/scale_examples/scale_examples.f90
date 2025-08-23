program scale_examples
    !! Examples demonstrating logarithmic and symlog scales
    use fortplot
    implicit none

    call log_scale_demo()
    call symlog_scale_demo()

contains

    subroutine log_scale_demo()
        real(wp), dimension(50) :: x_exp, y_exp
        integer :: i
        
        print *, "=== Scale Examples ==="
        
        ! Generate exponential data for log scale
        x_exp = [(real(i, wp), i=1, 50)]
        y_exp = exp(x_exp * 0.2_wp)
        
        call figure()
        call plot(x_exp, y_exp)
        call set_yscale('log')
        call title('Log Scale Example')
        call xlabel('x')
        call ylabel('exp(0.2x)')
        call savefig('output/example/fortran/scale_examples/log_scale.png')
        call savefig('output/example/fortran/scale_examples/log_scale.pdf')
        call savefig('output/example/fortran/scale_examples/log_scale.txt')
        
        print *, "Created: log_scale.png/pdf/txt"
        
    end subroutine log_scale_demo

    subroutine symlog_scale_demo()
        real(wp), dimension(50) :: x_exp, y_symlog
        integer :: i
        
        ! Generate data that goes through zero for symlog
        x_exp = [(real(i, wp), i=1, 50)]
        y_symlog = x_exp**3 - 50.0_wp * x_exp
        
        call figure()
        call plot(x_exp, y_symlog)
        call set_yscale('symlog')  ! threshold parameter not supported yet
        call title('Symlog Scale Example')
        call xlabel('x') 
        call ylabel('x³ - 50x')
        call savefig('output/example/fortran/scale_examples/symlog_scale.png')
        call savefig('output/example/fortran/scale_examples/symlog_scale.pdf')
        call savefig('output/example/fortran/scale_examples/symlog_scale.txt')
        
        print *, "Created: symlog_scale.png/pdf/txt"
        
    end subroutine symlog_scale_demo

end program scale_examples