program debug_marker_size
    !! Debug marker size and position
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(1), y(1), z(1)
    
    ! Single point at center
    x = [0.5_wp]
    y = [0.5_wp]
    z = [0.5_wp]
    
    call fig%initialize(800, 600)
    call fig%add_scatter(x, y, z, label="Center point", marker='o')
    call fig%set_title('Single center point')
    call fig%legend()
    call fig%savefig('output/debug_marker_size.png')
    
    print *, "Single point test saved"
end program debug_marker_size