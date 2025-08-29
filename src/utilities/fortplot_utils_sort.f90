module fortplot_utils_sort
    !! Sorting utilities for fortplot
    
    use iso_fortran_env, only: wp => real64
    implicit none
    private
    
    public :: sort_array
    
contains

    subroutine sort_array(arr)
        !! Simple bubble sort for small arrays (sufficient for boxplot quartiles)
        real(wp), intent(inout) :: arr(:)
        integer :: i, j, n
        real(wp) :: temp
        
        n = size(arr)
        do i = 1, n-1
            do j = 1, n-i
                if (arr(j) > arr(j+1)) then
                    temp = arr(j)
                    arr(j) = arr(j+1)
                    arr(j+1) = temp
                end if
            end do
        end do
    end subroutine sort_array

end module fortplot_utils_sort