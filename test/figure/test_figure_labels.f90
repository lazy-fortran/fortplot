program test_figure_labels
    use fortplot_figure, only: figure_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_figure_title_setting()
    call test_figure_xlabel_setting()
    call test_figure_ylabel_setting()
    call test_figure_labels_initialization()
    print *, "All figure label tests passed!"
    
contains

    subroutine test_figure_title_setting()
        type(figure_t) :: fig
        
        ! Act: Set figure title
        call fig%set_title("Test Plot Title")
        
        ! Assert: Title should be stored
        if (.not. allocated(fig%title)) then
            print *, "FAIL: Figure title not allocated after setting"
            stop 1
        end if
        
        if (fig%title /= "Test Plot Title") then
            print *, "FAIL: Figure title not set correctly"
            print *, "Expected: 'Test Plot Title', Got: '", fig%title, "'"
            stop 1
        end if
    end subroutine test_figure_title_setting

    subroutine test_figure_xlabel_setting()
        type(figure_t) :: fig
        
        ! Act: Set X-axis label
        call fig%set_xlabel("X Axis Label")
        
        ! Assert: X-label should be stored
        if (.not. allocated(fig%xlabel)) then
            print *, "FAIL: Figure xlabel not allocated after setting"
            stop 1
        end if
        
        if (fig%xlabel /= "X Axis Label") then
            print *, "FAIL: Figure xlabel not set correctly"
            print *, "Expected: 'X Axis Label', Got: '", fig%xlabel, "'"
            stop 1
        end if
    end subroutine test_figure_xlabel_setting

    subroutine test_figure_ylabel_setting()
        type(figure_t) :: fig
        
        ! Act: Set Y-axis label
        call fig%set_ylabel("Y Axis Label")
        
        ! Assert: Y-label should be stored
        if (.not. allocated(fig%ylabel)) then
            print *, "FAIL: Figure ylabel not allocated after setting"
            stop 1
        end if
        
        if (fig%ylabel /= "Y Axis Label") then
            print *, "FAIL: Figure ylabel not set correctly"
            print *, "Expected: 'Y Axis Label', Got: '", fig%ylabel, "'"
            stop 1
        end if
    end subroutine test_figure_ylabel_setting

    subroutine test_figure_labels_initialization()
        type(figure_t) :: fig
        
        ! Assert: Labels should not be allocated initially
        if (allocated(fig%title)) then
            print *, "FAIL: Figure title should not be allocated initially"
            stop 1
        end if
        
        if (allocated(fig%xlabel)) then
            print *, "FAIL: Figure xlabel should not be allocated initially"
            stop 1
        end if
        
        if (allocated(fig%ylabel)) then
            print *, "FAIL: Figure ylabel should not be allocated initially"
            stop 1
        end if
    end subroutine test_figure_labels_initialization

end program test_figure_labels