program test_colorbar_location_contract
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none
    character(len=6), parameter :: locations(4) = &
        [character(len=6) :: 'bottom', 'top', 'left', 'right']
    character(len=6), parameter :: styles(3) = &
        [character(len=6) :: 'line', 'filled', 'mesh']
    character(len=*), parameter :: out = &
        'build/test/output/fortplot_test_colorbar_location_contract/'
    real(wp), parameter :: levels(5) = [1.0_wp, 3.0_wp, 5.0_wp, 8.0_wp, 11.0_wp]
    real(wp) :: x(31), y(19), z(19, 31)
    real(wp) :: x_ticks(2), y_ticks(2), bar_ticks(1)
    character(len=1) :: axis_labels(2), bar_labels(1)
    integer :: i, j, location_index, style_index
    logical :: ok
    type(figure_t) :: fig

    call create_directory_runtime(out, ok)
    if (.not. ok) error stop 'Cannot create colorbar location artifact directory'
    x_ticks = [0.0_wp, 2.0_wp]
    y_ticks = [0.0_wp, 1.0_wp]
    bar_ticks = [1.0_wp]
    axis_labels = ' '
    bar_labels = ' '
    x = [(2.0_wp*real(i - 1, wp)/30.0_wp, i=1, size(x))]
    y = [(real(j - 1, wp)/18.0_wp, j=1, size(y))]
    do i = 1, size(x)
        do j = 1, size(y)
            z(j, i) = x(i) + 10.0_wp*y(j)
        end do
    end do
    do location_index = 1, size(locations)
        do style_index = 1, size(styles)
            call initialize(640, 480)
            select case (styles(style_index))
            case ('line')
                call fig%add_contour(x, y, z, levels)
            case ('filled')
                call fig%add_contour_filled(x, y, z, levels, show_colorbar=.false.)
            case ('mesh')
                call fig%add_pcolormesh(x, y, z, cmap='viridis')
            end select
            call fig%colorbar(location=trim(locations(location_index)), &
                ticks=bar_ticks, ticklabels=bar_labels)
            call save(trim(styles(style_index))//'_'//trim(locations(location_index)))
        end do
    end do
    do location_index = 1, 2
        do style_index = 1, 2
            call initialize(700, 420)
            if (style_index == 1) then
                call fig%add_contour(x, y, z, levels)
            else
                call fig%add_contour_filled(x, y, z, levels, show_colorbar=.false.)
            end if
            call fig%colorbar(location=trim(locations(location_index)), shrink=0.63_wp, &
                label='SCALAR', ticks=bar_ticks, ticklabels=bar_labels)
            call save('label_'//trim(styles(style_index))//'_'// &
                trim(locations(location_index)))
        end do
    end do
    print *, 'PASS: colorbar location PNG/PDF artifacts rendered'

contains

    subroutine initialize(width, height)
        integer, intent(in) :: width, height

        call fig%initialize(width=width, height=height)
        call fig%set_xlim(0.0_wp, 2.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)
        call fig%set_xticks(x_ticks, labels=axis_labels)
        call fig%set_yticks(y_ticks, labels=axis_labels)
    end subroutine initialize

    subroutine save(name)
        character(len=*), intent(in) :: name

        call fig%savefig(out//name//'.png')
        call fig%savefig(out//name//'.pdf')
    end subroutine save

end program test_colorbar_location_contract
