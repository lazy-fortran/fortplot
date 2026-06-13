module fortplot_subplot_layout
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: subplot_data_t
    use fortplot_raster, only: raster_context
    use fortplot_pdf, only: pdf_context
    implicit none

    private
    public :: compute_tight_subplot_margins

    ! matplotlib default fixed fractional subplot parameters
    ! (rcParams figure.subplot.*). The axes box spans these fractions of the
    ! figure and panels are separated by wspace/hspace of the per-axes size.
    real(wp), parameter :: BASE_LEFT = 0.125_wp
    real(wp), parameter :: BASE_RIGHT = 0.90_wp
    real(wp), parameter :: BASE_BOTTOM = 0.11_wp
    real(wp), parameter :: BASE_TOP = 0.88_wp
    real(wp), parameter :: WSPACE = 0.20_wp
    real(wp), parameter :: HSPACE = 0.20_wp

contains

    subroutine compute_tight_subplot_margins(backend, subplots_array, nr, nc, &
                                             xscale, yscale, symlog_threshold, left_f, &
                                             right_f, bottom_f, top_f, &
                                             ok, suptitle_height_frac)
        !! Compute per-cell subplot margins (fractional figure coordinates) on
        !! matplotlib's default fixed subplot parameters. The layout depends only
        !! on the grid shape, not on per-axes decoration sizes, so it reproduces
        !! pyplot's size-independent default spacing. When a suptitle is present
        !! the top row is lowered by its band so the figure-level title clears the
        !! top-row subplot titles. The scale arguments are accepted for interface
        !! stability but do not influence the box geometry.
        class(*), intent(in) :: backend
        type(subplot_data_t), intent(in) :: subplots_array(:, :)
        integer, intent(in) :: nr, nc
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), allocatable, intent(out) :: left_f(:, :), right_f(:, :)
        real(wp), allocatable, intent(out) :: bottom_f(:, :), top_f(:, :)
        logical, intent(out) :: ok
        real(wp), intent(in), optional :: suptitle_height_frac

        real(wp) :: sup_frac

        ok = .false.

        if (nr <= 0 .or. nc <= 0) return
        if (size(subplots_array, 1) /= nr) return
        if (size(subplots_array, 2) /= nc) return

        ! Only raster and PDF backends use fractional subplot positioning.
        select type (bk => backend)
        class is (raster_context)
        class is (pdf_context)
        class default
            return
        end select

        sup_frac = 0.0_wp
        if (present(suptitle_height_frac)) sup_frac = max(0.0_wp, suptitle_height_frac)

        allocate (left_f(nr, nc), right_f(nr, nc))
        allocate (bottom_f(nr, nc), top_f(nr, nc))

        call solve_default_grid(nr, nc, sup_frac, left_f, right_f, bottom_f, &
                                top_f, ok)
    end subroutine compute_tight_subplot_margins

    subroutine solve_default_grid(nr, nc, sup_frac, left_f, right_f, bottom_f, &
                                  top_f, ok)
        !! Place an nr x nc grid on matplotlib's default subplot parameters.
        integer, intent(in) :: nr, nc
        real(wp), intent(in) :: sup_frac
        real(wp), intent(out) :: left_f(:, :), right_f(:, :)
        real(wp), intent(out) :: bottom_f(:, :), top_f(:, :)
        logical, intent(out) :: ok

        integer :: i, j
        real(wp) :: grid_top
        real(wp) :: total_w, total_h
        real(wp) :: ax_w, ax_h, gap_w, gap_h
        real(wp) :: sub_left, sub_top

        ok = .false.

        grid_top = BASE_TOP - sup_frac
        total_w = BASE_RIGHT - BASE_LEFT
        total_h = grid_top - BASE_BOTTOM

        ax_w = total_w/(real(nc, wp) + WSPACE*real(max(0, nc - 1), wp))
        gap_w = WSPACE*ax_w
        ax_h = total_h/(real(nr, wp) + HSPACE*real(max(0, nr - 1), wp))
        gap_h = HSPACE*ax_h

        if (ax_w <= 0.0_wp .or. ax_h <= 0.0_wp) return

        do i = 1, nr
            sub_top = grid_top - real(i - 1, wp)*(ax_h + gap_h)
            do j = 1, nc
                sub_left = BASE_LEFT + real(j - 1, wp)*(ax_w + gap_w)
                left_f(i, j) = sub_left
                right_f(i, j) = sub_left + ax_w
                top_f(i, j) = sub_top
                bottom_f(i, j) = sub_top - ax_h
            end do
        end do

        ok = .true.
    end subroutine solve_default_grid

end module fortplot_subplot_layout
