module fortplot_3d_box
    !! 3D box frame rendering: back panes, pane gridlines, and box spines.
    !!
    !! Selects which of the cube's six panes face away from the viewer using the
    !! per-corner camera depth, fills those back panes light gray, draws their
    !! gridlines, and strokes the full twelve-edge box. Drawn before the data so
    !! panes and gridlines sit behind it, matching matplotlib mplot3d.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    implicit none

    private
    public :: draw_back_panes, draw_pane_gridlines
    public :: draw_back_spines, draw_front_spines
    public :: CORNER_MIN_MIN_MIN, CORNER_MAX_MIN_MIN, CORNER_MAX_MAX_MIN, &
              CORNER_MIN_MAX_MIN, CORNER_MIN_MIN_MAX, CORNER_MAX_MIN_MAX, &
              CORNER_MAX_MAX_MAX, CORNER_MIN_MAX_MAX

    ! matplotlib mplot3d pane fill (light gray) and pane gridline color.
    real(wp), parameter :: PANE_RGB(3) = [0.95_wp, 0.95_wp, 0.95_wp]
    real(wp), parameter :: GRID_RGB(3) = [0.7_wp, 0.7_wp, 0.7_wp]
    integer, parameter :: N_GRIDLINES = 4   ! interior gridlines per pane direction

    ! Corner indices for readability
    integer, parameter :: &
        CORNER_MIN_MIN_MIN = 1, &  ! (x_min, y_min, z_min)
        CORNER_MAX_MIN_MIN = 2, &  ! (x_max, y_min, z_min)
        CORNER_MAX_MAX_MIN = 3, &  ! (x_max, y_max, z_min)
        CORNER_MIN_MAX_MIN = 4, &  ! (x_min, y_max, z_min)
        CORNER_MIN_MIN_MAX = 5, &  ! (x_min, y_min, z_max)
        CORNER_MAX_MIN_MAX = 6, &  ! (x_max, y_min, z_max)
        CORNER_MAX_MAX_MAX = 7, &  ! (x_max, y_max, z_max)
        CORNER_MIN_MAX_MAX = 8     ! (x_min, y_max, z_max)

contains

    function cube_faces() result(faces)
        !! The six cube faces as corner-index quads (counter-clockwise).
        integer :: faces(4, 6)
        faces(:, 1) = [CORNER_MIN_MIN_MIN, CORNER_MAX_MIN_MIN, &
                       CORNER_MAX_MAX_MIN, CORNER_MIN_MAX_MIN]  ! z = 0 (bottom)
        faces(:, 2) = [CORNER_MIN_MIN_MAX, CORNER_MAX_MIN_MAX, &
                       CORNER_MAX_MAX_MAX, CORNER_MIN_MAX_MAX]  ! z = 1 (top)
        faces(:, 3) = [CORNER_MIN_MIN_MIN, CORNER_MAX_MIN_MIN, &
                       CORNER_MAX_MIN_MAX, CORNER_MIN_MIN_MAX]  ! y = 0
        faces(:, 4) = [CORNER_MIN_MAX_MIN, CORNER_MAX_MAX_MIN, &
                       CORNER_MAX_MAX_MAX, CORNER_MIN_MAX_MAX]  ! y = 1
        faces(:, 5) = [CORNER_MIN_MIN_MIN, CORNER_MIN_MAX_MIN, &
                       CORNER_MIN_MAX_MAX, CORNER_MIN_MIN_MAX]  ! x = 0
        faces(:, 6) = [CORNER_MAX_MIN_MIN, CORNER_MAX_MAX_MIN, &
                       CORNER_MAX_MAX_MAX, CORNER_MAX_MIN_MAX]  ! x = 1
    end function cube_faces

    function back_face_flags(corners_depth) result(is_back)
        !! Select the three back-facing panes: those whose mean camera depth is
        !! below the cube center (smaller depth = farther from the viewer). The
        !! three smallest-depth faces are the back panes, matching mplot3d.
        real(wp), intent(in) :: corners_depth(8)
        logical :: is_back(6)
        integer :: faces(4, 6), f, k, order(6)
        real(wp) :: face_depth(6)

        faces = cube_faces()
        do f = 1, 6
            face_depth(f) = 0.0_wp
            do k = 1, 4
                face_depth(f) = face_depth(f) + corners_depth(faces(k, f))
            end do
            face_depth(f) = face_depth(f)/4.0_wp
        end do

        do f = 1, 6
            order(f) = f
        end do
        call sort_faces_by_depth(face_depth, order)

        is_back = .false.
        is_back(order(1)) = .true.
        is_back(order(2)) = .true.
        is_back(order(3)) = .true.
    end function back_face_flags

    subroutine sort_faces_by_depth(face_depth, order)
        !! Ascending selection sort of face indices by depth.
        real(wp), intent(in) :: face_depth(6)
        integer, intent(inout) :: order(6)
        integer :: i, j, min_idx, tmp

        do i = 1, 5
            min_idx = i
            do j = i + 1, 6
                if (face_depth(order(j)) < face_depth(order(min_idx))) min_idx = j
            end do
            if (min_idx /= i) then
                tmp = order(i); order(i) = order(min_idx); order(min_idx) = tmp
            end if
        end do
    end subroutine sort_faces_by_depth

    subroutine draw_back_panes(ctx, corners_2d, corners_depth)
        !! Fill the three back-facing panes with light gray, behind the data.
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2, 8), corners_depth(8)
        integer :: faces(4, 6), f, k
        logical :: is_back(6)
        real(wp) :: xq(4), yq(4)

        faces = cube_faces()
        is_back = back_face_flags(corners_depth)
        call ctx%color(PANE_RGB(1), PANE_RGB(2), PANE_RGB(3))
        do f = 1, 6
            if (.not. is_back(f)) cycle
            do k = 1, 4
                xq(k) = corners_2d(1, faces(k, f))
                yq(k) = corners_2d(2, faces(k, f))
            end do
            call ctx%fill_quad(xq, yq)
        end do
        call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)
    end subroutine draw_back_panes

    subroutine draw_pane_gridlines(ctx, corners_2d, corners_depth)
        !! Draw interior gridlines across each back pane in light gray.
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2, 8), corners_depth(8)
        integer :: faces(4, 6), f
        logical :: is_back(6)

        faces = cube_faces()
        is_back = back_face_flags(corners_depth)
        call ctx%color(GRID_RGB(1), GRID_RGB(2), GRID_RGB(3))
        call ctx%set_line_width(0.5_wp)
        do f = 1, 6
            if (is_back(f)) call draw_face_grid(ctx, corners_2d, faces(:, f))
        end do
        call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)
    end subroutine draw_pane_gridlines

    subroutine draw_face_grid(ctx, corners_2d, face)
        !! Draw a regular grid on one quad face by interpolating its edges.
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2, 8)
        integer, intent(in) :: face(4)
        real(wp) :: p1(2), p2(2), p3(2), p4(2), a1(2), a2(2)
        real(wp) :: t
        integer :: g

        p1 = corners_2d(:, face(1))
        p2 = corners_2d(:, face(2))
        p3 = corners_2d(:, face(3))
        p4 = corners_2d(:, face(4))

        do g = 1, N_GRIDLINES
            t = real(g, wp)/real(N_GRIDLINES + 1, wp)
            ! Lines parallel to edge p1->p4 (interpolate along p1->p2 and p4->p3)
            a1 = p1 + t*(p2 - p1)
            a2 = p4 + t*(p3 - p4)
            call ctx%line(a1(1), a1(2), a2(1), a2(2))
            ! Lines parallel to edge p1->p2 (interpolate along p1->p4 and p2->p3)
            a1 = p1 + t*(p4 - p1)
            a2 = p2 + t*(p3 - p2)
            call ctx%line(a1(1), a1(2), a2(1), a2(2))
        end do
    end subroutine draw_face_grid

    subroutine draw_back_spines(ctx, corners_2d, corners_depth)
        !! Draw the box edges that lie behind the data (mean edge depth below the
        !! cube center). These render before the data so the data paints over them.
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2, 8), corners_depth(8)

        call draw_spines_by_side(ctx, corners_2d, corners_depth, front=.false.)
    end subroutine draw_back_spines

    subroutine draw_front_spines(ctx, corners_2d, corners_depth)
        !! Draw the box edges in front of the data (mean edge depth above the cube
        !! center). These render after the data so they occlude it, matching the
        !! global painter ordering of matplotlib mplot3d.
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2, 8), corners_depth(8)

        call draw_spines_by_side(ctx, corners_2d, corners_depth, front=.true.)
    end subroutine draw_front_spines

    subroutine draw_spines_by_side(ctx, corners_2d, corners_depth, front)
        !! Stroke the box edges on one side of the cube center depth. An edge is a
        !! front edge when the mean depth of its two corners is at or above the
        !! cube-center depth (larger depth = nearer the viewer).
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2, 8), corners_depth(8)
        logical, intent(in) :: front
        integer :: edges(2, 12), e
        real(wp) :: center_depth, edge_depth
        logical :: is_front

        edges = cube_edges()
        center_depth = sum(corners_depth)/8.0_wp
        call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(0.8_wp)
        do e = 1, 12
            edge_depth = 0.5_wp*(corners_depth(edges(1, e)) &
                                 + corners_depth(edges(2, e)))
            is_front = edge_depth >= center_depth
            if (is_front .neqv. front) cycle
            call ctx%line(corners_2d(1, edges(1, e)), corners_2d(2, edges(1, e)), &
                          corners_2d(1, edges(2, e)), corners_2d(2, edges(2, e)))
        end do
    end subroutine draw_spines_by_side

    function cube_edges() result(edges)
        !! The twelve cube edges as corner-index pairs.
        integer :: edges(2, 12)
        edges(:, 1) = [CORNER_MIN_MIN_MIN, CORNER_MAX_MIN_MIN]
        edges(:, 2) = [CORNER_MAX_MIN_MIN, CORNER_MAX_MAX_MIN]
        edges(:, 3) = [CORNER_MAX_MAX_MIN, CORNER_MIN_MAX_MIN]
        edges(:, 4) = [CORNER_MIN_MAX_MIN, CORNER_MIN_MIN_MIN]
        edges(:, 5) = [CORNER_MIN_MIN_MAX, CORNER_MAX_MIN_MAX]
        edges(:, 6) = [CORNER_MAX_MIN_MAX, CORNER_MAX_MAX_MAX]
        edges(:, 7) = [CORNER_MAX_MAX_MAX, CORNER_MIN_MAX_MAX]
        edges(:, 8) = [CORNER_MIN_MAX_MAX, CORNER_MIN_MIN_MAX]
        edges(:, 9) = [CORNER_MIN_MIN_MIN, CORNER_MIN_MIN_MAX]
        edges(:, 10) = [CORNER_MAX_MIN_MIN, CORNER_MAX_MIN_MAX]
        edges(:, 11) = [CORNER_MAX_MAX_MIN, CORNER_MAX_MAX_MAX]
        edges(:, 12) = [CORNER_MIN_MAX_MIN, CORNER_MIN_MAX_MAX]
    end function cube_edges

end module fortplot_3d_box
