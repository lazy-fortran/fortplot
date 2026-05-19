module fortplot_tt_edge_sort
    !! TrueType edge sorting utilities.
    !! Sorts glyph outline edges by y0 for scanline rasterization.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: tt_edge_t, sort_edges, sort_edges_quicksort, sort_edges_ins_sort

    type :: tt_edge_t
        real(dp) :: x0, y0
        real(dp) :: x1, y1
        logical :: invert
    end type tt_edge_t

contains

    subroutine sort_edges(edges, n)
        !! Sort edges by y0 ascending. Quicksort + insertion sort.
        type(tt_edge_t), intent(inout) :: edges(:)
        integer, intent(in) :: n

        call sort_edges_quicksort(edges, n)
        call sort_edges_ins_sort(edges, n)
    end subroutine sort_edges

    recursive subroutine sort_edges_quicksort(p, n)
        type(tt_edge_t), intent(inout) :: p(:)
        integer, intent(in) :: n
        type(tt_edge_t) :: t
        integer :: c01, c12, c, m, i, j, z

        if (n <= 12) return

        m = ishft(n, -1) + 1
        c01 = merge(1, 0, p(1)%y0 < p(m)%y0)
        c12 = merge(1, 0, p(m)%y0 < p(n)%y0)

        if (c01 /= c12) then
            c = merge(1, 0, p(1)%y0 < p(n)%y0)
            if (c == c12) then
                z = 1
            else
                z = n
            end if
            t = p(z); p(z) = p(m); p(m) = t
        end if

        t = p(1); p(1) = p(m); p(m) = t

        i = 2
        j = n
        do
            do while (i <= n .and. p(i)%y0 < p(1)%y0)
                i = i + 1
            end do
            do while (j >= 1 .and. p(1)%y0 < p(j)%y0)
                j = j - 1
            end do
            if (i >= j) exit
            t = p(i); p(i) = p(j); p(j) = t
            i = i + 1
            j = j - 1
        end do

        if (j < n - i + 1) then
            call sort_edges_quicksort(p(1:j), j)
            call sort_edges_quicksort(p(i:n), n - i + 1)
        else
            call sort_edges_quicksort(p(i:n), n - i + 1)
            call sort_edges_quicksort(p(1:j), j)
        end if
    end subroutine sort_edges_quicksort

    subroutine sort_edges_ins_sort(p, n)
        type(tt_edge_t), intent(inout) :: p(:)
        integer, intent(in) :: n
        type(tt_edge_t) :: t
        integer :: i, j

        do i = 2, n
            t = p(i)
            j = i
            do while (j > 1)
                if (.not. (t%y0 < p(j - 1)%y0)) exit
                p(j) = p(j - 1)
                j = j - 1
            end do
            if (i /= j) p(j) = t
        end do
    end subroutine sort_edges_ins_sort

end module fortplot_tt_edge_sort
