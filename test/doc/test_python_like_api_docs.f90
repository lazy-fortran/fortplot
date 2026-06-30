program test_python_like_api_docs
    implicit none

    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine figure(num, figsize, dpi)', &
                          [character(len=24) :: 'Parameters', 'num', 'figsize', 'dpi'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine subplot(nrows, ncols, index)', &
                          [character(len=24) :: 'Parameters', 'nrows', 'ncols', 'index'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine subplots(nrows, ncols, axes, sharex, sharey)', &
                          [character(len=24) :: 'Parameters', 'axes', 'sharex', 'sharey'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine savefig(filename, dpi, transparent, bbox_inches)', &
                          [character(len=24) :: 'Parameters', 'filename', 'bbox_inches'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine xlabel(label_text)', &
                          [character(len=24) :: 'Parameters', 'label_text'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine ylabel(label_text)', &
                          [character(len=24) :: 'Parameters', 'label_text'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine title(title_text)', &
                          [character(len=24) :: 'Parameters', 'title_text'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine legend(loc, box, fontsize, position)', &
                          [character(len=24) :: 'Parameters', 'loc', 'position'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine grid(visible, which, axis, alpha, linestyle, enabled)', &
                          [character(len=24) :: 'Parameters', 'visible', 'enabled'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_xscale(scale, linthresh, threshold, base, linscale)', &
                          [character(len=24) :: 'Parameters', 'scale', 'linthresh', 'threshold'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_yscale(scale, linthresh, threshold, base, linscale)', &
                          [character(len=24) :: 'Parameters', 'scale', 'linthresh', 'threshold'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine plot(x, y, label, linestyle, color, linewidth, marker, markersize, alpha)', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize', 'alpha'])

    print *, 'All python-like API doc tests passed.'

contains

    subroutine assert_doc_block(path, signature, patterns)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: signature
        character(len=*), dimension(:), intent(in) :: patterns

        integer, parameter :: max_lines = 4096
        character(len=1024) :: lines(max_lines)
        character(len=1024) :: line
        integer :: unit, ios, count, i, j
        logical :: found_signature, found_pattern

        count = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Cannot open ', trim(path)
            stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            count = count + 1
            if (count > max_lines) then
                print *, 'Too many lines in ', trim(path)
                close(unit)
                stop 1
            end if
            lines(count) = line
        end do
        close(unit)

        found_signature = .false.
        do i = 1, count
            if (index(lines(i), signature) > 0) then
                found_signature = .true.
                do j = 1, size(patterns)
                    found_pattern = block_has_pattern(lines, count, i, patterns(j))
                    if (.not. found_pattern) then
                        print *, 'Missing doc text in ', trim(path), ': ', trim(patterns(j))
                        stop 1
                    end if
                end do
                exit
            end if
        end do

        if (.not. found_signature) then
            print *, 'Missing signature in ', trim(path), ': ', trim(signature)
            stop 1
        end if
    end subroutine assert_doc_block

    logical function block_has_pattern(lines, count, signature_line, pattern)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: count, signature_line
        character(len=*), intent(in) :: pattern
        integer :: first_line, i

        first_line = max(1, signature_line - 24)
        block_has_pattern = .false.
        do i = first_line, signature_line - 1
            if (index(lines(i), pattern) > 0) then
                block_has_pattern = .true.
                return
            end if
        end do
    end function block_has_pattern

end program test_python_like_api_docs
