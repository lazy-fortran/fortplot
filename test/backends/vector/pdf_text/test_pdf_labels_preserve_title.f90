program test_pdf_labels_preserve_title
    use fortplot, only: figure, plot, savefig, title, xlabel, ylabel, wp
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = &
        'build/test/output/test_pdf_labels_preserve_title.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status

    call figure()
    call title('PersistentTitle')
    call ylabel('PersistentY')
    call xlabel('PersistentX')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream'
        stop 1
    end if

    call require_pdf_text(stream_text, 'PersistentTitle')
    call require_pdf_text(stream_text, 'PersistentX')
    call require_pdf_text(stream_text, 'PersistentY')

    print *, 'PASS: xlabel preserves existing PDF title and ylabel'

contains

    subroutine require_pdf_text(stream_text, needle)
        character(len=*), intent(in) :: stream_text
        character(len=*), intent(in) :: needle

        if (index(stream_text, '('//needle//') Tj') == 0) then
            print *, 'FAIL: missing PDF text ', needle
            stop 1
        end if
    end subroutine require_pdf_text
end program test_pdf_labels_preserve_title
