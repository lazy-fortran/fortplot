program test_docs_index_pages
    implicit none
    logical :: exists

    ! Ensure doc/cmake_example has an index page
    inquire(file="doc/cmake_example/index.md", exist=exists)
    if (.not. exists) then
        print *, "Missing doc/cmake_example/index.md"
        stop 1
    end if

    ! Ensure doc/archive has an index page
    inquire(file="doc/archive/index.md", exist=exists)
    if (.not. exists) then
        print *, "Missing doc/archive/index.md"
        stop 1
    end if
end program test_docs_index_pages

