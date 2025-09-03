program generate_example_docs
    !! Generate FORD documentation pages for fortplot examples
    use fortplot_documentation, only: PATH_MAX_LEN, get_example_count, get_example_dir, &
                                       get_example_name, process_example
    implicit none

    character(len=PATH_MAX_LEN) :: example_dir, example_name
    integer :: n_examples, i

    print *, "Generating example documentation..."

    n_examples = get_example_count()

    do i = 1, n_examples
        call get_example_dir(i, example_dir)
        call get_example_name(i, example_name)
        call process_example(trim(example_dir), trim(example_name))
    end do

    print *, "Documentation generation complete!"

end program generate_example_docs
