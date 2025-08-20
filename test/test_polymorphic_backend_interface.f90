program test_polymorphic_backend_interface
    !! Test suite for polymorphic backend interface behavior (Issue #140)
    !!
    !! GIVEN: An abstract backend interface with polymorphic methods
    !! WHEN: Different backend implementations are used
    !! THEN: All backends should support the same interface methods
    !! AND: Business logic should work identically across all backends  
    !! AND: Backend-specific behavior should be encapsulated within backends
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    implicit none
    
    call test_abstract_backend_interface_exists()
    call test_all_backends_implement_interface()
    call test_polymorphic_method_calls()
    call test_backend_initialization_configuration()
    call test_no_runtime_type_checking_needed()
    
contains

    subroutine test_abstract_backend_interface_exists()
        !! GIVEN: The need for polymorphic backend operations
        !! WHEN: Checking for abstract backend interface
        !! THEN: An abstract backend interface should be defined
        !! AND: It should include all required rendering methods
        
        logical :: interface_exists
        
        ! Check that an abstract backend interface exists
        call verify_abstract_backend_interface(interface_exists)
        if (.not. interface_exists) then
            error stop 'FAILED: Abstract backend interface not properly defined'
        end if
        
        print *, 'PASSED: Abstract backend interface exists'
    end subroutine test_abstract_backend_interface_exists

    subroutine verify_abstract_backend_interface(interface_exists)
        !! Verify that the abstract backend interface is properly defined
        logical, intent(out) :: interface_exists
        
        logical :: has_abstract_type, has_deferred_methods
        
        ! Check for abstract type in context or separate backend interface module
        call check_for_abstract_type(has_abstract_type)
        call check_for_deferred_methods(has_deferred_methods)
        
        interface_exists = has_abstract_type .and. has_deferred_methods
    end subroutine verify_abstract_backend_interface

    subroutine check_for_abstract_type(has_abstract_type)
        !! Check if an abstract backend type is defined
        logical, intent(out) :: has_abstract_type
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        has_abstract_type = .false.
        
        ! Check in existing context module first
        open(newunit=unit, file='src/fortplot_context.f90', status='old', action='read', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                
                ! Look for abstract type definition
                if (index(line, 'type, abstract') > 0) then
                    has_abstract_type = .true.
                    exit
                end if
            end do
            close(unit)
        end if
        
        ! If not found, check for dedicated backend interface module
        if (.not. has_abstract_type) then
            open(newunit=unit, file='src/fortplot_backend_interface.f90', status='old', &
                  action='read', iostat=iostat)
            if (iostat == 0) then
                do
                    read(unit, '(A)', iostat=iostat) line
                    if (iostat /= 0) exit
                    
                    if (index(line, 'type, abstract') > 0) then
                        has_abstract_type = .true.
                        exit
                    end if
                end do
                close(unit)
            end if
        end if
    end subroutine check_for_abstract_type

    subroutine check_for_deferred_methods(has_deferred_methods)
        !! Check if deferred methods are defined for polymorphic operations
        logical, intent(out) :: has_deferred_methods
        
        integer :: unit, iostat
        character(len=1000) :: line
        integer :: deferred_count
        
        has_deferred_methods = .false.
        deferred_count = 0
        
        ! Check context module for deferred procedures
        open(newunit=unit, file='src/fortplot_context.f90', status='old', action='read', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                
                if (index(line, 'deferred') > 0 .and. index(line, 'procedure') > 0) then
                    deferred_count = deferred_count + 1
                end if
            end do
            close(unit)
        end if
        
        ! Check dedicated backend interface module if it exists
        open(newunit=unit, file='src/fortplot_backend_interface.f90', status='old', &
              action='read', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                
                if (index(line, 'deferred') > 0 .and. index(line, 'procedure') > 0) then
                    deferred_count = deferred_count + 1
                end if
            end do
            close(unit)
        end if
        
        ! Should have multiple deferred methods for complete interface
        has_deferred_methods = deferred_count >= 5  ! Expect at least 5 core methods
    end subroutine check_for_deferred_methods

    subroutine test_all_backends_implement_interface()
        !! GIVEN: An abstract backend interface 
        !! WHEN: Checking concrete backend implementations
        !! THEN: All backends (PNG, PDF, ASCII) should extend the interface
        !! AND: Each should implement all required methods
        
        logical :: png_implements, pdf_implements, ascii_implements
        
        call check_backend_implements_interface('png', png_implements)
        call check_backend_implements_interface('pdf', pdf_implements)  
        call check_backend_implements_interface('ascii', ascii_implements)
        
        if (.not. png_implements) then
            error stop 'FAILED: PNG backend does not properly implement interface'
        end if
        
        if (.not. pdf_implements) then
            error stop 'FAILED: PDF backend does not properly implement interface'
        end if
        
        if (.not. ascii_implements) then
            error stop 'FAILED: ASCII backend does not properly implement interface'
        end if
        
        print *, 'PASSED: All backends implement the interface'
    end subroutine test_all_backends_implement_interface

    subroutine check_backend_implements_interface(backend_name, implements_interface)
        !! Check if a specific backend implements the abstract interface
        character(*), intent(in) :: backend_name
        logical, intent(out) :: implements_interface
        
        character(len=100) :: filename
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: extends_abstract, has_procedures
        
        implements_interface = .false.
        extends_abstract = .false.
        has_procedures = .false.
        
        ! Construct filename for backend module
        select case (backend_name)
        case ('png')
            filename = 'src/fortplot_png.f90'
        case ('pdf')  
            filename = 'src/fortplot_pdf.f90'
        case ('ascii')
            filename = 'src/fortplot_ascii.f90'
        case default
            return
        end select
        
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for type extension of abstract interface
            if (index(line, 'extends') > 0 .and. index(line, 'plot_context') > 0) then
                extends_abstract = .true.
            end if
            
            ! Look for procedure implementations
            if (index(line, 'procedure') > 0 .and. index(line, '::') > 0) then
                has_procedures = .true.
            end if
        end do
        
        close(unit)
        implements_interface = extends_abstract .and. has_procedures
    end subroutine check_backend_implements_interface

    subroutine test_polymorphic_method_calls()
        !! GIVEN: Polymorphic backend interface methods
        !! WHEN: Business logic calls rendering operations
        !! THEN: Methods should be called through polymorphic interface
        !! AND: No SELECT TYPE should be needed for method dispatch
        
        logical :: uses_polymorphic_calls
        
        call verify_polymorphic_usage('src/fortplot_figure_core.f90', uses_polymorphic_calls)
        if (.not. uses_polymorphic_calls) then
            error stop 'FAILED: figure_core does not use polymorphic method calls'
        end if
        
        call verify_polymorphic_usage('src/fortplot_legend.f90', uses_polymorphic_calls)
        if (.not. uses_polymorphic_calls) then
            error stop 'FAILED: legend does not use polymorphic method calls'
        end if
        
        print *, 'PASSED: Polymorphic method calls implemented'
    end subroutine test_polymorphic_method_calls

    subroutine verify_polymorphic_usage(file_path, uses_polymorphic_calls)
        !! Verify that a module uses polymorphic method calls appropriately
        character(*), intent(in) :: file_path
        logical, intent(out) :: uses_polymorphic_calls
        
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: has_backend_calls
        
        uses_polymorphic_calls = .false.
        has_backend_calls = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for polymorphic method calls through backend
            if (index(line, 'call backend%') > 0 .or. &
                index(line, 'call self%backend%') > 0) then
                has_backend_calls = .true.
                exit
            end if
        end do
        
        close(unit)
        uses_polymorphic_calls = has_backend_calls
    end subroutine verify_polymorphic_usage

    subroutine test_backend_initialization_configuration()
        !! GIVEN: Backend-specific behavior needs to be configured
        !! WHEN: Backends are initialized 
        !! THEN: All backend-specific configuration should happen at initialization
        !! AND: No runtime configuration based on backend type should be needed
        
        logical :: has_initialization_config
        
        call check_initialization_configuration(has_initialization_config)
        if (.not. has_initialization_config) then
            error stop 'FAILED: Backend initialization configuration not implemented'
        end if
        
        print *, 'PASSED: Backend initialization configuration implemented'
    end subroutine test_backend_initialization_configuration

    subroutine check_initialization_configuration(has_initialization_config)
        !! Check that backends are configured at initialization time
        logical, intent(out) :: has_initialization_config
        
        logical :: png_has_config, pdf_has_config, ascii_has_config
        
        call check_backend_initialization('src/fortplot_png.f90', png_has_config)
        call check_backend_initialization('src/fortplot_pdf.f90', pdf_has_config)
        call check_backend_initialization('src/fortplot_ascii.f90', ascii_has_config)
        
        has_initialization_config = png_has_config .and. pdf_has_config .and. ascii_has_config
    end subroutine check_initialization_configuration

    subroutine check_backend_initialization(file_path, has_config)
        !! Check if a backend has proper initialization configuration
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_config
        
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: has_init_routine
        
        has_config = .false.
        has_init_routine = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for initialization or configuration routines
            if (index(line, 'subroutine') > 0 .and. &
                (index(line, 'init') > 0 .or. index(line, 'config') > 0 .or. &
                 index(line, 'setup') > 0)) then
                has_init_routine = .true.
                exit
            end if
        end do
        
        close(unit)
        has_config = has_init_routine
    end subroutine check_backend_initialization

    subroutine test_no_runtime_type_checking_needed()
        !! GIVEN: Polymorphic backend interface with proper encapsulation
        !! WHEN: Business logic performs rendering operations
        !! THEN: No runtime type checking (SELECT TYPE) should be needed
        !! AND: All operations should work through polymorphic dispatch
        
        logical :: needs_runtime_checking
        
        call check_for_runtime_type_checking('src/fortplot_figure_core.f90', needs_runtime_checking)
        if (needs_runtime_checking) then
            error stop 'FAILED: figure_core still requires runtime type checking'
        end if
        
        call check_for_runtime_type_checking('src/fortplot_legend.f90', needs_runtime_checking)
        if (needs_runtime_checking) then
            error stop 'FAILED: legend still requires runtime type checking'
        end if
        
        call check_for_runtime_type_checking('src/fortplot_animation.f90', needs_runtime_checking)
        if (needs_runtime_checking) then
            error stop 'FAILED: animation still requires runtime type checking'  
        end if
        
        print *, 'PASSED: No runtime type checking needed'
    end subroutine test_no_runtime_type_checking_needed

    subroutine check_for_runtime_type_checking(file_path, needs_runtime_checking)
        !! Check if a module still requires runtime type checking
        character(*), intent(in) :: file_path
        logical, intent(out) :: needs_runtime_checking
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        needs_runtime_checking = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for SELECT TYPE statements (case insensitive)
            if (index(to_lowercase(line), 'select type') > 0) then
                ! Skip if it's in a comment
                if (index(line, '!') == 0 .or. &
                    (index(line, '!') > 0 .and. index(line, '!') > index(to_lowercase(line), 'select type'))) then
                    needs_runtime_checking = .true.
                    exit
                end if
            end if
        end do
        
        close(unit)
    end subroutine check_for_runtime_type_checking

    function to_lowercase(input_string) result(output_string)
        !! Convert string to lowercase for case-insensitive comparison
        character(*), intent(in) :: input_string
        character(len=len(input_string)) :: output_string
        integer :: i, ascii_val
        
        output_string = input_string
        do i = 1, len(output_string)
            ascii_val = ichar(output_string(i:i))
            if (ascii_val >= ichar('A') .and. ascii_val <= ichar('Z')) then
                output_string(i:i) = char(ascii_val + 32)
            end if
        end do
    end function to_lowercase

end program test_polymorphic_backend_interface