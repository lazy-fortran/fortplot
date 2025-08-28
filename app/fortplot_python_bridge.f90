program fortplot_python_bridge
    !! Simplified Python bridge program using consolidated interface
    !! This program simply calls the unified bridge mode from fortplot_python_interface
    use fortplot_python_interface, only: run_bridge_mode
    implicit none
    
    ! Run the bridge in command processing mode
    call run_bridge_mode()
    
end program fortplot_python_bridge