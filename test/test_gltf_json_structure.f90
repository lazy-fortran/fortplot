program test_gltf_json_structure
    !! Test GLTF JSON structure generation
    !! Following TDD: Write test first, then implementation
    
    use fortplot_gltf_base, only: gltf_asset_t
    use fortplot_gltf_writer, only: write_gltf_header, write_gltf_asset
    implicit none
    
    ! Running GLTF JSON structure tests
    
    call test_minimal_gltf_structure()
    call test_gltf_asset_info()
    
    print *, "All GLTF JSON structure tests passed!"
    
contains

    subroutine test_minimal_gltf_structure()
        !! Test that we can generate minimal valid GLTF 2.0 structure
        character(len=:), allocatable :: json
        
        ! Act
        json = write_gltf_header()
        
        ! Assert - Check for required GLTF 2.0 elements
        if (index(json, '"asset"') == 0) then
            error stop "GLTF header missing required 'asset' property"
        end if
        
        if (index(json, '"version"') == 0) then
            error stop "GLTF header missing required 'version' in asset"
        end if
        
        if (index(json, '"2.0"') == 0) then
            error stop "GLTF header must specify version 2.0"
        end if
        
    end subroutine test_minimal_gltf_structure
    
    subroutine test_gltf_asset_info()
        !! Test GLTF asset information structure
        type(gltf_asset_t) :: asset
        character(len=:), allocatable :: json
        
        ! Arrange
        asset%version = "2.0"
        asset%generator = "fortplotlib"
        
        ! Act
        json = write_gltf_asset(asset)
        
        ! Assert
        if (index(json, '"version":"2.0"') == 0) then
            error stop "Asset JSON missing version"
        end if
        
        if (index(json, '"generator":"fortplotlib"') == 0) then
            error stop "Asset JSON missing generator"
        end if
        
    end subroutine test_gltf_asset_info

end program test_gltf_json_structure