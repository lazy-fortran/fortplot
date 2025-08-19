program test_annotation_coordinate_systems
    !! Comprehensive coordinate system testing for text annotations
    !! Tests coordinate transformation accuracy and edge cases
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none

    call test_data_coordinate_edge_cases()
    call test_figure_coordinate_bounds()
    call test_axis_coordinate_normalization()
    call test_coordinate_system_mixing()
    call test_coordinate_transformation_consistency()
    call test_logarithmic_scale_coordinates()
    call test_negative_coordinate_handling()
    call test_coordinate_precision_accuracy()

contains

    subroutine test_data_coordinate_edge_cases()
        !! GIVEN: Data coordinates at extreme values
        !! WHEN: Transforming to pixel coordinates
        !! THEN: Transformations handle edge cases correctly
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_DATA
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]
        real(wp) :: data_bounds(4)
        
        ! Test with very large data range
        data_bounds = [1.0e-12_wp, 1.0e12_wp, -1.0e6_wp, 1.0e6_wp]
        annotation%x = 1.0e6_wp
        annotation%y = 0.0_wp
        annotation%coord_type = COORD_DATA
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        
        ! Should produce reasonable pixel coordinates
        if (pixel_x < 0.0_wp .or. pixel_x > 1000.0_wp) then
            error stop "FAIL: Large data range produces unreasonable pixel coordinates"
        end if
        
        print *, "PASS: Data coordinate edge cases test"
    end subroutine test_data_coordinate_edge_cases

    subroutine test_figure_coordinate_bounds()
        !! GIVEN: Figure coordinates outside 0-1 range
        !! WHEN: Transforming coordinates
        !! THEN: Out-of-bounds coordinates are handled appropriately
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_FIGURE
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: figure_size(2) = [800.0_wp, 600.0_wp]
        
        ! Test coordinates outside normal 0-1 range
        annotation%x = 1.5_wp  ! Outside figure
        annotation%y = -0.2_wp  ! Outside figure
        annotation%coord_type = COORD_FIGURE
        
        call transform_annotation_coordinates(annotation, figure_size, pixel_x, pixel_y)
        
        ! Should extrapolate beyond figure bounds
        if (pixel_x < 800.0_wp) then
            error stop "FAIL: Figure coordinate > 1.0 should extrapolate beyond figure"
        end if
        
        if (pixel_y > 0.0_wp) then
            error stop "FAIL: Figure coordinate < 0.0 should extrapolate beyond figure"
        end if
        
        print *, "PASS: Figure coordinate bounds test"
    end subroutine test_figure_coordinate_bounds

    subroutine test_axis_coordinate_normalization()
        !! GIVEN: Axis coordinates in 0-1 range
        !! WHEN: Transforming to plot area
        !! THEN: Coordinates map correctly to plot area bounds
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_AXIS
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [150.0_wp, 80.0_wp, 500.0_wp, 400.0_wp]
        
        annotation%coord_type = COORD_AXIS
        
        ! Test corner coordinates
        annotation%x = 0.0_wp
        annotation%y = 0.0_wp
        call transform_annotation_coordinates(annotation, plot_area, pixel_x, pixel_y)
        
        if (abs(pixel_x - 150.0_wp) > 0.1_wp) then
            error stop "FAIL: Axis coordinate (0,0) should map to plot area origin"
        end if
        
        annotation%x = 1.0_wp
        annotation%y = 1.0_wp
        call transform_annotation_coordinates(annotation, plot_area, pixel_x, pixel_y)
        
        if (abs(pixel_x - 650.0_wp) > 0.1_wp) then  ! 150 + 500
            error stop "FAIL: Axis coordinate (1,1) should map to plot area corner"
        end if
        
        print *, "PASS: Axis coordinate normalization test"
    end subroutine test_axis_coordinate_normalization

    subroutine test_coordinate_system_mixing()
        !! GIVEN: Annotations using different coordinate systems
        !! WHEN: Rendering multiple annotations
        !! THEN: Each annotation uses its specified coordinate system
        type(figure_t) :: fig
        
        call fig%initialize(600, 400)
        
        ! Mixed coordinate systems in same figure
        call fig%text(0.1_wp, 0.9_wp, "Figure coords", coord_type=COORD_FIGURE)
        call fig%text(0.5_wp, 0.5_wp, "Axis coords", coord_type=COORD_AXIS)
        call fig%add_plot([1.0_wp, 2.0_wp], [1.0_wp, 2.0_wp])
        call fig%text(1.5_wp, 1.5_wp, "Data coords", coord_type=COORD_DATA)
        
        call fig%savefig("test_mixed_coordinates.png")
        
        print *, "PASS: Coordinate system mixing test"
    end subroutine test_coordinate_system_mixing

    subroutine test_coordinate_transformation_consistency()
        !! GIVEN: Same logical position in different coordinate systems
        !! WHEN: Transforming coordinates
        !! THEN: All systems produce consistent pixel positions
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t
        use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
        
        type(text_annotation_t) :: ann_data, ann_figure, ann_axis
        real(wp) :: pixel_x_data, pixel_y_data
        real(wp) :: pixel_x_figure, pixel_y_figure
        real(wp) :: pixel_x_axis, pixel_y_axis
        
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]
        real(wp) :: data_bounds(4) = [0.0_wp, 10.0_wp, 0.0_wp, 5.0_wp]
        real(wp) :: figure_size(2) = [600.0_wp, 500.0_wp]
        
        ! Center position in different coordinate systems
        ann_data%x = 5.0_wp
        ann_data%y = 2.5_wp
        ann_data%coord_type = COORD_DATA
        call transform_annotation_coordinates(ann_data, plot_area, data_bounds, pixel_x_data, pixel_y_data)
        
        ann_axis%x = 0.5_wp
        ann_axis%y = 0.5_wp
        ann_axis%coord_type = COORD_AXIS
        call transform_annotation_coordinates(ann_axis, plot_area, pixel_x_axis, pixel_y_axis)
        
        ! Data center and axis center should produce same pixel coordinates
        if (abs(pixel_x_data - pixel_x_axis) > 1.0_wp) then
            error stop "FAIL: Data center and axis center should produce same X pixel coordinate"
        end if
        
        if (abs(pixel_y_data - pixel_y_axis) > 1.0_wp) then
            error stop "FAIL: Data center and axis center should produce same Y pixel coordinate"
        end if
        
        print *, "PASS: Coordinate transformation consistency test"
    end subroutine test_coordinate_transformation_consistency

    subroutine test_logarithmic_scale_coordinates()
        !! GIVEN: Data coordinates with logarithmic scaling
        !! WHEN: Transforming coordinates
        !! THEN: Logarithmic transformation is applied correctly
        use fortplot_annotations, only: transform_annotation_coordinates_log, text_annotation_t, COORD_DATA
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]
        real(wp) :: data_bounds(4) = [1.0_wp, 1000.0_wp, 0.1_wp, 100.0_wp]  ! Log scale ranges
        
        annotation%x = 10.0_wp  ! One decade from minimum
        annotation%y = 1.0_wp   ! One decade from minimum
        annotation%coord_type = COORD_DATA
        
        call transform_annotation_coordinates_log(annotation, plot_area, data_bounds, &
                                                 log_scale_x=.true., log_scale_y=.true., &
                                                 pixel_x=pixel_x, pixel_y=pixel_y)
        
        ! Log scale should place this at 1/3 of the way across plot
        ! log10(10) - log10(1) = 1, log10(1000) - log10(1) = 3, so 1/3 position
        if (abs(pixel_x - (100.0_wp + 400.0_wp/3.0_wp)) > 5.0_wp) then
            error stop "FAIL: Logarithmic X transformation incorrect"
        end if
        
        print *, "PASS: Logarithmic scale coordinates test"
    end subroutine test_logarithmic_scale_coordinates

    subroutine test_negative_coordinate_handling()
        !! GIVEN: Data coordinates with negative values
        !! WHEN: Transforming coordinates
        !! THEN: Negative coordinates are handled correctly
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_DATA
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]
        real(wp) :: data_bounds(4) = [-10.0_wp, 10.0_wp, -5.0_wp, 5.0_wp]  ! Symmetric around zero
        
        annotation%x = -5.0_wp  ! Negative coordinate
        annotation%y = 0.0_wp   ! Zero coordinate
        annotation%coord_type = COORD_DATA
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        
        ! -5 in range [-10, 10] should be at 1/4 position
        if (abs(pixel_x - (100.0_wp + 400.0_wp * 0.25_wp)) > 1.0_wp) then
            error stop "FAIL: Negative X coordinate transformation incorrect"
        end if
        
        ! 0 in range [-5, 5] should be at 1/2 position
        if (abs(pixel_y - (100.0_wp + 300.0_wp * 0.5_wp)) > 1.0_wp) then
            error stop "FAIL: Zero Y coordinate transformation incorrect"
        end if
        
        print *, "PASS: Negative coordinate handling test"
    end subroutine test_negative_coordinate_handling

    subroutine test_coordinate_precision_accuracy()
        !! GIVEN: High precision coordinate values
        !! WHEN: Transforming coordinates
        !! THEN: Precision is maintained within reasonable tolerance
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_DATA
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [0.0_wp, 0.0_wp, 1000.0_wp, 1000.0_wp]
        real(wp) :: data_bounds(4) = [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp]
        
        ! High precision coordinate
        annotation%x = 0.123456789012345_wp
        annotation%y = 0.987654321098765_wp
        annotation%coord_type = COORD_DATA
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        
        ! Verify precision is maintained
        if (abs(pixel_x - 123.456789012345_wp) > 1.0e-10_wp) then
            error stop "FAIL: High precision X coordinate transformation loses precision"
        end if
        
        if (abs(pixel_y - 987.654321098765_wp) > 1.0e-10_wp) then
            error stop "FAIL: High precision Y coordinate transformation loses precision"
        end if
        
        print *, "PASS: Coordinate precision accuracy test"
    end subroutine test_coordinate_precision_accuracy

end program test_annotation_coordinate_systems