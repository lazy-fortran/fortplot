module fortplot_text_rendering
    !! Thin facade module that re-exports shared layout utilities and raster hooks
  use fortplot_text_layout, only: calculate_text_width, calculate_text_width_with_size, &
                                      calculate_text_height, &
                                      calculate_text_height_with_size, &
                                      calculate_text_descent, &
                                      DEFAULT_FONT_SIZE, &
                                      TITLE_FONT_SIZE, TITLE_FONT_SIZE_PT, LABEL_FONT_SIZE, TICK_FONT_SIZE
    use fortplot_raster_text_rendering, only: render_text_to_image, render_text_with_size, &
                                              render_rotated_text_to_image
    implicit none

    private

    public :: render_text_to_image, calculate_text_width, calculate_text_height
    public :: calculate_text_height_with_size
    public :: render_rotated_text_to_image, calculate_text_descent
    public :: calculate_text_width_with_size, render_text_with_size
    public :: DEFAULT_FONT_SIZE, TITLE_FONT_SIZE, TITLE_FONT_SIZE_PT, LABEL_FONT_SIZE, TICK_FONT_SIZE

end module fortplot_text_rendering
