module fortplot_animation_constants
    implicit none
    private

    ! Animation configuration constants
    integer, parameter, public :: DEFAULT_FRAME_INTERVAL_MS = 50
    integer, parameter, public :: DEFAULT_ANIMATION_FPS = 10
    integer, parameter, public :: MIN_VALID_VIDEO_SIZE = 100
    integer, parameter, public :: MIN_EXPECTED_VIDEO_SIZE = 1000
    integer, parameter, public :: MAX_FILENAME_LENGTH = 255
    
    ! Enhanced recovery constants for exponential backoff
    integer, parameter, public :: MAX_RETRY_ATTEMPTS = 3
    integer, parameter, public :: BASE_RETRY_DELAY_MS = 100

end module fortplot_animation_constants