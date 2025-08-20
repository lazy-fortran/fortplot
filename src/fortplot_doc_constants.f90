module fortplot_doc_constants
    !! Constants for documentation generation
    implicit none
    private
    
    ! Public constants
    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES
    public :: VIDEO_WIDTH, VIDEO_HEIGHT
    public :: GITHUB_BASE_URL
    
    ! String length constants
    integer, parameter :: PATH_MAX_LEN = 256
    integer, parameter :: FILENAME_MAX_LEN = 256  
    integer, parameter :: LINE_MAX_LEN = 1024
    
    ! Array size constants
    integer, parameter :: MAX_EXAMPLES = 20
    integer, parameter :: MAX_MEDIA_FILES = 10
    
    ! Video dimensions
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600
    
    ! URL constants
    character(len=*), parameter :: GITHUB_BASE_URL = &
        'https://github.com/lazy-fortran/fortplot/blob/main/'

end module fortplot_doc_constants