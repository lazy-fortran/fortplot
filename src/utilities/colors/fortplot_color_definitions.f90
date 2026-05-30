module fortplot_color_definitions
    !! Color type definitions and constants for matplotlib-compatible color support
    !!
    !! Provides:
    !! - Unified color type with RGBA support
    !! - Named color constants (CSS4 + tab: names)
    !! - Single letter color mappings
    !! - Utility functions for color validation

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: color_t
    public :: NUM_NAMED_COLORS, named_color_names, named_color_values
    public :: single_letters, letter_to_named
    public :: clamp_to_unit, to_lowercase, to_lowercase_char

    ! Color type for unified representation
    type :: color_t
        real(wp) :: r = 0.0_wp
        real(wp) :: g = 0.0_wp
        real(wp) :: b = 0.0_wp
        real(wp) :: a = 1.0_wp  ! Alpha channel
        logical :: valid = .false.
    end type color_t

    ! Named color constants - CSS4 plus matplotlib tab: colors
    integer, parameter :: NUM_NAMED_COLORS = 158
    character(len=20), parameter :: named_color_names(NUM_NAMED_COLORS) = [ &
        'red                 ', &
        'green               ', &
        'blue                ', &
        'cyan                ', &
        'magenta             ', &
        'yellow              ', &
        'black               ', &
        'white               ', &
        'gray                ', &
        'orange              ', &
        'purple              ', &
        'brown               ', &
        'pink                ', &
        'olive               ', &
        'navy                ', &
        'lime                ', &
        'teal                ', &
        'silver              ', &
        'maroon              ', &
        'indigo              ', &
        'aliceblue           ', &
        'antiquewhite        ', &
        'aqua                ', &
        'aquamarine          ', &
        'azure               ', &
        'beige               ', &
        'bisque              ', &
        'blanchedalmond      ', &
        'blueviolet          ', &
        'burlywood           ', &
        'cadetblue           ', &
        'chartreuse          ', &
        'chocolate           ', &
        'coral               ', &
        'cornflowerblue      ', &
        'cornsilk            ', &
        'crimson             ', &
        'darkblue            ', &
        'darkcyan            ', &
        'darkgoldenrod       ', &
        'darkgray            ', &
        'darkgreen           ', &
        'darkgrey            ', &
        'darkkhaki           ', &
        'darkmagenta         ', &
        'darkolivegreen      ', &
        'darkorange          ', &
        'darkorchid          ', &
        'darkred             ', &
        'darksalmon          ', &
        'darkseagreen        ', &
        'darkslateblue       ', &
        'darkslategray       ', &
        'darkslategrey       ', &
        'darkturquoise       ', &
        'darkviolet          ', &
        'deeppink            ', &
        'deepskyblue         ', &
        'dimgray             ', &
        'dimgrey             ', &
        'dodgerblue          ', &
        'firebrick           ', &
        'floralwhite         ', &
        'forestgreen         ', &
        'fuchsia             ', &
        'gainsboro           ', &
        'ghostwhite          ', &
        'gold                ', &
        'goldenrod           ', &
        'greenyellow         ', &
        'grey                ', &
        'honeydew            ', &
        'hotpink             ', &
        'indianred           ', &
        'ivory               ', &
        'khaki               ', &
        'lavender            ', &
        'lavenderblush       ', &
        'lawngreen           ', &
        'lemonchiffon        ', &
        'lightblue           ', &
        'lightcoral          ', &
        'lightcyan           ', &
        'lightgoldenrodyellow', &
        'lightgray           ', &
        'lightgreen          ', &
        'lightgrey           ', &
        'lightpink           ', &
        'lightsalmon         ', &
        'lightseagreen       ', &
        'lightskyblue        ', &
        'lightslategray      ', &
        'lightslategrey      ', &
        'lightsteelblue      ', &
        'lightyellow         ', &
        'limegreen           ', &
        'linen               ', &
        'mediumaquamarine    ', &
        'mediumblue          ', &
        'mediumorchid        ', &
        'mediumpurple        ', &
        'mediumseagreen      ', &
        'mediumslateblue     ', &
        'mediumspringgreen   ', &
        'mediumturquoise     ', &
        'mediumvioletred     ', &
        'midnightblue        ', &
        'mintcream           ', &
        'mistyrose           ', &
        'moccasin            ', &
        'navajowhite         ', &
        'oldlace             ', &
        'olivedrab           ', &
        'orangered           ', &
        'orchid              ', &
        'palegoldenrod       ', &
        'palegreen           ', &
        'paleturquoise       ', &
        'palevioletred       ', &
        'papayawhip          ', &
        'peachpuff           ', &
        'peru                ', &
        'plum                ', &
        'powderblue          ', &
        'rebeccapurple       ', &
        'rosybrown           ', &
        'royalblue           ', &
        'saddlebrown         ', &
        'salmon              ', &
        'sandybrown          ', &
        'seagreen            ', &
        'seashell            ', &
        'sienna              ', &
        'skyblue             ', &
        'slateblue           ', &
        'slategray           ', &
        'slategrey           ', &
        'snow                ', &
        'springgreen         ', &
        'steelblue           ', &
        'tan                 ', &
        'thistle             ', &
        'tomato              ', &
        'turquoise           ', &
        'violet              ', &
        'wheat               ', &
        'whitesmoke          ', &
        'yellowgreen         ', &
        'tab:blue            ', &
        'tab:orange          ', &
        'tab:green           ', &
        'tab:red             ', &
        'tab:purple          ', &
        'tab:brown           ', &
        'tab:pink            ', &
        'tab:gray            ', &
        'tab:olive           ', &
        'tab:cyan            ' &
    ]

    real(wp), parameter :: named_color_values(3, NUM_NAMED_COLORS) = reshape([ &
        255.0_wp/255.0_wp, 0.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! red
        0.0_wp/255.0_wp, 128.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! green
        0.0_wp/255.0_wp, 0.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! blue
        0.0_wp/255.0_wp, 255.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! cyan
        255.0_wp/255.0_wp, 0.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! magenta
        255.0_wp/255.0_wp, 255.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! yellow
        0.0_wp/255.0_wp, 0.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! black
        255.0_wp/255.0_wp, 255.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! white
        128.0_wp/255.0_wp, 128.0_wp/255.0_wp, 128.0_wp/255.0_wp, & ! gray
        255.0_wp/255.0_wp, 165.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! orange
        128.0_wp/255.0_wp, 0.0_wp/255.0_wp, 128.0_wp/255.0_wp, & ! purple
        165.0_wp/255.0_wp, 42.0_wp/255.0_wp, 42.0_wp/255.0_wp, & ! brown
        255.0_wp/255.0_wp, 192.0_wp/255.0_wp, 203.0_wp/255.0_wp, & ! pink
        128.0_wp/255.0_wp, 128.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! olive
        0.0_wp/255.0_wp, 0.0_wp/255.0_wp, 128.0_wp/255.0_wp, & ! navy
        0.0_wp/255.0_wp, 255.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! lime
        0.0_wp/255.0_wp, 128.0_wp/255.0_wp, 128.0_wp/255.0_wp, & ! teal
        192.0_wp/255.0_wp, 192.0_wp/255.0_wp, 192.0_wp/255.0_wp, & ! silver
        128.0_wp/255.0_wp, 0.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! maroon
        75.0_wp/255.0_wp, 0.0_wp/255.0_wp, 130.0_wp/255.0_wp, & ! indigo
        240.0_wp/255.0_wp, 248.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! aliceblue
        250.0_wp/255.0_wp, 235.0_wp/255.0_wp, 215.0_wp/255.0_wp, & ! antiquewhite
        0.0_wp/255.0_wp, 255.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! aqua
        127.0_wp/255.0_wp, 255.0_wp/255.0_wp, 212.0_wp/255.0_wp, & ! aquamarine
        240.0_wp/255.0_wp, 255.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! azure
        245.0_wp/255.0_wp, 245.0_wp/255.0_wp, 220.0_wp/255.0_wp, & ! beige
        255.0_wp/255.0_wp, 228.0_wp/255.0_wp, 196.0_wp/255.0_wp, & ! bisque
        255.0_wp/255.0_wp, 235.0_wp/255.0_wp, 205.0_wp/255.0_wp, & ! blanchedalmond
        138.0_wp/255.0_wp, 43.0_wp/255.0_wp, 226.0_wp/255.0_wp, & ! blueviolet
        222.0_wp/255.0_wp, 184.0_wp/255.0_wp, 135.0_wp/255.0_wp, & ! burlywood
        95.0_wp/255.0_wp, 158.0_wp/255.0_wp, 160.0_wp/255.0_wp, & ! cadetblue
        127.0_wp/255.0_wp, 255.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! chartreuse
        210.0_wp/255.0_wp, 105.0_wp/255.0_wp, 30.0_wp/255.0_wp, & ! chocolate
        255.0_wp/255.0_wp, 127.0_wp/255.0_wp, 80.0_wp/255.0_wp, & ! coral
        100.0_wp/255.0_wp, 149.0_wp/255.0_wp, 237.0_wp/255.0_wp, & ! cornflowerblue
        255.0_wp/255.0_wp, 248.0_wp/255.0_wp, 220.0_wp/255.0_wp, & ! cornsilk
        220.0_wp/255.0_wp, 20.0_wp/255.0_wp, 60.0_wp/255.0_wp, & ! crimson
        0.0_wp/255.0_wp, 0.0_wp/255.0_wp, 139.0_wp/255.0_wp, & ! darkblue
        0.0_wp/255.0_wp, 139.0_wp/255.0_wp, 139.0_wp/255.0_wp, & ! darkcyan
        184.0_wp/255.0_wp, 134.0_wp/255.0_wp, 11.0_wp/255.0_wp, & ! darkgoldenrod
        169.0_wp/255.0_wp, 169.0_wp/255.0_wp, 169.0_wp/255.0_wp, & ! darkgray
        0.0_wp/255.0_wp, 100.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! darkgreen
        169.0_wp/255.0_wp, 169.0_wp/255.0_wp, 169.0_wp/255.0_wp, & ! darkgrey
        189.0_wp/255.0_wp, 183.0_wp/255.0_wp, 107.0_wp/255.0_wp, & ! darkkhaki
        139.0_wp/255.0_wp, 0.0_wp/255.0_wp, 139.0_wp/255.0_wp, & ! darkmagenta
        85.0_wp/255.0_wp, 107.0_wp/255.0_wp, 47.0_wp/255.0_wp, & ! darkolivegreen
        255.0_wp/255.0_wp, 140.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! darkorange
        153.0_wp/255.0_wp, 50.0_wp/255.0_wp, 204.0_wp/255.0_wp, & ! darkorchid
        139.0_wp/255.0_wp, 0.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! darkred
        233.0_wp/255.0_wp, 150.0_wp/255.0_wp, 122.0_wp/255.0_wp, & ! darksalmon
        143.0_wp/255.0_wp, 188.0_wp/255.0_wp, 143.0_wp/255.0_wp, & ! darkseagreen
        72.0_wp/255.0_wp, 61.0_wp/255.0_wp, 139.0_wp/255.0_wp, & ! darkslateblue
        47.0_wp/255.0_wp, 79.0_wp/255.0_wp, 79.0_wp/255.0_wp, & ! darkslategray
        47.0_wp/255.0_wp, 79.0_wp/255.0_wp, 79.0_wp/255.0_wp, & ! darkslategrey
        0.0_wp/255.0_wp, 206.0_wp/255.0_wp, 209.0_wp/255.0_wp, & ! darkturquoise
        148.0_wp/255.0_wp, 0.0_wp/255.0_wp, 211.0_wp/255.0_wp, & ! darkviolet
        255.0_wp/255.0_wp, 20.0_wp/255.0_wp, 147.0_wp/255.0_wp, & ! deeppink
        0.0_wp/255.0_wp, 191.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! deepskyblue
        105.0_wp/255.0_wp, 105.0_wp/255.0_wp, 105.0_wp/255.0_wp, & ! dimgray
        105.0_wp/255.0_wp, 105.0_wp/255.0_wp, 105.0_wp/255.0_wp, & ! dimgrey
        30.0_wp/255.0_wp, 144.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! dodgerblue
        178.0_wp/255.0_wp, 34.0_wp/255.0_wp, 34.0_wp/255.0_wp, & ! firebrick
        255.0_wp/255.0_wp, 250.0_wp/255.0_wp, 240.0_wp/255.0_wp, & ! floralwhite
        34.0_wp/255.0_wp, 139.0_wp/255.0_wp, 34.0_wp/255.0_wp, & ! forestgreen
        255.0_wp/255.0_wp, 0.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! fuchsia
        220.0_wp/255.0_wp, 220.0_wp/255.0_wp, 220.0_wp/255.0_wp, & ! gainsboro
        248.0_wp/255.0_wp, 248.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! ghostwhite
        255.0_wp/255.0_wp, 215.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! gold
        218.0_wp/255.0_wp, 165.0_wp/255.0_wp, 32.0_wp/255.0_wp, & ! goldenrod
        173.0_wp/255.0_wp, 255.0_wp/255.0_wp, 47.0_wp/255.0_wp, & ! greenyellow
        128.0_wp/255.0_wp, 128.0_wp/255.0_wp, 128.0_wp/255.0_wp, & ! grey
        240.0_wp/255.0_wp, 255.0_wp/255.0_wp, 240.0_wp/255.0_wp, & ! honeydew
        255.0_wp/255.0_wp, 105.0_wp/255.0_wp, 180.0_wp/255.0_wp, & ! hotpink
        205.0_wp/255.0_wp, 92.0_wp/255.0_wp, 92.0_wp/255.0_wp, & ! indianred
        255.0_wp/255.0_wp, 255.0_wp/255.0_wp, 240.0_wp/255.0_wp, & ! ivory
        240.0_wp/255.0_wp, 230.0_wp/255.0_wp, 140.0_wp/255.0_wp, & ! khaki
        230.0_wp/255.0_wp, 230.0_wp/255.0_wp, 250.0_wp/255.0_wp, & ! lavender
        255.0_wp/255.0_wp, 240.0_wp/255.0_wp, 245.0_wp/255.0_wp, & ! lavenderblush
        124.0_wp/255.0_wp, 252.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! lawngreen
        255.0_wp/255.0_wp, 250.0_wp/255.0_wp, 205.0_wp/255.0_wp, & ! lemonchiffon
        173.0_wp/255.0_wp, 216.0_wp/255.0_wp, 230.0_wp/255.0_wp, & ! lightblue
        240.0_wp/255.0_wp, 128.0_wp/255.0_wp, 128.0_wp/255.0_wp, & ! lightcoral
        224.0_wp/255.0_wp, 255.0_wp/255.0_wp, 255.0_wp/255.0_wp, & ! lightcyan
        250.0_wp/255.0_wp, 250.0_wp/255.0_wp, 210.0_wp/255.0_wp, & ! lightgoldenrodyellow
        211.0_wp/255.0_wp, 211.0_wp/255.0_wp, 211.0_wp/255.0_wp, & ! lightgray
        144.0_wp/255.0_wp, 238.0_wp/255.0_wp, 144.0_wp/255.0_wp, & ! lightgreen
        211.0_wp/255.0_wp, 211.0_wp/255.0_wp, 211.0_wp/255.0_wp, & ! lightgrey
        255.0_wp/255.0_wp, 182.0_wp/255.0_wp, 193.0_wp/255.0_wp, & ! lightpink
        255.0_wp/255.0_wp, 160.0_wp/255.0_wp, 122.0_wp/255.0_wp, & ! lightsalmon
        32.0_wp/255.0_wp, 178.0_wp/255.0_wp, 170.0_wp/255.0_wp, & ! lightseagreen
        135.0_wp/255.0_wp, 206.0_wp/255.0_wp, 250.0_wp/255.0_wp, & ! lightskyblue
        119.0_wp/255.0_wp, 136.0_wp/255.0_wp, 153.0_wp/255.0_wp, & ! lightslategray
        119.0_wp/255.0_wp, 136.0_wp/255.0_wp, 153.0_wp/255.0_wp, & ! lightslategrey
        176.0_wp/255.0_wp, 196.0_wp/255.0_wp, 222.0_wp/255.0_wp, & ! lightsteelblue
        255.0_wp/255.0_wp, 255.0_wp/255.0_wp, 224.0_wp/255.0_wp, & ! lightyellow
        50.0_wp/255.0_wp, 205.0_wp/255.0_wp, 50.0_wp/255.0_wp, & ! limegreen
        250.0_wp/255.0_wp, 240.0_wp/255.0_wp, 230.0_wp/255.0_wp, & ! linen
        102.0_wp/255.0_wp, 205.0_wp/255.0_wp, 170.0_wp/255.0_wp, & ! mediumaquamarine
        0.0_wp/255.0_wp, 0.0_wp/255.0_wp, 205.0_wp/255.0_wp, & ! mediumblue
        186.0_wp/255.0_wp, 85.0_wp/255.0_wp, 211.0_wp/255.0_wp, & ! mediumorchid
        147.0_wp/255.0_wp, 112.0_wp/255.0_wp, 219.0_wp/255.0_wp, & ! mediumpurple
        60.0_wp/255.0_wp, 179.0_wp/255.0_wp, 113.0_wp/255.0_wp, & ! mediumseagreen
        123.0_wp/255.0_wp, 104.0_wp/255.0_wp, 238.0_wp/255.0_wp, & ! mediumslateblue
        0.0_wp/255.0_wp, 250.0_wp/255.0_wp, 154.0_wp/255.0_wp, & ! mediumspringgreen
        72.0_wp/255.0_wp, 209.0_wp/255.0_wp, 204.0_wp/255.0_wp, & ! mediumturquoise
        199.0_wp/255.0_wp, 21.0_wp/255.0_wp, 133.0_wp/255.0_wp, & ! mediumvioletred
        25.0_wp/255.0_wp, 25.0_wp/255.0_wp, 112.0_wp/255.0_wp, & ! midnightblue
        245.0_wp/255.0_wp, 255.0_wp/255.0_wp, 250.0_wp/255.0_wp, & ! mintcream
        255.0_wp/255.0_wp, 228.0_wp/255.0_wp, 225.0_wp/255.0_wp, & ! mistyrose
        255.0_wp/255.0_wp, 228.0_wp/255.0_wp, 181.0_wp/255.0_wp, & ! moccasin
        255.0_wp/255.0_wp, 222.0_wp/255.0_wp, 173.0_wp/255.0_wp, & ! navajowhite
        253.0_wp/255.0_wp, 245.0_wp/255.0_wp, 230.0_wp/255.0_wp, & ! oldlace
        107.0_wp/255.0_wp, 142.0_wp/255.0_wp, 35.0_wp/255.0_wp, & ! olivedrab
        255.0_wp/255.0_wp, 69.0_wp/255.0_wp, 0.0_wp/255.0_wp, & ! orangered
        218.0_wp/255.0_wp, 112.0_wp/255.0_wp, 214.0_wp/255.0_wp, & ! orchid
        238.0_wp/255.0_wp, 232.0_wp/255.0_wp, 170.0_wp/255.0_wp, & ! palegoldenrod
        152.0_wp/255.0_wp, 251.0_wp/255.0_wp, 152.0_wp/255.0_wp, & ! palegreen
        175.0_wp/255.0_wp, 238.0_wp/255.0_wp, 238.0_wp/255.0_wp, & ! paleturquoise
        219.0_wp/255.0_wp, 112.0_wp/255.0_wp, 147.0_wp/255.0_wp, & ! palevioletred
        255.0_wp/255.0_wp, 239.0_wp/255.0_wp, 213.0_wp/255.0_wp, & ! papayawhip
        255.0_wp/255.0_wp, 218.0_wp/255.0_wp, 185.0_wp/255.0_wp, & ! peachpuff
        205.0_wp/255.0_wp, 133.0_wp/255.0_wp, 63.0_wp/255.0_wp, & ! peru
        221.0_wp/255.0_wp, 160.0_wp/255.0_wp, 221.0_wp/255.0_wp, & ! plum
        176.0_wp/255.0_wp, 224.0_wp/255.0_wp, 230.0_wp/255.0_wp, & ! powderblue
        102.0_wp/255.0_wp, 51.0_wp/255.0_wp, 153.0_wp/255.0_wp, & ! rebeccapurple
        188.0_wp/255.0_wp, 143.0_wp/255.0_wp, 143.0_wp/255.0_wp, & ! rosybrown
        65.0_wp/255.0_wp, 105.0_wp/255.0_wp, 225.0_wp/255.0_wp, & ! royalblue
        139.0_wp/255.0_wp, 69.0_wp/255.0_wp, 19.0_wp/255.0_wp, & ! saddlebrown
        250.0_wp/255.0_wp, 128.0_wp/255.0_wp, 114.0_wp/255.0_wp, & ! salmon
        244.0_wp/255.0_wp, 164.0_wp/255.0_wp, 96.0_wp/255.0_wp, & ! sandybrown
        46.0_wp/255.0_wp, 139.0_wp/255.0_wp, 87.0_wp/255.0_wp, & ! seagreen
        255.0_wp/255.0_wp, 245.0_wp/255.0_wp, 238.0_wp/255.0_wp, & ! seashell
        160.0_wp/255.0_wp, 82.0_wp/255.0_wp, 45.0_wp/255.0_wp, & ! sienna
        135.0_wp/255.0_wp, 206.0_wp/255.0_wp, 235.0_wp/255.0_wp, & ! skyblue
        106.0_wp/255.0_wp, 90.0_wp/255.0_wp, 205.0_wp/255.0_wp, & ! slateblue
        112.0_wp/255.0_wp, 128.0_wp/255.0_wp, 144.0_wp/255.0_wp, & ! slategray
        112.0_wp/255.0_wp, 128.0_wp/255.0_wp, 144.0_wp/255.0_wp, & ! slategrey
        255.0_wp/255.0_wp, 250.0_wp/255.0_wp, 250.0_wp/255.0_wp, & ! snow
        0.0_wp/255.0_wp, 255.0_wp/255.0_wp, 127.0_wp/255.0_wp, & ! springgreen
        70.0_wp/255.0_wp, 130.0_wp/255.0_wp, 180.0_wp/255.0_wp, & ! steelblue
        210.0_wp/255.0_wp, 180.0_wp/255.0_wp, 140.0_wp/255.0_wp, & ! tan
        216.0_wp/255.0_wp, 191.0_wp/255.0_wp, 216.0_wp/255.0_wp, & ! thistle
        255.0_wp/255.0_wp, 99.0_wp/255.0_wp, 71.0_wp/255.0_wp, & ! tomato
        64.0_wp/255.0_wp, 224.0_wp/255.0_wp, 208.0_wp/255.0_wp, & ! turquoise
        238.0_wp/255.0_wp, 130.0_wp/255.0_wp, 238.0_wp/255.0_wp, & ! violet
        245.0_wp/255.0_wp, 222.0_wp/255.0_wp, 179.0_wp/255.0_wp, & ! wheat
        245.0_wp/255.0_wp, 245.0_wp/255.0_wp, 245.0_wp/255.0_wp, & ! whitesmoke
        154.0_wp/255.0_wp, 205.0_wp/255.0_wp, 50.0_wp/255.0_wp, & ! yellowgreen
        31.0_wp/255.0_wp, 119.0_wp/255.0_wp, 180.0_wp/255.0_wp, & ! tab:blue
        255.0_wp/255.0_wp, 127.0_wp/255.0_wp, 14.0_wp/255.0_wp, & ! tab:orange
        44.0_wp/255.0_wp, 160.0_wp/255.0_wp, 44.0_wp/255.0_wp, & ! tab:green
        214.0_wp/255.0_wp, 39.0_wp/255.0_wp, 40.0_wp/255.0_wp, & ! tab:red
        148.0_wp/255.0_wp, 103.0_wp/255.0_wp, 189.0_wp/255.0_wp, & ! tab:purple
        140.0_wp/255.0_wp, 86.0_wp/255.0_wp, 75.0_wp/255.0_wp, & ! tab:brown
        227.0_wp/255.0_wp, 119.0_wp/255.0_wp, 194.0_wp/255.0_wp, & ! tab:pink
        127.0_wp/255.0_wp, 127.0_wp/255.0_wp, 127.0_wp/255.0_wp, & ! tab:gray
        188.0_wp/255.0_wp, 189.0_wp/255.0_wp, 34.0_wp/255.0_wp, & ! tab:olive
        23.0_wp/255.0_wp, 190.0_wp/255.0_wp, 207.0_wp/255.0_wp & ! tab:cyan
    ], [3, NUM_NAMED_COLORS])

    ! Single letter color mapping (matplotlib compatible)
    character(len=1), parameter :: single_letters(8) = ['r', 'g', 'b', 'c', 'm', 'y', 'k', 'w']
    integer, parameter :: letter_to_named(8) = [1, 2, 3, 4, 5, 6, 7, 8]

contains

    function clamp_to_unit(value) result(clamped)
        !! Clamp value to [0,1] range
        real(wp), intent(in) :: value
        real(wp) :: clamped

        clamped = max(0.0_wp, min(1.0_wp, value))
    end function clamp_to_unit

    subroutine to_lowercase(str)
        !! Convert string to lowercase in-place
        character(len=:), allocatable, intent(inout) :: str
        integer :: i

        do i = 1, len(str)
            call to_lowercase_char(str(i:i))
        end do
    end subroutine to_lowercase

    subroutine to_lowercase_char(char)
        !! Convert single character to lowercase
        character(len=1), intent(inout) :: char

        if (char >= 'A' .and. char <= 'Z') then
            char = achar(iachar(char) + 32)
        end if
    end subroutine to_lowercase_char

end module fortplot_color_definitions
