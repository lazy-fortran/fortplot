module fortplot_color_conversions
    !! Color space conversions and colormap functionality
    !! 
    !! Provides:
    !! - RGB to HSV/LAB color space conversions
    !! - Colormap application to data arrays
    !! - Built-in colormaps: viridis, plasma, coolwarm
    !! - Efficient array-based colormap operations
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_color_definitions, only: clamp_to_unit, to_lowercase
    implicit none
    
    private
    public :: rgb_to_hsv, rgb_to_lab
    public :: apply_colormap_to_array

contains

    ! Color space conversions
    subroutine rgb_to_hsv(rgb, hsv)
        !! Convert RGB to HSV color space
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(out) :: hsv(3)
        
        real(wp) :: r, g, b, max_val, min_val, delta
        
        r = rgb(1)
        g = rgb(2)
        b = rgb(3)
        
        max_val = max(r, max(g, b))
        min_val = min(r, min(g, b))
        delta = max_val - min_val
        
        ! Value
        hsv(3) = max_val
        
        ! Saturation
        if (max_val > 0.0_wp) then
            hsv(2) = delta / max_val
        else
            hsv(2) = 0.0_wp
        end if
        
        ! Hue
        if (delta == 0.0_wp) then
            hsv(1) = 0.0_wp
        else if (max_val == r) then
            hsv(1) = 60.0_wp * modulo((g - b) / delta, 6.0_wp)
        else if (max_val == g) then
            hsv(1) = 60.0_wp * ((b - r) / delta + 2.0_wp)
        else
            hsv(1) = 60.0_wp * ((r - g) / delta + 4.0_wp)
        end if
    end subroutine rgb_to_hsv
    
    subroutine rgb_to_lab(rgb, lab)
        !! Convert RGB to LAB color space (simplified implementation)
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(out) :: lab(3)
        
        real(wp) :: xyz(3)
        
        ! First convert RGB to XYZ, then XYZ to LAB
        call rgb_to_xyz(rgb, xyz)
        call xyz_to_lab(xyz, lab)
    end subroutine rgb_to_lab
    
    subroutine rgb_to_xyz(rgb, xyz)
        !! Convert RGB to XYZ color space
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(out) :: xyz(3)
        
        real(wp) :: r, g, b
        
        ! Gamma correction
        r = gamma_correct(rgb(1))
        g = gamma_correct(rgb(2))
        b = gamma_correct(rgb(3))
        
        ! sRGB to XYZ transformation matrix
        xyz(1) = 0.4124564_wp * r + 0.3575761_wp * g + 0.1804375_wp * b
        xyz(2) = 0.2126729_wp * r + 0.7151522_wp * g + 0.0721750_wp * b
        xyz(3) = 0.0193339_wp * r + 0.1191920_wp * g + 0.9503041_wp * b
    end subroutine rgb_to_xyz
    
    function gamma_correct(channel) result(corrected)
        !! Apply gamma correction for sRGB
        real(wp), intent(in) :: channel
        real(wp) :: corrected
        
        if (channel <= 0.04045_wp) then
            corrected = channel / 12.92_wp
        else
            corrected = ((channel + 0.055_wp) / 1.055_wp)**2.4_wp
        end if
    end function gamma_correct
    
    subroutine xyz_to_lab(xyz, lab)
        !! Convert XYZ to LAB color space
        real(wp), intent(in) :: xyz(3)
        real(wp), intent(out) :: lab(3)
        
        real(wp), parameter :: XN = 0.95047_wp  ! D65 illuminant
        real(wp), parameter :: YN = 1.00000_wp
        real(wp), parameter :: ZN = 1.08883_wp
        
        real(wp) :: fx, fy, fz
        
        fx = lab_f(xyz(1) / XN)
        fy = lab_f(xyz(2) / YN)
        fz = lab_f(xyz(3) / ZN)
        
        lab(1) = 116.0_wp * fy - 16.0_wp  ! L*
        lab(2) = 500.0_wp * (fx - fy)     ! a*
        lab(3) = 200.0_wp * (fy - fz)     ! b*
    end subroutine xyz_to_lab
    
    function lab_f(t) result(f_val)
        !! LAB conversion helper function
        real(wp), intent(in) :: t
        real(wp) :: f_val
        
        real(wp), parameter :: DELTA = 6.0_wp / 29.0_wp
        
        if (t > DELTA**3) then
            f_val = t**(1.0_wp/3.0_wp)
        else
            f_val = t / (3.0_wp * DELTA**2) + 4.0_wp / 29.0_wp
        end if
    end function lab_f
    
    ! Colormap application for large arrays
    subroutine apply_colormap_to_array(values, colormap, rgb_mapped)
        !! Apply colormap to array of values efficiently
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in) :: colormap
        real(wp), intent(out) :: rgb_mapped(:,:)
        
        integer :: i, n_points
        real(wp) :: val_min, val_max, normalized_val
        
        n_points = size(values)
        val_min = minval(values)
        val_max = maxval(values)
        
        ! Avoid division by zero
        if (val_max == val_min) then
            rgb_mapped = 0.5_wp  ! Mid-gray for uniform data
            return
        end if
        
        do i = 1, n_points
            normalized_val = (values(i) - val_min) / (val_max - val_min)
            call apply_colormap_value(normalized_val, colormap, rgb_mapped(:, i))
        end do
    end subroutine apply_colormap_to_array
    
    subroutine apply_colormap_value(normalized_val, colormap, rgb)
        !! Apply colormap to single normalized value [0,1]
        real(wp), intent(in) :: normalized_val
        character(len=*), intent(in) :: colormap
        real(wp), intent(out) :: rgb(3)
        
        character(len=:), allocatable :: cmap_lower
        real(wp) :: t
        
        t = clamp_to_unit(normalized_val)
        cmap_lower = trim(colormap)
        call to_lowercase(cmap_lower)
        
        select case (cmap_lower)
        case ('viridis')
            call viridis_colormap(t, rgb)
        case ('plasma')
            call plasma_colormap(t, rgb)
        case ('coolwarm')
            call coolwarm_colormap(t, rgb)
        case default
            ! Default to simple grayscale
            rgb = [t, t, t]
        end select
    end subroutine apply_colormap_value
    
    subroutine viridis_colormap(t, rgb)
        !! Simplified viridis colormap
        real(wp), intent(in) :: t
        real(wp), intent(out) :: rgb(3)
        
        ! Simplified viridis approximation
        rgb(1) = 0.267004_wp + t * (0.993248_wp - 0.267004_wp)  ! Purple to yellow
        rgb(2) = 0.004874_wp + t * (0.906157_wp - 0.004874_wp)
        rgb(3) = 0.329415_wp + t * (0.143936_wp - 0.329415_wp)
    end subroutine viridis_colormap
    
    subroutine plasma_colormap(t, rgb)
        !! Simplified plasma colormap  
        real(wp), intent(in) :: t
        real(wp), intent(out) :: rgb(3)
        
        rgb(1) = 0.050383_wp + t * (0.940015_wp - 0.050383_wp)  ! Dark to bright
        rgb(2) = 0.029803_wp + t * (0.975158_wp - 0.029803_wp)
        rgb(3) = 0.527975_wp + t * (0.131326_wp - 0.527975_wp)
    end subroutine plasma_colormap
    
    subroutine coolwarm_colormap(t, rgb)
        !! Coolwarm diverging colormap
        real(wp), intent(in) :: t
        real(wp), intent(out) :: rgb(3)
        
        if (t < 0.5_wp) then
            ! Cool side (blue to white)
            rgb(1) = 0.230_wp + 2.0_wp * t * (1.0_wp - 0.230_wp)
            rgb(2) = 0.299_wp + 2.0_wp * t * (1.0_wp - 0.299_wp)
            rgb(3) = 0.754_wp + 2.0_wp * t * (1.0_wp - 0.754_wp)
        else
            ! Warm side (white to red)
            rgb(1) = 1.0_wp + 2.0_wp * (t - 0.5_wp) * (0.706_wp - 1.0_wp)
            rgb(2) = 1.0_wp + 2.0_wp * (t - 0.5_wp) * (0.016_wp - 1.0_wp)
            rgb(3) = 1.0_wp + 2.0_wp * (t - 0.5_wp) * (0.150_wp - 1.0_wp)
        end if
    end subroutine coolwarm_colormap

end module fortplot_color_conversions