module fortplot_colormap
    !! Colormap functionality for contour plots
    !! Provides color interpolation for different colormaps like matplotlib
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_colormap_color, colormap_value_to_color, validate_colormap_name
    
contains

    subroutine get_colormap_color(value, colormap, color)
        !! Get RGB color from colormap for a normalized value [0,1]
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: colormap
        real(wp), dimension(3), intent(out) :: color
        
        real(wp) :: t
        
        ! Clamp value to [0,1]
        t = max(0.0_wp, min(1.0_wp, value))
        
        select case (trim(colormap))
        case ('seaborn', 'colorblind', 'crest')
            call crest_colormap(t, color)
        case ('viridis')
            call viridis_colormap(t, color)
        case ('plasma')
            call plasma_colormap(t, color)
        case ('inferno')
            call inferno_colormap(t, color)
        case ('coolwarm')
            call coolwarm_colormap(t, color)
        case ('jet')
            call jet_colormap(t, color)
        case default
            call crest_colormap(t, color)  ! Use colorblind-friendly as default
        end select
    end subroutine get_colormap_color

    subroutine colormap_value_to_color(z_value, z_min, z_max, colormap, color)
        !! Convert data value to RGB color using colormap
        real(wp), intent(in) :: z_value, z_min, z_max
        character(len=*), intent(in) :: colormap
        real(wp), dimension(3), intent(out) :: color
        
        real(wp) :: normalized_value
        
        if (abs(z_max - z_min) < 1e-10_wp) then
            normalized_value = 0.5_wp
        else
            normalized_value = (z_value - z_min) / (z_max - z_min)
        end if
        
        call get_colormap_color(normalized_value, colormap, color)
    end subroutine colormap_value_to_color

    subroutine viridis_colormap(t, color)
        !! Viridis colormap implementation (matplotlib-like)
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        ! Viridis color control points (simplified)
        real(wp), dimension(5) :: r_points = [0.267_wp, 0.229_wp, 0.173_wp, 0.329_wp, 0.993_wp]
        real(wp), dimension(5) :: g_points = [0.005_wp, 0.322_wp, 0.531_wp, 0.758_wp, 0.906_wp]
        real(wp), dimension(5) :: b_points = [0.329_wp, 0.545_wp, 0.737_wp, 0.525_wp, 0.144_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine viridis_colormap

    subroutine plasma_colormap(t, color)
        !! Plasma colormap implementation
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(5) :: r_points = [0.050_wp, 0.504_wp, 0.783_wp, 0.947_wp, 0.940_wp]
        real(wp), dimension(5) :: g_points = [0.030_wp, 0.063_wp, 0.216_wp, 0.542_wp, 0.975_wp]
        real(wp), dimension(5) :: b_points = [0.528_wp, 0.614_wp, 0.631_wp, 0.457_wp, 0.131_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine plasma_colormap

    subroutine inferno_colormap(t, color)
        !! Inferno colormap implementation
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(5) :: r_points = [0.001_wp, 0.258_wp, 0.642_wp, 0.936_wp, 0.988_wp]
        real(wp), dimension(5) :: g_points = [0.000_wp, 0.021_wp, 0.179_wp, 0.493_wp, 0.645_wp]
        real(wp), dimension(5) :: b_points = [0.014_wp, 0.146_wp, 0.225_wp, 0.151_wp, 0.041_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine inferno_colormap


    subroutine coolwarm_colormap(t, color)
        !! Cool-warm diverging colormap
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(3) :: r_points = [0.230_wp, 0.865_wp, 0.706_wp]
        real(wp), dimension(3) :: g_points = [0.299_wp, 0.865_wp, 0.016_wp]
        real(wp), dimension(3) :: b_points = [0.754_wp, 0.865_wp, 0.150_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine coolwarm_colormap

    subroutine jet_colormap(t, color)
        !! Jet colormap (classic rainbow)
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(5) :: r_points = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.5_wp]
        real(wp), dimension(5) :: g_points = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp]
        real(wp), dimension(5) :: b_points = [0.5_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine jet_colormap

    subroutine crest_colormap(t, color)
        !! Crest colormap - seaborn colorblind-friendly sequential colormap
        !! Light blue to dark blue, perceptually uniform and colorblind-safe
        real(wp), intent(in) :: t
        real(wp), dimension(3), intent(out) :: color
        
        real(wp), dimension(5) :: r_points = [0.855_wp, 0.627_wp, 0.373_wp, 0.188_wp, 0.063_wp]
        real(wp), dimension(5) :: g_points = [0.929_wp, 0.851_wp, 0.671_wp, 0.447_wp, 0.282_wp]
        real(wp), dimension(5) :: b_points = [0.941_wp, 0.918_wp, 0.847_wp, 0.698_wp, 0.502_wp]
        
        call interpolate_colormap(t, r_points, g_points, b_points, color)
    end subroutine crest_colormap




    subroutine interpolate_colormap(t, r_points, g_points, b_points, color)
        !! Interpolate between color control points
        real(wp), intent(in) :: t
        real(wp), dimension(:), intent(in) :: r_points, g_points, b_points
        real(wp), dimension(3), intent(out) :: color
        
        integer :: n_points, i
        real(wp) :: dt, weight, t_scaled
        
        n_points = size(r_points)
        
        if (n_points == 1) then
            color = [r_points(1), g_points(1), b_points(1)]
            return
        end if
        
        ! Scale t to [0, n_points-1]
        t_scaled = t * real(n_points - 1, wp)
        i = int(t_scaled) + 1
        
        if (i >= n_points) then
            color = [r_points(n_points), g_points(n_points), b_points(n_points)]
        else if (i <= 1) then
            color = [r_points(1), g_points(1), b_points(1)]
        else
            dt = t_scaled - real(i - 1, wp)
            weight = dt
            
            color(1) = r_points(i) * (1.0_wp - weight) + r_points(i + 1) * weight
            color(2) = g_points(i) * (1.0_wp - weight) + g_points(i + 1) * weight
            color(3) = b_points(i) * (1.0_wp - weight) + b_points(i + 1) * weight
        end if
    end subroutine interpolate_colormap

    pure function validate_colormap_name(colormap) result(is_valid)
        !! Validate if colormap name is supported
        character(len=*), intent(in) :: colormap
        logical :: is_valid
        
        select case (trim(colormap))
        case ('seaborn', 'colorblind', 'crest', 'viridis', 'plasma', 'inferno', &
              'coolwarm', 'jet')
            is_valid = .true.
        case default
            is_valid = .false.
        end select
    end function validate_colormap_name

end module fortplot_colormap