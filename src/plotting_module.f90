module plotting_module
    implicit none
    
    private
    public :: plot_context, setup_canvas, draw_sine_wave, draw_coordinate_axes, save_plot
    
    type, abstract :: plot_context
        integer :: width, height
        real :: x_min, x_max, y_min, y_max
    contains
        procedure(line_interface), deferred :: line
        procedure(color_interface), deferred :: color
        procedure(text_interface), deferred :: text
        procedure(save_interface), deferred :: save
    end type plot_context
    
    abstract interface
        subroutine line_interface(this, x1, y1, x2, y2)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: x1, y1, x2, y2
        end subroutine line_interface
        
        subroutine color_interface(this, r, g, b)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: r, g, b
        end subroutine color_interface
        
        subroutine text_interface(this, x, y, text)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: x, y
            character(len=*), intent(in) :: text
        end subroutine text_interface
        
        subroutine save_interface(this, filename)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine save_interface
    end interface
    
contains

    subroutine setup_canvas(ctx, width, height)
        class(plot_context), intent(inout) :: ctx
        integer, intent(in) :: width, height
        
        ctx%width = width
        ctx%height = height
        ctx%x_min = -1.0
        ctx%x_max = 1.0
        ctx%y_min = -1.0
        ctx%y_max = 1.0
    end subroutine setup_canvas
    
    subroutine draw_sine_wave(ctx, amplitude, cycles)
        class(plot_context), intent(inout) :: ctx
        real, intent(in) :: amplitude
        integer, intent(in) :: cycles
        
        call ctx%color(0.0, 0.0, 1.0)
        call render_parametric_curve(ctx, amplitude, cycles)
    end subroutine draw_sine_wave
    
    subroutine draw_coordinate_axes(ctx)
        class(plot_context), intent(inout) :: ctx
        
        call ctx%color(0.25, 0.25, 0.25)
        call draw_axes_lines(ctx)
        call draw_axis_ticks(ctx)
        call draw_axis_labels(ctx)
    end subroutine draw_coordinate_axes
    
    subroutine save_plot(ctx, filename)
        class(plot_context), intent(inout) :: ctx
        character(len=*), intent(in) :: filename
        
        call ctx%save(filename)
    end subroutine save_plot

    subroutine render_parametric_curve(ctx, amplitude, cycles)
        class(plot_context), intent(inout) :: ctx
        real, intent(in) :: amplitude
        integer, intent(in) :: cycles
        real :: x_prev, y_prev, x_curr, y_curr
        real :: t, dt, pi, omega
        integer :: i, steps
        
        pi = 4.0 * atan(1.0)
        omega = real(cycles) * 2.0 * pi
        steps = ctx%width * 2
        dt = omega / real(steps - 1)
        
        call calculate_first_point(0.0, amplitude, omega, x_prev, y_prev)
        
        do i = 2, steps
            t = real(i - 1) * dt
            call calculate_curve_point(t, amplitude, omega, x_curr, y_curr)
            call ctx%line(x_prev, y_prev, x_curr, y_curr)
            x_prev = x_curr
            y_prev = y_curr
        end do
    end subroutine render_parametric_curve

    subroutine calculate_first_point(t, amplitude, omega, x, y)
        real, intent(in) :: t, amplitude, omega
        real, intent(out) :: x, y
        
        x = (t / omega) * 2.0 - 1.0
        y = amplitude * sin(t)
    end subroutine calculate_first_point

    subroutine calculate_curve_point(t, amplitude, omega, x, y)
        real, intent(in) :: t, amplitude, omega
        real, intent(out) :: x, y
        
        x = (t / omega) * 2.0 - 1.0
        y = amplitude * sin(t)
    end subroutine calculate_curve_point

    subroutine draw_axes_lines(ctx)
        class(plot_context), intent(inout) :: ctx
        
        call ctx%line(ctx%x_min, 0.0, ctx%x_max, 0.0)
        call ctx%line(0.0, ctx%y_min, 0.0, ctx%y_max)
    end subroutine draw_axes_lines

    subroutine draw_axis_ticks(ctx)
        class(plot_context), intent(inout) :: ctx
        real :: tick_size, pos
        integer :: i
        
        tick_size = 0.02
        
        do i = -4, 4
            if (i == 0) cycle
            pos = real(i) * 0.25
            if (abs(pos) <= 1.0) then
                call ctx%line(pos, -tick_size, pos, tick_size)
                call ctx%line(-tick_size, pos, tick_size, pos)
            end if
        end do
    end subroutine draw_axis_ticks

    subroutine draw_axis_labels(ctx)
        class(plot_context), intent(inout) :: ctx
        real :: pos, label_offset
        integer :: i
        character(len=10) :: label_text
        
        label_offset = 0.08
        
        do i = -4, 4
            if (i == 0) cycle
            pos = real(i) * 0.25
            if (abs(pos) <= 1.0) then
                write(label_text, '(F5.2)') pos
                call ctx%text(pos, -label_offset, trim(adjustl(label_text)))
                if (abs(pos) > 0.01) then
                    call ctx%text(-label_offset, pos, trim(adjustl(label_text)))
                end if
            end if
        end do
    end subroutine draw_axis_labels

end module plotting_module