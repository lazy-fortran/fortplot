#!/usr/bin/env python3
"""
Analyze matplotlib's exact tick and axis label positioning
Following TDD: First understand what matplotlib actually does
"""

import matplotlib.pyplot as plt
import numpy as np

def analyze_matplotlib_positioning():
    """Create a figure and analyze exact text positioning"""
    
    # Create exactly the same plot as our Fortran basic_plots
    x = np.arange(50) * 4.0 * np.pi / 49.0
    y = np.sin(x)
    
    fig, ax = plt.subplots(figsize=(6.4, 4.8), dpi=100)  # Exactly 640x480
    ax.plot(x, y, label='sin(x)')
    ax.set_title('Simple Sine Wave')
    ax.set_xlabel('x')
    ax.set_ylabel('sin(x)')
    
    # Get the figure's renderer and exact positions
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    
    print("=== Matplotlib Positioning Analysis ===")
    print(f"Figure size: {fig.get_size_inches()} inches")
    print(f"Figure DPI: {fig.dpi}")
    actual_width = int(fig.get_size_inches()[0]*fig.dpi)
    actual_height = int(fig.get_size_inches()[1]*fig.dpi)
    print(f"Actual pixels: {actual_width}x{actual_height}")
    
    # Force to 640x480 if different
    if actual_width != 640 or actual_height != 480:
        print("Adjusting to exactly 640x480...")
        fig.set_size_inches(640/fig.dpi, 480/fig.dpi)
        fig.canvas.draw()  # Redraw with new size
        actual_width, actual_height = 640, 480
    
    # Get axes position in figure coordinates
    bbox = ax.get_position()
    fig_width, fig_height = fig.get_size_inches() * fig.dpi
    
    # Convert to pixel coordinates
    plot_left = bbox.x0 * fig_width
    plot_bottom = bbox.y0 * fig_height  
    plot_width = bbox.width * fig_width
    plot_height = bbox.height * fig_height
    plot_right = plot_left + plot_width
    plot_top = plot_bottom + plot_height
    
    print(f"\nPlot area in pixels:")
    print(f"  Left: {plot_left:.0f}")
    print(f"  Right: {plot_right:.0f}")
    print(f"  Bottom: {plot_bottom:.0f} (matplotlib Y-up)")
    print(f"  Top: {plot_top:.0f}")
    print(f"  Width: {plot_width:.0f}")
    print(f"  Height: {plot_height:.0f}")
    
    # Convert to PNG coordinates (Y-down)
    png_plot_bottom = fig_height - plot_top
    png_plot_top = fig_height - plot_bottom
    
    print(f"\nPlot area in PNG coordinates (Y-down):")
    print(f"  Left: {plot_left:.0f}")
    print(f"  Right: {plot_right:.0f}")
    print(f"  Bottom: {png_plot_bottom:.0f}")
    print(f"  Top: {png_plot_top:.0f}")
    
    # Analyze tick label positioning
    print(f"\n=== TICK LABELS ===")
    
    # X-axis tick labels
    x_ticklabels = ax.get_xticklabels()
    if x_ticklabels:
        # Get first tick label for analysis
        tick_label = x_ticklabels[0]
        tick_pos = tick_label.get_position()
        tick_bbox = tick_label.get_window_extent(renderer)
        
        print(f"X-tick label '{tick_label.get_text()}':")
        print(f"  Position (data coords): {tick_pos}")
        print(f"  Bbox (pixels): x={tick_bbox.x0:.0f}-{tick_bbox.x1:.0f}, y={tick_bbox.y0:.0f}-{tick_bbox.y1:.0f}")
        print(f"  Distance from plot bottom: {tick_bbox.y1 - png_plot_top:.0f} pixels")
        
    # Y-axis tick labels  
    y_ticklabels = ax.get_yticklabels()
    if y_ticklabels:
        tick_label = y_ticklabels[len(y_ticklabels)//2]  # Middle tick
        tick_pos = tick_label.get_position()
        tick_bbox = tick_label.get_window_extent(renderer)
        
        print(f"\nY-tick label '{tick_label.get_text()}':")
        print(f"  Position (data coords): {tick_pos}")
        print(f"  Bbox (pixels): x={tick_bbox.x0:.0f}-{tick_bbox.x1:.0f}, y={tick_bbox.y0:.0f}-{tick_bbox.y1:.0f}")
        print(f"  Distance from plot left: {plot_left - tick_bbox.x1:.0f} pixels")
    
    # Analyze axis label positioning
    print(f"\n=== AXIS LABELS ===")
    
    # X-axis label
    xlabel = ax.get_xlabel()
    xlabel_obj = ax.xaxis.label
    xlabel_bbox = xlabel_obj.get_window_extent(renderer)
    
    print(f"X-axis label '{xlabel}':")
    print(f"  Bbox (pixels): x={xlabel_bbox.x0:.0f}-{xlabel_bbox.x1:.0f}, y={xlabel_bbox.y0:.0f}-{xlabel_bbox.y1:.0f}")
    print(f"  Center X: {(xlabel_bbox.x0 + xlabel_bbox.x1)/2:.0f} (plot center: {(plot_left + plot_right)/2:.0f})")
    print(f"  Distance from plot bottom: {xlabel_bbox.y1 - png_plot_top:.0f} pixels")
    
    # Y-axis label
    ylabel = ax.get_ylabel()
    ylabel_obj = ax.yaxis.label
    ylabel_bbox = ylabel_obj.get_window_extent(renderer)
    
    print(f"\nY-axis label '{ylabel}':")
    print(f"  Bbox (pixels): x={ylabel_bbox.x0:.0f}-{ylabel_bbox.x1:.0f}, y={ylabel_bbox.y0:.0f}-{ylabel_bbox.y1:.0f}")
    print(f"  Center Y: {(ylabel_bbox.y0 + ylabel_bbox.y1)/2:.0f} (plot center: {(png_plot_bottom + png_plot_top)/2:.0f})")
    print(f"  Distance from plot left: {plot_left - ylabel_bbox.x1:.0f} pixels")
    
    # Save for visual verification
    plt.savefig('test/matplotlib_positioning_analysis.png', dpi=100)
    print(f"\nSaved analysis plot to test/matplotlib_positioning_analysis.png")
    
    plt.close()

if __name__ == "__main__":
    analyze_matplotlib_positioning()