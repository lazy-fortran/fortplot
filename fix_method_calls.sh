#!/bin/bash

# Fix all fig% method calls in fortplot.f90

sed -i 's/call fig%add_plot(/call add_plot(fig, /g' src/fortplot.f90
sed -i 's/call fig%add_contour(/call add_contour(fig, /g' src/fortplot.f90
sed -i 's/call fig%add_contour_filled(/call add_contour_filled(fig, /g' src/fortplot.f90
sed -i 's/call fig%add_pcolormesh(/call add_pcolormesh(fig, /g' src/fortplot.f90
sed -i 's/call fig%streamplot(/call streamplot(fig, /g' src/fortplot.f90
sed -i 's/call fig%bar(/call bar(fig, /g' src/fortplot.f90
sed -i 's/call fig%barh(/call barh(fig, /g' src/fortplot.f90
sed -i 's/call fig%hist(/call hist(fig, /g' src/fortplot.f90
sed -i 's/call fig%errorbar(/call errorbar(fig, /g' src/fortplot.f90
sed -i 's/call fig%show(/call show(fig, /g' src/fortplot.f90
sed -i 's/call fig%initialize(/call initialize_figure(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_xlabel(/call set_xlabel(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_ylabel(/call set_ylabel(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_title(/call set_title(fig, /g' src/fortplot.f90
sed -i 's/call fig%legend(/call figure_legend(fig, /g' src/fortplot.f90
sed -i 's/call fig%savefig(/call savefig(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_xlim(/call set_xlim(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_ylim(/call set_ylim(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_xscale(/call set_xscale(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_yscale(/call set_yscale(fig, /g' src/fortplot.f90
sed -i 's/call fig%set_line_width(/call set_line_width(fig, /g' src/fortplot.f90

echo "Fixed all method calls"