#!/bin/bash

# Fix all qualified function calls in fortplot.f90 wrapper functions

sed -i 's/call add_plot(fig,/call figure_add_plot(fig,/g' src/fortplot.f90
sed -i 's/call add_contour(fig,/call figure_add_contour(fig,/g' src/fortplot.f90
sed -i 's/call add_contour_filled(fig,/call figure_add_contour_filled(fig,/g' src/fortplot.f90
sed -i 's/call add_pcolormesh(fig,/call figure_add_pcolormesh(fig,/g' src/fortplot.f90
sed -i 's/call streamplot(fig,/call figure_streamplot(fig,/g' src/fortplot.f90
sed -i 's/call bar(fig,/call figure_bar(fig,/g' src/fortplot.f90
sed -i 's/call barh(fig,/call figure_barh(fig,/g' src/fortplot.f90
sed -i 's/call hist(fig,/call figure_hist(fig,/g' src/fortplot.f90
sed -i 's/call errorbar(fig,/call figure_errorbar(fig,/g' src/fortplot.f90
sed -i 's/call add_3d_plot(fig,/call figure_add_3d_plot(fig,/g' src/fortplot.f90
sed -i 's/call add_surface(fig,/call figure_add_surface(fig,/g' src/fortplot.f90
sed -i 's/call savefig(fig,/call figure_savefig(fig,/g' src/fortplot.f90

echo "Fixed all qualified function calls"