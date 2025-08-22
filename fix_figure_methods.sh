#!/bin/bash

# Fix figure type-bound method calls in fortplot.f90

sed -i 's/call initialize_figure(fig,/call fig%initialize(/g' src/fortplot.f90
sed -i 's/call set_xlabel(fig,/call fig%set_xlabel(/g' src/fortplot.f90
sed -i 's/call set_ylabel(fig,/call fig%set_ylabel(/g' src/fortplot.f90
sed -i 's/call set_title(fig,/call fig%set_title(/g' src/fortplot.f90
sed -i 's/call set_xlim(fig,/call fig%set_xlim(/g' src/fortplot.f90
sed -i 's/call set_ylim(fig,/call fig%set_ylim(/g' src/fortplot.f90
sed -i 's/call set_xscale(fig,/call fig%set_xscale(/g' src/fortplot.f90
sed -i 's/call set_yscale(fig,/call fig%set_yscale(/g' src/fortplot.f90
sed -i 's/call set_line_width(fig,/call fig%set_line_width(/g' src/fortplot.f90

# Fix calls with empty argument lists (initialize)
sed -i 's/call fig%initialize( )/call fig%initialize()/g' src/fortplot.f90

echo "Fixed figure type-bound method calls"