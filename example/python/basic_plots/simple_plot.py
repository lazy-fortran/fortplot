import numpy as np
import _fortplotlib

x = np.linspace(0,1,100)
y = np.cos(x)

_fortplotlib.fortplot.figure(640, 480)
_fortplotlib.fortplot.plot(x, y, 'simple')
_fortplotlib.fortplot.title('simple')
_fortplotlib.fortplot.xlabel('x')
_fortplotlib.fortplot.ylabel('y')
_fortplotlib.fortplot.savefig('simple.pdf')
_fortplotlib.fortplot.savefig('simple.png')
_fortplotlib.fortplot.savefig('simple.txt')
