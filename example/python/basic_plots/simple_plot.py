import numpy as np
import fortplotlib.fortplot as plt

x = np.linspace(0,1,100)
y = np.cos(x)

plt.figure(640, 480)
plt.plot(x, y, 'simple')
plt.title('simple')
plt.xlabel('x')
plt.ylabel('y')
plt.savefig('simple.pdf')
plt.savefig('simple.png')
plt.savefig('simple.txt')
