import numpy as np
import matplotlib.pyplot as plt

# Generate data
x = np.arange(0, 100) / 5.0
sx = np.sin(x)
cx = np.cos(x)

# Create PNG figure
plt.plot(x, sx, label='sin(x)')
plt.plot(x, cx, label='cos(x)')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Sine and Cosine Functions')
plt.legend()
plt.savefig('output_pyplot.png')
plt.close()

# Create PDF figure
plt.plot(x, sx, label='sin(x)')
plt.plot(x, cx, label='cos(x)')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Sine and Cosine Functions')
plt.legend()
plt.savefig('output_pyplot.pdf')
plt.close()
