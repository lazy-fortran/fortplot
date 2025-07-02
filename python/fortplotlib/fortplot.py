import os
import platform
import numpy as np
import fortplotlib._fortplotlib as _fortplotlib

DEFAULT_DPI = 100

def figure(figsize=[6.4, 4.8]):
    width = int(figsize[0] * DEFAULT_DPI)
    height = int(figsize[1] * DEFAULT_DPI)
    _fortplotlib.fortplot.figure(width, height)

def plot(x, y, linestyle="-", label=""):
    if not isinstance(x, np.ndarray):
        x = np.array(x)
    if not isinstance(y, np.ndarray):
        y = np.array(y)
    _fortplotlib.fortplot.plot(x, y, linestyle, label)

def title(label):
    if label is None:
        label = ""
    _fortplotlib.fortplot.title(label)

def xlabel(xlabel):
    if xlabel is None:
        xlabel = ""
    _fortplotlib.fortplot.xlabel(xlabel)

def ylabel(ylabel):
    if ylabel is None:
        ylabel = ""
    _fortplotlib.fortplot.ylabel(ylabel)

def savefig(filename):
    _fortplotlib.fortplot.savefig(filename)

def show():
    _fortplotlib.fortplot.savefig("/tmp/show.pdf")

    if platform.system() == "Linux":
        os.system("xdg-open /tmp/show.pdf")
    elif platform.system() == "Darwin":
        os.system("open /tmp/show.pdf")
    else:
        os.startfile("/tmp/show.pdf")
