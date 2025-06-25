all: fetch-matplotlib fetch-pyplot-fortran

fetch-matplotlib:
	curl -L https://github.com/matplotlib/matplotlib/archive/refs/tags/v3.10.3.tar.gz | tar xz
	mv matplotlib-3.10.3 matplotlib

fetch-pyplot-fortran:
	git clone --depth 1 https://github.com/jacobwilliams/pyplot-fortran.git

clean:
	rm -rf matplotlib
	rm -rf pyplot-fortran

.PHONY: all clean fetch-matplotlib fetch-pyplot-fortran
