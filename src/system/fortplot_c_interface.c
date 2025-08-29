/* 
 * C interface for fortplot Python bindings
 *
 * This provides a simple C wrapper around the Fortran Python interface
 * that can be called from Python using ctypes, avoiding F2PY complexity.
 */

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

/* Fortran function prototypes (name mangling convention) */
extern void fortplot_python_interface_show_figure_(bool *blocking);
extern void fortplot_python_interface_show_viewer_(bool *blocking);
extern void fortplot_python_interface_figure_(int *width, int *height);
extern void fortplot_python_interface_plot_(double *x, double *y, int *n, 
                                           char *label, char *linestyle, 
                                           int label_len, int linestyle_len);
extern void fortplot_python_interface_savefig_(char *filename, int filename_len);
extern void fortplot_python_interface_title_(char *text, int text_len);
extern void fortplot_python_interface_xlabel_(char *text, int text_len);
extern void fortplot_python_interface_ylabel_(char *text, int text_len);
extern void fortplot_python_interface_legend_(void);
extern void fortplot_python_interface_scatter_(double *x, double *y, int *n,
                                              double *s, double *c,
                                              char *label, char *marker,
                                              double *markersize,
                                              double *color,
                                              char *colormap,
                                              double *vmin, double *vmax,
                                              bool *show_colorbar,
                                              int label_len, int marker_len,
                                              int colormap_len);
extern void fortplot_python_interface_histogram_(double *data, int *n,
                                                int *bins, bool *density,
                                                char *label, double *color,
                                                int label_len);

/* C wrapper functions for Python ctypes */

void c_show_figure(int blocking) {
    bool block = blocking != 0;
    fortplot_python_interface_show_figure_(&block);
}

void c_show_viewer(int blocking) {
    bool block = blocking != 0;
    fortplot_python_interface_show_viewer_(&block);
}

void c_figure(int width, int height) {
    fortplot_python_interface_figure_(&width, &height);
}

void c_plot(double *x, double *y, int n, const char *label, const char *linestyle) {
    int label_len = label ? strlen(label) : 0;
    int linestyle_len = linestyle ? strlen(linestyle) : 0;
    char *label_copy = label ? strdup(label) : NULL;
    char *linestyle_copy = linestyle ? strdup(linestyle) : NULL;
    
    fortplot_python_interface_plot_(x, y, &n, label_copy, linestyle_copy,
                                   label_len, linestyle_len);
    
    if (label_copy) free(label_copy);
    if (linestyle_copy) free(linestyle_copy);
}

void c_savefig(const char *filename) {
    int filename_len = strlen(filename);
    char *filename_copy = strdup(filename);
    fortplot_python_interface_savefig_(filename_copy, filename_len);
    free(filename_copy);
}

void c_title(const char *text) {
    int text_len = strlen(text);
    char *text_copy = strdup(text);
    fortplot_python_interface_title_(text_copy, text_len);
    free(text_copy);
}

void c_xlabel(const char *text) {
    int text_len = strlen(text);
    char *text_copy = strdup(text);
    fortplot_python_interface_xlabel_(text_copy, text_len);
    free(text_copy);
}

void c_ylabel(const char *text) {
    int text_len = strlen(text);
    char *text_copy = strdup(text);
    fortplot_python_interface_ylabel_(text_copy, text_len);
    free(text_copy);
}

void c_legend(void) {
    fortplot_python_interface_legend_();
}

void c_scatter(double *x, double *y, int n, const char *label) {
    int label_len = label ? strlen(label) : 0;
    char *label_copy = label ? strdup(label) : NULL;
    
    // Call with minimal parameters for simplicity
    fortplot_python_interface_scatter_(x, y, &n, NULL, NULL,
                                      label_copy, NULL, NULL, NULL, NULL,
                                      NULL, NULL, NULL,
                                      label_len, 0, 0);
    
    if (label_copy) free(label_copy);
}

void c_histogram(double *data, int n, const char *label) {
    int label_len = label ? strlen(label) : 0;
    char *label_copy = label ? strdup(label) : NULL;
    
    // Call with minimal parameters for simplicity
    fortplot_python_interface_histogram_(data, &n, NULL, NULL,
                                        label_copy, NULL, label_len);
    
    if (label_copy) free(label_copy);
}