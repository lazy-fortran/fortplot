#ifndef FREETYPE_WRAPPER_H
#define FREETYPE_WRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif

// Glyph rendering result structure
typedef struct {
    int width;
    int height;
    int left;
    int top;
    int advance_x;
    unsigned char* buffer;
    int buffer_size;
} glyph_info_t;

// Initialize FreeType with a specific font file
int ft_wrapper_init(const char* font_path);

// Initialize FreeType with system fonts (tries common paths)
int ft_wrapper_init_system_font(void);

// Clean up FreeType resources
void ft_wrapper_cleanup(void);

// Render a single character and return glyph information
int ft_wrapper_render_char(int char_code, glyph_info_t* glyph_info);

// Free glyph bitmap buffer
void ft_wrapper_free_glyph(glyph_info_t* glyph_info);

// Render entire text string to bitmap
int ft_wrapper_render_text(const char* text, int* width, int* height, unsigned char** buffer);

// Free text bitmap buffer
void ft_wrapper_free_text_buffer(unsigned char* buffer);

// Check if FreeType is initialized
int ft_wrapper_is_initialized(void);

#ifdef __cplusplus
}
#endif

#endif // FREETYPE_WRAPPER_H