#include <ft2build.h>
#include FT_FREETYPE_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Simple wrapper structure to hide FreeType complexity from Fortran
typedef struct {
    FT_Library library;
    FT_Face face;
    int initialized;
} ft_wrapper_t;

// Global wrapper instance
static ft_wrapper_t ft_wrapper = {0};

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

// Initialize FreeType and load a font
int ft_wrapper_init(const char* font_path) {
    FT_Error error;

    if (ft_wrapper.initialized) {
        return 0; // Already initialized
    }

    // Initialize FreeType library
    error = FT_Init_FreeType(&ft_wrapper.library);
    if (error) {
        fprintf(stderr, "FreeType: Failed to initialize library (error %d)\n", error);
        return -1;
    }

    // Load font face
    error = FT_New_Face(ft_wrapper.library, font_path, 0, &ft_wrapper.face);
    if (error) {
        fprintf(stderr, "FreeType: Failed to load font '%s' (error %d)\n", font_path, error);
        FT_Done_FreeType(ft_wrapper.library);
        return -1;
    }

    // Set pixel size (12 pixels)
    error = FT_Set_Pixel_Sizes(ft_wrapper.face, 0, 12);
    if (error) {
        fprintf(stderr, "FreeType: Failed to set pixel size (error %d)\n", error);
        FT_Done_Face(ft_wrapper.face);
        FT_Done_FreeType(ft_wrapper.library);
        return -1;
    }

    ft_wrapper.initialized = 1;
    printf("FreeType: Successfully initialized with font '%s'\n", font_path);
    return 0;
}

// Try to initialize with system fonts
int ft_wrapper_init_system_font(void) {
    const char* font_paths[] = {
        "/System/Library/Fonts/Helvetica.ttc",        // macOS
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", // Debian Linux
        "/usr/share/fonts/TTF/DejaVuSans.ttf",            // Arch Linux
        "/Windows/Fonts/arial.ttf",                   // Windows
        NULL
    };

    for (int i = 0; font_paths[i] != NULL; i++) {
        if (ft_wrapper_init(font_paths[i]) == 0) {
            return 0;
        }
    }

    fprintf(stderr, "FreeType: Failed to load any system font\n");
    return -1;
}

// Clean up FreeType resources
void ft_wrapper_cleanup(void) {
    if (ft_wrapper.initialized) {
        FT_Done_Face(ft_wrapper.face);
        FT_Done_FreeType(ft_wrapper.library);
        ft_wrapper.initialized = 0;
        printf("FreeType: Cleaned up resources\n");
    }
}

// Render a single character and return glyph information
int ft_wrapper_render_char(int char_code, glyph_info_t* glyph_info) {
    FT_Error error;
    FT_GlyphSlot slot;

    if (!ft_wrapper.initialized) {
        fprintf(stderr, "FreeType: Not initialized\n");
        return -1;
    }

    if (!glyph_info) {
        fprintf(stderr, "FreeType: Invalid glyph_info pointer\n");
        return -1;
    }

    // Load and render the character
    error = FT_Load_Char(ft_wrapper.face, char_code, FT_LOAD_RENDER);
    if (error) {
        fprintf(stderr, "FreeType: Failed to load character %d (error %d)\n", char_code, error);
        return -1;
    }

    slot = ft_wrapper.face->glyph;

    // Copy glyph information
    glyph_info->width = slot->bitmap.width;
    glyph_info->height = slot->bitmap.rows;
    glyph_info->left = slot->bitmap_left;
    glyph_info->top = slot->bitmap_top;
    glyph_info->advance_x = slot->advance.x >> 6; // Convert from 26.6 fixed point

    // Copy bitmap data
    int buffer_size = glyph_info->width * glyph_info->height;
    if (buffer_size > 0 && slot->bitmap.buffer) {
        glyph_info->buffer = malloc(buffer_size);
        if (glyph_info->buffer) {
            memcpy(glyph_info->buffer, slot->bitmap.buffer, buffer_size);
            glyph_info->buffer_size = buffer_size;
        } else {
            fprintf(stderr, "FreeType: Failed to allocate bitmap buffer\n");
            return -1;
        }
    } else {
        glyph_info->buffer = NULL;
        glyph_info->buffer_size = 0;
    }

    return 0;
}

// Render character with rotation (angle in degrees)
int ft_wrapper_render_char_rotated(int char_code, glyph_info_t* glyph_info, double angle_degrees) {
    FT_Error error;
    FT_GlyphSlot slot;
    FT_Matrix matrix;
    FT_Vector pen;

    if (!ft_wrapper.initialized) {
        fprintf(stderr, "FreeType: Not initialized\n");
        return -1;
    }

    if (!glyph_info) {
        fprintf(stderr, "FreeType: Invalid glyph_info pointer\n");
        return -1;
    }

    // Convert angle to radians
    double angle_rad = angle_degrees * 3.14159265359 / 180.0;

    // Set up transformation matrix for rotation only
    matrix.xx = (FT_Fixed)(cos(angle_rad) * 0x10000L);
    matrix.xy = (FT_Fixed)(-sin(angle_rad) * 0x10000L);
    matrix.yx = (FT_Fixed)(sin(angle_rad) * 0x10000L);
    matrix.yy = (FT_Fixed)(cos(angle_rad) * 0x10000L);

    // Set transformation
    FT_Set_Transform(ft_wrapper.face, &matrix, NULL);

    // Load and render the character
    error = FT_Load_Char(ft_wrapper.face, char_code, FT_LOAD_RENDER);
    if (error) {
        fprintf(stderr, "FreeType: Failed to load character %d (error %d)\n", char_code, error);
        // Reset transformation
        FT_Set_Transform(ft_wrapper.face, NULL, NULL);
        return -1;
    }

    slot = ft_wrapper.face->glyph;

    // Copy glyph information
    glyph_info->width = slot->bitmap.width;
    glyph_info->height = slot->bitmap.rows;
    glyph_info->left = slot->bitmap_left;
    glyph_info->top = slot->bitmap_top;
    glyph_info->advance_x = slot->advance.x >> 6;

    // Copy bitmap data
    int buffer_size = glyph_info->width * glyph_info->height;
    if (buffer_size > 0 && slot->bitmap.buffer) {
        glyph_info->buffer = malloc(buffer_size);
        if (glyph_info->buffer) {
            memcpy(glyph_info->buffer, slot->bitmap.buffer, buffer_size);
            glyph_info->buffer_size = buffer_size;
        } else {
            fprintf(stderr, "FreeType: Failed to allocate bitmap buffer\n");
            // Reset transformation
            FT_Set_Transform(ft_wrapper.face, NULL, NULL);
            return -1;
        }
    } else {
        glyph_info->buffer = NULL;
        glyph_info->buffer_size = 0;
    }

    // Reset transformation for subsequent normal text
    FT_Set_Transform(ft_wrapper.face, NULL, NULL);

    return 0;
}

// Free glyph bitmap buffer
void ft_wrapper_free_glyph(glyph_info_t* glyph_info) {
    if (glyph_info && glyph_info->buffer) {
        free(glyph_info->buffer);
        glyph_info->buffer = NULL;
        glyph_info->buffer_size = 0;
    }
}

// Simple text rendering function that returns rendered text as bitmap
int ft_wrapper_render_text(const char* text, int* width, int* height, unsigned char** buffer) {
    if (!ft_wrapper.initialized || !text || !width || !height || !buffer) {
        return -1;
    }

    int text_len = strlen(text);
    if (text_len == 0) {
        return -1;
    }

    // Calculate total text dimensions
    int total_width = 0;
    int max_height = 0;
    int max_bearing = 0;

    for (int i = 0; i < text_len; i++) {
        glyph_info_t glyph;
        if (ft_wrapper_render_char(text[i], &glyph) == 0) {
            total_width += glyph.advance_x;
            if (glyph.height > max_height) {
                max_height = glyph.height;
            }
            if (glyph.top > max_bearing) {
                max_bearing = glyph.top;
            }
            ft_wrapper_free_glyph(&glyph);
        }
    }

    if (total_width == 0 || max_height == 0) {
        return -1;
    }

    // Allocate output buffer
    *width = total_width;
    *height = max_height;
    int buffer_size = (*width) * (*height);
    *buffer = calloc(buffer_size, 1); // Initialize to 0 (transparent)

    if (!*buffer) {
        return -1;
    }

    // Render each character
    int pen_x = 0;
    for (int i = 0; i < text_len; i++) {
        glyph_info_t glyph;
        if (ft_wrapper_render_char(text[i], &glyph) == 0) {
            // Calculate glyph position
            int glyph_x = pen_x + glyph.left;
            int glyph_y = max_bearing - glyph.top;

            // Copy glyph bitmap to output buffer
            for (int row = 0; row < glyph.height; row++) {
                for (int col = 0; col < glyph.width; col++) {
                    int src_idx = row * glyph.width + col;
                    int dst_x = glyph_x + col;
                    int dst_y = glyph_y + row;

                    if (dst_x >= 0 && dst_x < *width && dst_y >= 0 && dst_y < *height) {
                        int dst_idx = dst_y * (*width) + dst_x;
                        if (glyph.buffer && src_idx < glyph.buffer_size) {
                            (*buffer)[dst_idx] = glyph.buffer[src_idx];
                        }
                    }
                }
            }

            pen_x += glyph.advance_x;
            ft_wrapper_free_glyph(&glyph);
        }
    }

    return 0;
}

// Free text bitmap buffer
void ft_wrapper_free_text_buffer(unsigned char* buffer) {
    if (buffer) {
        free(buffer);
    }
}

// Get kerning adjustment between two characters
int ft_wrapper_get_kerning(int left_char, int right_char) {
    FT_Vector kerning;
    FT_UInt left_glyph, right_glyph;

    if (!ft_wrapper.initialized || !FT_HAS_KERNING(ft_wrapper.face)) {
        return 0;
    }

    left_glyph = FT_Get_Char_Index(ft_wrapper.face, left_char);
    right_glyph = FT_Get_Char_Index(ft_wrapper.face, right_char);

    if (FT_Get_Kerning(ft_wrapper.face, left_glyph, right_glyph, FT_KERNING_DEFAULT, &kerning) == 0) {
        return kerning.x >> 6; // Convert from 26.6 fixed point to pixels
    }

    return 0;
}

// Check if FreeType is initialized
int ft_wrapper_is_initialized(void) {
    return ft_wrapper.initialized;
}
