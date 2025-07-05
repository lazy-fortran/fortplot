/**
 * STB Image Write C wrapper for fortplot
 * Provides simplified C interface to stb_image_write.h for JPEG output
 */

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "../thirdparty/stb_image_write.h"
#include <stdio.h>
#include <stdlib.h>

// Wrapper for JPEG writing
int stb_write_jpeg_wrapper(const char* filename, int width, int height, int components, const void* data, int quality) {
    return stbi_write_jpg(filename, width, height, components, data, quality);
}

// Wrapper for in-memory JPEG writing
typedef struct {
    unsigned char* data;
    int size;
    int capacity;
} jpeg_buffer_t;

static void stb_write_callback(void *context, void *data, int size) {
    jpeg_buffer_t* buffer = (jpeg_buffer_t*)context;
    
    // Expand buffer if needed
    if (buffer->size + size > buffer->capacity) {
        buffer->capacity = (buffer->size + size) * 2;
        buffer->data = (unsigned char*)realloc(buffer->data, buffer->capacity);
    }
    
    // Copy data
    memcpy(buffer->data + buffer->size, data, size);
    buffer->size += size;
}

int stb_write_jpeg_to_memory(unsigned char** output_data, int* output_size, 
                             int width, int height, int components, const void* data, int quality) {
    jpeg_buffer_t buffer = {0};
    buffer.capacity = width * height * components + 1024; // Initial size estimate
    buffer.data = (unsigned char*)malloc(buffer.capacity);
    
    if (!buffer.data) return 0;
    
    int result = stbi_write_jpg_to_func(stb_write_callback, &buffer, width, height, components, data, quality);
    
    if (result) {
        *output_data = buffer.data;
        *output_size = buffer.size;
    } else {
        free(buffer.data);
        *output_data = NULL;
        *output_size = 0;
    }
    
    return result;
}

void stb_free_jpeg_memory(unsigned char* data) {
    free(data);
}