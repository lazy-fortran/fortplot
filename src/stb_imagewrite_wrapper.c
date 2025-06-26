/*
 * STB Image Write Wrapper for fortplotlib
 * Provides simple C interface for compression and CRC32 functions from stb_image_write.h
 */

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "../thirdparty/stb_image_write.h"
#include <stdlib.h>

/*
 * Wrapper for STB's compression function
 * Returns allocated compressed data that must be freed by caller
 * compressed_len: pointer to store the actual compressed size
 * quality: compression quality (1-10, higher = better compression)
 */
unsigned char* stb_compress_data(unsigned char* data, int data_len, int* compressed_len, int quality)
{
    return stbi_zlib_compress(data, data_len, compressed_len, quality);
}

/*
 * Wrapper for STB's CRC32 function
 * Returns CRC32 value as unsigned int
 */
unsigned int stb_crc32(unsigned char* data, int data_len)
{
    return stbiw__crc32(data, data_len);
}

/*
 * Helper function to free memory allocated by STB functions
 */
void stb_free_data(unsigned char* data)
{
    if (data) {
        free(data);
    }
}