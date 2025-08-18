#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <ctype.h>

typedef struct {
    FILE* pipe;
    pid_t pid;
} ffmpeg_pipe_t;

static ffmpeg_pipe_t current_pipe = {NULL, 0};

// Forward declarations
int close_ffmpeg_pipe_c(void);
int is_safe_filename_c(const char* filename);

// Validate filename for safety (no command injection)
int is_safe_filename_c(const char* filename) {
    if (!filename || strlen(filename) == 0 || strlen(filename) > 512) {
        return 0;  // Invalid
    }
    
    // Check each character
    for (const char* p = filename; *p; p++) {
        char c = *p;
        
        // Only allow alphanumeric, dash, underscore, dot, and slash
        if (!isalnum(c) && c != '-' && c != '_' && c != '.' && c != '/') {
            return 0;  // Unsafe character
        }
        
        // Reject shell metacharacters
        if (c == ';' || c == '|' || c == '&' || c == '$' || c == '`' ||
            c == '(' || c == ')' || c == '{' || c == '}' || c == '<' ||
            c == '>' || c == '*' || c == '?' || c == '[' || c == ']' ||
            c == '!' || c == '#' || c == '"' || c == '\'' || c == '\\') {
            return 0;  // Shell metacharacter
        }
    }
    
    // Check for directory traversal
    if (strstr(filename, "..")) {
        return 0;  // Directory traversal attempt
    }
    
    // Check for dangerous paths
    if (strncmp(filename, "/dev/", 5) == 0 ||
        strncmp(filename, "/proc/", 6) == 0 ||
        strncmp(filename, "/sys/", 5) == 0) {
        return 0;  // Dangerous system path
    }
    
    return 1;  // Safe
}

// Open pipe to ffmpeg command
int open_ffmpeg_pipe_c(const char* filename, int fps) {
    // Close any existing pipe
    if (current_pipe.pipe != NULL) {
        close_ffmpeg_pipe_c();
    }
    
    // SECURITY: Validate filename safety
    if (!is_safe_filename_c(filename)) {
        fprintf(stderr, "Security error: Unsafe filename rejected: %s\n", filename);
        return -1;
    }
    
    // SECURITY: Validate fps parameter
    if (fps < 1 || fps > 120) {
        fprintf(stderr, "Security error: Invalid fps value: %d\n", fps);
        return -1;
    }
    
    // SECURITY: In hardened mode, disable external program execution
    fprintf(stderr, "Security: FFmpeg pipe disabled in secure mode\n");
    fprintf(stderr, "Info: Animation would be saved to: %s at %d fps\n", filename, fps);
    fprintf(stderr, "Note: For video generation, use external ffmpeg manually\n");
    
    // Return error to indicate operation not supported in secure mode
    return -1;
}

// Write PNG data to ffmpeg pipe
int write_png_to_pipe_c(const unsigned char* png_data, size_t data_size) {
    // SECURITY: Pipe operations disabled in secure mode
    fprintf(stderr, "Security: PNG pipe write disabled in secure mode\n");
    fprintf(stderr, "Info: Would write %zu bytes of PNG data\n", data_size);
    return -1;  // Operation not supported in secure mode
}

// Close ffmpeg pipe and wait for completion
int close_ffmpeg_pipe_c(void) {
    // SECURITY: No pipe operations in secure mode
    if (current_pipe.pipe != NULL) {
        // This should never happen in secure mode, but clean up if somehow set
        fprintf(stderr, "Warning: Cleaning up unexpected pipe in secure mode\n");
        current_pipe.pipe = NULL;
        current_pipe.pid = 0;
    }
    
    return 0;  // Success (no operation needed)
}

// Check if ffmpeg is available
int check_ffmpeg_available_c(void) {
    // SECURITY: External program checking disabled in secure mode
    fprintf(stderr, "Security: External program availability check disabled\n");
    fprintf(stderr, "Info: Assuming ffmpeg not available for security\n");
    return 0;  // Always report not available in secure mode
}