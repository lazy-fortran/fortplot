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
int is_ffmpeg_enabled(void);

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

// Check if FFmpeg functionality should be enabled
int is_ffmpeg_enabled(void) {
    // Enable FFmpeg in CI environments or when explicitly allowed
    const char* ci_env = getenv("CI");
    const char* github_actions = getenv("GITHUB_ACTIONS");
    const char* enable_ffmpeg = getenv("FORTPLOT_ENABLE_FFMPEG");
    const char* in_container = getenv("RUNNER_OS");
    
    // Enable if any CI environment detected or explicitly enabled
    if ((ci_env && strcmp(ci_env, "true") == 0) ||
        (github_actions && strcmp(github_actions, "true") == 0) ||
        (enable_ffmpeg && strcmp(enable_ffmpeg, "1") == 0) ||
        in_container != NULL) {
        return 1;
    }
    
    // Disabled by default for security
    return 0;
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
    
    // Check if FFmpeg is enabled for this environment
    if (!is_ffmpeg_enabled()) {
        fprintf(stderr, "Security: FFmpeg pipe disabled in secure mode\n");
        fprintf(stderr, "Info: Animation would be saved to: %s at %d fps\n", filename, fps);
        fprintf(stderr, "Note: Set FORTPLOT_ENABLE_FFMPEG=1 or run in CI to enable FFmpeg\n");
        return -1;
    }
    
    // Build FFmpeg command with security controls
    char command[1024];
    int ret = snprintf(command, sizeof(command), 
        "ffmpeg -y -f image2pipe -vcodec png -r %d -i - -vcodec libx264 -pix_fmt yuv420p \"%s\" 2>/dev/null",
        fps, filename);
    
    if (ret >= sizeof(command) || ret < 0) {
        fprintf(stderr, "Error: FFmpeg command too long or formatting failed\n");
        return -1;
    }
    
    // Open pipe to FFmpeg
    current_pipe.pipe = popen(command, "w");
    if (current_pipe.pipe == NULL) {
        fprintf(stderr, "Error: Failed to start FFmpeg process\n");
        return -1;
    }
    
    fprintf(stderr, "Info: FFmpeg pipe opened for: %s at %d fps\n", filename, fps);
    return 0;
}

// Write PNG data to ffmpeg pipe
int write_png_to_pipe_c(const unsigned char* png_data, size_t data_size) {
    if (!is_ffmpeg_enabled()) {
        fprintf(stderr, "Security: PNG pipe write disabled in secure mode\n");
        fprintf(stderr, "Info: Would write %zu bytes of PNG data\n", data_size);
        return -1;
    }
    
    if (current_pipe.pipe == NULL) {
        fprintf(stderr, "Error: FFmpeg pipe not open\n");
        return -1;
    }
    
    if (png_data == NULL || data_size == 0) {
        fprintf(stderr, "Error: Invalid PNG data\n");
        return -1;
    }
    
    size_t written = fwrite(png_data, 1, data_size, current_pipe.pipe);
    if (written != data_size) {
        fprintf(stderr, "Error: Failed to write PNG data to pipe (%zu/%zu bytes)\n", written, data_size);
        return -1;
    }
    
    fflush(current_pipe.pipe);
    return 0;
}

// Close ffmpeg pipe and wait for completion
int close_ffmpeg_pipe_c(void) {
    if (!is_ffmpeg_enabled()) {
        if (current_pipe.pipe != NULL) {
            fprintf(stderr, "Warning: Cleaning up unexpected pipe in secure mode\n");
            current_pipe.pipe = NULL;
            current_pipe.pid = 0;
        }
        return 0;
    }
    
    if (current_pipe.pipe == NULL) {
        return 0;  // No pipe to close
    }
    
    int status = pclose(current_pipe.pipe);
    current_pipe.pipe = NULL;
    current_pipe.pid = 0;
    
    if (status == -1) {
        fprintf(stderr, "Error: Failed to close FFmpeg pipe\n");
        return -1;
    }
    
    int exit_status = WEXITSTATUS(status);
    if (exit_status != 0) {
        fprintf(stderr, "Warning: FFmpeg exited with status %d\n", exit_status);
        return -1;
    }
    
    fprintf(stderr, "Info: FFmpeg pipe closed successfully\n");
    return 0;
}

// Check if ffmpeg is available
int check_ffmpeg_available_c(void) {
    if (!is_ffmpeg_enabled()) {
        fprintf(stderr, "Security: External program availability check disabled\n");
        fprintf(stderr, "Info: Assuming ffmpeg not available for security\n");
        return 0;
    }
    
    // Test if ffmpeg command is available
    int status = system("ffmpeg -version >/dev/null 2>&1");
    if (status == 0) {
        fprintf(stderr, "Info: FFmpeg is available\n");
        return 1;
    } else {
        fprintf(stderr, "Warning: FFmpeg not found or not working\n");
        return 0;
    }
}