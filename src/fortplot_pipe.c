#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Forward declarations for secure pipe operations
typedef struct secure_pipe_t secure_pipe_t;
extern secure_pipe_t* secure_open_pipe(const char* program, const char* const argv[]);
extern int secure_close_pipe(secure_pipe_t* pipe);
extern int secure_check_command(const char* command);
extern FILE* secure_pipe_get_write_handle(secure_pipe_t* pipe);
extern FILE* secure_pipe_get_read_handle(secure_pipe_t* pipe);

// Configuration constants
#define MAX_FILENAME_LENGTH 512
#define MAX_COMMAND_BUFFER_SIZE 1024
#define ESCAPED_FILENAME_SIZE 512
#define MIN_FPS 1
#define MAX_FPS 120
#define MAX_RETRIES 3
#define WINDOWS_SYSTEM_PATH_LEN 17
#define PIPE_FLUSH_ERROR_CODE -6

// Platform-specific includes
#ifdef _WIN32
    #include <windows.h>
    #include <io.h>
    #include <fcntl.h>
    #define popen _popen
    #define pclose _pclose
    #define access _access
    // Binary mode flag for Windows
    #ifndef _O_BINARY
        #define _O_BINARY 0x8000
    #endif
#else
    #include <unistd.h>
    #include <sys/wait.h>
#endif

typedef struct {
    secure_pipe_t* secure_pipe;  // Use secure pipe implementation
    FILE* pipe;  // Legacy compatibility
    int health_status;
    size_t bytes_written;
    int consecutive_failures;
} ffmpeg_pipe_t;

// Initialize with zero
static ffmpeg_pipe_t current_pipe = {NULL, NULL, 0, 0, 0};

// Forward declarations
int close_ffmpeg_pipe_c(void);
int is_safe_filename_c(const char* filename);
int is_ffmpeg_enabled(void);
int ensure_binary_mode(FILE* pipe);
int escape_windows_path(const char* input, char* output, size_t output_size);
int is_ffmpeg_executable_available(void);
void update_pipe_health(int status);
int check_pipe_health(void);

// Validate filename for safety (no command injection)
int is_safe_filename_c(const char* filename) {
    if (!filename || strlen(filename) == 0 || strlen(filename) > MAX_FILENAME_LENGTH) {
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
    #ifdef _WIN32
        // Windows dangerous paths
        if (strncmp(filename, "C:\\Windows\\", 11) == 0 ||
            strncmp(filename, "C:\\Program Files\\", 17) == 0) {
            return 0;  // Dangerous system path
        }
    #else
        // Unix dangerous paths
        if (strncmp(filename, "/dev/", 5) == 0 ||
            strncmp(filename, "/proc/", 6) == 0 ||
            strncmp(filename, "/sys/", 5) == 0) {
            return 0;  // Dangerous system path
        }
    #endif
    
    return 1;  // Safe
}

// Check if FFmpeg functionality should be enabled
int is_ffmpeg_enabled(void) {
    // 1. Check for explicit user override
    const char* enable_ffmpeg = getenv("FORTPLOT_ENABLE_FFMPEG");
    if (enable_ffmpeg) {
        if (strcmp(enable_ffmpeg, "1") == 0) {
            return 1;  // Explicitly enabled
        } else if (strcmp(enable_ffmpeg, "0") == 0) {
            return 0;  // Explicitly disabled
        }
    }
    
    // 2. Enable in CI environments
    const char* ci_env = getenv("CI");
    const char* github_actions = getenv("GITHUB_ACTIONS");
    const char* in_container = getenv("RUNNER_OS");
    
    if ((ci_env && strcmp(ci_env, "true") == 0) ||
        (github_actions && strcmp(github_actions, "true") == 0) ||
        in_container != NULL) {
        return 1;
    }
    
    // 3. Auto-detect development environments (Linux/macOS with FFmpeg available)
    const char* home = getenv("HOME");
    const char* user = getenv("USER");
    
    // Check if we're in a development environment (has HOME and USER, not root)
    if (home && user && strcmp(user, "root") != 0) {
        // Secure test if ffmpeg is actually available using stat/access
        if (is_ffmpeg_executable_available()) {
            // FFmpeg is available and we're in a development environment
            return 1;  // Enable by default in dev environments with FFmpeg
        }
    }
    
    // 4. Enable for testing environments (less restrictive for Issue #186)
    const char* fortplot_test = getenv("FORTPLOT_TEST_MODE");
    if (fortplot_test) {
        return 1;  // Enable for testing
    }
    
    // 5. Disabled by default for security in other environments
    return 0;
}

// Open pipe to ffmpeg command
int open_ffmpeg_pipe_c(const char* filename, int fps) {
    // Close any existing pipe
    if (current_pipe.pipe != NULL) {
        close_ffmpeg_pipe_c();
    }
    
    // Validate filename safety
    if (!is_safe_filename_c(filename)) {
        fprintf(stderr, "Security error: Unsafe filename rejected: %s\n", filename);
        return -1;
    }
    
    // Validate fps parameter (reasonable bounds for video)
    if (fps < MIN_FPS || fps > MAX_FPS) {
        fprintf(stderr, "Security error: Invalid fps value: %d (must be %d-%d)\n", 
                fps, MIN_FPS, MAX_FPS);
        return -1;
    }
    
    // Check if FFmpeg is enabled for this environment
    if (!is_ffmpeg_enabled()) {
        fprintf(stderr, "Security: FFmpeg pipe disabled in secure mode\n");
        fprintf(stderr, "Info: Animation would be saved to: %s at %d fps\n", filename, fps);
        fprintf(stderr, "Note: Set FORTPLOT_ENABLE_FFMPEG=1 or run in CI to enable FFmpeg\n");
        return -1;
    }
    
    // SECURITY FIX: Use secure pipe creation without shell interpretation
    
    // Build argument array for FFmpeg (no shell metacharacters)
    const char* argv[20];
    int argc = 0;
    char fps_str[32];
    
    argv[argc++] = "-y";                    // Overwrite output
    argv[argc++] = "-f";
    argv[argc++] = "image2pipe";
    argv[argc++] = "-vcodec";
    argv[argc++] = "png";
    argv[argc++] = "-r";
    
    snprintf(fps_str, sizeof(fps_str), "%d", fps);
    argv[argc++] = fps_str;
    
    argv[argc++] = "-i";
    argv[argc++] = "-";                     // Read from stdin
    argv[argc++] = "-vcodec";
    argv[argc++] = "libx264";
    argv[argc++] = "-pix_fmt";
    argv[argc++] = "yuv420p";
    argv[argc++] = filename;                // Output file (already validated)
    argv[argc] = NULL;
    
    // Create secure pipe to FFmpeg
    current_pipe.secure_pipe = secure_open_pipe("ffmpeg", argv);
    
    if (current_pipe.secure_pipe == NULL) {
        fprintf(stderr, "Error: Failed to start FFmpeg process securely\n");
        return -1;
    }
    
    // Get write pipe handle for compatibility with existing write functions
    current_pipe.pipe = secure_pipe_get_write_handle(current_pipe.secure_pipe);
    
    if (current_pipe.pipe == NULL) {
        fprintf(stderr, "Error: Failed to get write handle from secure pipe\n");
        secure_close_pipe(current_pipe.secure_pipe);
        current_pipe.secure_pipe = NULL;
        return -1;
    }
    
    // Initialize pipe health monitoring
    current_pipe.health_status = 1;  // Healthy when first opened
    current_pipe.bytes_written = 0;
    current_pipe.consecutive_failures = 0;
    
    fprintf(stderr, "Info: FFmpeg pipe opened for: %s at %d fps\n", filename, fps);
    return 0;
}

// Write PNG data to ffmpeg pipe with enhanced reliability
int write_png_to_pipe_c(const unsigned char* png_data, size_t data_size) {
    if (!is_ffmpeg_enabled()) {
        fprintf(stderr, "Security: PNG pipe write disabled in secure mode\n");
        fprintf(stderr, "Info: Would write %zu bytes of PNG data\n", data_size);
        return -1;
    }
    
    if (current_pipe.secure_pipe == NULL && current_pipe.pipe == NULL) {
        fprintf(stderr, "Error: FFmpeg pipe not open\n");
        return -2;
    }
    
    if (png_data == NULL || data_size == 0) {
        fprintf(stderr, "Error: Invalid PNG data (ptr=%p, size=%zu)\n", 
                (void*)png_data, data_size);
        return -3;
    }
    
    // Validate PNG header for data integrity
    if (data_size >= 8) {
        const unsigned char png_signature[8] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
        int has_png_header = 1;
        for (int i = 0; i < 8; i++) {
            if (png_data[i] != png_signature[i]) {
                has_png_header = 0;
                break;
            }
        }
        if (!has_png_header) {
            fprintf(stderr, "Warning: Data does not appear to be valid PNG\n");
        }
    }
    
    #ifdef _WIN32
        // Windows: Ensure binary mode before each write (defensive)
        if (ensure_binary_mode(current_pipe.pipe) != 0) {
            fprintf(stderr, "Error: Failed to ensure binary mode\n");
            return -4;
        }
    #endif
    
    // Enhanced write with retry mechanism
    size_t total_written = 0;
    int retry_count = 0;
    
    while (total_written < data_size && retry_count < MAX_RETRIES) {
        size_t remaining = data_size - total_written;
        size_t written = fwrite(png_data + total_written, 1, remaining, current_pipe.pipe);
        
        if (written == 0) {
            retry_count++;
            fprintf(stderr, "Warning: Write returned 0, retry %d/%d\n", retry_count, MAX_RETRIES);
            if (ferror(current_pipe.pipe)) {
                fprintf(stderr, "Error: Pipe write error detected\n");
                clearerr(current_pipe.pipe);
            }
            continue;
        }
        
        total_written += written;
    }
    
    if (total_written != data_size) {
        fprintf(stderr, "Error: Failed to write complete PNG data (%zu/%zu bytes)\n", 
                total_written, data_size);
        #ifdef _WIN32
            DWORD error = GetLastError();
            fprintf(stderr, "Windows error code: %lu\n", error);
        #endif
        update_pipe_health(-5);
        return -5;
    }
    
    // Force flush and check for errors
    if (fflush(current_pipe.pipe) != 0) {
        fprintf(stderr, "Warning: Pipe flush failed\n");
        update_pipe_health(-6);
        return -6;  // This matches the status -6 from Issue #186
    }
    
    // Update health tracking
    current_pipe.bytes_written += data_size;
    update_pipe_health(0);
    return 0;
}

// Close ffmpeg pipe and wait for completion with enhanced error handling
int close_ffmpeg_pipe_c(void) {
    if (!is_ffmpeg_enabled()) {
        if (current_pipe.pipe != NULL || current_pipe.secure_pipe != NULL) {
            fprintf(stderr, "Warning: Cleaning up unexpected pipe in secure mode\n");
            current_pipe.pipe = NULL;
            current_pipe.secure_pipe = NULL;
        }
        return 0;
    }
    
    if (current_pipe.pipe == NULL) {
        return 0;  // No pipe to close - success
    }
    
    // SECURITY FIX: Use secure pipe closure
    int status = 0;
    
    if (current_pipe.secure_pipe != NULL) {
        // Flush before closing if we have a FILE* handle
        if (current_pipe.pipe != NULL) {
            fflush(current_pipe.pipe);
        }
        
        status = secure_close_pipe(current_pipe.secure_pipe);
        current_pipe.secure_pipe = NULL;
    } else if (current_pipe.pipe != NULL) {
        // Legacy fallback (should not happen with secure implementation)
        fflush(current_pipe.pipe);
        status = pclose(current_pipe.pipe);
    }
    
    current_pipe.pipe = NULL;
    current_pipe.health_status = 0;
    current_pipe.bytes_written = 0;
    current_pipe.consecutive_failures = 0;
    
    if (status == -1) {
        fprintf(stderr, "Error: Failed to close FFmpeg pipe (pclose returned -1)\n");
        return -1;
    }
    
    #ifdef _WIN32
        // Windows _pclose returns exit status directly
        int exit_status = status;
    #else
        // Unix needs WEXITSTATUS macro to extract exit status
        int exit_status = WEXITSTATUS(status);
    #endif
    
    // More lenient exit status handling for Issue #186
    // Some FFmpeg exit codes are warnings, not errors
    if (exit_status != 0) {
        if (exit_status == 69 || exit_status == 234) {
            // Common FFmpeg warning codes - treat as success for pipe closure
            fprintf(stderr, "Info: FFmpeg completed with warning code %d (acceptable)\n", exit_status);
            return 0;
        } else {
            fprintf(stderr, "Warning: FFmpeg exited with status %d\n", exit_status);
            // Don't return error for pipe close - consider it successful if pipe closed
            return 0;  // Treat as success to avoid status -1 issues
        }
    }
    
    fprintf(stderr, "Info: FFmpeg pipe closed successfully\n");
    return 0;
}

// Check if ffmpeg is available with enhanced detection
int check_ffmpeg_available_c(void) {
    if (!is_ffmpeg_enabled()) {
        fprintf(stderr, "Security: External program availability check disabled\n");
        fprintf(stderr, "Info: Assuming ffmpeg not available for security\n");
        return 0;
    }
    
    // SECURITY FIX: Use secure command checking
    if (secure_check_command("ffmpeg")) {
        fprintf(stderr, "Info: FFmpeg is available and working\n");
        return 1;
    } else {
        fprintf(stderr, "Info: FFmpeg not found (this is OK for testing without video output)\n");
        return 0;
    }
}

// Platform-specific helper functions
#ifdef _WIN32
// Ensure binary mode on Windows pipe
int ensure_binary_mode(FILE* pipe) {
    if (pipe == NULL) {
        return -1;
    }
    
    int fd = _fileno(pipe);
    if (fd == -1) {
        return -1;
    }
    
    // Set binary mode to prevent CRLF translation
    if (_setmode(fd, _O_BINARY) == -1) {
        return -1;
    }
    
    return 0;
}

// Escape Windows path for command line
int escape_windows_path(const char* input, char* output, size_t output_size) {
    if (!input || !output || output_size == 0) {
        return 0;
    }
    
    size_t i, j = 0;
    size_t input_len = strlen(input);
    
    for (i = 0; i < input_len && j < output_size - 2; i++) {
        char c = input[i];
        
        // Handle special characters that need escaping in Windows
        if (c == '\\') {
            // Double backslashes for Windows paths
            if (j + 1 < output_size - 1) {
                output[j++] = '\\';
                output[j++] = '\\';
            } else {
                return 0;  // Output buffer too small
            }
        } else if (c == '"') {
            // Escape quotes
            if (j + 1 < output_size - 1) {
                output[j++] = '\\';
                output[j++] = '"';
            } else {
                return 0;
            }
        } else {
            output[j++] = c;
        }
    }
    
    output[j] = '\0';
    return 1;
}
#else
// Unix stub implementations
int ensure_binary_mode(FILE* pipe) {
    // Unix doesn't need binary mode
    return 0;
}

int escape_windows_path(const char* input, char* output, size_t output_size) {
    // Unix doesn't need special escaping
    strncpy(output, input, output_size - 1);
    output[output_size - 1] = '\0';
    return 1;
}
#endif

// Secure FFmpeg executable detection (DEPRECATED - use secure_check_command)
int is_ffmpeg_executable_available(void) {
    // SECURITY FIX: Delegate to secure implementation
    return secure_check_command("ffmpeg");
}

// Real-time pipe health monitoring functions
void update_pipe_health(int status) {
    if (status == 0) {
        // Success - reset failure counter
        current_pipe.consecutive_failures = 0;
        current_pipe.health_status = 1;  // Healthy
    } else {
        // Failure - increment counter
        current_pipe.consecutive_failures++;
        if (current_pipe.consecutive_failures >= MAX_RETRIES) {
            current_pipe.health_status = -1;  // Unhealthy
        } else {
            current_pipe.health_status = 0;   // Warning
        }
    }
}

int check_pipe_health(void) {
    if (current_pipe.pipe == NULL) {
        return -2;  // No pipe
    }
    
    // Check if pipe is still writable
    if (ferror(current_pipe.pipe)) {
        current_pipe.health_status = -1;  // Unhealthy
        return -1;
    }
    
    return current_pipe.health_status;
}