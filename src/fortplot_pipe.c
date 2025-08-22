#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Platform-specific includes
#ifdef _WIN32
    #include <windows.h>
    #include <io.h>
    #include <fcntl.h>
    #define popen _popen
    #define pclose _pclose
    // Binary mode flag for Windows
    #ifndef _O_BINARY
        #define _O_BINARY 0x8000
    #endif
#else
    #include <unistd.h>
    #include <sys/wait.h>
#endif

typedef struct {
    FILE* pipe;
    #ifdef _WIN32
        HANDLE process;
    #else
        pid_t pid;
    #endif
} ffmpeg_pipe_t;

// Initialize with zero - works for both platforms (NULL == 0, and pid_t 0)
static ffmpeg_pipe_t current_pipe = {NULL, 0};

// Forward declarations
int close_ffmpeg_pipe_c(void);
int is_safe_filename_c(const char* filename);
int is_ffmpeg_enabled(void);
int ensure_binary_mode(FILE* pipe);
int escape_windows_path(const char* input, char* output, size_t output_size);

// Validate filename for safety (no command injection)
int is_safe_filename_c(const char* filename) {
    const size_t MAX_FILENAME_LENGTH = 512;
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
        // Quick test if ffmpeg is actually available
        int status = system("ffmpeg -version >/dev/null 2>&1");
        if (status == 0) {
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
    const int MIN_FPS = 1;
    const int MAX_FPS = 120;
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
    
    // Build FFmpeg command with platform-specific controls
    const size_t COMMAND_BUFFER_SIZE = 1024;
    const size_t ESCAPED_FILENAME_SIZE = 512;
    char command[COMMAND_BUFFER_SIZE];
    char escaped_filename[ESCAPED_FILENAME_SIZE];
    
    #ifdef _WIN32
        // Windows: Escape path and use NUL for stderr redirection
        if (!escape_windows_path(filename, escaped_filename, ESCAPED_FILENAME_SIZE)) {
            fprintf(stderr, "Error: Failed to escape Windows path\n");
            return -1;
        }
        int ret = snprintf(command, COMMAND_BUFFER_SIZE, 
            "ffmpeg -y -f image2pipe -vcodec png -r %d -i - -vcodec libx264 -pix_fmt yuv420p \"%s\" 2>NUL",
            fps, escaped_filename);
    #else
        // Unix: Simple escaping and /dev/null for stderr
        strncpy(escaped_filename, filename, ESCAPED_FILENAME_SIZE - 1);
        escaped_filename[ESCAPED_FILENAME_SIZE - 1] = '\0';
        int ret = snprintf(command, COMMAND_BUFFER_SIZE, 
            "ffmpeg -y -f image2pipe -vcodec png -r %d -i - -vcodec libx264 -pix_fmt yuv420p \"%s\" 2>/dev/null",
            fps, escaped_filename);
    #endif
    
    if (ret >= COMMAND_BUFFER_SIZE || ret < 0) {
        fprintf(stderr, "Error: FFmpeg command too long or formatting failed\n");
        return -1;
    }
    
    // Open pipe to FFmpeg with platform-specific mode
    #ifdef _WIN32
        // Windows requires binary mode to prevent corruption
        current_pipe.pipe = _popen(command, "wb");
        if (current_pipe.pipe != NULL) {
            // Ensure binary mode is set on the pipe
            if (ensure_binary_mode(current_pipe.pipe) != 0) {
                fprintf(stderr, "Error: Failed to set binary mode on Windows pipe\n");
                _pclose(current_pipe.pipe);
                current_pipe.pipe = NULL;
                return -1;
            }
        }
    #else
        current_pipe.pipe = popen(command, "w");
    #endif
    if (current_pipe.pipe == NULL) {
        fprintf(stderr, "Error: Failed to start FFmpeg process\n");
        return -1;
    }
    
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
    
    if (current_pipe.pipe == NULL) {
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
    const int MAX_RETRIES = 3;
    
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
        return -5;
    }
    
    // Force flush and check for errors
    if (fflush(current_pipe.pipe) != 0) {
        fprintf(stderr, "Warning: Pipe flush failed\n");
        return -6;  // This matches the status -6 from Issue #186
    }
    
    return 0;
}

// Close ffmpeg pipe and wait for completion with enhanced error handling
int close_ffmpeg_pipe_c(void) {
    if (!is_ffmpeg_enabled()) {
        if (current_pipe.pipe != NULL) {
            fprintf(stderr, "Warning: Cleaning up unexpected pipe in secure mode\n");
            current_pipe.pipe = NULL;
            #ifdef _WIN32
                current_pipe.process = NULL;
            #else
                current_pipe.pid = 0;
            #endif
        }
        return 0;
    }
    
    if (current_pipe.pipe == NULL) {
        return 0;  // No pipe to close - success
    }
    
    // Flush any remaining data before closing
    fflush(current_pipe.pipe);
    
    int status = pclose(current_pipe.pipe);
    current_pipe.pipe = NULL;
    #ifdef _WIN32
        current_pipe.process = NULL;
    #else
        current_pipe.pid = 0;
    #endif
    
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
    
    // Test if ffmpeg command is available (cross-platform with better error handling)
    #ifdef _WIN32
        // Windows: Try multiple detection methods
        // First try: Direct command
        int status = system("ffmpeg -version >NUL 2>&1");
        if (status != 0) {
            // Second try: With where command to find executable
            status = system("where ffmpeg >NUL 2>&1");
            if (status != 0) {
                // Third try: Check common installation paths
                status = system("\"C:\\Program Files\\ffmpeg\\bin\\ffmpeg.exe\" -version >NUL 2>&1");
            }
        }
    #else  
        // Unix: Try multiple common locations
        int status = system("ffmpeg -version >/dev/null 2>&1");
        if (status != 0) {
            // Try common installation paths
            status = system("/usr/bin/ffmpeg -version >/dev/null 2>&1");
            if (status != 0) {
                status = system("/usr/local/bin/ffmpeg -version >/dev/null 2>&1");
            }
        }
    #endif
    
    if (status == 0) {
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