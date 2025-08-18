#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <ctype.h>
#include <limits.h>

typedef struct {
    FILE* pipe;
    pid_t pid;
} ffmpeg_pipe_t;

static ffmpeg_pipe_t current_pipe = {NULL, 0};

// Forward declarations
int close_ffmpeg_pipe_c(void);
int validate_filename_secure(const char* filename);
int validate_fps_secure(int fps);
char* escape_shell_argument(const char* arg);

// Open pipe to ffmpeg command
int open_ffmpeg_pipe_c(const char* filename, int fps) {
    char command[1024];
    char *escaped_filename = NULL;
    
    // Close any existing pipe
    if (current_pipe.pipe != NULL) {
        close_ffmpeg_pipe_c();
    }
    
    // Validate inputs
    if (!validate_filename_secure(filename)) {
        return -2;  // Invalid filename
    }
    
    if (!validate_fps_secure(fps)) {
        return -3;  // Invalid fps
    }
    
    // Properly escape filename for shell
    escaped_filename = escape_shell_argument(filename);
    if (escaped_filename == NULL) {
        return -4;  // Memory allocation failed
    }
    
    // Build ffmpeg command with escaped filename
    // Enhanced parameters for proper MP4 container structure and adequate size
    int ret = snprintf(command, sizeof(command),
        "ffmpeg -f image2pipe -vcodec png -r %d -i - "
        "-c:v libx264 -pix_fmt yuv420p -crf 15 -preset medium "
        "-movflags +faststart -profile:v baseline -level 3.0 "
        "-g %d -keyint_min %d -sc_threshold 0 -minrate 64k -maxrate 512k "
        "-bufsize 256k -y %s 2>/dev/null",
        fps, fps * 2, fps, escaped_filename);
    
    free(escaped_filename);
    
    if (ret >= sizeof(command)) {
        return -5;  // Command too long
    }
    
    // Open pipe to ffmpeg
    current_pipe.pipe = popen(command, "w");
    
    if (current_pipe.pipe == NULL) {
        return -1;
    }
    
    return 0;  // Success
}

// Write PNG data to ffmpeg pipe
int write_png_to_pipe_c(const unsigned char* png_data, size_t data_size) {
    if (current_pipe.pipe == NULL) {
        return -1;
    }
    
    size_t written = fwrite(png_data, 1, data_size, current_pipe.pipe);
    
    if (written != data_size) {
        return -1;
    }
    
    // Flush to ensure data is sent to ffmpeg
    fflush(current_pipe.pipe);
    
    return 0;  // Success
}

// Close ffmpeg pipe and wait for completion
int close_ffmpeg_pipe_c(void) {
    int status = 0;
    
    if (current_pipe.pipe != NULL) {
        status = pclose(current_pipe.pipe);
        current_pipe.pipe = NULL;
        current_pipe.pid = 0;
    }
    
    return status;
}

// Check if ffmpeg is available
int check_ffmpeg_available_c(void) {
    // Use execvp with fork instead of system() to avoid shell injection
    pid_t pid = fork();
    if (pid == 0) {
        // Child process
        char *args[] = {"which", "ffmpeg", NULL};
        execvp("which", args);
        _exit(127);  // execvp failed
    } else if (pid > 0) {
        // Parent process
        int status;
        waitpid(pid, &status, 0);
        return (WIFEXITED(status) && WEXITSTATUS(status) == 0) ? 1 : 0;
    } else {
        // fork failed
        return 0;
    }
}

// Validate filename for security
int validate_filename_secure(const char* filename) {
    if (filename == NULL || strlen(filename) == 0) {
        return 0;
    }
    
    size_t len = strlen(filename);
    if (len > 255) {  // Reasonable filename length limit
        return 0;
    }
    
    // Check for dangerous characters
    for (size_t i = 0; i < len; i++) {
        char c = filename[i];
        
        // Allow alphanumeric, dash, underscore, dot, forward slash for paths
        if (!isalnum(c) && c != '-' && c != '_' && c != '.' && c != '/') {
            return 0;  // Dangerous character found
        }
        
        // Prevent directory traversal
        if (strstr(filename, "..") != NULL) {
            return 0;
        }
    }
    
    // Check for valid video file extension
    const char* valid_extensions[] = {".mp4", ".avi", ".mkv", NULL};
    int has_valid_ext = 0;
    for (int i = 0; valid_extensions[i] != NULL; i++) {
        if (strstr(filename, valid_extensions[i]) != NULL) {
            has_valid_ext = 1;
            break;
        }
    }
    
    return has_valid_ext;
}

// Validate fps for security
int validate_fps_secure(int fps) {
    return (fps >= 1 && fps <= 120);  // Reasonable fps range
}

// Escape shell argument safely
char* escape_shell_argument(const char* arg) {
    if (arg == NULL) {
        return NULL;
    }
    
    size_t len = strlen(arg);
    // Allocate worst case: each char could need escaping + quotes + null terminator
    char* escaped = malloc(len * 2 + 3);
    if (escaped == NULL) {
        return NULL;
    }
    
    // Use single quotes to prevent shell interpretation
    escaped[0] = '\'';
    size_t j = 1;
    
    for (size_t i = 0; i < len; i++) {
        if (arg[i] == '\'') {
            // Escape single quote: end quote, add escaped quote, start new quote
            strcpy(&escaped[j], "'\\'");
            j += 4;
        } else {
            escaped[j++] = arg[i];
        }
    }
    
    escaped[j++] = '\'';
    escaped[j] = '\0';
    
    return escaped;
}

// Secure ffprobe validation using execvp
int validate_with_ffprobe_c(const char* filename) {
    if (!validate_filename_secure(filename)) {
        return -2;  // Invalid filename
    }
    
    pid_t pid = fork();
    if (pid == 0) {
        // Child process - redirect stdout/stderr to /dev/null
        freopen("/dev/null", "w", stdout);
        freopen("/dev/null", "w", stderr);
        
        char *args[] = {"ffprobe", "-v", "error", "-show_format", (char*)filename, NULL};
        execvp("ffprobe", args);
        _exit(127);  // execvp failed
    } else if (pid > 0) {
        // Parent process
        int status;
        waitpid(pid, &status, 0);
        return (WIFEXITED(status) && WEXITSTATUS(status) == 0) ? 0 : -1;
    } else {
        // fork failed
        return -1;
    }
}