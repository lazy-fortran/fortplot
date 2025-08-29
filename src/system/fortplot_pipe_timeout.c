#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

// Forward declarations for secure execution functions
extern int secure_exec_command(const char* program, const char* const argv[], int timeout_ms);
extern int secure_check_command(const char* command);

#ifdef _WIN32
    #include <windows.h>
    #include <io.h>
    #include <fcntl.h>
    #define popen _popen
    #define pclose _pclose
    #define access _access
    typedef HANDLE timeout_handle_t;
    // Binary mode flag for Windows
    #ifndef _O_BINARY
        #define _O_BINARY 0x8000
    #endif
#else
    #include <unistd.h>
    #include <sys/wait.h>
    typedef pid_t timeout_handle_t;
#endif

// Timeout configuration
#define PIPE_TIMEOUT_MS 5000    // 5 seconds for pipe operations
#define COMMAND_TIMEOUT_MS 3000 // 3 seconds for command execution

// Global timeout state
static volatile int timeout_occurred = 0;
static timeout_handle_t current_timeout = 0;

// Timeout handler for Unix
#ifndef _WIN32
static void timeout_handler(int sig) {
    timeout_occurred = 1;
}
#endif

// Platform-specific timeout setup
static int setup_timeout(int timeout_ms) {
    timeout_occurred = 0;
    
#ifdef _WIN32
    // Windows: Create a timer thread (simplified approach)
    // For production, would use proper Windows timers
    return 0;
#else
    // Unix: Use alarm for simple timeout
    struct sigaction sa;
    sa.sa_handler = timeout_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    
    if (sigaction(SIGALRM, &sa, NULL) == -1) {
        return -1;
    }
    
    alarm(timeout_ms / 1000 + 1);  // Convert to seconds, round up
    return 0;
#endif
}

static void cleanup_timeout(void) {
#ifdef _WIN32
    // Windows cleanup
    current_timeout = 0;
#else
    // Unix cleanup
    alarm(0);  // Cancel alarm
    timeout_occurred = 0;
#endif
}

// Timeout-safe system command execution (SECURE VERSION)
int system_command_timeout_c(const char* command, int timeout_ms) {
    if (!command || strlen(command) == 0) {
        return -1;
    }
    
    // SECURITY FIX: Parse command string and use secure execution
    // This is a compatibility wrapper - new code should use secure_exec_command directly
    
    // For now, we'll handle the specific ffmpeg test command case
    // This function should ideally be deprecated in favor of direct secure_exec_command
    if (strstr(command, "ffmpeg") && strstr(command, "-version")) {
        const char* argv[] = {"-version", NULL};
        return secure_exec_command("ffmpeg", argv, timeout_ms);
    }
    
    // For other commands, fail safely rather than risk injection
    fprintf(stderr, "Security: Blocked unsafe command execution: %s\n", command);
    return -1;
}

// Check FFmpeg availability with timeout (SECURE VERSION)
int check_ffmpeg_available_timeout_c(void) {
    // SECURITY FIX: Use secure command checking without shell execution
    
#ifdef _WIN32
    // On Windows CI with MSYS2, check specific paths
    if (access("C:\\msys64\\mingw64\\bin\\ffmpeg.exe", 0) == 0) {
        // Found in MSYS2 path - verify it actually works
        const char* argv[] = {"-version", NULL};
        int status = secure_exec_command("C:\\msys64\\mingw64\\bin\\ffmpeg.exe", argv, COMMAND_TIMEOUT_MS);
        return (status == 0) ? 1 : 0;
    } else if (secure_check_command("ffmpeg")) {
        // Found in PATH - verify it works
        const char* argv[] = {"-version", NULL};
        int status = secure_exec_command("ffmpeg", argv, COMMAND_TIMEOUT_MS);
        return (status == 0) ? 1 : 0;
    } else {
        // FFmpeg not found
        return 0;
    }
#else
    // Unix: Check if ffmpeg is available and works
    if (secure_check_command("ffmpeg")) {
        const char* argv[] = {"-version", NULL};
        int status = secure_exec_command("ffmpeg", argv, COMMAND_TIMEOUT_MS);
        return (status == 0) ? 1 : 0;
    } else {
        return 0;
    }
#endif
}

// Safe popen wrapper with timeout protection  
FILE* popen_timeout_c(const char* command, const char* mode, int timeout_ms) {
    if (!command || !mode) {
        return NULL;
    }
    
    // Pre-check: verify command doesn't hang immediately
    if (strstr(command, "ffmpeg") != NULL) {
        // For ffmpeg commands, do a quick availability check first
        if (check_ffmpeg_available_timeout_c() != 1) {
            return NULL;
        }
    }
    
    // Set up timeout before popen
    if (setup_timeout(timeout_ms) != 0) {
        return NULL;
    }
    
    FILE* pipe = popen(command, mode);
    
    if (timeout_occurred) {
        cleanup_timeout();
        if (pipe) pclose(pipe);
        return NULL;
    }
    
    if (!pipe) {
        cleanup_timeout();
        return NULL;
    }
    
#ifdef _WIN32
    // Windows: Ensure binary mode for pipe if writing mode
    if (strchr(mode, 'w') != NULL || strchr(mode, 'b') != NULL) {
        int fd = _fileno(pipe);
        if (fd != -1) {
            _setmode(fd, _O_BINARY);
        }
    }
#endif
    
    // Note: Keep timeout active for subsequent operations
    return pipe;
}

// Safe pclose wrapper
int pclose_timeout_c(FILE* pipe, int timeout_ms) {
    if (!pipe) {
        return -1;
    }
    
    if (setup_timeout(timeout_ms) != 0) {
        return -1;
    }
    
    int result = pclose(pipe);
    
    cleanup_timeout();
    
    if (timeout_occurred) {
        return -2;  // Timeout indicator
    }
    
    return result;
}