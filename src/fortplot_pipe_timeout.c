#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#ifdef _WIN32
    #include <windows.h>
    #include <io.h>
    #include <fcntl.h>
    #define popen _popen
    #define pclose _pclose
    typedef HANDLE timeout_handle_t;
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

// Timeout-safe system command execution
int system_command_timeout_c(const char* command, int timeout_ms) {
    if (!command || strlen(command) == 0) {
        return -1;
    }
    
    if (setup_timeout(timeout_ms) != 0) {
        return -1;
    }
    
    int result = system(command);
    
    cleanup_timeout();
    
    if (timeout_occurred) {
        return -2;  // Timeout indicator
    }
    
    return result;
}

// Check FFmpeg availability with timeout
int check_ffmpeg_available_timeout_c(void) {
    const char* test_command;
    
#ifdef _WIN32
    test_command = "ffmpeg -version >nul 2>&1";
#else
    test_command = "ffmpeg -version >/dev/null 2>&1";
#endif
    
    int status = system_command_timeout_c(test_command, COMMAND_TIMEOUT_MS);
    
    if (status == -2) {
        // Timeout occurred - assume not available
        return 0;  // Not available due to timeout
    } else if (status == 0) {
        return 1;   // Available
    } else {
        return 0;   // Not available
    }
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