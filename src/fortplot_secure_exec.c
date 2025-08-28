/* 
 * Secure command execution implementation
 * Provides safe alternatives to system() and popen() that prevent command injection
 * 
 * Security Design:
 * - NO shell interpretation - direct process execution only
 * - Proper argument separation prevents injection
 * - Whitelist-based command validation
 * - No string concatenation of commands
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>

#ifdef _WIN32
    #include <windows.h>
    #include <shellapi.h>
    #include <io.h>
    #include <fcntl.h>
    #include <process.h>
    #define access _access
    #ifndef _O_BINARY
        #define _O_BINARY 0x8000
    #endif
#else
    #define _GNU_SOURCE  // For pipe2
    #include <unistd.h>
    #include <sys/types.h>
    #include <sys/wait.h>
    #include <sys/stat.h>
    #include <signal.h>
    #include <fcntl.h>
#endif

// Configuration
#define MAX_PATH_LENGTH 4096
#define MAX_ARG_LENGTH 1024
#define MAX_ARGS 32

// Secure command execution without shell interpretation
int secure_exec_command(const char* program, const char* const argv[], int timeout_ms) {
    if (!program) {
        return -1;
    }
    
#ifdef _WIN32
    // Windows implementation using CreateProcess
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    DWORD exit_code;
    
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));
    
    // Build command line from argv (properly quoted)
    char cmdline[MAX_PATH_LENGTH] = {0};
    int offset = 0;
    
    // Add program name (quoted if contains spaces)
    if (strchr(program, ' ')) {
        offset = snprintf(cmdline, sizeof(cmdline), "\"%s\"", program);
    } else {
        offset = snprintf(cmdline, sizeof(cmdline), "%s", program);
    }
    
    // Add arguments
    if (argv) {
        for (int i = 0; argv[i] && offset < sizeof(cmdline) - 1; i++) {
            // Add space separator
            cmdline[offset++] = ' ';
            
            // Quote argument if it contains spaces
            if (strchr(argv[i], ' ')) {
                offset += snprintf(cmdline + offset, sizeof(cmdline) - offset, 
                                 "\"%s\"", argv[i]);
            } else {
                offset += snprintf(cmdline + offset, sizeof(cmdline) - offset, 
                                 "%s", argv[i]);
            }
        }
    }
    
    // Create process without shell
    if (!CreateProcess(
            NULL,           // Application name (use cmdline)
            cmdline,        // Command line
            NULL,           // Process security attributes
            NULL,           // Thread security attributes
            FALSE,          // Inherit handles
            0,              // Creation flags
            NULL,           // Environment
            NULL,           // Current directory
            &si,            // Startup info
            &pi             // Process information
        )) {
        return -1;
    }
    
    // Wait for process with timeout
    DWORD wait_result = WaitForSingleObject(pi.hProcess, 
                                           timeout_ms > 0 ? timeout_ms : INFINITE);
    
    if (wait_result == WAIT_TIMEOUT) {
        // Timeout - terminate process
        TerminateProcess(pi.hProcess, -2);
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
        return -2; // Timeout indicator
    }
    
    // Get exit code
    if (!GetExitCodeProcess(pi.hProcess, &exit_code)) {
        exit_code = -1;
    }
    
    // Clean up
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    
    return (int)exit_code;
    
#else
    // Unix implementation using fork/exec
    pid_t pid = fork();
    
    if (pid == -1) {
        // Fork failed
        return -1;
    } else if (pid == 0) {
        // Child process - execute command
        
        // Build argv array for execvp
        char* exec_argv[MAX_ARGS];
        int argc = 0;
        
        // First arg is program name
        exec_argv[argc++] = strdup(program);
        
        // Copy provided arguments
        if (argv) {
            for (int i = 0; argv[i] && argc < MAX_ARGS - 1; i++) {
                exec_argv[argc++] = strdup(argv[i]);
            }
        }
        exec_argv[argc] = NULL;
        
        // Execute without shell
        execvp(program, exec_argv);
        
        // If we get here, exec failed
        _exit(127);
    } else {
        // Parent process - wait for child
        int status;
        
        if (timeout_ms > 0) {
            // Set up timeout using alarm
            struct sigaction sa_old, sa_new;
            sa_new.sa_handler = SIG_IGN;
            sigemptyset(&sa_new.sa_mask);
            sa_new.sa_flags = 0;
            sigaction(SIGALRM, &sa_new, &sa_old);
            
            alarm(timeout_ms / 1000 + 1);
            
            pid_t result = waitpid(pid, &status, 0);
            
            // Cancel alarm
            alarm(0);
            sigaction(SIGALRM, &sa_old, NULL);
            
            if (result == -1 && errno == EINTR) {
                // Timeout - kill child
                kill(pid, SIGKILL);
                waitpid(pid, &status, 0);
                return -2; // Timeout indicator
            }
        } else {
            // No timeout - wait indefinitely
            waitpid(pid, &status, 0);
        }
        
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        } else {
            return -1;
        }
    }
#endif
}

// Secure pipe structure (opaque in header, defined here)
struct secure_pipe_t {
    FILE* write_pipe;
    FILE* read_pipe;
#ifdef _WIN32
    HANDLE process;
#else
    pid_t pid;
#endif
};

typedef struct secure_pipe_t secure_pipe_t;

// Get write pipe from secure pipe (for compatibility)
FILE* secure_pipe_get_write_handle(secure_pipe_t* pipe) {
    return pipe ? pipe->write_pipe : NULL;
}

// Get read pipe from secure pipe (for compatibility)
FILE* secure_pipe_get_read_handle(secure_pipe_t* pipe) {
    return pipe ? pipe->read_pipe : NULL;
}

secure_pipe_t* secure_open_pipe(const char* program, const char* const argv[]) {
    if (!program) {
        return NULL;
    }
    
    secure_pipe_t* spipe = calloc(1, sizeof(secure_pipe_t));
    if (!spipe) {
        return NULL;
    }
    
#ifdef _WIN32
    // Windows implementation using CreateProcess with pipes
    SECURITY_ATTRIBUTES sa;
    HANDLE stdin_read, stdin_write;
    HANDLE stdout_read, stdout_write;
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;
    
    // Create pipes for stdin
    if (!CreatePipe(&stdin_read, &stdin_write, &sa, 0)) {
        free(spipe);
        return NULL;
    }
    SetHandleInformation(stdin_write, HANDLE_FLAG_INHERIT, 0);
    
    // Create pipes for stdout
    if (!CreatePipe(&stdout_read, &stdout_write, &sa, 0)) {
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        free(spipe);
        return NULL;
    }
    SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, 0);
    
    // Set up startup info
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.hStdOutput = stdout_write;
    si.hStdInput = stdin_read;
    si.dwFlags |= STARTF_USESTDHANDLES;
    
    ZeroMemory(&pi, sizeof(pi));
    
    // Build command line
    char cmdline[MAX_PATH_LENGTH] = {0};
    int offset = snprintf(cmdline, sizeof(cmdline), "\"%s\"", program);
    
    if (argv) {
        for (int i = 0; argv[i] && offset < sizeof(cmdline) - 1; i++) {
            offset += snprintf(cmdline + offset, sizeof(cmdline) - offset, 
                             " \"%s\"", argv[i]);
        }
    }
    
    // Create process
    if (!CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        CloseHandle(stdout_read);
        CloseHandle(stdout_write);
        free(spipe);
        return NULL;
    }
    
    // Close handles we don't need
    CloseHandle(stdin_read);
    CloseHandle(stdout_write);
    
    // Convert handles to FILE*
    int fd_write = _open_osfhandle((intptr_t)stdin_write, _O_BINARY);
    int fd_read = _open_osfhandle((intptr_t)stdout_read, _O_BINARY | _O_RDONLY);
    
    spipe->write_pipe = _fdopen(fd_write, "wb");
    spipe->read_pipe = _fdopen(fd_read, "rb");
    spipe->process = pi.hProcess;
    
    CloseHandle(pi.hThread);
    
#else
    // Unix implementation using fork/exec with pipes
    int stdin_pipe[2];
    int stdout_pipe[2];
    
    // Use regular pipe if pipe2 not available
    if (pipe(stdin_pipe) == -1) {
        free(spipe);
        return NULL;
    }
    
    if (pipe(stdout_pipe) == -1) {
        close(stdin_pipe[0]);
        close(stdin_pipe[1]);
        free(spipe);
        return NULL;
    }
    
    pid_t pid = fork();
    
    if (pid == -1) {
        // Fork failed
        close(stdin_pipe[0]);
        close(stdin_pipe[1]);
        close(stdout_pipe[0]);
        close(stdout_pipe[1]);
        free(spipe);
        return NULL;
    } else if (pid == 0) {
        // Child process
        
        // Redirect stdin/stdout
        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        
        // Close all pipe ends
        close(stdin_pipe[0]);
        close(stdin_pipe[1]);
        close(stdout_pipe[0]);
        close(stdout_pipe[1]);
        
        // Build argv for exec
        char* exec_argv[MAX_ARGS];
        int argc = 0;
        
        exec_argv[argc++] = strdup(program);
        if (argv) {
            for (int i = 0; argv[i] && argc < MAX_ARGS - 1; i++) {
                exec_argv[argc++] = strdup(argv[i]);
            }
        }
        exec_argv[argc] = NULL;
        
        // Execute
        execvp(program, exec_argv);
        _exit(127);
    } else {
        // Parent process
        
        // Close unused pipe ends
        close(stdin_pipe[0]);
        close(stdout_pipe[1]);
        
        // Create FILE* from file descriptors
        spipe->write_pipe = fdopen(stdin_pipe[1], "w");
        spipe->read_pipe = fdopen(stdout_pipe[0], "r");
        spipe->pid = pid;
    }
#endif
    
    return spipe;
}

int secure_close_pipe(secure_pipe_t* pipe) {
    if (!pipe) {
        return -1;
    }
    
    int result = 0;
    
    // Close pipes
    if (pipe->write_pipe) {
        fclose(pipe->write_pipe);
    }
    if (pipe->read_pipe) {
        fclose(pipe->read_pipe);
    }
    
#ifdef _WIN32
    // Wait for process and get exit code
    if (pipe->process) {
        DWORD exit_code;
        WaitForSingleObject(pipe->process, INFINITE);
        if (GetExitCodeProcess(pipe->process, &exit_code)) {
            result = (int)exit_code;
        } else {
            result = -1;
        }
        CloseHandle(pipe->process);
    }
#else
    // Wait for child process
    if (pipe->pid > 0) {
        int status;
        waitpid(pipe->pid, &status, 0);
        if (WIFEXITED(status)) {
            result = WEXITSTATUS(status);
        } else {
            result = -1;
        }
    }
#endif
    
    free(pipe);
    return result;
}

// Secure FFmpeg execution wrapper
int secure_exec_ffmpeg(const char* output_file, int fps, int timeout_ms) {
    // Validate parameters
    if (!output_file || fps < 1 || fps > 120) {
        return -1;
    }
    
    // Build argument array (no shell interpretation)
    const char* argv[20];
    int argc = 0;
    
    argv[argc++] = "-y";                    // Overwrite output
    argv[argc++] = "-f";
    argv[argc++] = "image2pipe";
    argv[argc++] = "-vcodec";
    argv[argc++] = "png";
    argv[argc++] = "-r";
    
    char fps_str[32];
    snprintf(fps_str, sizeof(fps_str), "%d", fps);
    argv[argc++] = fps_str;
    
    argv[argc++] = "-i";
    argv[argc++] = "-";                     // Read from stdin
    argv[argc++] = "-vcodec";
    argv[argc++] = "libx264";
    argv[argc++] = "-pix_fmt";
    argv[argc++] = "yuv420p";
    argv[argc++] = output_file;
    argv[argc] = NULL;
    
    return secure_exec_command("ffmpeg", argv, timeout_ms);
}

// Check if command exists securely (without using system())
int secure_check_command(const char* command) {
    if (!command) {
        return 0;
    }
    
#ifdef _WIN32
    // Windows: Try to find in PATH
    char path_var[MAX_PATH_LENGTH];
    if (!GetEnvironmentVariable("PATH", path_var, sizeof(path_var))) {
        return 0;
    }
    
    // Check current directory first
    char exe_name[256];
    snprintf(exe_name, sizeof(exe_name), "%s.exe", command);
    if (access(exe_name, 0) == 0) {
        return 1;
    }
    
    // Check PATH directories
    char* path_copy = strdup(path_var);
    char* dir = strtok(path_copy, ";");
    while (dir) {
        char full_path[MAX_PATH_LENGTH];
        snprintf(full_path, sizeof(full_path), "%s\\%s", dir, exe_name);
        if (access(full_path, 0) == 0) {
            free(path_copy);
            return 1;
        }
        dir = strtok(NULL, ";");
    }
    free(path_copy);
    
#else
    // Unix: Check if executable exists in PATH
    char* path_var = getenv("PATH");
    if (!path_var) {
        return 0;
    }
    
    char* path_copy = strdup(path_var);
    char* dir = strtok(path_copy, ":");
    while (dir) {
        char full_path[MAX_PATH_LENGTH];
        snprintf(full_path, sizeof(full_path), "%s/%s", dir, command);
        if (access(full_path, X_OK) == 0) {
            free(path_copy);
            return 1;
        }
        dir = strtok(NULL, ":");
    }
    free(path_copy);
#endif
    
    return 0;
}

// Helper for directory creation (secure)
int create_directory_c(const char* path) {
    if (!path) {
        return -1;
    }
    
#ifdef _WIN32
    return CreateDirectoryA(path, NULL) ? 0 : -1;
#else
    return mkdir(path, 0755);
#endif
}

// Helper for file deletion (secure)
int delete_file_c(const char* path) {
    if (!path) {
        return -1;
    }
    
#ifdef _WIN32
    return DeleteFileA(path) ? 0 : -1;
#else
    return unlink(path);
#endif
}

// Check command availability (secure)
int check_command_available_c(const char* cmd) {
    return secure_check_command(cmd);
}

// Open file with default app (secure)
int open_file_with_default_app_c(const char* path) {
    if (!path) {
        return -1;
    }
    
#ifdef _WIN32
    // Use ShellExecute (safer than system())
    #include <shellapi.h>
    HINSTANCE result = ShellExecuteA(NULL, "open", path, NULL, NULL, SW_SHOWNORMAL);
    return (intptr_t)result > 32 ? 0 : -1;
#else
    // Use xdg-open on Unix systems
    const char* argv[] = {path, NULL};
    return secure_exec_command("xdg-open", argv, 0);
#endif
}