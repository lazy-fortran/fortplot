#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

// Platform-specific includes
#ifdef _WIN32
    #include <windows.h>
    #include <shellapi.h>
    #include <io.h>
    #include <direct.h>
    #include <process.h>
    #define access _access
    #define mkdir(path, mode) _mkdir(path)
    #define R_OK 4
#else
    #include <sys/stat.h>
    #include <unistd.h>
    #include <sys/wait.h>
#endif

// Secure directory creation
int create_directory_c(const char* path) {
    if (path == NULL || strlen(path) == 0) {
        return -1;
    }
    
    // Use mkdir system call directly - safer than shell commands
    // This creates the final directory, not parent directories
    #ifdef _WIN32
        if (mkdir(path) == 0) {
            return 0;  // Success
        }
        
        if (errno == EEXIST) {
            // Directory already exists - check if it's actually a directory
            DWORD attrs = GetFileAttributesA(path);
            if (attrs != INVALID_FILE_ATTRIBUTES && (attrs & FILE_ATTRIBUTE_DIRECTORY)) {
                return 0;  // Success - directory exists
            }
        }
    #else
        if (mkdir(path, 0755) == 0) {
            return 0;  // Success
        }
        
        if (errno == EEXIST) {
            // Directory already exists - check if it's actually a directory
            struct stat st;
            if (stat(path, &st) == 0 && S_ISDIR(st.st_mode)) {
                return 0;  // Success - directory exists
            }
        }
    #endif
    
    // For parent directory creation, we need to handle it recursively
    char* path_copy = strdup(path);
    if (path_copy == NULL) {
        return -1;
    }
    
    #ifdef _WIN32
        char* slash = strrchr(path_copy, '\\');
        if (slash == NULL) {
            slash = strrchr(path_copy, '/');  // Support forward slash too
        }
    #else
        char* slash = strrchr(path_copy, '/');
    #endif
    
    if (slash != NULL && slash != path_copy) {
        *slash = '\0';
        // Recursively create parent
        int parent_result = create_directory_c(path_copy);
        if (parent_result != 0) {
            free(path_copy);
            return parent_result;
        }
        
        // Now try to create the directory again
        #ifdef _WIN32
            if (mkdir(path) == 0 || errno == EEXIST) {
                free(path_copy);
                return 0;
            }
        #else
            if (mkdir(path, 0755) == 0 || errno == EEXIST) {
                free(path_copy);
                return 0;
            }
        #endif
    }
    
    free(path_copy);
    return -1;
}

// Secure file opening with default application
int open_file_with_default_app_c(const char* filename) {
    if (filename == NULL || strlen(filename) == 0) {
        return -1;
    }
    
    // Check if file exists first
    if (access(filename, R_OK) != 0) {
        return -2;  // File doesn't exist or not readable
    }
    
    #ifdef _WIN32
        // Windows: Use ShellExecuteA to open with default application
        HINSTANCE result = ShellExecuteA(NULL, "open", filename, NULL, NULL, SW_SHOWNORMAL);
        if ((uintptr_t)result > 32) {
            return 0;  // Success
        } else {
            return -1;  // Error occurred
        }
    #else
        // Unix/Linux: Use fork and exec
        pid_t pid = fork();
        if (pid == 0) {
            // Child process - try different methods to open file
            
            // Try xdg-open first (most Unix-like systems)
            char *args_xdg[] = {"xdg-open", (char*)filename, NULL};
            execvp("xdg-open", args_xdg);
            
            // If xdg-open failed, try open (macOS)
            char *args_open[] = {"open", (char*)filename, NULL};
            execvp("open", args_open);
            
            // If both failed, exit with error
            _exit(127);
        } else if (pid > 0) {
            // Parent process - don't wait for child to complete
            // This allows the application to open asynchronously
            return 0;  // Success (child process started)
        } else {
            // fork failed
            return -1;
        }
    #endif
}

// Secure command availability check
int check_command_available_c(const char* command_name) {
    if (command_name == NULL || strlen(command_name) == 0) {
        return -1;
    }
    
    #ifdef _WIN32
        // Windows: Use 'where' command instead of 'which'
        char command[512];
        int ret = snprintf(command, sizeof(command), "where \"%s\" >nul 2>&1", command_name);
        if (ret >= sizeof(command) || ret < 0) {
            return -1;  // Command too long
        }
        
        int result = system(command);
        return (result == 0) ? 0 : -1;
    #else
        // Unix/Linux: Use fork and exec with 'which'
        pid_t pid = fork();
        if (pid == 0) {
            // Child process - redirect stdout/stderr to /dev/null
            freopen("/dev/null", "w", stdout);
            freopen("/dev/null", "w", stderr);
            
            char *args[] = {"which", (char*)command_name, NULL};
            execvp("which", args);
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
    #endif
}

// Secure file deletion
int delete_file_c(const char* filename) {
    if (filename == NULL || strlen(filename) == 0) {
        return -1;
    }
    
    #ifdef _WIN32
        // Windows: Use DeleteFileA
        if (DeleteFileA(filename)) {
            return 0;  // Success
        }
        
        DWORD error = GetLastError();
        if (error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND) {
            return 0;  // File doesn't exist - consider this success
        }
        
        return -1;  // Error occurred
    #else
        // Unix/Linux: Use unlink system call directly - safer than shell commands
        if (unlink(filename) == 0) {
            return 0;  // Success
        }
        
        if (errno == ENOENT) {
            return 0;  // File doesn't exist - consider this success
        }
        
        return -1;  // Error occurred
    #endif
}