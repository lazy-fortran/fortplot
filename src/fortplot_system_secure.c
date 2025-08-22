#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

// Platform-specific includes
#ifdef _WIN32
    #include <windows.h>
    #include <shellapi.h>
    #include <shlobj.h>
    #include <io.h>
    #include <direct.h>
    #include <process.h>
    #define access _access
    #define R_OK 4
#else
    #include <sys/stat.h>
    #include <unistd.h>
    #include <sys/wait.h>
#endif

// Helper function to normalize path separators for Windows
#ifdef _WIN32
static char* normalize_path_windows(const char* path) {
    if (path == NULL) return NULL;
    
    char* normalized = strdup(path);
    if (normalized == NULL) return NULL;
    
    // Convert forward slashes to backslashes on Windows
    for (char* p = normalized; *p; p++) {
        if (*p == '/') {
            *p = '\\';
        }
    }
    return normalized;
}
#endif

// Helper function to create directory recursively on Windows - simple approach
#ifdef _WIN32
static int create_directory_recursive_windows(const char* path) {
    if (path == NULL || strlen(path) == 0) {
        return -1;
    }
    
    // Try to create the directory directly first
    if (_mkdir(path) == 0) {
        return 0;  // Success
    }
    
    // If it exists and is a directory, that's fine
    if (errno == EEXIST) {
        DWORD attrs = GetFileAttributesA(path);
        if (attrs != INVALID_FILE_ATTRIBUTES && (attrs & FILE_ATTRIBUTE_DIRECTORY)) {
            return 0;  // Success - directory exists
        }
    }
    
    // If creation failed due to missing parent, create parent first
    if (errno == ENOENT) {
        char* path_copy = strdup(path);
        if (path_copy == NULL) {
            return -1;
        }
        
        // Find the last backslash or forward slash
        char* last_backslash = strrchr(path_copy, '\\');
        char* last_forwardslash = strrchr(path_copy, '/');
        char* last_slash = (last_backslash > last_forwardslash) ? last_backslash : last_forwardslash;
        if (last_slash != NULL && last_slash != path_copy) {
            // Don't try to create root directories like "C:" or single letters
            if (!(last_slash == path_copy + 2 && path_copy[1] == ':')) {
                *last_slash = '\0';
                
                // Recursively create parent
                if (create_directory_recursive_windows(path_copy) == 0) {
                    free(path_copy);
                    // Now try to create our directory again
                    if (_mkdir(path) == 0 || errno == EEXIST) {
                        return 0;
                    }
                    return -1;
                }
            }
        }
        free(path_copy);
    }
    
    return -1;
}
#endif

// Secure directory creation with recursive parent creation
int create_directory_c(const char* path) {
    if (path == NULL || strlen(path) == 0) {
        return -1;
    }
    
    #ifdef _WIN32
        // Special handling for Unix-style /tmp directory on Windows
        char* effective_path = NULL;
        if (strcmp(path, "/tmp") == 0) {
            // Map /tmp to Windows temp directory
            char* temp_dir = getenv("TEMP");
            if (temp_dir == NULL) {
                temp_dir = "C:\\Windows\\Temp";
            }
            effective_path = strdup(temp_dir);
        } else if (strncmp(path, "/tmp/", 5) == 0) {
            // Map /tmp/filename to %TEMP%\filename
            char* temp_dir = getenv("TEMP");
            if (temp_dir == NULL) {
                temp_dir = "C:\\Windows\\Temp";
            }
            size_t len = strlen(temp_dir) + strlen(path) - 4 + 1; // -4 for "/tmp", +1 for null
            effective_path = malloc(len);
            if (effective_path != NULL) {
                snprintf(effective_path, len, "%s%s", temp_dir, path + 4); // skip "/tmp"
            }
        } else {
            effective_path = strdup(path);
        }
        
        if (effective_path == NULL) {
            return -1;
        }
        
        // Normalize path for Windows (convert / to \)
        char* normalized_path = normalize_path_windows(effective_path);
        free(effective_path);
        
        if (normalized_path == NULL) {
            return -1;
        }
        
        // Use recursive helper function
        int result = create_directory_recursive_windows(normalized_path);
        free(normalized_path);
        return result;
        
    #else
        // Unix/Linux path handling
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
        
        // For parent directory creation, we need to handle it recursively
        char* path_copy = strdup(path);
        if (path_copy == NULL) {
            return -1;
        }
        
        char* slash = strrchr(path_copy, '/');
        if (slash != NULL && slash != path_copy) {
            *slash = '\0';
            // Recursively create parent
            int parent_result = create_directory_c(path_copy);
            if (parent_result != 0) {
                free(path_copy);
                return parent_result;
            }
            
            // Now try to create the directory again
            if (mkdir(path, 0755) == 0 || errno == EEXIST) {
                free(path_copy);
                return 0;
            }
        }
        
        free(path_copy);
        return -1;
    #endif
}

// Secure file opening with default application
int open_file_with_default_app_c(const char* filename) {
    if (filename == NULL || strlen(filename) == 0) {
        return -1;
    }
    
    #ifdef _WIN32
        // Special handling for Unix-style /tmp paths on Windows
        char* effective_path = NULL;
        if (strncmp(filename, "/tmp/", 5) == 0) {
            // Map /tmp/filename to %TEMP%\filename
            char* temp_dir = getenv("TEMP");
            if (temp_dir == NULL) {
                temp_dir = "C:\\Windows\\Temp";
            }
            size_t len = strlen(temp_dir) + strlen(filename) - 4 + 1; // -4 for "/tmp", +1 for null
            effective_path = malloc(len);
            if (effective_path != NULL) {
                snprintf(effective_path, len, "%s%s", temp_dir, filename + 4); // skip "/tmp"
                // Normalize path separators
                for (char* p = effective_path; *p; p++) {
                    if (*p == '/') {
                        *p = '\\';
                    }
                }
            }
        } else {
            effective_path = normalize_path_windows(filename);
        }
        
        if (effective_path == NULL) {
            return -1;
        }
        
        // Check if file exists first
        if (access(effective_path, R_OK) != 0) {
            free(effective_path);
            return -2;  // File doesn't exist or not readable
        }
        
        // Windows: Use ShellExecuteA to open with default application
        HINSTANCE result = ShellExecuteA(NULL, "open", effective_path, NULL, NULL, SW_SHOWNORMAL);
        free(effective_path);
        
        if ((uintptr_t)result > 32) {
            return 0;  // Success
        } else {
            return -1;  // Error occurred
        }
    #else
        // Check if file exists first
        if (access(filename, R_OK) != 0) {
            return -2;  // File doesn't exist or not readable
        }
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
        // Special handling for Unix-style /tmp paths on Windows
        char* effective_path = NULL;
        if (strncmp(filename, "/tmp/", 5) == 0) {
            // Map /tmp/filename to %TEMP%\filename
            char* temp_dir = getenv("TEMP");
            if (temp_dir == NULL) {
                temp_dir = "C:\\Windows\\Temp";
            }
            size_t len = strlen(temp_dir) + strlen(filename) - 4 + 1; // -4 for "/tmp", +1 for null
            effective_path = malloc(len);
            if (effective_path != NULL) {
                snprintf(effective_path, len, "%s%s", temp_dir, filename + 4); // skip "/tmp"
                // Normalize path separators
                for (char* p = effective_path; *p; p++) {
                    if (*p == '/') {
                        *p = '\\';
                    }
                }
            }
        } else {
            effective_path = normalize_path_windows(filename);
        }
        
        if (effective_path == NULL) {
            return -1;
        }
        
        // Windows: Use DeleteFileA
        if (DeleteFileA(effective_path)) {
            free(effective_path);
            return 0;  // Success
        }
        
        DWORD error = GetLastError();
        free(effective_path);
        
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