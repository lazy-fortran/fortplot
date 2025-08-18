#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <sys/wait.h>

// Secure directory creation
int create_directory_c(const char* path) {
    if (path == NULL || strlen(path) == 0) {
        return -1;
    }
    
    // Use mkdir system call directly - safer than shell commands
    // This creates the final directory, not parent directories
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
}

// Secure command availability check
int check_command_available_c(const char* command_name) {
    if (command_name == NULL || strlen(command_name) == 0) {
        return -1;
    }
    
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
}

// Secure file deletion
int delete_file_c(const char* filename) {
    if (filename == NULL || strlen(filename) == 0) {
        return -1;
    }
    
    // Use unlink system call directly - safer than shell commands
    if (unlink(filename) == 0) {
        return 0;  // Success
    }
    
    if (errno == ENOENT) {
        return 0;  // File doesn't exist - consider this success
    }
    
    return -1;  // Error occurred
}