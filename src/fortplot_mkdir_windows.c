#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#include <string.h>
#include <stdio.h>

// Create directory on Windows using Windows API
// Returns: 1 on success, 0 on failure
int create_directory_windows_c(const char* path) {
    if (!path || strlen(path) == 0) {
        return 0;
    }
    
    // First check if directory already exists
    DWORD attrib = GetFileAttributesA(path);
    if (attrib != INVALID_FILE_ATTRIBUTES && 
        (attrib & FILE_ATTRIBUTE_DIRECTORY)) {
        // Directory already exists
        return 1;
    }
    
    // Try to create directory using _mkdir (simpler than CreateDirectory)
    // _mkdir returns 0 on success, -1 on failure
    if (_mkdir(path) == 0) {
        return 1;  // Success
    }
    
    // If failed, could be because parent doesn't exist
    // Create parent directories recursively
    char parent[MAX_PATH];
    strncpy(parent, path, MAX_PATH - 1);
    parent[MAX_PATH - 1] = '\0';
    
    // Find last separator
    char* last_sep = strrchr(parent, '\\');
    if (!last_sep) {
        last_sep = strrchr(parent, '/');
    }
    
    if (last_sep && last_sep != parent) {
        // Terminate at separator to get parent path
        *last_sep = '\0';
        
        // Recursively create parent
        if (!create_directory_windows_c(parent)) {
            return 0;  // Parent creation failed
        }
        
        // Try again to create the original directory
        if (_mkdir(path) == 0) {
            return 1;
        }
    }
    
    // Check one more time if it exists (maybe created by another process)
    attrib = GetFileAttributesA(path);
    if (attrib != INVALID_FILE_ATTRIBUTES && 
        (attrib & FILE_ATTRIBUTE_DIRECTORY)) {
        return 1;
    }
    
    return 0;  // Failed
}

#else
// Unix implementation - fallback
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>

int create_directory_windows_c(const char* path) {
    // On Unix, use mkdir with proper permissions
    if (mkdir(path, 0755) == 0) {
        return 1;
    }
    
    // If it exists, that's OK
    if (errno == EEXIST) {
        struct stat st;
        if (stat(path, &st) == 0 && S_ISDIR(st.st_mode)) {
            return 1;
        }
    }
    
    return 0;
}
#endif