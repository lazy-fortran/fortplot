#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

// Maximum recursion depth to prevent infinite loops
#define MAX_RECURSION_DEPTH 50

// Internal recursive function with depth tracking
static int create_directory_windows_recursive(const char* path, int depth) {
    if (!path || strlen(path) == 0) {
        return 0;
    }
    
    // Prevent infinite recursion
    if (depth > MAX_RECURSION_DEPTH) {
        return 0;  // Too deep, abort
    }
    
    // First check if directory already exists
    DWORD attrib = GetFileAttributesA(path);
    if (attrib != INVALID_FILE_ATTRIBUTES && 
        (attrib & FILE_ATTRIBUTE_DIRECTORY)) {
        // Directory already exists
        return 1;
    }
    
    // Try to create directory using _mkdir
    if (_mkdir(path) == 0) {
        return 1;  // Success
    }
    
    // Check errno to see why it failed
    if (errno == EEXIST) {
        // Already exists - double check it's a directory
        attrib = GetFileAttributesA(path);
        if (attrib != INVALID_FILE_ATTRIBUTES && 
            (attrib & FILE_ATTRIBUTE_DIRECTORY)) {
            return 1;
        }
        return 0;  // Exists but not a directory
    }
    
    if (errno != ENOENT) {
        // Some other error (permissions, etc)
        return 0;
    }
    
    // ENOENT means parent doesn't exist
    // SECURITY FIX: Buffer overflow protection for parent path
    char parent[MAX_PATH];
    size_t path_len = strlen(path);
    
    if (path_len >= MAX_PATH) {
        return 0; // Path too long - prevent buffer overflow
    }
    
    strncpy(parent, path, MAX_PATH - 1);
    parent[MAX_PATH - 1] = '\0';
    
    // Avoid infinite loop on root paths
    size_t len = strlen(parent);
    if (len <= 3) {  // e.g., "C:\" or "/"
        return 0;  // Can't create root
    }
    
    // Find last separator
    char* last_sep = strrchr(parent, '\\');
    if (!last_sep) {
        last_sep = strrchr(parent, '/');
    }
    
    if (last_sep && last_sep != parent && (last_sep - parent) > 2) {
        // Terminate at separator to get parent path
        *last_sep = '\0';
        
        // Don't recurse if parent is same as current (avoid infinite loop)
        if (strcmp(parent, path) == 0) {
            return 0;
        }
        
        // Recursively create parent with increased depth
        if (!create_directory_windows_recursive(parent, depth + 1)) {
            return 0;  // Parent creation failed
        }
        
        // Try again to create the original directory
        if (_mkdir(path) == 0) {
            return 1;
        }
        
        // Final check if it now exists
        attrib = GetFileAttributesA(path);
        if (attrib != INVALID_FILE_ATTRIBUTES && 
            (attrib & FILE_ATTRIBUTE_DIRECTORY)) {
            return 1;
        }
    }
    
    return 0;  // Failed
}

// Public function
int create_directory_windows_c(const char* path) {
    return create_directory_windows_recursive(path, 0);
}

#else
// Unix implementation - simpler
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>

// SECURITY ENHANCEMENT: Define MAX_PATH for Unix systems
#ifndef MAX_PATH
#define MAX_PATH 4096
#endif

int create_directory_windows_c(const char* path) {
    struct stat st;
    
    // Check if already exists
    if (stat(path, &st) == 0) {
        if (S_ISDIR(st.st_mode)) {
            return 1;  // Already exists as directory
        }
        return 0;  // Exists but not a directory
    }
    
    // Try to create
    if (mkdir(path, 0755) == 0) {
        return 1;
    }
    
    // If parent doesn't exist, create it
    if (errno == ENOENT) {
        // SECURITY FIX: Safe parent path handling without strdup leak
        static char parent_buffer[MAX_PATH];
        
        if (strlen(path) >= sizeof(parent_buffer)) {
            return 0; // Path too long - prevent buffer overflow
        }
        
        strncpy(parent_buffer, path, sizeof(parent_buffer) - 1);
        parent_buffer[sizeof(parent_buffer) - 1] = '\0';
        char* parent = parent_buffer;
        
        // Find last separator
        char* last_sep = strrchr(parent, '/');
        if (last_sep && last_sep != parent) {
            *last_sep = '\0';
            
            // Recursively create parent - SECURITY FIX: No free() needed for static buffer
            int result = create_directory_windows_c(parent);
            
            if (!result) {
                return 0;
            }
            
            // Try again
            if (mkdir(path, 0755) == 0) {
                return 1;
            }
        }
    }
    
    // Final check
    if (stat(path, &st) == 0 && S_ISDIR(st.st_mode)) {
        return 1;
    }
    
    return 0;
}
#endif