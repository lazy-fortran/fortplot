#include <stddef.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>

int fortplot_list_directory(const char *path, char *buffer, size_t buffer_len) {
    size_t path_len;
    size_t used = 0;
    char search[MAX_PATH];
    HANDLE handle;
    WIN32_FIND_DATAA data;

    if (buffer_len == 0) {
        return -2;
    }

    path_len = strlen(path);
    if (path_len + 3 >= (size_t)MAX_PATH) {
        return -3;
    }

    strcpy(search, path);
    if (path_len > 0 && search[path_len - 1] != '\\' && search[path_len - 1] != '/') {
        search[path_len++] = '\\';
    }
    search[path_len++] = '*';
    search[path_len] = '\0';

    handle = FindFirstFileA(search, &data);
    if (handle == INVALID_HANDLE_VALUE) {
        return -1;
    }

    do {
        const char *name = data.cFileName;
        size_t name_len = strlen(name);

        if ((data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) {
            continue;
        }

        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) {
            continue;
        }

        if (used + name_len + 1 >= buffer_len) {
            FindClose(handle);
            return -2;
        }

        memcpy(buffer + used, name, name_len);
        used += name_len;
        buffer[used++] = '\n';
    } while (FindNextFileA(handle, &data));

    FindClose(handle);

    if (used < buffer_len) {
        buffer[used] = '\0';
    }

    return (int)used;
}

#else

#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static int is_regular_file(const char *dir, const char *name) {
    char full_path[PATH_MAX];
    struct stat st;

    if (snprintf(full_path, sizeof(full_path), "%s/%s", dir, name) >= (int)sizeof(full_path)) {
        return 0;
    }

    if (stat(full_path, &st) != 0) {
        return 0;
    }

    return S_ISREG(st.st_mode);
}

int fortplot_list_directory(const char *path, char *buffer, size_t buffer_len) {
    DIR *dir;
    struct dirent *entry;
    size_t used = 0;

    if (buffer_len == 0) {
        return -2;
    }

    dir = opendir(path);
    if (dir == NULL) {
        return -1;
    }

    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        size_t name_len = strlen(name);
        int is_dir = 0;

        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) {
            continue;
        }

#ifdef DT_DIR
        if (entry->d_type == DT_DIR) {
            is_dir = 1;
        }
#endif

#ifdef DT_REG
        if (entry->d_type == DT_REG) {
            is_dir = 0;
        }
#endif

#ifdef DT_UNKNOWN
        if (entry->d_type == DT_UNKNOWN) {
            is_dir = !is_regular_file(path, name);
        }
#endif

        if (is_dir) {
            continue;
        }

        if (!is_regular_file(path, name)) {
            continue;
        }

        if (used + name_len + 1 >= buffer_len) {
            closedir(dir);
            return -2;
        }

        memcpy(buffer + used, name, name_len);
        used += name_len;
        buffer[used++] = '\n';
    }

    closedir(dir);

    if (used < buffer_len) {
        buffer[used] = '\0';
    }

    return (int)used;
}

#endif

