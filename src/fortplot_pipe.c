#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

typedef struct {
    FILE* pipe;
    pid_t pid;
} ffmpeg_pipe_t;

static ffmpeg_pipe_t current_pipe = {NULL, 0};

// Forward declarations
int close_ffmpeg_pipe_c(void);

// Open pipe to ffmpeg command
int open_ffmpeg_pipe_c(const char* filename, int fps) {
    char command[512];
    
    // Close any existing pipe
    if (current_pipe.pipe != NULL) {
        close_ffmpeg_pipe_c();
    }
    
    // Build ffmpeg command for pipe input with quality settings for validation
    snprintf(command, sizeof(command),
        "ffmpeg -f image2pipe -vcodec png -r %d -i - "
        "-c:v libx264 -pix_fmt yuv420p -crf 12 -preset ultrafast -tune stillimage -y %s 2>/dev/null",
        fps, filename);
    
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
    int status = system("which ffmpeg >/dev/null 2>&1");
    return (status == 0) ? 1 : 0;
}