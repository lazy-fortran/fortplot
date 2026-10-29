#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif
#include <stdlib.h>

int fortplot_test_color_setenv(const char *name, const char *value)
{
#ifdef _WIN32
    return _putenv_s(name, value);
#else
    return setenv(name, value, 1);
#endif
}

int fortplot_test_color_unsetenv(const char *name)
{
#ifdef _WIN32
    return _putenv_s(name, "");
#else
    return unsetenv(name);
#endif
}

int fortplot_test_color_env_supports_empty(void)
{
#ifdef _WIN32
    return 0;
#else
    return 1;
#endif
}
