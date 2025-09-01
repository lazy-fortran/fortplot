#include <stdlib.h>
#include <string.h>
#ifdef __has_include
#  if __has_include(<strings.h>)
#    include <strings.h>
#  endif
#endif

/*
 * Return 1 if FORTPLOT_DEBUG is set to "1" or "true" (any case), else 0.
 * Using getenv ensures dynamic environment changes are visible during tests.
 */
int fortplot_is_debug_enabled(void) {
    const char *v = getenv("FORTPLOT_DEBUG");
    if (!v || !*v) return 0;
    if (strcmp(v, "1") == 0) return 1;
#ifdef strcasecmp
    if (strcasecmp(v, "true") == 0) return 1;
    if (strcasecmp(v, "false") == 0) return 0;
#else
    /* Fallback: case-sensitive checks if strcasecmp is unavailable */
    if (strcmp(v, "true") == 0) return 1;
    if (strcmp(v, "false") == 0) return 0;
#endif
    if (strcmp(v, "0") == 0) return 0;
    return 0;
}

/*
 * Portable environment mutators: set/unset variable at runtime.
 * - POSIX: use setenv/unsetenv
 * - Windows: use _putenv_s/putenv semantics (empty value unsets)
 */
static int fp_setenv_portable(const char *name, const char *value, int overwrite) {
#if defined(_WIN32)
    /* On Windows, environment is process-local; emulate overwrite semantics */
    if (!overwrite) {
        const char *cur = getenv(name);
        if (cur && *cur) return 0;
    }
    /* _putenv_s returns 0 on success */
    return _putenv_s(name, value ? value : "");
#else
    return setenv(name, value ? value : "", overwrite);
#endif
}

static int fp_unsetenv_portable(const char *name) {
#if defined(_WIN32)
    /* Empty value removes the variable in the Windows CRT */
    return _putenv_s(name, "");
#else
    return unsetenv(name);
#endif
}

/* Convenience helpers for tests: safely set/unset FORTPLOT_DEBUG */
int fortplot_debug_set_1(void) {
    return fp_setenv_portable("FORTPLOT_DEBUG", "1", 1);
}

int fortplot_debug_set_true(void) {
    return fp_setenv_portable("FORTPLOT_DEBUG", "true", 1);
}

int fortplot_debug_set_0(void) {
    return fp_setenv_portable("FORTPLOT_DEBUG", "0", 1);
}

int fortplot_debug_unset(void) {
    return fp_unsetenv_portable("FORTPLOT_DEBUG");
}
