# File Security

## Path Validation

fortplot validates all file paths to prevent security vulnerabilities.

### Blocked Patterns

```fortran
! These paths are rejected
call fig%savefig("./malicious.png")      ! Current directory reference  
call fig%savefig("../config.txt")       ! Directory traversal
call fig%savefig("/.bashrc")            ! Hidden dot files
call fig%savefig("file; rm -rf /")      ! Command injection
```

### Safe Patterns

```fortran
! These paths are allowed
call fig%savefig("output.png")          ! Simple filename
call fig%savefig("plots/figure1.png")   ! Subdirectory
call fig%savefig("/tmp/output.pdf")     ! Absolute path
```

### Validation Rules

- **No directory traversal**: `..` patterns blocked
- **No current directory**: `./` patterns blocked  
- **No hidden files**: `/.` patterns blocked
- **No command injection**: Shell metacharacters blocked
- **No control characters**: Null bytes, newlines blocked

### Implementation

```fortran
use fortplot_security, only: is_safe_path

if (.not. is_safe_path(filename)) then
    ! Path rejected - file not created
    return
end if
```

All `savefig()` calls automatically validate paths. Invalid paths are logged and rejected without creating files.