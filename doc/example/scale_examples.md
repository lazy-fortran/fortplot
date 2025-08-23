title: Scale Examples
---

# Scale Examples

This example demonstrates different axis scaling options including logarithmic and symmetric logarithmic (symlog) scales.

## Source Files

## Source Code

**Python:**· **Fortran:** [scale_examples.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/scale_examples/scale_examples.f90)

**Python:** **Python:** [scale_examples.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/scale_examples/scale_examples.py)

```fortran
program scale_examples
    !! Examples demonstrating logarithmic and symlog scales
    use fortplot
    implicit none

    call log_scale_demo()
    call symlog_scale_demo()

contains

    subroutine log_scale_demo()
        real(wp), dimension(50) :: x_exp, y_exp
        integer :: i

        print *, "=== Scale Examples ==="

        ! Generate exponential data for log scale
        x_exp = [(real(i, wp), i=1, 50)]
        y_exp = exp(x_exp * 0.2_wp)

        call figure()
        call plot(x_exp, y_exp)
        call set_yscale('log')
        call title('Log Scale Example')
        call xlabel('x')
        call ylabel('exp(0.2x)')
        call savefig('output/example/fortran/scale_examples/log_scale.png')
        call savefig('output/example/fortran/scale_examples/log_scale.pdf')
        call savefig('output/example/fortran/scale_examples/log_scale.txt')

        print *, "Created: log_scale.png/pdf/txt"

    end subroutine log_scale_demo

    subroutine symlog_scale_demo()
        real(wp), dimension(50) :: x_exp, y_symlog
        integer :: i

        ! Generate data that goes through zero for symlog
        x_exp = [(real(i, wp), i=1, 50)]
        y_symlog = x_exp**3 - 50.0_wp * x_exp

        call figure()
        call plot(x_exp, y_symlog)
        call set_yscale('symlog')  ! threshold parameter not supported yet
        call title('Symlog Scale Example')
        call xlabel('x')
        call ylabel('x^3 - 50x')
        call savefig('output/example/fortran/scale_examples/symlog_scale.png')
        call savefig('output/example/fortran/scale_examples/symlog_scale.pdf')
        call savefig('output/example/fortran/scale_examples/symlog_scale.txt')

        print *, "Created: symlog_scale.png/pdf/txt"

    end subroutine symlog_scale_demo

end program scale_examples
```

## Features Demonstrated

- **Logarithmic scaling**: For exponential growth visualization
- **Symmetric log**: Handles positive and negative values with log-like behavior
- **Linear threshold**: Symlog parameter controls transition to linear near zero
- **Automatic tick generation**: Smart tick placement for non-linear scales

## Output

### Log Scale

![log_scale.png](../../media/examples/scale_examples/log_scale.png)

ASCII output:
```
%PDF-1.4
%
2 0 obj
<<
/Type /Catalog
/Pages 3 0 R
>>
endobj
3 0 obj
<<
/Type /Pages
/Kids [4 0 R]
/Count 1
>>
endobj
4 0 obj
<<
/Type /Page
/Parent 3 0 R
/MediaBox [0 0 595.0 842.0]
/Resources <<
  /Font <<
    /F5 5 0 R
    /F6 6 0 R
  >>
>>
/Contents 7 0 R
>>
endobj
5 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj
6 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Symbol
>>
endobj
7 0 obj
<<
/Length 23
>>
stream
q
1 w
1 J
1 j
0 0 1 RG

endstream
endobj
xref
0 8
0000000000 65535 f
0000000000 00000 n
0000000013 00000 n
0000000056 00000 n
0000000106 00000 n
0000000244 00000 n
0000000307 00000 n
0000000367 00000 n
trailer
<<
/Size 8
/Root 2 0 R
>>
startxref
432
%%EOF
```

[Download PDF](../../media/examples/scale_examples/log_scale.pdf                                                                                                                                                                                                                                                   )

### Symlog Scale

![symlog_scale.png](../../media/examples/scale_examples/symlog_scale.png)

ASCII output:
```
%PDF-1.4
%
2 0 obj
<<
/Type /Catalog
/Pages 3 0 R
>>
endobj
3 0 obj
<<
/Type /Pages
/Kids [4 0 R]
/Count 1
>>
endobj
4 0 obj
<<
/Type /Page
/Parent 3 0 R
/MediaBox [0 0 595.0 842.0]
/Resources <<
  /Font <<
    /F5 5 0 R
    /F6 6 0 R
  >>
>>
/Contents 7 0 R
>>
endobj
5 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj
6 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Symbol
>>
endobj
7 0 obj
<<
/Length 23
>>
stream
q
1 w
1 J
1 j
0 0 1 RG

endstream
endobj
xref
0 8
0000000000 65535 f
0000000000 00000 n
0000000013 00000 n
0000000056 00000 n
0000000106 00000 n
0000000244 00000 n
0000000307 00000 n
0000000367 00000 n
trailer
<<
/Size 8
/Root 2 0 R
>>
startxref
432
%%EOF
```

[Download PDF](../../media/examples/scale_examples/symlog_scale.pdf                                                                                                                                                                                                                                                )

