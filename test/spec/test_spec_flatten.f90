program test_spec_flatten
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use fortplot, only: wp, spec_t, json_to_spec, spec_to_json
    implicit none
    type(spec_t) :: spec, again
    integer :: status, i
    character(len=:), allocatable :: json, large
    character(len=24) :: number
    character(len=*), parameter :: encoding = &
        '"mark":"line","encoding":{"x":{"field":"x"},"y":{"field":"y"}}'

    call json_to_spec('{"data":{"values":[{"x":[1,2],"y":[1,4]}]},'// &
        '"transform":[{"flatten":["x","y"]}],'//encoding//'}', spec, status)
    if (status /= 0) error stop 'Cannot parse compact coordinates'
    if (spec%data%nrows /= 2) error stop 'Wrong flattened row count'
    if (any(spec%data%columns(1)%values /= [1.0_wp, 2.0_wp])) &
        error stop 'Flattened x values differ'
    if (any(spec%data%columns(2)%values /= [1.0_wp, 4.0_wp])) &
        error stop 'Flattened y values differ'

    call json_to_spec('{"transform":[{"flatten":["x","y"]}],'// &
        '"data":{"values":[{"series":"alpha","x":[1,2],"y":[3]},'// &
        '{"series":"beta","x":[],"y":[null,5]}]},'//encoding//'}', spec, status)
    if (status /= 0) error stop 'Transform before data failed'
    if (spec%data%nrows /= 4) error stop 'Unequal arrays must use longest length'
    if (any(spec%data%columns(1)%string_values /= &
        ['alpha', 'alpha', 'beta ', 'beta '])) error stop 'Scalar labels not repeated'
    if (.not. ieee_is_nan(spec%data%columns(3)%values(2))) &
        error stop 'Missing y must be null'
    if (.not. ieee_is_nan(spec%data%columns(2)%values(4))) &
        error stop 'Missing x must be null'
    if (.not. ieee_is_nan(spec%data%columns(3)%values(3))) &
        error stop 'Null array item must remain null'

    call json_to_spec('{"layer":[{"transform":[{"as":["x","y"],'// &
        '"flatten":["xs","ys"]}],'//encoding//'}],'// &
        '"data":{"values":[{"xs":[1,2],"ys":[3,4]}]}}', spec, status)
    if (status /= 0) error stop 'Layer transform on inherited data failed'
    if (spec%layers(1)%data%nrows /= 2) error stop 'Inherited row count wrong'
    if (any(spec%layers(1)%data%columns(3)%values /= [1.0_wp, 2.0_wp])) &
        error stop 'Flatten alias values differ'
    json = spec_to_json(spec)
    call json_to_spec(json, again, status)
    if (status /= 0) error stop 'Array aliases cannot round trip'

    call json_to_spec('{"data":{"values":[{"x":[],"y":[]}]},'// &
        '"transform":[{"flatten":["x","y"]}],'//encoding//'}', spec, status)
    if (status /= 0 .or. spec%data%nrows /= 0) error stop 'Empty arrays failed'
    call json_to_spec('{"data":{"values":[]},"transform":['// &
        '{"flatten":["x","y"]}],'//encoding//'}', spec, status)
    if (status /= 0) error stop 'Empty dataset failed'

    call json_to_spec('{"data":{"values":[{"x":[1],"y":[2]}]},'// &
        encoding//'}', spec, status)
    if (status /= 84) error stop 'Arrays must require an explicit flatten'
    call json_to_spec('{"transform":[{"calculate":"1","as":"x"}]}', &
        spec, status)
    if (status /= 79) error stop 'Unsupported transform not diagnosed'
    call json_to_spec('{"data":{"values":[{"x":[1,]}]}}', spec, status)
    if (status == 0) error stop 'Trailing array comma accepted'
    call json_to_spec('{"data":{"values":[{"x":[1,2', spec, status)
    if (status == 0) error stop 'Truncated array accepted'
    call json_to_spec('{"data":{"values":[{"x":[[1]]}]}}', spec, status)
    if (status == 0) error stop 'Nested array silently accepted'

    call json_to_spec('{"data":{"values":[{"x":[1,2],"s":["A","B"]}]},'// &
        '"transform":[{"flatten":["x","s"]}]}', spec, status)
    if (status /= 0) error stop 'String arrays failed'
    if (any(spec%data%columns(2)%string_values /= ['A', 'B'])) &
        error stop 'String array values differ'

    call json_to_spec('{"data":{"values":[{"x":[1,2,3],"s":["",null]}]},'// &
        '"transform":[{"flatten":["x","s"]}]}', spec, status)
    if (status /= 0) error stop 'Null string array failed'
    if (.not. spec%data%columns(2)%string_valid(1)) error stop 'Empty string lost'
    if (any(spec%data%columns(2)%string_valid(2:3))) &
        error stop 'Null strings confused with empty string'
    json = spec_to_json(spec)
    call json_to_spec(json, again, status)
    if (status /= 0) error stop 'Null string round trip failed'
    if (any(again%data%columns(2)%string_valid .neqv. &
        spec%data%columns(2)%string_valid)) error stop 'Null string validity lost'
    call json_to_spec('{"data":{"values":[{"a":[1],"x":1,"y":2}]},'// &
        '"encoding":{"x":{"field":"a"}},"layer":[{'//encoding//'}]}', &
        spec, status)
    if (status /= 0) error stop 'Layer encoding override rejected parent array'

    call json_to_spec('{"data":{"values":[{"a":[1,2],"b":["","B"]}]},'// &
        '"transform":[{"flatten":["a"],"as":["x"]},'// &
        '{"flatten":["b"],"as":["s"]}]}', spec, status)
    if (status /= 0) error stop 'Chained flatten aliases failed'
    if (spec%data%nrows /= 4) error stop 'Chained flatten row count wrong'
    if (any(spec%data%columns(3)%values /= [1.0_wp, 1.0_wp, 2.0_wp, 2.0_wp])) &
        error stop 'Chained flatten scalar values differ'
    if (any(spec%data%columns(4)%string_values /= [character(len=1) :: '', 'B', '', 'B'])) &
        error stop 'Chained flatten string values differ'

    large = '{"data":{"values":[{"x":['
    do i = 1, 10000
        write (number, '(i0)') i
        if (i > 1) large = large//','
        large = large//trim(number)
    end do
    large = large//']}]},"transform":[{"flatten":["x"],"as":["value"]}]}'
    call json_to_spec(large, spec, status)
    if (status /= 0) error stop 'Large array failed'
    if (spec%data%nrows /= 10000) error stop 'Large array truncated'
    if (spec%data%columns(2)%values(10000) /= 10000.0_wp) &
        error stop 'Large array values differ'
    print *, 'PASS: explicit flatten, aliases, inheritance, nulls and arrays'
end program test_spec_flatten
