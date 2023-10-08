(module
    (type $i32_to_void (func (param i32)))
    (import "zig" "printValue" (func $printValue (type $i32_to_void)))
    (func (export "example")
        i32.const 2147483647
        i32.const 2
        i32.mul
        call $printValue
    )
)