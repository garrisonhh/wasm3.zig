(module
    (type $i32_to_void (func (param i32)))
    (import "zig" "printValue" (func $printValue (type $i32_to_void)))
    (func (export "example")
        i32.const 420
        call $printValue
    )
)