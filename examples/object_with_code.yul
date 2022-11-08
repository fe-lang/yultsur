object "runtime" {
    code {
        function f() -> t {
            let x := add(1, 2)
            t := add(x, 8)
        }
    }
}