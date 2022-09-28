static BUILTINS: phf::Map<&'static str, (&'static str, u64, u64)> = phf::phf_map! {
    "stop" => ("stop", 0, 0),
    "add" => ("add", 2, 1),
    "sub" => ("sub", 2, 1),
    "mul" => ("mul", 2, 1),
    "div" => ("div", 2, 1),
    "sdiv" => ("sdiv", 2, 1),
    "mod" => ("mod", 2, 1),
    "smod" => ("smod", 2, 1),
    "exp" => ("exp", 2, 1),
    "not" => ("not", 1, 1),
    "lt" => ("lt", 2, 1),
    "gt" => ("gt", 2, 1),
    "slt" => ("slt", 2, 1),
    "sgt" => ("sgt", 2, 1),
    "eq" => ("eq", 2, 1),
    "iszero" => ("iszero", 1, 1),
    "and" => ("and", 2, 1),
    "or" => ("or", 2, 1),
    "xor" => ("xor", 2, 1),
    "byte" => ("byte", 2, 1),
    "shl" => ("shl", 2, 1),
    "shr" => ("shr", 2, 1),
    "sar" => ("sar", 2, 1),
    "addmod" => ("addmod", 3, 1),
    "mulmod" => ("mulmod", 3, 1),
    "signextend" => ("signextend", 2, 1),
    "keccak256" => ("keccak256", 2, 1),
    "address" => ("address", 0, 1),
    "balance" => ("balance", 1, 1),
    "origin" => ("origin", 0, 1),
    "caller" => ("caller", 0, 1),
    "callvalue" => ("callvalue", 0, 1),
    "calldataload" => ("calldataload", 1, 1),
    "calldatasize" => ("calldatasize", 0, 1),
    "calldatacopy" => ("calldatacopy", 3, 0),
    "codesize" => ("codesize", 0, 1),
    "codecopy" => ("codecopy", 3, 0),
    "gasprice" => ("gasprice", 0, 1),
    "extcodesize" => ("extcodesize", 1, 1),
    "extcodecopy" => ("extcodecopy", 4, 0),
    "returndatasize" => ("returndatasize", 0, 1),
    "returndatacopy" => ("returndatacopy", 3, 0),
    "extcodehash" => ("extcodehash", 1, 1),
    "blockhash" => ("blockhash", 1, 1),
    "coinbase" => ("coinbase", 0, 1),
    "timestamp" => ("timestamp", 0, 1),
    "number" => ("number", 0, 1),
    "difficulty" => ("difficulty", 0, 1),
    "gaslimit" => ("gaslimit", 0, 1),
    "chainid" => ("chainid", 0, 1),
    "selfbalance" => ("selfbalance", 0, 1),
    "basefee" => ("basefee", 0, 1),
    "pop" => ("pop", 1, 0),
    "mload" => ("mload", 1, 1),
    "mstore" => ("mstore", 2, 0),
    "mstore8" => ("mstore8", 2, 0),
    "sload" => ("sload", 1, 1),
    "sstore" => ("sstore", 2, 0),
    // "jump" => ("jump", 1, 0),
    // "jumpi" => ("jumpi", 2, 0),
    // "pc" => ("pc", 0, 1),
    "msize" => ("msize", 0, 1),
    "gas" => ("gas", 0, 1),
    "log0" => ("log0", 2, 0),
    "log1" => ("log1", 3, 0),
    "log2" => ("log2", 4, 0),
    "log3" => ("log3", 5, 0),
    "log4" => ("log4", 6, 0),
    "create" => ("create", 3, 1),
    "call" => ("call", 7, 1),
    "callcode" => ("callcode", 7, 1),
    "return" => ("return", 2, 0),
    "delegatecall" => ("delegatecall", 6, 1),
    "staticcall" => ("staticcall", 6, 1),
    "create2" => ("create2", 4, 1),
    "revert" => ("revert", 2, 0),
    "invalid" => ("invalid", 0, 0),
    "selfdestruct" => ("selfdestruct", 1, 0),

    "memoryguard" => ("memoryguard", 1, 1),
};

pub trait Dialect {
    fn is_builtin(name: &str) -> bool {
        Self::builtin(name).is_some()
    }
    fn builtin(name: &str) -> Option<Builtin>;
}

pub struct Builtin {
    pub name: &'static str,
    pub parameters: u64,
    pub returns: u64,
}

pub struct EVMDialect;

impl Dialect for EVMDialect {
    fn builtin(name: &str) -> Option<Builtin> {
        BUILTINS
            .get(name)
            .map(|(name, parameters, returns)| Builtin {
                name: *name,
                parameters: *parameters,
                returns: *returns,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn some_basics() {
        assert!(EVMDialect::is_builtin("add"));
        assert!(!EVMDialect::is_builtin("dup"));
        assert!(!EVMDialect::is_builtin("dup1"));
        assert!(!EVMDialect::is_builtin("swap1"));
        assert!(!EVMDialect::is_builtin("jump"));
        assert!(!EVMDialect::is_builtin("jumpi"));
        assert!(!EVMDialect::is_builtin("pc"));
        assert!(EVMDialect::is_builtin("memoryguard"));
    }
}
