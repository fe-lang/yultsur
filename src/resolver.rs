use std::{
    collections::{BTreeMap, HashMap},
    marker::PhantomData,
};

use crate::{dialect::Dialect, visitor::ASTModifier, yul::*};

/// Resolves all references in the given AST and returns a
/// hash map from id to function signature for each user-defined function.
pub fn resolve<D: Dialect>(ast: &mut Block) -> Result<BTreeMap<u64, FunctionSignature>, String> {
    let mut r = Resolver::<D>::new();
    r.visit_block(ast);
    if r.errors.is_empty() {
        Ok(std::mem::take(&mut r.function_signatures))
    } else {
        Err(r.errors.join("\n"))
    }
}

/// Resolves all references in `to_resolve` given an already resolved ast
/// `reference`. This can be used to resolve e.g. an expression to be evaluated.
/// This only considers symbols defined at the top level of the reference block.
pub fn resolve_inside<D: Dialect>(
    to_resolve: &mut Expression,
    reference: &Block,
) -> Result<(), String> {
    let mut r = Resolver::<D>::new();
    r.enter_block_immut(reference);
    for statement in &reference.statements {
        if let Statement::VariableDeclaration(var) = &statement {
            r.exit_variable_declaration_immut(var);
        }
    }
    r.visit_expression(to_resolve);
    if r.errors.is_empty() {
        Ok(())
    } else {
        Err(r.errors.join("\n"))
    }
}

struct Resolver<D: Dialect> {
    active_variables: Vec<HashMap<String, u64>>,
    active_functions: Vec<HashMap<String, u64>>,
    function_signatures: BTreeMap<u64, FunctionSignature>,
    // TODO we should not need that.
    marker: PhantomData<D>,
    errors: Vec<String>,
}

#[derive(Debug)]
pub struct FunctionSignature {
    pub parameters: u64,
    pub returns: u64,
}

fn find_symbol(table: &[HashMap<String, u64>], symbol: &String) -> Option<u64> {
    for map in table.iter().rev() {
        if let Some(id) = map.get(symbol) {
            return Some(*id);
        }
    }
    None
}

impl<D: Dialect> Resolver<D> {
    fn new() -> Resolver<D> {
        Resolver::<D> {
            active_variables: Vec::new(),
            active_functions: Vec::new(),
            function_signatures: BTreeMap::new(),
            marker: PhantomData,
            errors: Vec::new(),
        }
    }
    fn activate_variable(&mut self, symbol: &Identifier) {
        if let IdentifierID::Declaration(id) = symbol.id {
            if self
                .active_variables
                .last_mut()
                .unwrap()
                .insert(symbol.name.clone(), id)
                .is_some()
            {
                self.errors.push(format!(
                    "Identifier \"{}\" already declared at this point.",
                    symbol.name
                ))
            }
        } else {
            panic!()
        }
    }
    fn resolve(&mut self, symbol: &String) -> IdentifierID {
        if D::is_builtin(symbol.as_str()) {
            return IdentifierID::BuiltinReference;
        }
        // TODO we should not find it in both.
        if let Some(id) = find_symbol(&self.active_variables, symbol) {
            return IdentifierID::Reference(id);
        }
        if let Some(id) = find_symbol(&self.active_functions, symbol) {
            return IdentifierID::Reference(id);
        }
        self.errors.push(format!("Symbol not found: \"{symbol}\""));
        IdentifierID::UnresolvedReference
    }

    fn enter_block_immut(&mut self, block: &Block) {
        self.active_variables.push(HashMap::new());
        self.active_functions.push(HashMap::new());
        for st in &block.statements {
            if let Statement::FunctionDefinition(f) = st {
                if let IdentifierID::Declaration(id) = f.name.id {
                    if self
                        .active_functions
                        .last_mut()
                        .unwrap()
                        .insert(f.name.name.clone(), id)
                        .is_some()
                    {
                        self.errors
                            .push(format!("Function \"{}\" already declared.", f.name))
                    }
                    self.function_signatures.insert(
                        id,
                        FunctionSignature {
                            parameters: f.parameters.len() as u64,
                            returns: f.returns.len() as u64,
                        },
                    );
                } else {
                    panic!()
                }
            }
        }
    }

    fn exit_block_immut(&mut self) {
        self.active_variables.pop();
        self.active_functions.pop();
    }

    fn exit_variable_declaration_immut(&mut self, variables: &VariableDeclaration) {
        for var in &variables.variables {
            self.activate_variable(var);
        }
    }
}

impl<D: Dialect> ASTModifier for Resolver<D> {
    fn enter_block(&mut self, block: &mut Block) {
        self.enter_block_immut(block);
    }
    fn exit_block(&mut self, _block: &mut Block) {
        self.exit_block_immut();
    }
    fn exit_variable_declaration(&mut self, variables: &mut VariableDeclaration) {
        self.exit_variable_declaration_immut(variables);
    }
    fn exit_identifier(&mut self, identifier: &mut Identifier) {
        if identifier.id == IdentifierID::UnresolvedReference {
            identifier.id = self.resolve(&identifier.name);
        }
    }
    fn visit_function_definition(&mut self, fun_def: &mut FunctionDefinition) {
        let outer_variables = std::mem::take(&mut self.active_variables);
        self.active_variables.push(HashMap::new());
        for var in fun_def.parameters.iter().chain(fun_def.returns.iter()) {
            self.activate_variable(var);
        }
        self.visit_block(&mut fun_def.body);
        self.active_variables.pop();
        self.active_variables = outer_variables;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dialect::EVMDialect,
        yul_parser::{parse_block, parse_expression},
    };

    #[test]
    fn with_dialect() {
        let mut ast = Block {
            statements: vec![Statement::Expression(Expression::FunctionCall(
                FunctionCall {
                    function: Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "add".to_string(),
                        location: None,
                    },
                    arguments: vec![],
                    location: None,
                },
            ))],
            location: None,
        };
        resolve::<EVMDialect>(&mut ast).expect("Should resolve properly.");
    }

    #[test]
    fn test_resolve_inside() {
        let source = "{ let x := 7 function f(a, b) -> c {} let y := 9 }";
        let mut block = parse_block(source).unwrap();
        resolve::<EVMDialect>(&mut block).expect("Should resolve properly.");
        let mut expr_x = parse_expression("x").unwrap();
        let mut expr_f = parse_expression("f").unwrap();
        let mut expr_y = parse_expression("y").unwrap();
        resolve_inside::<EVMDialect>(&mut expr_x, &block).expect("");
        assert_eq!(
            expr_x,
            Expression::Identifier(Identifier {
                id: IdentifierID::Reference(2),
                name: "x".to_string(),
                // NOTE: It's not 0-1 because parse_expression() internally converts the expression
                // to `{ x }`
                location: Some(SourceLocation { start: 2, end: 3 }),
            })
        );
        resolve_inside::<EVMDialect>(&mut expr_f, &block).expect("");
        assert_eq!(
            expr_f,
            Expression::Identifier(Identifier {
                id: IdentifierID::Reference(3),
                name: "f".to_string(),
                location: Some(SourceLocation { start: 2, end: 3 }),
            })
        );
        resolve_inside::<EVMDialect>(&mut expr_y, &block).expect("");
        assert_eq!(
            expr_y,
            Expression::Identifier(Identifier {
                id: IdentifierID::Reference(7),
                name: "y".to_string(),
                location: Some(SourceLocation { start: 2, end: 3 }),
            })
        );
    }

    #[test]
    fn resolve_identifier_not_found() {
        let source = "{ let x := x }";
        let mut block = parse_block(source).unwrap();
        assert_eq!(
            resolve::<EVMDialect>(&mut block).unwrap_err(),
            "Symbol not found: \"x\""
        );
    }

    #[test]
    fn variable_already_declared() {
        let source = "{ let x := 1 let x := 2 }";
        let mut block = parse_block(source).unwrap();
        assert_eq!(
            resolve::<EVMDialect>(&mut block).unwrap_err(),
            "Identifier \"x\" already declared at this point."
        );
    }

    #[test]
    fn function_already_declared() {
        let source = "{ function f() {} function f() {} }";
        let mut block = parse_block(source).unwrap();
        assert_eq!(
            resolve::<EVMDialect>(&mut block).unwrap_err(),
            "Function \"f\" already declared."
        );
    }

    #[test]
    fn multi_error() {
        let source = "{ let f := g() let f := 1 }";
        let mut block = parse_block(source).unwrap();
        assert_eq!(
            resolve::<EVMDialect>(&mut block).unwrap_err(),
            "Symbol not found: \"g\"\nIdentifier \"f\" already declared at this point."
        );
    }
}
