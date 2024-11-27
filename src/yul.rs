use std::fmt;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct SourceLocation {
    pub start: usize,
    pub end: usize,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum InnerRoot {
    Object(Object),
    Block(Block),
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Root {
    pub inner: InnerRoot,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Object {
    pub name: String,
    pub code: Code,
    pub data: Vec<Data>,
    pub objects: Vec<Object>,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Code {
    pub body: Block,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct Data {
    pub name: String,
    pub data: String,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct Literal {
    pub literal: String,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum IdentifierID {
    /// This is where the identifier is declared.
    /// The parameter is unique across the program.
    Declaration(u64),
    /// This is a reference to an identifier declared elsewhere.
    /// The parameter is the same as the one of the declaration.
    Reference(u64),
    /// This is a reference to a builtin symbol.
    BuiltinReference,
    /// This is a reference, but it has not yet been resolved.
    UnresolvedReference,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct Identifier {
    pub id: IdentifierID,
    pub name: String,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct FunctionCall {
    pub function: Identifier,
    pub arguments: Vec<Expression>,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub returns: Vec<Identifier>,
    pub body: Block,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct VariableDeclaration {
    pub variables: Vec<Identifier>,
    pub value: Option<Expression>,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Assignment {
    pub variables: Vec<Identifier>,
    pub value: Expression,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct If {
    pub condition: Expression,
    pub body: Block,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Case {
    pub literal: Option<Literal>,
    pub body: Block,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct Switch {
    pub expression: Expression,
    pub cases: Vec<Case>,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct ForLoop {
    pub pre: Block,
    pub condition: Expression,
    pub post: Block,
    pub body: Block,
    pub location: Option<SourceLocation>,
}

#[derive(Hash, Clone, PartialEq, Debug)]
pub enum Statement {
    Block(Block),
    FunctionDefinition(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
    Assignment(Assignment),
    Expression(Expression),
    If(If),
    Switch(Switch),
    ForLoop(ForLoop),
    Break,
    Continue,
    Leave,
}

impl Identifier {
    pub fn new(identifier: &str, id: IdentifierID, location: Option<SourceLocation>) -> Self {
        Identifier {
            id,
            name: identifier.to_string(),
            location,
        }
    }
}

impl Literal {
    pub fn new(literal: &str, location: Option<SourceLocation>) -> Self {
        Literal {
            literal: literal.to_string(),
            location,
        }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.function)?;
        write!(
            f,
            "{}",
            self.arguments
                .iter()
                .map(|argument| format!("{}", argument))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        write!(f, ")")
    }
}

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}(", self.name)?;
        write!(
            f,
            "{}",
            self.parameters
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        write!(f, ")")?;
        if !self.returns.is_empty() {
            write!(f, " -> ")?;
            write!(
                f,
                "{}",
                self.returns
                    .iter()
                    .map(|identifier| format!("{}", identifier))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        write!(f, " {}", self.body)
    }
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.variables.is_empty() {
            panic!("VariableDeclaration must have identifiers")
        }
        write!(f, "let ")?;
        write!(
            f,
            "{}",
            self.variables
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        if let Some(expression) = &self.value {
            write!(f, " := {}", expression)
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // FIXME: should validate this on the new/default trait
        if self.variables.is_empty() {
            panic!("Assignment must have identifiers")
        }
        write!(
            f,
            "{}",
            self.variables
                .iter()
                .map(|identifier| format!("{}", identifier))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        write!(f, " := {}", self.value)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Literal(ref literal) => write!(f, "{}", literal),
            Expression::Identifier(ref identifier) => write!(f, "{}", identifier),
            Expression::FunctionCall(ref functioncall) => write!(f, "{}", functioncall),
        }
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {}", self.condition, self.body)
    }
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(literal) = &self.literal {
            // FIXME: should validate this on the new/default trait
            if literal.literal.is_empty() {
                panic!("Case with literal should not be empty");
            }
            write!(f, "case {} {}", literal, self.body)
        } else {
            write!(f, "default {}", self.body)
        }
    }
}

impl fmt::Display for Switch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "switch {}\n{}",
            self.expression,
            self.cases
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl fmt::Display for ForLoop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "for {} {} {} {}",
            self.pre, self.condition, self.post, self.body
        )
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Block(ref block) => write!(f, "{}", block),
            Statement::FunctionDefinition(ref function) => write!(f, "{}", function),
            Statement::VariableDeclaration(ref variabledeclaration) => {
                write!(f, "{}", variabledeclaration)
            }
            Statement::Assignment(ref assignment) => write!(f, "{}", assignment),
            Statement::Expression(ref expression) => write!(f, "{}", expression),
            Statement::If(ref if_statement) => write!(f, "{}", if_statement),
            Statement::Switch(ref switch) => write!(f, "{}", switch),
            Statement::ForLoop(ref forloop) => write!(f, "{}", forloop),
            Statement::Break => write!(f, "break"),
            Statement::Continue => write!(f, "continue"),
            Statement::Leave => write!(f, "leave"),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statements = self
            .statements
            .iter()
            .map(|s| format!("{}", s))
            .collect::<Vec<_>>()
            .join("\n");
        if statements.is_empty() {
            write!(f, "{{ }}")
        } else if statements.len() >= 20 || statements.contains('\n') {
            write!(f, "{{\n    {}\n}}", statements.replace('\n', "\n    "))
        } else {
            write!(f, "{{ {} }}", statements)
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "object {} {{\n{}\n{}}}",
            self.name,
            format!("    {}", self.code).replace('\n', "\n    "),
            self.data
                .iter()
                .map(|data| format!("    {}\n", data))
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "code {}", self.body)
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "data {} {}", self.name, self.data)
    }
}

impl fmt::Display for Root {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.inner {
            InnerRoot::Object(obj) => write!(f, "{}", obj),
            InnerRoot::Block(block) => write!(f, "{}", block),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal() {
        assert_eq!(
            Literal {
                literal: "testliteral".to_string(),
                location: None,
            }
            .to_string(),
            "testliteral"
        );
    }

    #[test]
    fn identifier() {
        assert_eq!(
            Identifier {
                id: IdentifierID::UnresolvedReference,
                name: "testidentifier".to_string(),
                location: None,
            }
            .to_string(),
            "testidentifier"
        );
    }

    #[test]
    fn functioncall() {
        assert_eq!(
            FunctionCall {
                function: Identifier {
                    id: IdentifierID::UnresolvedReference,
                    name: "test".to_string(),
                    location: None,
                },
                arguments: vec![
                    Expression::Identifier(Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "test".to_string(),
                        location: None,
                    }),
                    Expression::Literal(Literal {
                        literal: "literal".to_string(),
                        location: None,
                    }),
                ],
                location: None,
            }
            .to_string(),
            "test(test, literal)"
        );
    }

    #[test]
    fn if_statement() {
        assert_eq!(
            If {
                condition: Expression::Literal(Literal {
                    literal: "literal".to_string(),
                    location: None,
                }),
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "if literal { }"
        );
    }

    #[test]
    fn block_empty() {
        assert_eq!(
            Block {
                statements: vec![],
                location: None,
            }
            .to_string(),
            "{ }"
        );
    }

    #[test]
    fn block_nested() {
        assert_eq!(
            Block {
                statements: vec![Statement::Block(Block {
                    statements: vec![],
                    location: None,
                })],
                location: None,
            }
            .to_string(),
            "{ { } }"
        );
    }

    #[test]
    fn block_literal() {
        assert_eq!(
            Block {
                statements: vec![Statement::Expression(Expression::Literal(Literal {
                    literal: "literal".to_string(),
                    location: None,
                }))],
                location: None,
            }
            .to_string(),
            "{ literal }"
        );
    }

    #[test]
    fn assignment_single() {
        assert_eq!(
            Assignment {
                variables: vec![Identifier {
                    id: IdentifierID::UnresolvedReference,
                    name: "a".to_string(),
                    location: None,
                }],
                value: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    location: None,
                }),
                location: None,
            }
            .to_string(),
            "a := 1"
        );
    }

    #[test]
    fn assignment_multi() {
        assert_eq!(
            Assignment {
                variables: vec![
                    Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "a".to_string(),
                        location: None,
                    },
                    Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "b".to_string(),
                        location: None,
                    },
                    Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "c".to_string(),
                        location: None,
                    },
                ],
                value: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    location: None,
                }),
                location: None,
            }
            .to_string(),
            "a, b, c := 1"
        );
    }

    #[test]
    fn variabledeclaration_empty() {
        assert_eq!(
            VariableDeclaration {
                variables: vec![Identifier {
                    id: IdentifierID::Declaration(1),
                    name: "a".to_string(),
                    location: None,
                }],
                value: None,
                location: None,
            }
            .to_string(),
            "let a"
        );
    }

    #[test]
    fn variabledeclaration_single() {
        assert_eq!(
            VariableDeclaration {
                variables: vec![Identifier {
                    id: IdentifierID::Declaration(1),
                    name: "a".to_string(),
                    location: None,
                }],
                value: Some(Expression::Literal(Literal {
                    literal: "1".to_string(),
                    location: None,
                })),
                location: None,
            }
            .to_string(),
            "let a := 1"
        );
    }

    #[test]
    fn variabledeclaration_multi() {
        assert_eq!(
            VariableDeclaration {
                variables: vec![
                    Identifier {
                        id: IdentifierID::Declaration(1),
                        name: "a".to_string(),
                        location: None,
                    },
                    Identifier {
                        id: IdentifierID::Declaration(2),
                        name: "b".to_string(),
                        location: None,
                    },
                    Identifier {
                        id: IdentifierID::Declaration(3),
                        name: "c".to_string(),
                        location: None,
                    },
                ],
                value: Some(Expression::Literal(Literal {
                    literal: "1".to_string(),
                    location: None,
                })),
                location: None,
            }
            .to_string(),
            "let a, b, c := 1"
        );
    }

    #[test]
    fn functiondefinition_basic() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: IdentifierID::Declaration(1),
                    name: "name".to_string(),
                    location: None,
                },
                parameters: vec![],
                returns: vec![],
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "function name() { }"
        );
    }

    #[test]
    fn functiondefinition_single_arg() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: IdentifierID::Declaration(1),
                    name: "name".to_string(),
                    location: None,
                },
                parameters: vec![Identifier {
                    id: IdentifierID::UnresolvedReference,
                    name: "a".to_string(),
                    location: None,
                }],
                returns: vec![],
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "function name(a) { }"
        );
    }

    #[test]
    fn functiondefinition_single_ret() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: IdentifierID::Declaration(1),
                    name: "name".to_string(),
                    location: None,
                },
                parameters: vec![],
                returns: vec![Identifier {
                    id: IdentifierID::Declaration(2),
                    name: "a".to_string(),
                    location: None,
                }],
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "function name() -> a { }"
        );
    }

    #[test]
    fn functiondefinition_multi() {
        assert_eq!(
            FunctionDefinition {
                name: Identifier {
                    id: IdentifierID::Declaration(1),
                    name: "name".to_string(),
                    location: None,
                },
                parameters: vec![
                    Identifier {
                        id: IdentifierID::Declaration(2),
                        name: "a".to_string(),
                        location: None,
                    },
                    Identifier {
                        id: IdentifierID::Declaration(3),
                        name: "b".to_string(),
                        location: None,
                    },
                ],
                returns: vec![
                    Identifier {
                        id: IdentifierID::Declaration(1),
                        name: "c".to_string(),
                        location: None,
                    },
                    Identifier {
                        id: IdentifierID::Declaration(2),
                        name: "d".to_string(),
                        location: None,
                    },
                ],
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "function name(a, b) -> c, d { }"
        );
    }

    #[test]
    fn case() {
        assert_eq!(
            Case {
                literal: Some(Literal {
                    literal: "literal".to_string(),
                    location: None,
                }),
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "case literal { }"
        );
    }

    #[test]
    fn case_default() {
        assert_eq!(
            Case {
                literal: None,
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "default { }"
        );
    }

    #[test]
    fn switch() {
        assert_eq!(
            Switch {
                expression: Expression::Literal(Literal {
                    literal: "3".to_string(),
                    location: None,
                }),
                cases: vec![
                    Case {
                        literal: Some(Literal {
                            literal: "1".to_string(),
                            location: None,
                        }),
                        body: Block {
                            statements: vec![],
                            location: None,
                        },
                        location: None,
                    },
                    Case {
                        literal: None,
                        body: Block {
                            statements: vec![],
                            location: None,
                        },
                        location: None,
                    },
                ],
                location: None,
            }
            .to_string(),
            "switch 3\ncase 1 { }\ndefault { }"
        );
    }

    #[test]
    fn forloop() {
        assert_eq!(
            ForLoop {
                pre: Block {
                    statements: vec![],
                    location: None,
                },
                condition: Expression::Literal(Literal {
                    literal: "1".to_string(),
                    location: None,
                }),
                post: Block {
                    statements: vec![],
                    location: None,
                },
                body: Block {
                    statements: vec![],
                    location: None,
                },
                location: None,
            }
            .to_string(),
            "for { } 1 { } { }"
        );
    }
    #[test]
    fn leave() {
        assert_eq!(
            Block {
                statements: vec![Statement::Leave],
                location: None,
            }
            .to_string(),
            "{ leave }"
        );
    }
}
