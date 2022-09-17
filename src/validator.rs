use crate::yul::*;

use crate::fallible_visitor::FallibleASTVisitor;

#[derive(Default)]
struct Validator;

impl FallibleASTVisitor for Validator {
    type Error = String;

    fn visit_statement(&mut self, st: &Statement) -> Result<(), Self::Error> {
        match st {
            Statement::Switch(ref switch) => self.visit_switch(switch),
            _ => Ok(()),
        }
    }

    fn visit_case(&mut self, case: &Case) -> Result<(), Self::Error> {
        if let Some(literal) = &case.literal {
            if literal.literal.is_empty() {
                return Err("Case literal cannot be empty".to_string());
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal() {
        assert!(
            !Validator::default()
                .visit_literal(&Literal {
                    literal: "test".to_string(),
                    location: None,
                })
                .is_err(),
            ""
        );
    }

    #[test]
    fn case_invalid_default() {
        assert!(
            Validator::default()
                .visit_case(&Case {
                    literal: Some(Literal {
                        literal: "".to_string(),
                        location: None,
                    }),
                    body: Block {
                        statements: vec![],
                        location: None
                    },
                    location: None,
                })
                .is_err(),
            ""
        );
    }

    #[test]
    fn complex_example() {
        assert!(
            !Validator::default()
                .visit_block(&Block {
                    statements: vec![Statement::Switch(Switch {
                        expression: Expression::Identifier(Identifier {
                            id: IdentifierID::UnresolvedReference,
                            name: "shouldbebool".to_string(),
                            location: None,
                        }),
                        cases: vec![
                            Case {
                                literal: Some(Literal {
                                    literal: "true".to_string(),
                                    location: None,
                                }),
                                body: Block {
                                    statements: vec![],
                                    location: None
                                },
                                location: None,
                            },
                            Case {
                                literal: None,
                                body: Block {
                                    statements: vec![],
                                    location: None
                                },
                                location: None,
                            },
                        ],
                        location: None,
                    })],
                    location: None,
                })
                .is_err(),
            ""
        );
    }
}
