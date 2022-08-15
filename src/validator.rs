use yul::*;

pub trait Validator: Send + Sync {
    fn validate(&self) -> Result<(), String>;
}

impl Validator for Literal {
    fn validate(&self) -> Result<(), String> {
        Ok(())
    }
}

impl Validator for Identifier {
    fn validate(&self) -> Result<(), String> {
        Ok(())
    }
}

impl Validator for Block {
    fn validate(&self) -> Result<(), String> {
        for statement in &self.statements {
            statement.validate()?;
        }
        Ok(())
    }
}

impl Validator for Statement {
    fn validate(&self) -> Result<(), String> {
        match *self {
            Statement::Switch(ref switch) => switch.validate(),
            _ => Ok(()),
        }
    }
}

impl Validator for Case {
    fn validate(&self) -> Result<(), String> {
        if let Some(literal) = &self.literal {
            literal.validate()?;
            if literal.literal.len() == 0 {
                return Err("Case literal cannot be empty".to_string());
            }
        }
        Ok(())
    }
}

impl Validator for Switch {
    fn validate(&self) -> Result<(), String> {
        for case in &self.cases {
            case.validate()?;
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
            !Literal {
                literal: "test".to_string(),
            }
            .validate()
            .is_err(),
            ""
        );
    }

    #[test]
    fn case_invalid_default() {
        assert!(
            Case {
                literal: Some(Literal {
                    literal: "".to_string(),
                }),
                body: Block { statements: vec![] },
            }
            .validate()
            .is_err(),
            ""
        );
    }

    #[test]
    fn complex_example() {
        assert!(
            !Block {
                statements: vec![Statement::Switch(Switch {
                    expression: Expression::Identifier(Identifier {
                        id: IdentifierID::UnresolvedReference,
                        name: "shouldbebool".to_string(),
                    }),
                    cases: vec![
                        Case {
                            literal: Some(Literal {
                                literal: "true".to_string(),
                            }),
                            body: Block { statements: vec![] },
                        },
                        Case {
                            literal: None,
                            body: Block { statements: vec![] },
                        },
                    ],
                })],
            }
            .validate()
            .is_err(),
            ""
        );
    }
}
