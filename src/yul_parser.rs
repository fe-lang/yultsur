use yul::*;

use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "yul.pest"]
struct BlockParser;

impl Identifier {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Identifier {
        let name = pair.as_str().to_string();

        // TOOD much nicer to call some function that returns the id
        *next_identifier += 1;
        Identifier {
            id: IdentifierID::Declaration(*next_identifier),
            name,
        }
    }

    fn from_reference(pair: Pair<Rule>) -> Identifier {
        let name = pair.as_str().to_string();

        Identifier {
            id: IdentifierID::UnresolvedReference,
            name,
        }
    }

    fn list(pair: Pair<Rule>, next_identifier: &mut u64) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::identifier => {
                    identifiers.push(Identifier::from(p, next_identifier));
                }
                _ => unreachable!(),
            }
        }
        identifiers
    }

    fn list_reference(pair: Pair<Rule>) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::identifier => {
                    identifiers.push(Identifier::from_reference(p));
                }
                _ => unreachable!(),
            }
        }
        identifiers
    }
}

impl Literal {
    fn from(pair: Pair<Rule>) -> Literal {
        let mut token_iter = pair.into_inner();
        let literal = token_iter.next().unwrap().as_str().to_string();

        Literal { literal }
    }
}

impl FunctionCall {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> FunctionCall {
        let mut token_iter = pair.into_inner();
        let function = Identifier::from_reference(token_iter.next().unwrap());
        let arguments = token_iter
            .map(|p| match p.as_rule() {
                Rule::expression => Expression::from(p, next_identifier),
                _ => unreachable!(),
            })
            .collect();

        FunctionCall {
            function,
            arguments,
        }
    }
}

impl Expression {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Expression {
        let mut token_iter = pair.into_inner();
        let p = token_iter.next().unwrap();
        match p.as_rule() {
            Rule::function_call => Expression::FunctionCall(FunctionCall::from(p, next_identifier)),
            Rule::identifier => Expression::Identifier(Identifier::from_reference(p)),
            Rule::literal => Expression::Literal(Literal::from(p)),
            _ => unreachable!(),
        }
    }
}

impl Case {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Case {
        let mut token_iter = pair.into_inner();
        let literal = Literal::from(token_iter.next().unwrap());
        let body = Block::from(token_iter.next().unwrap(), next_identifier);

        Case {
            literal: Some(literal),
            body,
        }
    }

    fn from_default(pair: Pair<Rule>, next_identifier: &mut u64) -> Case {
        let mut token_iter = pair.into_inner();
        let body = Block::from(token_iter.next().unwrap(), next_identifier);

        Case {
            literal: None,
            body,
        }
    }
}

impl Switch {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Switch {
        let mut token_iter = pair.into_inner();
        let expression = Expression::from(token_iter.next().unwrap(), next_identifier);
        let cases = token_iter
            .map(|p| match p.as_rule() {
                Rule::case => Case::from(p, next_identifier),
                Rule::default => Case::from_default(p, next_identifier),
                _ => unreachable!(),
            })
            .collect();

        Switch { expression, cases }
    }
}

impl Assignment {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Assignment {
        let mut token_iter = pair.into_inner();
        let variables = Identifier::list_reference(token_iter.next().unwrap());
        let value = Expression::from(token_iter.next().unwrap(), next_identifier);

        Assignment { variables, value }
    }
}

impl VariableDeclaration {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> VariableDeclaration {
        let mut token_iter = pair.into_inner();

        let variables = Identifier::list(token_iter.next().unwrap(), next_identifier);
        let value = token_iter
            .next()
            .map(|e| Expression::from(e, next_identifier));

        VariableDeclaration { variables, value }
    }
}

impl FunctionDefinition {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> FunctionDefinition {
        let mut token_iter = pair.into_inner();
        let name = Identifier::from(token_iter.next().unwrap(), next_identifier);

        let current = token_iter.next().unwrap();
        let (parameters, current) = match current.as_rule() {
            Rule::parameter_list => (
                Identifier::list(current, next_identifier),
                token_iter.next().unwrap(),
            ),
            _ => (vec![], current),
        };
        let (returns, current) = match current.as_rule() {
            Rule::identifier_list => (
                Identifier::list(current, next_identifier),
                token_iter.next().unwrap(),
            ),
            _ => (vec![], current),
        };
        let body = Block::from(current, next_identifier);

        FunctionDefinition {
            name,
            parameters,
            returns,
            body,
        }
    }
}

impl If {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> If {
        let mut token_iter = pair.into_inner();
        let condition = Expression::from(token_iter.next().unwrap(), next_identifier);
        let body = Block::from(token_iter.next().unwrap(), next_identifier);

        If { condition, body }
    }
}

impl ForLoop {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> ForLoop {
        let mut token_iter = pair.into_inner();
        let pre = Block::from(token_iter.next().unwrap(), next_identifier);
        let condition = Expression::from(token_iter.next().unwrap(), next_identifier);
        let post = Block::from(token_iter.next().unwrap(), next_identifier);
        let body = Block::from(token_iter.next().unwrap(), next_identifier);

        ForLoop {
            pre,
            condition,
            post,
            body,
        }
    }
}

impl Statement {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Statement {
        let mut token_iter = pair.into_inner();
        let p = token_iter.next().unwrap();
        match p.as_rule() {
            Rule::block => Statement::Block(Block::from(p, next_identifier)),
            Rule::function_definition => {
                Statement::FunctionDefinition(FunctionDefinition::from(p, next_identifier))
            }
            Rule::variable_declaration => {
                Statement::VariableDeclaration(VariableDeclaration::from(p, next_identifier))
            }
            Rule::assignment => Statement::Assignment(Assignment::from(p, next_identifier)),
            Rule::expression => Statement::Expression(Expression::from(p, next_identifier)),
            Rule::switch => Statement::Switch(Switch::from(p, next_identifier)),
            Rule::if_statement => Statement::If(If::from(p, next_identifier)),
            Rule::for_loop => Statement::ForLoop(ForLoop::from(p, next_identifier)),
            Rule::break_statement => Statement::Break,
            Rule::continue_statement => Statement::Continue,
            Rule::leave => Statement::Leave,
            _ => unreachable!(),
        }
    }
}

impl Block {
    fn from(pair: Pair<Rule>, next_identifier: &mut u64) -> Block {
        let mut statements: Vec<Statement> = vec![];
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::statement => {
                    statements.push(Statement::from(p, next_identifier));
                }
                _ => unreachable!(),
            }
        }
        Block { statements }
    }
}

pub fn parse_block(source: &str) -> Block {
    let mut pairs = BlockParser::parse(Rule::block, &source).unwrap();
    let mut next_identifier = 1u64;
    Block::from(pairs.next().unwrap(), &mut next_identifier)
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs::read_to_string;

    #[test]
    fn continue_statement() {
        test_file("examples/continue.yul");
    }

    #[test]
    fn break_statement() {
        test_file("examples/break.yul");
    }

    #[test]
    fn for_loop() {
        test_file("examples/for.yul");
    }

    #[test]
    fn if_statement() {
        test_file("examples/if.yul");
    }

    #[test]
    fn switch() {
        test_file("examples/switch.yul");
    }

    #[test]
    fn assignment() {
        test_file("examples/assignment.yul");
    }

    #[test]
    fn var_decl() {
        test_file("examples/var_decl.yul");
    }

    #[test]
    fn empty_nested_blocks() {
        test_file("examples/nested_blocks.yul");
    }

    #[test]
    fn empty_function() {
        test_file("examples/power_function_signature.yul");
    }

    #[test]
    fn empty_block() {
        test_file("examples/empty_block.yul");
    }

    #[test]
    fn function_call() {
        test_file("examples/function_call.yul");
    }

    #[test]
    fn leave() {
        test_file("examples/leave.yul");
    }

    fn test_file(filename: &str) {
        let source = read_to_string(filename).unwrap();
        let block = parse_block(&source);
        assert_eq!(source, block.to_string());
    }
}
