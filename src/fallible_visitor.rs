use crate::yul::*;

pub trait FallibleASTVisitor {
    type Error;

    fn enter_statement(&mut self, _st: &Statement) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_statement(&mut self, st: &Statement) -> Result<(), Self::Error> {
        self.enter_statement(st)?;
        match st {
            Statement::Block(block) => self.visit_block(block)?,
            Statement::FunctionDefinition(fun) => self.visit_function_definition(fun)?,
            Statement::VariableDeclaration(variable) => {
                self.visit_variable_declaration(variable)?
            }
            Statement::Assignment(assignment) => self.visit_assignment(assignment)?,
            Statement::Expression(expression) => self.visit_expression(expression)?,
            Statement::If(if_st) => self.visit_if(if_st)?,
            Statement::Switch(switch) => self.visit_switch(switch)?,
            Statement::ForLoop(for_loop) => self.visit_for(for_loop)?,
            Statement::Break => self.visit_break()?,
            Statement::Continue => self.visit_continue()?,
            Statement::Leave => self.visit_leave()?,
        };
        self.exit_statement(st)
    }
    fn exit_statement(&mut self, _st: &Statement) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_block(&mut self, _block: &Block) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_block(&mut self, block: &Block) -> Result<(), Self::Error> {
        self.enter_block(block)?;
        for s in &block.statements {
            self.visit_statement(s)?;
        }
        self.exit_block(block)
    }
    fn exit_block(&mut self, _block: &Block) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_function_definition(
        &mut self,
        _fun_def: &FunctionDefinition,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_function_definition(
        &mut self,
        fun_def: &FunctionDefinition,
    ) -> Result<(), Self::Error> {
        self.enter_function_definition(fun_def)?;
        self.visit_identifier(&fun_def.name)?;
        self.visit_identifier_vector(&fun_def.parameters)?;
        self.visit_identifier_vector(&fun_def.returns)?;
        self.visit_block(&fun_def.body)?;
        self.exit_function_definition(fun_def)
    }
    fn exit_function_definition(
        &mut self,
        _fun_def: &FunctionDefinition,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_variable_declaration(
        &mut self,
        _variable: &VariableDeclaration,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_variable_declaration(
        &mut self,
        variable: &VariableDeclaration,
    ) -> Result<(), Self::Error> {
        self.enter_variable_declaration(variable)?;
        self.visit_identifier_vector(&variable.variables)?;
        if let Some(value) = &variable.value {
            self.visit_expression(value)?;
        }
        self.exit_variable_declaration(variable)
    }
    fn exit_variable_declaration(
        &mut self,
        _variable: &VariableDeclaration,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    // helper
    fn visit_identifier_vector(&mut self, identifiers: &[Identifier]) -> Result<(), Self::Error> {
        for i in identifiers {
            self.visit_identifier(i)?;
        }
        Ok(())
    }

    fn enter_assignment(&mut self, _assignment: &Assignment) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_assignment(&mut self, assignment: &Assignment) -> Result<(), Self::Error> {
        self.enter_assignment(assignment)?;
        self.visit_identifier_vector(&assignment.variables)?;
        self.visit_expression(&assignment.value)?;
        self.exit_assignment(assignment)
    }
    fn exit_assignment(&mut self, _assignment: &Assignment) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_expression(&mut self, _expression: &Expression) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
        self.enter_expression(expression)?;
        match expression {
            Expression::Literal(literal) => self.visit_literal(literal)?,
            Expression::Identifier(identifier) => self.visit_identifier(identifier)?,
            Expression::FunctionCall(function) => self.visit_function_call(function)?,
        }
        self.exit_expression(expression)
    }
    fn exit_expression(&mut self, _expression: &Expression) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_literal(&mut self, _literal: &Literal) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_literal(&mut self, literal: &Literal) -> Result<(), Self::Error> {
        self.enter_literal(literal)?;
        self.exit_literal(literal)
    }
    fn exit_literal(&mut self, _literal: &Literal) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_identifier(&mut self, _identifier: &Identifier) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_identifier(&mut self, identifier: &Identifier) -> Result<(), Self::Error> {
        self.enter_identifier(identifier)?;
        self.exit_identifier(identifier)
    }
    fn exit_identifier(&mut self, _identifier: &Identifier) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_function_call(&mut self, _fun_call: &FunctionCall) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_function_call(&mut self, fun_call: &FunctionCall) -> Result<(), Self::Error> {
        self.enter_function_call(fun_call)?;
        self.visit_identifier(&fun_call.function)?;
        for a in &fun_call.arguments {
            self.visit_expression(a)?;
        }
        self.exit_function_call(fun_call)
    }
    fn exit_function_call(&mut self, _fun_call: &FunctionCall) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_if(&mut self, _x: &If) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_if(&mut self, ifs: &If) -> Result<(), Self::Error> {
        self.enter_if(ifs)?;
        self.visit_expression(&ifs.condition)?;
        self.visit_block(&ifs.body)?;
        self.exit_if(ifs)
    }
    fn exit_if(&mut self, _x: &If) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_switch(&mut self, _x: &Switch) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_switch(&mut self, switch: &Switch) -> Result<(), Self::Error> {
        self.enter_switch(switch)?;
        self.visit_expression(&switch.expression)?;
        for c in &switch.cases {
            self.enter_case(&c)?;
            self.visit_case(&c)?;
            self.exit_case(&c)?;
        }
        self.exit_switch(switch)
    }

    fn enter_case(&mut self, _case: &Case) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_case(&mut self, case: &Case) -> Result<(), Self::Error> {
        if let Some(literal) = &case.literal {
            self.visit_literal(literal)?;
        }
        self.visit_block(&case.body)
    }
    fn exit_case(&mut self, _case: &Case) -> Result<(), Self::Error> {
        Ok(())
    }

    fn exit_switch(&mut self, _x: &Switch) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_for(&mut self, _x: &ForLoop) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_for(&mut self, for_loop: &ForLoop) -> Result<(), Self::Error> {
        self.enter_for(for_loop)?;
        self.visit_block(&for_loop.pre)?;
        self.visit_expression(&for_loop.condition)?;
        self.visit_block(&for_loop.post)?;
        self.visit_block(&for_loop.body)?;
        self.exit_for(for_loop)
    }
    fn exit_for(&mut self, _x: &ForLoop) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_break(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_break(&mut self) -> Result<(), Self::Error> {
        self.enter_break()?;
        self.exit_break()
    }
    fn exit_break(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_continue(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_continue(&mut self) -> Result<(), Self::Error> {
        self.enter_continue()?;
        self.exit_continue()
    }
    fn exit_continue(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_leave(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_leave(&mut self) -> Result<(), Self::Error> {
        self.enter_leave()?;
        self.exit_leave()
    }
    fn exit_leave(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub trait FallibleASTModifier {
    type Error;

    fn enter_statement(&mut self, _st: &mut Statement) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_statement(&mut self, st: &mut Statement) -> Result<(), Self::Error> {
        self.enter_statement(st)?;
        match st {
            Statement::Block(block) => self.visit_block(block)?,
            Statement::FunctionDefinition(fun) => self.visit_function_definition(fun)?,
            Statement::VariableDeclaration(variable) => {
                self.visit_variable_declaration(variable)?
            }
            Statement::Assignment(assignment) => self.visit_assignment(assignment)?,
            Statement::Expression(expression) => self.visit_expression(expression)?,
            Statement::If(if_st) => self.visit_if(if_st)?,
            Statement::Switch(switch) => self.visit_switch(switch)?,
            Statement::ForLoop(for_loop) => self.visit_for(for_loop)?,
            Statement::Break => self.visit_break()?,
            Statement::Continue => self.visit_continue()?,
            Statement::Leave => self.visit_leave()?,
        };
        self.exit_statement(st)
    }
    fn exit_statement(&mut self, _st: &mut Statement) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_block(&mut self, _block: &mut Block) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_block(&mut self, block: &mut Block) -> Result<(), Self::Error> {
        self.enter_block(block)?;
        for s in block.statements.iter_mut() {
            self.visit_statement(s)?;
        }
        self.exit_block(block)
    }
    fn exit_block(&mut self, _block: &mut Block) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_function_definition(
        &mut self,
        _fun_def: &mut FunctionDefinition,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_function_definition(
        &mut self,
        fun_def: &mut FunctionDefinition,
    ) -> Result<(), Self::Error> {
        self.enter_function_definition(fun_def)?;
        self.visit_identifier(&mut fun_def.name)?;
        self.visit_identifier_vector(&mut fun_def.parameters)?;
        self.visit_identifier_vector(&mut fun_def.returns)?;
        self.visit_block(&mut fun_def.body)?;
        self.exit_function_definition(fun_def)
    }
    fn exit_function_definition(
        &mut self,
        _fun_def: &mut FunctionDefinition,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_variable_declaration(
        &mut self,
        _variable: &mut VariableDeclaration,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_variable_declaration(
        &mut self,
        variable: &mut VariableDeclaration,
    ) -> Result<(), Self::Error> {
        self.enter_variable_declaration(variable)?;
        self.visit_identifier_vector(&mut variable.variables)?;
        if let Some(value) = &mut variable.value {
            self.visit_expression(value)?;
        }
        self.exit_variable_declaration(variable)
    }
    fn exit_variable_declaration(
        &mut self,
        _variable: &mut VariableDeclaration,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    // helper
    fn visit_identifier_vector(
        &mut self,
        identifiers: &mut [Identifier],
    ) -> Result<(), Self::Error> {
        for i in identifiers.iter_mut() {
            self.visit_identifier(i)?;
        }
        Ok(())
    }

    fn enter_assignment(&mut self, _assignment: &mut Assignment) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_assignment(&mut self, assignment: &mut Assignment) -> Result<(), Self::Error> {
        self.enter_assignment(assignment)?;
        self.visit_identifier_vector(&mut assignment.variables)?;
        self.visit_expression(&mut assignment.value)?;
        self.exit_assignment(assignment)
    }
    fn exit_assignment(&mut self, _assignment: &mut Assignment) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_expression(&mut self, _expression: &mut Expression) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_expression(&mut self, expression: &mut Expression) -> Result<(), Self::Error> {
        self.enter_expression(expression)?;
        match expression {
            Expression::Literal(literal) => self.visit_literal(literal)?,
            Expression::Identifier(identifier) => self.visit_identifier(identifier)?,
            Expression::FunctionCall(function) => self.visit_function_call(function)?,
        }
        self.exit_expression(expression)
    }
    fn exit_expression(&mut self, _expression: &mut Expression) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_literal(&mut self, _literal: &mut Literal) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_literal(&mut self, literal: &mut Literal) -> Result<(), Self::Error> {
        self.enter_literal(literal)?;
        self.exit_literal(literal)
    }
    fn exit_literal(&mut self, _literal: &mut Literal) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_identifier(&mut self, _identifier: &mut Identifier) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_identifier(&mut self, identifier: &mut Identifier) -> Result<(), Self::Error> {
        self.enter_identifier(identifier)?;
        self.exit_identifier(identifier)
    }
    fn exit_identifier(&mut self, _identifier: &mut Identifier) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_function_call(&mut self, _fun_call: &mut FunctionCall) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_function_call(&mut self, fun_call: &mut FunctionCall) -> Result<(), Self::Error> {
        self.enter_function_call(fun_call)?;
        self.visit_identifier(&mut fun_call.function)?;
        for a in fun_call.arguments.iter_mut() {
            self.visit_expression(a)?;
        }
        self.exit_function_call(fun_call)
    }
    fn exit_function_call(&mut self, _fun_call: &mut FunctionCall) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_if(&mut self, _x: &mut If) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_if(&mut self, ifs: &mut If) -> Result<(), Self::Error> {
        self.enter_if(ifs)?;
        self.visit_expression(&mut ifs.condition)?;
        self.visit_block(&mut ifs.body)?;
        self.exit_if(ifs)
    }
    fn exit_if(&mut self, _x: &mut If) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_switch(&mut self, _x: &mut Switch) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_switch(&mut self, switch: &mut Switch) -> Result<(), Self::Error> {
        self.enter_switch(switch)?;
        self.visit_expression(&mut switch.expression)?;
        for c in &mut switch.cases {
            self.enter_case(c)?;
            self.visit_case(c)?;
            self.exit_case(c)?;
        }
        self.exit_switch(switch)
    }
    fn exit_switch(&mut self, _x: &mut Switch) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_case(&mut self, _case: &mut Case) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_case(&mut self, case: &mut Case) -> Result<(), Self::Error> {
        if let Some(literal) = &mut case.literal {
            self.visit_literal(literal)?;
        }
        self.visit_block(&mut case.body)
    }
    fn exit_case(&mut self, _case: &mut Case) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_for(&mut self, _x: &mut ForLoop) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_for(&mut self, for_loop: &mut ForLoop) -> Result<(), Self::Error> {
        self.enter_for(for_loop)?;
        self.visit_block(&mut for_loop.pre)?;
        self.visit_expression(&mut for_loop.condition)?;
        self.visit_block(&mut for_loop.post)?;
        self.visit_block(&mut for_loop.body)?;
        self.exit_for(for_loop)
    }
    fn exit_for(&mut self, _x: &mut ForLoop) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_break(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_break(&mut self) -> Result<(), Self::Error> {
        self.enter_break()?;
        self.exit_break()
    }
    fn exit_break(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_continue(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_continue(&mut self) -> Result<(), Self::Error> {
        self.enter_continue()?;
        self.exit_continue()
    }
    fn exit_continue(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_leave(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_leave(&mut self) -> Result<(), Self::Error> {
        self.enter_leave()?;
        self.exit_leave()
    }
    fn exit_leave(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}
