package parsing

import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, RuleNode, TerminalNode}
import sg.edu.ntu.hchen.VHDLParser._
import sg.edu.ntu.hchen.VHDLVisitor

final class TVisitor(var vInfo: Option[VInfo]) extends Keeper(vInfo) with VHDLVisitor[Unit] {

  override def visitAccess_type_definition(ctx: Access_type_definitionContext): Unit = visitChildren(ctx)

  override def visitAcross_aspect(ctx: Across_aspectContext): Unit = visitChildren(ctx)

  override def visitActual_designator(ctx: Actual_designatorContext): Unit = visitChildren(ctx)

  override def visitActual_parameter_part(ctx: Actual_parameter_partContext): Unit = visitChildren(ctx)

  override def visitActual_part(ctx: Actual_partContext): Unit = visitChildren(ctx)

  override def visitAdding_operator(ctx: Adding_operatorContext): Unit = visitChildren(ctx)

  override def visitAggregate(ctx: AggregateContext): Unit = visitChildren(ctx)

  override def visitAlias_declaration(ctx: Alias_declarationContext): Unit = visitChildren(ctx)

  override def visitAlias_designator(ctx: Alias_designatorContext): Unit = visitChildren(ctx)

  override def visitAlias_indication(ctx: Alias_indicationContext): Unit = visitChildren(ctx)

  override def visitAllocator(ctx: AllocatorContext): Unit = visitChildren(ctx)

  override def visitArchitecture_body(ctx: Architecture_bodyContext): Unit = visitChildren(ctx)

  override def visitArchitecture_declarative_part(ctx: Architecture_declarative_partContext): Unit = visitChildren(ctx)

  override def visitArchitecture_statement(ctx: Architecture_statementContext): Unit = visitChildren(ctx)

  override def visitArchitecture_statement_part(ctx: Architecture_statement_partContext): Unit = visitChildren(ctx)

  override def visitArray_nature_definition(ctx: Array_nature_definitionContext): Unit = visitChildren(ctx)

  override def visitArray_type_definition(ctx: Array_type_definitionContext): Unit = visitChildren(ctx)

  override def visitAssertion(ctx: AssertionContext): Unit = visitChildren(ctx)

  override def visitAssertion_statement(ctx: Assertion_statementContext): Unit = visitChildren(ctx)

  override def visitAssociation_element(ctx: Association_elementContext): Unit = visitChildren(ctx)

  override def visitAssociation_list(ctx: Association_listContext): Unit = visitChildren(ctx)

  override def visitAttribute_declaration(ctx: Attribute_declarationContext): Unit = visitChildren(ctx)

  override def visitAttribute_designator(ctx: Attribute_designatorContext): Unit = visitChildren(ctx)

  override def visitAttribute_specification(ctx: Attribute_specificationContext): Unit = visitChildren(ctx)

  override def visitBase_unit_declaration(ctx: Base_unit_declarationContext): Unit = visitChildren(ctx)

  override def visitBinding_indication(ctx: Binding_indicationContext): Unit = visitChildren(ctx)

  override def visitBlock_configuration(ctx: Block_configurationContext): Unit = visitChildren(ctx)

  override def visitBlock_declarative_item(ctx: Block_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitBlock_declarative_part(ctx: Block_declarative_partContext): Unit = visitChildren(ctx)

  override def visitBlock_header(ctx: Block_headerContext): Unit = visitChildren(ctx)

  override def visitBlock_specification(ctx: Block_specificationContext): Unit = visitChildren(ctx)

  override def visitBlock_statement(ctx: Block_statementContext): Unit = visitChildren(ctx)

  override def visitBlock_statement_part(ctx: Block_statement_partContext): Unit = visitChildren(ctx)

  override def visitBranch_quantity_declaration(ctx: Branch_quantity_declarationContext): Unit = visitChildren(ctx)

  override def visitBreak_element(ctx: Break_elementContext): Unit = visitChildren(ctx)

  override def visitBreak_list(ctx: Break_listContext): Unit = visitChildren(ctx)

  override def visitBreak_selector_clause(ctx: Break_selector_clauseContext): Unit = visitChildren(ctx)

  override def visitBreak_statement(ctx: Break_statementContext): Unit = visitChildren(ctx)

  override def visitCase_statement(ctx: Case_statementContext): Unit = visitChildren(ctx)

  override def visitCase_statement_alternative(ctx: Case_statement_alternativeContext): Unit = visitChildren(ctx)

  override def visitChoice(ctx: ChoiceContext): Unit = visitChildren(ctx)

  override def visitChoices(ctx: ChoicesContext): Unit = visitChildren(ctx)

  override def visitComponent_configuration(ctx: Component_configurationContext): Unit = visitChildren(ctx)

  override def visitComponent_declaration(ctx: Component_declarationContext): Unit = visitChildren(ctx)

  override def visitComponent_instantiation_statement(ctx: Component_instantiation_statementContext): Unit = visitChildren(ctx)

  override def visitComponent_specification(ctx: Component_specificationContext): Unit = visitChildren(ctx)

  override def visitComposite_nature_definition(ctx: Composite_nature_definitionContext): Unit = visitChildren(ctx)

  override def visitComposite_type_definition(ctx: Composite_type_definitionContext): Unit = visitChildren(ctx)

  override def visitConcurrent_assertion_statement(ctx: Concurrent_assertion_statementContext): Unit = visitChildren(ctx)

  override def visitConcurrent_break_statement(ctx: Concurrent_break_statementContext): Unit = visitChildren(ctx)

  override def visitConcurrent_procedure_call_statement(ctx: Concurrent_procedure_call_statementContext): Unit = visitChildren(ctx)

  override def visitConcurrent_signal_assignment_statement(ctx: Concurrent_signal_assignment_statementContext): Unit = {}

  override def visitCondition(ctx: ConditionContext): Unit = visitChildren(ctx)

  override def visitCondition_clause(ctx: Condition_clauseContext): Unit = visitChildren(ctx)

  override def visitConditional_signal_assignment(ctx: Conditional_signal_assignmentContext): Unit = visitChildren(ctx)

  override def visitConditional_waveforms(ctx: Conditional_waveformsContext): Unit = visitChildren(ctx)

  override def visitConfiguration_declaration(ctx: Configuration_declarationContext): Unit = visitChildren(ctx)

  override def visitConfiguration_declarative_item(ctx: Configuration_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitConfiguration_declarative_part(ctx: Configuration_declarative_partContext): Unit = visitChildren(ctx)

  override def visitConfiguration_item(ctx: Configuration_itemContext): Unit = visitChildren(ctx)

  override def visitConfiguration_specification(ctx: Configuration_specificationContext): Unit = visitChildren(ctx)

  override def visitConstant_declaration(ctx: Constant_declarationContext): Unit = {}

  override def visitConstrained_array_definition(ctx: Constrained_array_definitionContext): Unit = visitChildren(ctx)

  override def visitConstrained_nature_definition(ctx: Constrained_nature_definitionContext): Unit = visitChildren(ctx)

  override def visitConstraint(ctx: ConstraintContext): Unit = visitChildren(ctx)

  override def visitContext_clause(ctx: Context_clauseContext): Unit = visitChildren(ctx)

  override def visitContext_item(ctx: Context_itemContext): Unit = visitChildren(ctx)

  override def visitDelay_mechanism(ctx: Delay_mechanismContext): Unit = visitChildren(ctx)

  override def visitDesign_file(ctx: Design_fileContext): Unit = visitChildren(ctx)

  override def visitDesign_unit(ctx: Design_unitContext): Unit = visitChildren(ctx)

  override def visitDesignator(ctx: DesignatorContext): Unit = visitChildren(ctx)

  override def visitDirection(ctx: DirectionContext): Unit = visitChildren(ctx)

  override def visitDisconnection_specification(ctx: Disconnection_specificationContext): Unit = visitChildren(ctx)

  override def visitDiscrete_range(ctx: Discrete_rangeContext): Unit = visitChildren(ctx)

  override def visitElement_association(ctx: Element_associationContext): Unit = visitChildren(ctx)

  override def visitElement_declaration(ctx: Element_declarationContext): Unit = visitChildren(ctx)

  override def visitElement_subnature_definition(ctx: Element_subnature_definitionContext): Unit = visitChildren(ctx)

  override def visitElement_subtype_definition(ctx: Element_subtype_definitionContext): Unit = visitChildren(ctx)

  override def visitEntity_aspect(ctx: Entity_aspectContext): Unit = visitChildren(ctx)

  override def visitEntity_class(ctx: Entity_classContext): Unit = visitChildren(ctx)

  override def visitEntity_class_entry(ctx: Entity_class_entryContext): Unit = visitChildren(ctx)

  override def visitEntity_class_entry_list(ctx: Entity_class_entry_listContext): Unit = visitChildren(ctx)

  override def visitEntity_declaration(ctx: Entity_declarationContext): Unit = {}

  override def visitEntity_declarative_item(ctx: Entity_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitEntity_declarative_part(ctx: Entity_declarative_partContext): Unit = visitChildren(ctx)

  override def visitEntity_designator(ctx: Entity_designatorContext): Unit = visitChildren(ctx)

  override def visitEntity_header(ctx: Entity_headerContext): Unit = visitChildren(ctx)

  override def visitEntity_name_list(ctx: Entity_name_listContext): Unit = visitChildren(ctx)

  override def visitEntity_specification(ctx: Entity_specificationContext): Unit = visitChildren(ctx)

  override def visitEntity_statement(ctx: Entity_statementContext): Unit = visitChildren(ctx)

  override def visitEntity_statement_part(ctx: Entity_statement_partContext): Unit = visitChildren(ctx)

  override def visitEntity_tag(ctx: Entity_tagContext): Unit = visitChildren(ctx)

  override def visitEnumeration_literal(ctx: Enumeration_literalContext): Unit = visitChildren(ctx)

  override def visitEnumeration_type_definition(ctx: Enumeration_type_definitionContext): Unit = visitChildren(ctx)

  override def visitExit_statement(ctx: Exit_statementContext): Unit = visitChildren(ctx)

  override def visitExpression(ctx: ExpressionContext): Unit = visitChildren(ctx)

  override def visitFactor(ctx: FactorContext): Unit = visitChildren(ctx)

  override def visitFile_declaration(ctx: File_declarationContext): Unit = visitChildren(ctx)

  override def visitFile_logical_name(ctx: File_logical_nameContext): Unit = visitChildren(ctx)

  override def visitFile_open_information(ctx: File_open_informationContext): Unit = visitChildren(ctx)

  override def visitFile_type_definition(ctx: File_type_definitionContext): Unit = visitChildren(ctx)

  override def visitFormal_parameter_list(ctx: Formal_parameter_listContext): Unit = visitChildren(ctx)

  override def visitFormal_part(ctx: Formal_partContext): Unit = visitChildren(ctx)

  override def visitFree_quantity_declaration(ctx: Free_quantity_declarationContext): Unit = visitChildren(ctx)

  override def visitGenerate_statement(ctx: Generate_statementContext): Unit = visitChildren(ctx)

  override def visitGeneration_scheme(ctx: Generation_schemeContext): Unit = visitChildren(ctx)

  override def visitGeneric_clause(ctx: Generic_clauseContext): Unit = visitChildren(ctx)

  override def visitGeneric_list(ctx: Generic_listContext): Unit = visitChildren(ctx)

  override def visitGeneric_map_aspect(ctx: Generic_map_aspectContext): Unit = visitChildren(ctx)

  override def visitGroup_constituent(ctx: Group_constituentContext): Unit = visitChildren(ctx)

  override def visitGroup_constituent_list(ctx: Group_constituent_listContext): Unit = visitChildren(ctx)

  override def visitGroup_declaration(ctx: Group_declarationContext): Unit = visitChildren(ctx)

  override def visitGroup_template_declaration(ctx: Group_template_declarationContext): Unit = visitChildren(ctx)

  override def visitGuarded_signal_specification(ctx: Guarded_signal_specificationContext): Unit = visitChildren(ctx)

  override def visitIdentifier(ctx: IdentifierContext): Unit = visitChildren(ctx)

  override def visitIdentifier_list(ctx: Identifier_listContext): Unit = visitChildren(ctx)

  override def visitIf_statement(ctx: If_statementContext): Unit = visitChildren(ctx)

  override def visitIndex_constraint(ctx: Index_constraintContext): Unit = visitChildren(ctx)

  override def visitIndex_specification(ctx: Index_specificationContext): Unit = visitChildren(ctx)

  override def visitIndex_subtype_definition(ctx: Index_subtype_definitionContext): Unit = visitChildren(ctx)

  override def visitInstantiated_unit(ctx: Instantiated_unitContext): Unit = visitChildren(ctx)

  override def visitInstantiation_list(ctx: Instantiation_listContext): Unit = visitChildren(ctx)

  override def visitInterface_constant_declaration(ctx: Interface_constant_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_declaration(ctx: Interface_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_element(ctx: Interface_elementContext): Unit = visitChildren(ctx)

  override def visitInterface_file_declaration(ctx: Interface_file_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_signal_list(ctx: Interface_signal_listContext): Unit = visitChildren(ctx)

  override def visitInterface_port_list(ctx: Interface_port_listContext): Unit = visitChildren(ctx)

  override def visitInterface_list(ctx: Interface_listContext): Unit = visitChildren(ctx)

  override def visitInterface_quantity_declaration(ctx: Interface_quantity_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_port_declaration(ctx: Interface_port_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_signal_declaration(ctx: Interface_signal_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_terminal_declaration(ctx: Interface_terminal_declarationContext): Unit = visitChildren(ctx)

  override def visitInterface_variable_declaration(ctx: Interface_variable_declarationContext): Unit = visitChildren(ctx)

  override def visitIteration_scheme(ctx: Iteration_schemeContext): Unit = visitChildren(ctx)

  override def visitLabel_colon(ctx: Label_colonContext): Unit = visitChildren(ctx)

  override def visitLibrary_clause(ctx: Library_clauseContext): Unit = visitChildren(ctx)

  override def visitLibrary_unit(ctx: Library_unitContext): Unit = visitChildren(ctx)

  override def visitLiteral(ctx: LiteralContext): Unit = visitChildren(ctx)

  override def visitLogical_name(ctx: Logical_nameContext): Unit = visitChildren(ctx)

  override def visitLogical_name_list(ctx: Logical_name_listContext): Unit = visitChildren(ctx)

  override def visitLogical_operator(ctx: Logical_operatorContext): Unit = visitChildren(ctx)

  override def visitLoop_statement(ctx: Loop_statementContext): Unit = visitChildren(ctx)

  override def visitSignal_mode(ctx: Signal_modeContext): Unit = visitChildren(ctx)

  override def visitMultiplying_operator(ctx: Multiplying_operatorContext): Unit = visitChildren(ctx)

  override def visitName(ctx: NameContext): Unit = visitChildren(ctx)

  override def visitName_part(ctx: Name_partContext): Unit = visitChildren(ctx)

  override def visitName_attribute_part(ctx: Name_attribute_partContext): Unit = visitChildren(ctx)

  override def visitName_function_call_or_indexed_part(ctx: Name_function_call_or_indexed_partContext): Unit = visitChildren(ctx)

  override def visitName_slice_part(ctx: Name_slice_partContext): Unit = visitChildren(ctx)

  override def visitSelected_name(ctx: Selected_nameContext): Unit = visitChildren(ctx)

  override def visitNature_declaration(ctx: Nature_declarationContext): Unit = visitChildren(ctx)

  override def visitNature_definition(ctx: Nature_definitionContext): Unit = visitChildren(ctx)

  override def visitNature_element_declaration(ctx: Nature_element_declarationContext): Unit = visitChildren(ctx)

  override def visitNext_statement(ctx: Next_statementContext): Unit = visitChildren(ctx)

  override def visitNumeric_literal(ctx: Numeric_literalContext): Unit = visitChildren(ctx)

  override def visitObject_declaration(ctx: Object_declarationContext): Unit = visitChildren(ctx)

  override def visitOpts(ctx: OptsContext): Unit = visitChildren(ctx)

  override def visitPackage_body(ctx: Package_bodyContext): Unit = visitChildren(ctx)

  override def visitPackage_body_declarative_item(ctx: Package_body_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitPackage_body_declarative_part(ctx: Package_body_declarative_partContext): Unit = visitChildren(ctx)

  override def visitPackage_declaration(ctx: Package_declarationContext): Unit = visitChildren(ctx)

  override def visitPackage_declarative_item(ctx: Package_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitPackage_declarative_part(ctx: Package_declarative_partContext): Unit = visitChildren(ctx)

  override def visitParameter_specification(ctx: Parameter_specificationContext): Unit = visitChildren(ctx)

  override def visitPhysical_literal(ctx: Physical_literalContext): Unit = visitChildren(ctx)

  override def visitPhysical_type_definition(ctx: Physical_type_definitionContext): Unit = visitChildren(ctx)

  override def visitPort_clause(ctx: Port_clauseContext): Unit = visitChildren(ctx)

  override def visitPort_list(ctx: Port_listContext): Unit = {}

  override def visitPort_map_aspect(ctx: Port_map_aspectContext): Unit = visitChildren(ctx)

  override def visitPrimary(ctx: PrimaryContext): Unit = visitChildren(ctx)

  override def visitPrimary_unit(ctx: Primary_unitContext): Unit = visitChildren(ctx)

  override def visitProcedural_declarative_item(ctx: Procedural_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitProcedural_declarative_part(ctx: Procedural_declarative_partContext): Unit = visitChildren(ctx)

  override def visitProcedural_statement_part(ctx: Procedural_statement_partContext): Unit = visitChildren(ctx)

  override def visitProcedure_call(ctx: Procedure_callContext): Unit = visitChildren(ctx)

  override def visitProcedure_call_statement(ctx: Procedure_call_statementContext): Unit = visitChildren(ctx)

  override def visitProcess_declarative_item(ctx: Process_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitProcess_declarative_part(ctx: Process_declarative_partContext): Unit = visitChildren(ctx)

  override def visitProcess_statement(ctx: Process_statementContext): Unit = {}

  override def visitProcess_statement_part(ctx: Process_statement_partContext): Unit = visitChildren(ctx)

  override def visitQualified_expression(ctx: Qualified_expressionContext): Unit = visitChildren(ctx)

  override def visitQuantity_declaration(ctx: Quantity_declarationContext): Unit = visitChildren(ctx)

  override def visitQuantity_list(ctx: Quantity_listContext): Unit = visitChildren(ctx)

  override def visitQuantity_specification(ctx: Quantity_specificationContext): Unit = visitChildren(ctx)

  override def visitRange(ctx: RangeContext): Unit = visitChildren(ctx)

  override def visitExplicit_range(ctx: Explicit_rangeContext): Unit = visitChildren(ctx)

  override def visitRange_constraint(ctx: Range_constraintContext): Unit = visitChildren(ctx)

  override def visitRecord_nature_definition(ctx: Record_nature_definitionContext): Unit = visitChildren(ctx)

  override def visitRecord_type_definition(ctx: Record_type_definitionContext): Unit = {}

  override def visitRelation(ctx: RelationContext): Unit = visitChildren(ctx)

  override def visitRelational_operator(ctx: Relational_operatorContext): Unit = visitChildren(ctx)

  override def visitReport_statement(ctx: Report_statementContext): Unit = visitChildren(ctx)

  override def visitReturn_statement(ctx: Return_statementContext): Unit = visitChildren(ctx)

  override def visitScalar_nature_definition(ctx: Scalar_nature_definitionContext): Unit = visitChildren(ctx)

  override def visitScalar_type_definition(ctx: Scalar_type_definitionContext): Unit = visitChildren(ctx)

  override def visitSecondary_unit(ctx: Secondary_unitContext): Unit = visitChildren(ctx)

  override def visitSecondary_unit_declaration(ctx: Secondary_unit_declarationContext): Unit = visitChildren(ctx)

  override def visitSelected_signal_assignment(ctx: Selected_signal_assignmentContext): Unit = visitChildren(ctx)

  override def visitSelected_waveforms(ctx: Selected_waveformsContext): Unit = visitChildren(ctx)

  override def visitSensitivity_clause(ctx: Sensitivity_clauseContext): Unit = visitChildren(ctx)

  override def visitSensitivity_list(ctx: Sensitivity_listContext): Unit = visitChildren(ctx)

  override def visitSequence_of_statements(ctx: Sequence_of_statementsContext): Unit = visitChildren(ctx)

  override def visitSequential_statement(ctx: Sequential_statementContext): Unit = visitChildren(ctx)

  override def visitShift_expression(ctx: Shift_expressionContext): Unit = visitChildren(ctx)

  override def visitShift_operator(ctx: Shift_operatorContext): Unit = visitChildren(ctx)

  override def visitSignal_assignment_statement(ctx: Signal_assignment_statementContext): Unit = visitChildren(ctx)

  override def visitSignal_declaration(ctx: Signal_declarationContext): Unit = {}

  override def visitSignal_kind(ctx: Signal_kindContext): Unit = visitChildren(ctx)

  override def visitSignal_list(ctx: Signal_listContext): Unit = visitChildren(ctx)

  override def visitSignature(ctx: SignatureContext): Unit = visitChildren(ctx)

  override def visitSimple_expression(ctx: Simple_expressionContext): Unit = visitChildren(ctx)

  override def visitSimple_simultaneous_statement(ctx: Simple_simultaneous_statementContext): Unit = visitChildren(ctx)

  override def visitSimultaneous_alternative(ctx: Simultaneous_alternativeContext): Unit = visitChildren(ctx)

  override def visitSimultaneous_case_statement(ctx: Simultaneous_case_statementContext): Unit = visitChildren(ctx)

  override def visitSimultaneous_if_statement(ctx: Simultaneous_if_statementContext): Unit = visitChildren(ctx)

  override def visitSimultaneous_procedural_statement(ctx: Simultaneous_procedural_statementContext): Unit = visitChildren(ctx)

  override def visitSimultaneous_statement(ctx: Simultaneous_statementContext): Unit = visitChildren(ctx)

  override def visitSimultaneous_statement_part(ctx: Simultaneous_statement_partContext): Unit = visitChildren(ctx)

  override def visitSource_aspect(ctx: Source_aspectContext): Unit = visitChildren(ctx)

  override def visitSource_quantity_declaration(ctx: Source_quantity_declarationContext): Unit = visitChildren(ctx)

  override def visitStep_limit_specification(ctx: Step_limit_specificationContext): Unit = visitChildren(ctx)

  override def visitSubnature_declaration(ctx: Subnature_declarationContext): Unit = visitChildren(ctx)

  override def visitSubnature_indication(ctx: Subnature_indicationContext): Unit = visitChildren(ctx)

  override def visitSubprogram_body(ctx: Subprogram_bodyContext): Unit = visitChildren(ctx)

  override def visitSubprogram_declaration(ctx: Subprogram_declarationContext): Unit = visitChildren(ctx)

  override def visitSubprogram_declarative_item(ctx: Subprogram_declarative_itemContext): Unit = visitChildren(ctx)

  override def visitSubprogram_declarative_part(ctx: Subprogram_declarative_partContext): Unit = visitChildren(ctx)

  override def visitSubprogram_kind(ctx: Subprogram_kindContext): Unit = visitChildren(ctx)

  override def visitSubprogram_specification(ctx: Subprogram_specificationContext): Unit = visitChildren(ctx)

  override def visitProcedure_specification(ctx: Procedure_specificationContext): Unit = visitChildren(ctx)

  override def visitFunction_specification(ctx: Function_specificationContext): Unit = visitChildren(ctx)

  override def visitSubprogram_statement_part(ctx: Subprogram_statement_partContext): Unit = visitChildren(ctx)

  override def visitSubtype_declaration(ctx: Subtype_declarationContext): Unit = visitChildren(ctx)

  override def visitSubtype_indication(ctx: Subtype_indicationContext): Unit = visitChildren(ctx)

  override def visitSuffix(ctx: SuffixContext): Unit = visitChildren(ctx)

  override def visitTarget(ctx: TargetContext): Unit = visitChildren(ctx)

  override def visitTerm(ctx: TermContext): Unit = visitChildren(ctx)

  override def visitTerminal_aspect(ctx: Terminal_aspectContext): Unit = visitChildren(ctx)

  override def visitTerminal_declaration(ctx: Terminal_declarationContext): Unit = visitChildren(ctx)

  override def visitThrough_aspect(ctx: Through_aspectContext): Unit = visitChildren(ctx)

  override def visitTimeout_clause(ctx: Timeout_clauseContext): Unit = visitChildren(ctx)

  override def visitTolerance_aspect(ctx: Tolerance_aspectContext): Unit = visitChildren(ctx)

  override def visitType_declaration(ctx: Type_declarationContext): Unit = visitChildren(ctx)

  override def visitType_definition(ctx: Type_definitionContext): Unit = visitChildren(ctx)

  override def visitUnconstrained_array_definition(ctx: Unconstrained_array_definitionContext): Unit = visitChildren(ctx)

  override def visitUnconstrained_nature_definition(ctx: Unconstrained_nature_definitionContext): Unit = visitChildren(ctx)

  override def visitUse_clause(ctx: Use_clauseContext): Unit = visitChildren(ctx)

  override def visitVariable_assignment_statement(ctx: Variable_assignment_statementContext): Unit = visitChildren(ctx)

  override def visitVariable_declaration(ctx: Variable_declarationContext): Unit = {}

  override def visitWait_statement(ctx: Wait_statementContext): Unit = visitChildren(ctx)

  override def visitWaveform(ctx: WaveformContext): Unit = visitChildren(ctx)

  override def visitWaveform_element(ctx: Waveform_elementContext): Unit = visitChildren(ctx)

  override def visitTerminal(node: TerminalNode): Unit = {}

  override def visitChildren(node: RuleNode): Unit = {}

  override def visitErrorNode(node: ErrorNode): Unit = {}

  override def visit(tree: ParseTree): Unit = {}

  override def visitAbstract_literal(ctx: Abstract_literalContext): Unit = visitChildren(ctx)
}
