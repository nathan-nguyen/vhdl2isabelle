package parsing

import parsing.V2IUtils._
import sg.edu.ntu.hchen.VHDLBaseVisitor
import sg.edu.ntu.hchen.VHDLParser._

import scala.collection.JavaConversions._


final class TVisitor(protected[this] var vInfo: Option[VInfo]) extends VHDLBaseVisitor[Unit] with Keeper {

  override def visitAbstract_literal(ctx: Abstract_literalContext): Unit = super.visitAbstract_literal(ctx)

  override def visitAccess_type_definition(ctx: Access_type_definitionContext): Unit = super.visitAccess_type_definition(ctx)

  override def visitAcross_aspect(ctx: Across_aspectContext): Unit = super.visitAcross_aspect(ctx)

  override def visitActual_designator(ctx: Actual_designatorContext): Unit = super.visitActual_designator(ctx)

  override def visitActual_parameter_part(ctx: Actual_parameter_partContext): Unit = super.visitActual_parameter_part(ctx)

  override def visitActual_part(ctx: Actual_partContext): Unit = super.visitActual_part(ctx)

  override def visitAdding_operator(ctx: Adding_operatorContext): Unit = super.visitAdding_operator(ctx)

  override def visitAggregate(ctx: AggregateContext): Unit = super.visitAggregate(ctx)

  override def visitAlias_declaration(ctx: Alias_declarationContext): Unit = super.visitAlias_declaration(ctx)

  override def visitAlias_designator(ctx: Alias_designatorContext): Unit = super.visitAlias_designator(ctx)

  override def visitAlias_indication(ctx: Alias_indicationContext): Unit = super.visitAlias_indication(ctx)

  override def visitAllocator(ctx: AllocatorContext): Unit = super.visitAllocator(ctx)

  override def visitArchitecture_body(ctx: Architecture_bodyContext): Unit = super.visitArchitecture_body(ctx)

  override def visitArchitecture_declarative_part(ctx: Architecture_declarative_partContext): Unit = super.visitArchitecture_declarative_part(ctx)

  override def visitArchitecture_statement(ctx: Architecture_statementContext): Unit = super.visitArchitecture_statement(ctx)

  override def visitArchitecture_statement_part(ctx: Architecture_statement_partContext): Unit = super.visitArchitecture_statement_part(ctx)

  override def visitArray_nature_definition(ctx: Array_nature_definitionContext): Unit = super.visitArray_nature_definition(ctx)

  override def visitArray_type_definition(ctx: Array_type_definitionContext): Unit = super.visitArray_type_definition(ctx)

  override def visitAssertion(ctx: AssertionContext): Unit = super.visitAssertion(ctx)

  override def visitAssertion_statement(ctx: Assertion_statementContext): Unit = super.visitAssertion_statement(ctx)

  override def visitAssociation_element(ctx: Association_elementContext): Unit = super.visitAssociation_element(ctx)

  override def visitAssociation_list(ctx: Association_listContext): Unit = super.visitAssociation_list(ctx)

  override def visitAttribute_declaration(ctx: Attribute_declarationContext): Unit = super.visitAttribute_declaration(ctx)

  override def visitAttribute_designator(ctx: Attribute_designatorContext): Unit = super.visitAttribute_designator(ctx)

  override def visitAttribute_specification(ctx: Attribute_specificationContext): Unit = super.visitAttribute_specification(ctx)

  override def visitBase_unit_declaration(ctx: Base_unit_declarationContext): Unit = super.visitBase_unit_declaration(ctx)

  override def visitBinding_indication(ctx: Binding_indicationContext): Unit = super.visitBinding_indication(ctx)

  override def visitBlock_configuration(ctx: Block_configurationContext): Unit = super.visitBlock_configuration(ctx)

  override def visitBlock_declarative_item(ctx: Block_declarative_itemContext): Unit = super.visitBlock_declarative_item(ctx)

  override def visitBlock_declarative_part(ctx: Block_declarative_partContext): Unit = super.visitBlock_declarative_part(ctx)

  override def visitBlock_header(ctx: Block_headerContext): Unit = super.visitBlock_header(ctx)

  override def visitBlock_specification(ctx: Block_specificationContext): Unit = super.visitBlock_specification(ctx)

  override def visitBlock_statement(ctx: Block_statementContext): Unit = super.visitBlock_statement(ctx)

  override def visitBlock_statement_part(ctx: Block_statement_partContext): Unit = super.visitBlock_statement_part(ctx)

  override def visitBranch_quantity_declaration(ctx: Branch_quantity_declarationContext): Unit = super.visitBranch_quantity_declaration(ctx)

  override def visitBreak_element(ctx: Break_elementContext): Unit = super.visitBreak_element(ctx)

  override def visitBreak_list(ctx: Break_listContext): Unit = super.visitBreak_list(ctx)

  override def visitBreak_selector_clause(ctx: Break_selector_clauseContext): Unit = super.visitBreak_selector_clause(ctx)

  override def visitBreak_statement(ctx: Break_statementContext): Unit = super.visitBreak_statement(ctx)

  override def visitCase_statement(ctx: Case_statementContext): Unit = super.visitCase_statement(ctx)

  override def visitCase_statement_alternative(ctx: Case_statement_alternativeContext): Unit = super.visitCase_statement_alternative(ctx)

  override def visitChoice(ctx: ChoiceContext): Unit = super.visitChoice(ctx)

  override def visitChoices(ctx: ChoicesContext): Unit = super.visitChoices(ctx)

  override def visitComponent_configuration(ctx: Component_configurationContext): Unit = super.visitComponent_configuration(ctx)

  override def visitComponent_declaration(ctx: Component_declarationContext): Unit = super.visitComponent_declaration(ctx)

  override def visitComponent_instantiation_statement(ctx: Component_instantiation_statementContext): Unit = super.visitComponent_instantiation_statement(ctx)

  override def visitComponent_specification(ctx: Component_specificationContext): Unit = super.visitComponent_specification(ctx)

  override def visitComposite_nature_definition(ctx: Composite_nature_definitionContext): Unit = super.visitComposite_nature_definition(ctx)

  override def visitComposite_type_definition(ctx: Composite_type_definitionContext): Unit = super.visitComposite_type_definition(ctx)

  override def visitConcurrent_assertion_statement(ctx: Concurrent_assertion_statementContext): Unit = super.visitConcurrent_assertion_statement(ctx)

  override def visitConcurrent_break_statement(ctx: Concurrent_break_statementContext): Unit = super.visitConcurrent_break_statement(ctx)

  override def visitConcurrent_procedure_call_statement(ctx: Concurrent_procedure_call_statementContext): Unit = super.visitConcurrent_procedure_call_statement(ctx)

  override def visitConcurrent_signal_assignment_statement(ctx: Concurrent_signal_assignment_statementContext): Unit = {
    val concurrentSignalAssign = VConcurrentSignalAssignStat(ctx)
    concurrentSignalAssign match {
      case VConcurrentSignalAssignStatC(_, _, condSignAssign) => {
        val targetName = condSignAssign.target.getName.getOrElse(defaultTargetName(s"${concurrentSignalAssign}"))
        logger.info(s"${targetName}")
      }
      case VConcurrentSignalAssignStatS(_, _, selectSignalAssign) => {
        val targetName = selectSignalAssign.target.getName.getOrElse(defaultTargetName(s"${concurrentSignalAssign}"))
      }
    }
    super.visitConcurrent_signal_assignment_statement(ctx)
  }

  override def visitCondition(ctx: ConditionContext): Unit = super.visitCondition(ctx)

  override def visitCondition_clause(ctx: Condition_clauseContext): Unit = super.visitCondition_clause(ctx)

  override def visitConditional_signal_assignment(ctx: Conditional_signal_assignmentContext): Unit = super.visitConditional_signal_assignment(ctx)

  override def visitConditional_waveforms(ctx: Conditional_waveformsContext): Unit = super.visitConditional_waveforms(ctx)

  override def visitConfiguration_declaration(ctx: Configuration_declarationContext): Unit = super.visitConfiguration_declaration(ctx)

  override def visitConfiguration_declarative_item(ctx: Configuration_declarative_itemContext): Unit = super.visitConfiguration_declarative_item(ctx)

  override def visitConfiguration_declarative_part(ctx: Configuration_declarative_partContext): Unit = super.visitConfiguration_declarative_part(ctx)

  override def visitConfiguration_item(ctx: Configuration_itemContext): Unit = super.visitConfiguration_item(ctx)

  override def visitConfiguration_specification(ctx: Configuration_specificationContext): Unit = super.visitConfiguration_specification(ctx)

  override def visitConstant_declaration(ctx: Constant_declarationContext): Unit = {
    val constDecl = VConstDecl(ctx)
    for {
      id <- constDecl.idList
    } {
      genIVariable(id, constDecl.vExp, constDecl.subtypeInd)
    }
    super.visitConstant_declaration(ctx)
  }

  override def visitConstrained_array_definition(ctx: Constrained_array_definitionContext): Unit = super.visitConstrained_array_definition(ctx)

  override def visitConstrained_nature_definition(ctx: Constrained_nature_definitionContext): Unit = super.visitConstrained_nature_definition(ctx)

  override def visitConstraint(ctx: ConstraintContext): Unit = super.visitConstraint(ctx)

  override def visitContext_clause(ctx: Context_clauseContext): Unit = super.visitContext_clause(ctx)

  override def visitContext_item(ctx: Context_itemContext): Unit = super.visitContext_item(ctx)

  override def visitDelay_mechanism(ctx: Delay_mechanismContext): Unit = super.visitDelay_mechanism(ctx)

  override def visitDesign_file(ctx: Design_fileContext): Unit = super.visitDesign_file(ctx)

  override def visitDesign_unit(ctx: Design_unitContext): Unit = super.visitDesign_unit(ctx)

  override def visitDesignator(ctx: DesignatorContext): Unit = super.visitDesignator(ctx)

  override def visitDirection(ctx: DirectionContext): Unit = super.visitDirection(ctx)

  override def visitDisconnection_specification(ctx: Disconnection_specificationContext): Unit = super.visitDisconnection_specification(ctx)

  override def visitDiscrete_range(ctx: Discrete_rangeContext): Unit = super.visitDiscrete_range(ctx)

  override def visitElement_association(ctx: Element_associationContext): Unit = super.visitElement_association(ctx)

  override def visitElement_declaration(ctx: Element_declarationContext): Unit = super.visitElement_declaration(ctx)

  override def visitElement_subnature_definition(ctx: Element_subnature_definitionContext): Unit = super.visitElement_subnature_definition(ctx)

  override def visitElement_subtype_definition(ctx: Element_subtype_definitionContext): Unit = super.visitElement_subtype_definition(ctx)

  override def visitEntity_aspect(ctx: Entity_aspectContext): Unit = super.visitEntity_aspect(ctx)

  override def visitEntity_class(ctx: Entity_classContext): Unit = super.visitEntity_class(ctx)

  override def visitEntity_class_entry(ctx: Entity_class_entryContext): Unit = super.visitEntity_class_entry(ctx)

  override def visitEntity_class_entry_list(ctx: Entity_class_entry_listContext): Unit = super.visitEntity_class_entry_list(ctx)

  override def visitEntity_declaration(ctx: Entity_declarationContext): Unit = {
    definedEntities += ctx.identifier().head.getText
    super.visitEntity_declaration(ctx)
  }

  override def visitEntity_declarative_item(ctx: Entity_declarative_itemContext): Unit = super.visitEntity_declarative_item(ctx)

  override def visitEntity_declarative_part(ctx: Entity_declarative_partContext): Unit = super.visitEntity_declarative_part(ctx)

  override def visitEntity_designator(ctx: Entity_designatorContext): Unit = super.visitEntity_designator(ctx)

  override def visitEntity_header(ctx: Entity_headerContext): Unit = super.visitEntity_header(ctx)

  override def visitEntity_name_list(ctx: Entity_name_listContext): Unit = super.visitEntity_name_list(ctx)

  override def visitEntity_specification(ctx: Entity_specificationContext): Unit = super.visitEntity_specification(ctx)

  override def visitEntity_statement(ctx: Entity_statementContext): Unit = super.visitEntity_statement(ctx)

  override def visitEntity_statement_part(ctx: Entity_statement_partContext): Unit = super.visitEntity_statement_part(ctx)

  override def visitEntity_tag(ctx: Entity_tagContext): Unit = super.visitEntity_tag(ctx)

  override def visitEnumeration_literal(ctx: Enumeration_literalContext): Unit = super.visitEnumeration_literal(ctx)

  override def visitEnumeration_type_definition(ctx: Enumeration_type_definitionContext): Unit = super.visitEnumeration_type_definition(ctx)

  override def visitExit_statement(ctx: Exit_statementContext): Unit = super.visitExit_statement(ctx)

  override def visitExpression(ctx: ExpressionContext): Unit = super.visitExpression(ctx)

  override def visitFactor(ctx: FactorContext): Unit = super.visitFactor(ctx)

  override def visitFile_declaration(ctx: File_declarationContext): Unit = super.visitFile_declaration(ctx)

  override def visitFile_logical_name(ctx: File_logical_nameContext): Unit = super.visitFile_logical_name(ctx)

  override def visitFile_open_information(ctx: File_open_informationContext): Unit = super.visitFile_open_information(ctx)

  override def visitFile_type_definition(ctx: File_type_definitionContext): Unit = super.visitFile_type_definition(ctx)

  override def visitFormal_parameter_list(ctx: Formal_parameter_listContext): Unit = super.visitFormal_parameter_list(ctx)

  override def visitFormal_part(ctx: Formal_partContext): Unit = super.visitFormal_part(ctx)

  override def visitFree_quantity_declaration(ctx: Free_quantity_declarationContext): Unit = super.visitFree_quantity_declaration(ctx)

  override def visitGenerate_statement(ctx: Generate_statementContext): Unit = super.visitGenerate_statement(ctx)

  override def visitGeneration_scheme(ctx: Generation_schemeContext): Unit = super.visitGeneration_scheme(ctx)

  override def visitGeneric_clause(ctx: Generic_clauseContext): Unit = super.visitGeneric_clause(ctx)

  override def visitGeneric_list(ctx: Generic_listContext): Unit = super.visitGeneric_list(ctx)

  override def visitGeneric_map_aspect(ctx: Generic_map_aspectContext): Unit = super.visitGeneric_map_aspect(ctx)

  override def visitGroup_constituent(ctx: Group_constituentContext): Unit = super.visitGroup_constituent(ctx)

  override def visitGroup_constituent_list(ctx: Group_constituent_listContext): Unit = super.visitGroup_constituent_list(ctx)

  override def visitGroup_declaration(ctx: Group_declarationContext): Unit = super.visitGroup_declaration(ctx)

  override def visitGroup_template_declaration(ctx: Group_template_declarationContext): Unit = super.visitGroup_template_declaration(ctx)

  override def visitGuarded_signal_specification(ctx: Guarded_signal_specificationContext): Unit = super.visitGuarded_signal_specification(ctx)

  override def visitIdentifier(ctx: IdentifierContext): Unit = super.visitIdentifier(ctx)

  override def visitIdentifier_list(ctx: Identifier_listContext): Unit = super.visitIdentifier_list(ctx)

  override def visitIf_statement(ctx: If_statementContext): Unit = super.visitIf_statement(ctx)

  override def visitIndex_constraint(ctx: Index_constraintContext): Unit = super.visitIndex_constraint(ctx)

  override def visitIndex_specification(ctx: Index_specificationContext): Unit = super.visitIndex_specification(ctx)

  override def visitIndex_subtype_definition(ctx: Index_subtype_definitionContext): Unit = super.visitIndex_subtype_definition(ctx)

  override def visitInstantiated_unit(ctx: Instantiated_unitContext): Unit = super.visitInstantiated_unit(ctx)

  override def visitInstantiation_list(ctx: Instantiation_listContext): Unit = super.visitInstantiation_list(ctx)

  override def visitInterface_constant_declaration(ctx: Interface_constant_declarationContext): Unit = super.visitInterface_constant_declaration(ctx)

  override def visitInterface_declaration(ctx: Interface_declarationContext): Unit = super.visitInterface_declaration(ctx)

  override def visitInterface_element(ctx: Interface_elementContext): Unit = super.visitInterface_element(ctx)

  override def visitInterface_file_declaration(ctx: Interface_file_declarationContext): Unit = super.visitInterface_file_declaration(ctx)

  override def visitInterface_signal_list(ctx: Interface_signal_listContext): Unit = super.visitInterface_signal_list(ctx)

  override def visitInterface_port_list(ctx: Interface_port_listContext): Unit = super.visitInterface_port_list(ctx)

  override def visitInterface_list(ctx: Interface_listContext): Unit = super.visitInterface_list(ctx)

  override def visitInterface_quantity_declaration(ctx: Interface_quantity_declarationContext): Unit = super.visitInterface_quantity_declaration(ctx)

  override def visitInterface_port_declaration(ctx: Interface_port_declarationContext): Unit = super.visitInterface_port_declaration(ctx)

  override def visitInterface_signal_declaration(ctx: Interface_signal_declarationContext): Unit = super.visitInterface_signal_declaration(ctx)


  override def visitInterface_terminal_declaration(ctx: Interface_terminal_declarationContext): Unit = super.visitInterface_terminal_declaration(ctx)

  override def visitInterface_variable_declaration(ctx: Interface_variable_declarationContext): Unit = super.visitInterface_variable_declaration(ctx)

  override def visitIteration_scheme(ctx: Iteration_schemeContext): Unit = super.visitIteration_scheme(ctx)

  override def visitLabel_colon(ctx: Label_colonContext): Unit = super.visitLabel_colon(ctx)

  override def visitLibrary_clause(ctx: Library_clauseContext): Unit = super.visitLibrary_clause(ctx)

  override def visitLibrary_unit(ctx: Library_unitContext): Unit = super.visitLibrary_unit(ctx)

  override def visitLiteral(ctx: LiteralContext): Unit = super.visitLiteral(ctx)

  override def visitLogical_name(ctx: Logical_nameContext): Unit = super.visitLogical_name(ctx)

  override def visitLogical_name_list(ctx: Logical_name_listContext): Unit = super.visitLogical_name_list(ctx)

  override def visitLogical_operator(ctx: Logical_operatorContext): Unit = super.visitLogical_operator(ctx)

  override def visitLoop_statement(ctx: Loop_statementContext): Unit = super.visitLoop_statement(ctx)

  override def visitSignal_mode(ctx: Signal_modeContext): Unit = super.visitSignal_mode(ctx)

  override def visitMultiplying_operator(ctx: Multiplying_operatorContext): Unit = super.visitMultiplying_operator(ctx)

  override def visitName(ctx: NameContext): Unit = super.visitName(ctx)

  override def visitName_part(ctx: Name_partContext): Unit = super.visitName_part(ctx)

  override def visitName_attribute_part(ctx: Name_attribute_partContext): Unit = super.visitName_attribute_part(ctx)

  override def visitName_function_call_or_indexed_part(ctx: Name_function_call_or_indexed_partContext): Unit = super.visitName_function_call_or_indexed_part(ctx)

  override def visitName_slice_part(ctx: Name_slice_partContext): Unit = super.visitName_slice_part(ctx)

  override def visitSelected_name(ctx: Selected_nameContext): Unit = super.visitSelected_name(ctx)

  override def visitNature_declaration(ctx: Nature_declarationContext): Unit = super.visitNature_declaration(ctx)

  override def visitNature_definition(ctx: Nature_definitionContext): Unit = super.visitNature_definition(ctx)

  override def visitNature_element_declaration(ctx: Nature_element_declarationContext): Unit = super.visitNature_element_declaration(ctx)

  override def visitNext_statement(ctx: Next_statementContext): Unit = super.visitNext_statement(ctx)

  override def visitNumeric_literal(ctx: Numeric_literalContext): Unit = super.visitNumeric_literal(ctx)

  override def visitObject_declaration(ctx: Object_declarationContext): Unit = super.visitObject_declaration(ctx)

  override def visitOpts(ctx: OptsContext): Unit = super.visitOpts(ctx)

  override def visitPackage_body(ctx: Package_bodyContext): Unit = super.visitPackage_body(ctx)

  override def visitPackage_body_declarative_item(ctx: Package_body_declarative_itemContext): Unit = super.visitPackage_body_declarative_item(ctx)

  override def visitPackage_body_declarative_part(ctx: Package_body_declarative_partContext): Unit = super.visitPackage_body_declarative_part(ctx)


  override def visitPackage_declaration(ctx: Package_declarationContext): Unit = super.visitPackage_declaration(ctx)

  override def visitPackage_declarative_item(ctx: Package_declarative_itemContext): Unit = super.visitPackage_declarative_item(ctx)

  override def visitPackage_declarative_part(ctx: Package_declarative_partContext): Unit = super.visitPackage_declarative_part(ctx)

  override def visitParameter_specification(ctx: Parameter_specificationContext): Unit = super.visitParameter_specification(ctx)

  override def visitPhysical_literal(ctx: Physical_literalContext): Unit = super.visitPhysical_literal(ctx)

  override def visitPhysical_type_definition(ctx: Physical_type_definitionContext): Unit = super.visitPhysical_type_definition(ctx)

  override def visitPort_clause(ctx: Port_clauseContext): Unit = super.visitPort_clause(ctx)

  override def visitPort_list(ctx: Port_listContext): Unit = {
    for {
      port_declaration <- ctx.interface_port_list().interface_port_declaration()
      interfacePortDecl = VInterfacePortDecl(port_declaration)
      id <- interfacePortDecl.idList
    } {
      val mode = interfacePortDecl.mode
      val sti = interfacePortDecl.subtypeInd
      val expOption = interfacePortDecl.vExp
      genIPort(id, expOption, sti, mode, "connected")
    }
    super.visitPort_list(ctx)
  }

  override def visitPort_map_aspect(ctx: Port_map_aspectContext): Unit = super.visitPort_map_aspect(ctx)

  override def visitPrimary(ctx: PrimaryContext): Unit = super.visitPrimary(ctx)

  override def visitPrimary_unit(ctx: Primary_unitContext): Unit = super.visitPrimary_unit(ctx)

  override def visitProcedural_declarative_item(ctx: Procedural_declarative_itemContext): Unit = super.visitProcedural_declarative_item(ctx)

  override def visitProcedural_declarative_part(ctx: Procedural_declarative_partContext): Unit = super.visitProcedural_declarative_part(ctx)

  override def visitProcedural_statement_part(ctx: Procedural_statement_partContext): Unit = super.visitProcedural_statement_part(ctx)

  override def visitProcedure_call(ctx: Procedure_callContext): Unit = super.visitProcedure_call(ctx)

  override def visitProcedure_call_statement(ctx: Procedure_call_statementContext): Unit = super.visitProcedure_call_statement(ctx)

  override def visitProcess_declarative_item(ctx: Process_declarative_itemContext): Unit = super.visitProcess_declarative_item(ctx)

  override def visitProcess_declarative_part(ctx: Process_declarative_partContext): Unit = super.visitProcess_declarative_part(ctx)

  override def visitProcess_statement(ctx: Process_statementContext): Unit = {

    super.visitProcess_statement(ctx)
  }

  override def visitProcess_statement_part(ctx: Process_statement_partContext): Unit = super.visitProcess_statement_part(ctx)

  override def visitQualified_expression(ctx: Qualified_expressionContext): Unit = super.visitQualified_expression(ctx)

  override def visitQuantity_declaration(ctx: Quantity_declarationContext): Unit = super.visitQuantity_declaration(ctx)

  override def visitQuantity_list(ctx: Quantity_listContext): Unit = super.visitQuantity_list(ctx)

  override def visitQuantity_specification(ctx: Quantity_specificationContext): Unit = super.visitQuantity_specification(ctx)

  override def visitRange(ctx: RangeContext): Unit = super.visitRange(ctx)

  override def visitExplicit_range(ctx: Explicit_rangeContext): Unit = super.visitExplicit_range(ctx)

  override def visitRange_constraint(ctx: Range_constraintContext): Unit = super.visitRange_constraint(ctx)

  override def visitRecord_nature_definition(ctx: Record_nature_definitionContext): Unit = super.visitRecord_nature_definition(ctx)

  //  DEFINITION
  override def visitRecord_type_definition(ctx: Record_type_definitionContext): Unit = {
    val recordTypeDef = VRecordTypeDef(ctx)
    val items = for {
      elementDecl <- recordTypeDef.elementDecls
      flattened <- elementDecl.flatten
    } yield flattened
    val typeDeclId = ctx.getParent.getParent.getParent.asInstanceOf[Type_declarationContext].identifier().getText
    typeInfo +=(typeDeclId, items)
    super.visitRecord_type_definition(ctx)
  }

  override def visitRelation(ctx: RelationContext): Unit = super.visitRelation(ctx)

  override def visitRelational_operator(ctx: Relational_operatorContext): Unit = super.visitRelational_operator(ctx)

  override def visitReport_statement(ctx: Report_statementContext): Unit = super.visitReport_statement(ctx)

  override def visitReturn_statement(ctx: Return_statementContext): Unit = super.visitReturn_statement(ctx)

  override def visitScalar_nature_definition(ctx: Scalar_nature_definitionContext): Unit = super.visitScalar_nature_definition(ctx)

  override def visitScalar_type_definition(ctx: Scalar_type_definitionContext): Unit = super.visitScalar_type_definition(ctx)

  override def visitSecondary_unit(ctx: Secondary_unitContext): Unit = super.visitSecondary_unit(ctx)

  override def visitSecondary_unit_declaration(ctx: Secondary_unit_declarationContext): Unit = super.visitSecondary_unit_declaration(ctx)

  override def visitSelected_signal_assignment(ctx: Selected_signal_assignmentContext): Unit = super.visitSelected_signal_assignment(ctx)

  override def visitSelected_waveforms(ctx: Selected_waveformsContext): Unit = super.visitSelected_waveforms(ctx)

  override def visitSensitivity_clause(ctx: Sensitivity_clauseContext): Unit = super.visitSensitivity_clause(ctx)

  override def visitSensitivity_list(ctx: Sensitivity_listContext): Unit = super.visitSensitivity_list(ctx)

  override def visitSequence_of_statements(ctx: Sequence_of_statementsContext): Unit = super.visitSequence_of_statements(ctx)

  override def visitSequential_statement(ctx: Sequential_statementContext): Unit = super.visitSequential_statement(ctx)

  override def visitShift_expression(ctx: Shift_expressionContext): Unit = super.visitShift_expression(ctx)

  override def visitShift_operator(ctx: Shift_operatorContext): Unit = super.visitShift_operator(ctx)

  override def visitSignal_assignment_statement(ctx: Signal_assignment_statementContext): Unit = super.visitSignal_assignment_statement(ctx)

  override def visitSignal_declaration(ctx: Signal_declarationContext): Unit = {
    val signalDecl = VSignalDecl(ctx)
    for {
      id <- signalDecl.idList
    } {
      val signalKind = signalDecl.signalKind.getOrElse("register").toLowerCase
      val sti = signalDecl.subtypeInd
      genISignal(id, sti, signalKind)
    }
    super.visitSignal_declaration(ctx)
  }

  override def visitSignal_kind(ctx: Signal_kindContext): Unit = super.visitSignal_kind(ctx)

  override def visitSignal_list(ctx: Signal_listContext): Unit = super.visitSignal_list(ctx)

  override def visitSignature(ctx: SignatureContext): Unit = super.visitSignature(ctx)

  override def visitSimple_expression(ctx: Simple_expressionContext): Unit = super.visitSimple_expression(ctx)

  override def visitSimple_simultaneous_statement(ctx: Simple_simultaneous_statementContext): Unit = super.visitSimple_simultaneous_statement(ctx)

  override def visitSimultaneous_alternative(ctx: Simultaneous_alternativeContext): Unit = super.visitSimultaneous_alternative(ctx)

  override def visitSimultaneous_case_statement(ctx: Simultaneous_case_statementContext): Unit = super.visitSimultaneous_case_statement(ctx)

  override def visitSimultaneous_if_statement(ctx: Simultaneous_if_statementContext): Unit = super.visitSimultaneous_if_statement(ctx)

  override def visitSimultaneous_procedural_statement(ctx: Simultaneous_procedural_statementContext): Unit = super.visitSimultaneous_procedural_statement(ctx)

  override def visitSimultaneous_statement(ctx: Simultaneous_statementContext): Unit = super.visitSimultaneous_statement(ctx)

  override def visitSimultaneous_statement_part(ctx: Simultaneous_statement_partContext): Unit = super.visitSimultaneous_statement_part(ctx)

  override def visitSource_aspect(ctx: Source_aspectContext): Unit = super.visitSource_aspect(ctx)

  override def visitSource_quantity_declaration(ctx: Source_quantity_declarationContext): Unit = super.visitSource_quantity_declaration(ctx)

  override def visitStep_limit_specification(ctx: Step_limit_specificationContext): Unit = super.visitStep_limit_specification(ctx)

  override def visitSubnature_declaration(ctx: Subnature_declarationContext): Unit = super.visitSubnature_declaration(ctx)

  override def visitSubnature_indication(ctx: Subnature_indicationContext): Unit = super.visitSubnature_indication(ctx)

  override def visitSubprogram_body(ctx: Subprogram_bodyContext): Unit = super.visitSubprogram_body(ctx)

  override def visitSubprogram_declaration(ctx: Subprogram_declarationContext): Unit = super.visitSubprogram_declaration(ctx)

  override def visitSubprogram_declarative_item(ctx: Subprogram_declarative_itemContext): Unit = super.visitSubprogram_declarative_item(ctx)

  override def visitSubprogram_declarative_part(ctx: Subprogram_declarative_partContext): Unit = super.visitSubprogram_declarative_part(ctx)

  override def visitSubprogram_kind(ctx: Subprogram_kindContext): Unit = super.visitSubprogram_kind(ctx)

  override def visitSubprogram_specification(ctx: Subprogram_specificationContext): Unit = super.visitSubprogram_specification(ctx)

  override def visitProcedure_specification(ctx: Procedure_specificationContext): Unit = super.visitProcedure_specification(ctx)

  override def visitFunction_specification(ctx: Function_specificationContext): Unit = super.visitFunction_specification(ctx)

  override def visitSubprogram_statement_part(ctx: Subprogram_statement_partContext): Unit = super.visitSubprogram_statement_part(ctx)

  override def visitSubtype_declaration(ctx: Subtype_declarationContext): Unit = super.visitSubtype_declaration(ctx)

  override def visitSubtype_indication(ctx: Subtype_indicationContext): Unit = super.visitSubtype_indication(ctx)

  override def visitSuffix(ctx: SuffixContext): Unit = super.visitSuffix(ctx)

  override def visitTarget(ctx: TargetContext): Unit = super.visitTarget(ctx)

  override def visitTerm(ctx: TermContext): Unit = super.visitTerm(ctx)

  override def visitTerminal_aspect(ctx: Terminal_aspectContext): Unit = super.visitTerminal_aspect(ctx)

  override def visitTerminal_declaration(ctx: Terminal_declarationContext): Unit = super.visitTerminal_declaration(ctx)

  override def visitThrough_aspect(ctx: Through_aspectContext): Unit = super.visitThrough_aspect(ctx)

  override def visitTimeout_clause(ctx: Timeout_clauseContext): Unit = super.visitTimeout_clause(ctx)

  override def visitTolerance_aspect(ctx: Tolerance_aspectContext): Unit = super.visitTolerance_aspect(ctx)

  override def visitType_declaration(ctx: Type_declarationContext): Unit = super.visitType_declaration(ctx)

  override def visitType_definition(ctx: Type_definitionContext): Unit = super.visitType_definition(ctx)

  override def visitUnconstrained_array_definition(ctx: Unconstrained_array_definitionContext): Unit = super.visitUnconstrained_array_definition(ctx)

  override def visitUnconstrained_nature_definition(ctx: Unconstrained_nature_definitionContext): Unit = super.visitUnconstrained_nature_definition(ctx)

  override def visitUse_clause(ctx: Use_clauseContext): Unit = super.visitUse_clause(ctx)

  override def visitVariable_assignment_statement(ctx: Variable_assignment_statementContext): Unit = super.visitVariable_assignment_statement(ctx)

  override def visitVariable_declaration(ctx: Variable_declarationContext): Unit = {
    val variabledecl = VVarDecl(ctx)
    for {
      id <- variabledecl.idList
    } yield {
      genIVariable(id, variabledecl.vExp, variabledecl.subtypeInd)
    }
    super.visitVariable_declaration(ctx)
  }

  override def visitWait_statement(ctx: Wait_statementContext): Unit = super.visitWait_statement(ctx)

  override def visitWaveform(ctx: WaveformContext): Unit = super.visitWaveform(ctx)

  override def visitWaveform_element(ctx: Waveform_elementContext): Unit = super.visitWaveform_element(ctx)
}
