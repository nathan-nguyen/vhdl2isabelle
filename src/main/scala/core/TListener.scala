package core

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, TerminalNode}
import sg.edu.ntu.hchen.VHDLListener
import sg.edu.ntu.hchen.VHDLParser._

import scala.collection.JavaConversions._

class TListener(vInfo: Option[VInfo]) extends Keeper(vInfo) with VHDLListener {
  override def enterAbstract_literal(ctx: Abstract_literalContext): Unit = {}

  override def enterEnumeration_literal(ctx: Enumeration_literalContext): Unit = {}

  override def exitLiteral(ctx: LiteralContext): Unit = {}

  override def exitProcedural_statement_part(ctx: Procedural_statement_partContext): Unit = {}

  override def enterActual_designator(ctx: Actual_designatorContext): Unit = {}

  override def enterVariable_assignment_statement(ctx: Variable_assignment_statementContext): Unit = {}

  override def enterConstraint(ctx: ConstraintContext): Unit = {}

  override def exitRecord_nature_definition(ctx: Record_nature_definitionContext): Unit = {}

  override def enterInterface_file_declaration(ctx: Interface_file_declarationContext): Unit = {}

  override def enterInterface_port_declaration(ctx: Interface_port_declarationContext): Unit = {}

  override def exitComponent_instantiation_statement(ctx: Component_instantiation_statementContext): Unit = {}

  override def exitSensitivity_list(ctx: Sensitivity_listContext): Unit = {}

  override def enterVariable_declaration(ctx: Variable_declarationContext): Unit = {
    val variabledecl = VVarDecl(ctx)
    for {
      id <- variabledecl.idList
    } yield {
      genIVariable(id, variabledecl.vExp, variabledecl.subtypeInd)
    }
  }

  override def exitGenerate_statement(ctx: Generate_statementContext): Unit = {
    val genStat = VGenStat(ctx)
    conc_stmt_complexes += genStat.toI(defInfo)
    //    logger.info(s"${genStat.toI(defInfo)}")
  }

  override def exitEntity_declarative_item(ctx: Entity_declarative_itemContext): Unit = {}

  override def exitPhysical_type_definition(ctx: Physical_type_definitionContext): Unit = {}

  override def exitRange_constraint(ctx: Range_constraintContext): Unit = {}

  override def exitInterface_terminal_declaration(ctx: Interface_terminal_declarationContext): Unit = {}

  override def exitTarget(ctx: TargetContext): Unit = {}

  override def exitExpression(ctx: ExpressionContext): Unit = {}

  override def enterQuantity_declaration(ctx: Quantity_declarationContext): Unit = {}

  override def enterInterface_declaration(ctx: Interface_declarationContext): Unit = {}

  override def exitRelation(ctx: RelationContext): Unit = {}

  override def enterConstrained_array_definition(ctx: Constrained_array_definitionContext): Unit = {}

  override def enterPackage_body(ctx: Package_bodyContext): Unit = {}

  override def enterSignal_kind(ctx: Signal_kindContext): Unit = {}

  override def exitUse_clause(ctx: Use_clauseContext): Unit = {}

  override def exitInstantiation_list(ctx: Instantiation_listContext): Unit = {}

  override def enterInstantiated_unit(ctx: Instantiated_unitContext): Unit = {}

  override def enterActual_parameter_part(ctx: Actual_parameter_partContext): Unit = {}

  override def enterProcess_declarative_part(ctx: Process_declarative_partContext): Unit = {}

  override def exitSubprogram_statement_part(ctx: Subprogram_statement_partContext): Unit = {}

  override def exitActual_designator(ctx: Actual_designatorContext): Unit = {}

  override def exitConfiguration_declarative_part(ctx: Configuration_declarative_partContext): Unit = {}

  override def enterSimultaneous_if_statement(ctx: Simultaneous_if_statementContext): Unit = {}

  override def enterNumeric_literal(ctx: Numeric_literalContext): Unit = {}

  override def enterChoices(ctx: ChoicesContext): Unit = {}

  override def enterDirection(ctx: DirectionContext): Unit = {}

  //  DEFINITION
  override def enterRecord_type_definition(ctx: Record_type_definitionContext): Unit = {
    val recordTypeDef = VRecordTypeDef(ctx)
    val items = for {
      elementDecl <- recordTypeDef.elementDecls
      flattened <- elementDecl.flatten
    } yield flattened
    val typeDeclId = ctx.getParent.getParent.getParent.asInstanceOf[Type_declarationContext].identifier().getText
    typeInfo +=(VCustomizedType(typeDeclId), items)
  }

  override def exitTerm(ctx: TermContext): Unit = {}

  override def enterArray_type_definition(ctx: Array_type_definitionContext): Unit = {}

  override def exitSelected_waveforms(ctx: Selected_waveformsContext): Unit = {}

  override def exitConstant_declaration(ctx: Constant_declarationContext): Unit = {
    val constDecl = VConstDecl(ctx)
    for {
      id <- constDecl.idList
    } {
      genIVariable(id, constDecl.vExp, constDecl.subtypeInd)
    }
  }

  override def exitAssertion_statement(ctx: Assertion_statementContext): Unit = {}

  override def enterWaveform_element(ctx: Waveform_elementContext): Unit = {}

  override def enterReturn_statement(ctx: Return_statementContext): Unit = {}

  override def enterIdentifier_list(ctx: Identifier_listContext): Unit = {}

  override def enterExplicit_range(ctx: Explicit_rangeContext): Unit = {}

  override def exitElement_subnature_definition(ctx: Element_subnature_definitionContext): Unit = {}

  override def enterSimultaneous_case_statement(ctx: Simultaneous_case_statementContext): Unit = {}

  override def enterAcross_aspect(ctx: Across_aspectContext): Unit = {}

  override def exitComponent_declaration(ctx: Component_declarationContext): Unit = {}

  override def enterInterface_constant_declaration(ctx: Interface_constant_declarationContext): Unit = {}

  override def exitGeneric_map_aspect(ctx: Generic_map_aspectContext): Unit = {}

  override def enterBlock_specification(ctx: Block_specificationContext): Unit = {}

  override def exitExplicit_range(ctx: Explicit_rangeContext): Unit = {}

  override def exitConstrained_nature_definition(ctx: Constrained_nature_definitionContext): Unit = {}

  override def enterSignal_declaration(ctx: Signal_declarationContext): Unit = {
    val signalDecl = VSignalDecl(ctx)
    for {
      id <- signalDecl.idList
    } {
      val signalKind = signalDecl.signalKind.map(SignalKind.withName).getOrElse(SignalKind.register)
      val sti = signalDecl.subtypeInd
      genISignal(id, sti, signalKind)
    }
  }

  override def enterDesign_unit(ctx: Design_unitContext): Unit = {}

  override def exitAcross_aspect(ctx: Across_aspectContext): Unit = {}

  override def exitSource_aspect(ctx: Source_aspectContext): Unit = {}

  override def exitExit_statement(ctx: Exit_statementContext): Unit = {}

  override def enterCase_statement_alternative(ctx: Case_statement_alternativeContext): Unit = {}

  override def enterSensitivity_clause(ctx: Sensitivity_clauseContext): Unit = {}

  override def enterSource_aspect(ctx: Source_aspectContext): Unit = {}

  override def exitPackage_declarative_part(ctx: Package_declarative_partContext): Unit = {}

  override def exitConfiguration_specification(ctx: Configuration_specificationContext): Unit = {}

  override def exitConditional_waveforms(ctx: Conditional_waveformsContext): Unit = {}

  override def enterSubtype_indication(ctx: Subtype_indicationContext): Unit = {}

  override def exitBranch_quantity_declaration(ctx: Branch_quantity_declarationContext): Unit = {}

  override def enterConfiguration_declarative_item(ctx: Configuration_declarative_itemContext): Unit = {}

  override def exitScalar_nature_definition(ctx: Scalar_nature_definitionContext): Unit = {}

  override def enterIndex_constraint(ctx: Index_constraintContext): Unit = {}

  override def exitEntity_declaration(ctx: Entity_declarationContext): Unit = {
    definedEntities += ctx.identifier().head.getText
  }

  override def exitAlias_designator(ctx: Alias_designatorContext): Unit = {}

  override def exitIndex_specification(ctx: Index_specificationContext): Unit = {}

  override def exitRecord_type_definition(ctx: Record_type_definitionContext): Unit = {}

  override def exitNature_declaration(ctx: Nature_declarationContext): Unit = {}

  override def exitSignal_kind(ctx: Signal_kindContext): Unit = {}

  override def enterEntity_header(ctx: Entity_headerContext): Unit = {}

  override def exitOpts(ctx: OptsContext): Unit = {}

  override def enterBranch_quantity_declaration(ctx: Branch_quantity_declarationContext): Unit = {}

  override def enterElement_subtype_definition(ctx: Element_subtype_definitionContext): Unit = {}

  override def exitDesignator(ctx: DesignatorContext): Unit = {}

  override def enterConfiguration_declaration(ctx: Configuration_declarationContext): Unit = {}

  override def enterDelay_mechanism(ctx: Delay_mechanismContext): Unit = {}

  override def exitIteration_scheme(ctx: Iteration_schemeContext): Unit = {}

  override def enterAggregate(ctx: AggregateContext): Unit = {}

  override def enterLogical_name_list(ctx: Logical_name_listContext): Unit = {}

  override def exitInterface_signal_list(ctx: Interface_signal_listContext): Unit = {}

  override def exitInterface_list(ctx: Interface_listContext): Unit = {}

  override def enterChoice(ctx: ChoiceContext): Unit = {}

  override def enterConditional_waveforms(ctx: Conditional_waveformsContext): Unit = {}

  override def exitDesign_unit(ctx: Design_unitContext): Unit = {}

  override def exitName_function_call_or_indexed_part(ctx: Name_function_call_or_indexed_partContext): Unit = {}

  override def enterLogical_operator(ctx: Logical_operatorContext): Unit = {}

  override def enterAttribute_specification(ctx: Attribute_specificationContext): Unit = {}

  override def enterStep_limit_specification(ctx: Step_limit_specificationContext): Unit = {}

  override def exitBreak_list(ctx: Break_listContext): Unit = {}

  override def enterElement_subnature_definition(ctx: Element_subnature_definitionContext): Unit = {}

  override def exitAttribute_designator(ctx: Attribute_designatorContext): Unit = {}

  override def exitVariable_assignment_statement(ctx: Variable_assignment_statementContext): Unit = {}

  override def enterElement_declaration(ctx: Element_declarationContext): Unit = {}

  override def exitContext_clause(ctx: Context_clauseContext): Unit = {}

  override def exitIndex_constraint(ctx: Index_constraintContext): Unit = {}

  override def exitWaveform_element(ctx: Waveform_elementContext): Unit = {}

  override def exitReturn_statement(ctx: Return_statementContext): Unit = {}

  override def exitWaveform(ctx: WaveformContext): Unit = {}

  override def exitCondition(ctx: ConditionContext): Unit = {}

  override def enterEntity_statement_part(ctx: Entity_statement_partContext): Unit = {}

  override def enterPackage_declaration(ctx: Package_declarationContext): Unit = {}

  override def enterBreak_statement(ctx: Break_statementContext): Unit = {}

  override def enterSubprogram_kind(ctx: Subprogram_kindContext): Unit = {}

  override def exitPort_list(ctx: Port_listContext): Unit = {}

  override def exitSelected_name(ctx: Selected_nameContext): Unit = {}

  override def exitBlock_statement_part(ctx: Block_statement_partContext): Unit = {}

  override def enterName_slice_part(ctx: Name_slice_partContext): Unit = {}

  override def exitLogical_operator(ctx: Logical_operatorContext): Unit = {}

  override def exitContext_item(ctx: Context_itemContext): Unit = {}

  override def exitElement_association(ctx: Element_associationContext): Unit = {}

  override def exitConditional_signal_assignment(ctx: Conditional_signal_assignmentContext): Unit = {
    //    NOTE: parent is concurrent_signal_assignment
  }

  override def exitLogical_name(ctx: Logical_nameContext): Unit = {}

  override def enterInterface_signal_list(ctx: Interface_signal_listContext): Unit = {}

  override def enterBlock_statement(ctx: Block_statementContext): Unit = {}

  override def exitEntity_specification(ctx: Entity_specificationContext): Unit = {}

  override def exitProcedure_specification(ctx: Procedure_specificationContext): Unit = {}

  override def enterSignal_assignment_statement(ctx: Signal_assignment_statementContext): Unit = {}

  override def enterNature_element_declaration(ctx: Nature_element_declarationContext): Unit = {}

  override def enterDesign_file(ctx: Design_fileContext): Unit = {}

  override def enterEntity_class_entry_list(ctx: Entity_class_entry_listContext): Unit = {}

  override def enterComposite_nature_definition(ctx: Composite_nature_definitionContext): Unit = {}

  override def exitSequential_statement(ctx: Sequential_statementContext): Unit = {}

  override def enterBreak_selector_clause(ctx: Break_selector_clauseContext): Unit = {}

  override def exitInterface_constant_declaration(ctx: Interface_constant_declarationContext): Unit = {
    val interfaceConstDecl = VInterfaceConstDecl(ctx)
    for (id <- interfaceConstDecl.idList) {
      genIVariable(id, interfaceConstDecl.vExp, interfaceConstDecl.subtypeInd)
    }
  }

  override def exitAttribute_declaration(ctx: Attribute_declarationContext): Unit = {}

  override def enterSubnature_indication(ctx: Subnature_indicationContext): Unit = {}

  override def exitInterface_port_declaration(ctx: Interface_port_declarationContext): Unit = {}

  override def enterShift_operator(ctx: Shift_operatorContext): Unit = {}

  override def enterPort_map_aspect(ctx: Port_map_aspectContext): Unit = {}

  override def enterFunction_specification(ctx: Function_specificationContext): Unit = {}

  override def enterProcess_declarative_item(ctx: Process_declarative_itemContext): Unit = {}

  override def exitGeneration_scheme(ctx: Generation_schemeContext): Unit = {}

  override def enterGeneration_scheme(ctx: Generation_schemeContext): Unit = {}

  override def exitConfiguration_declarative_item(ctx: Configuration_declarative_itemContext): Unit = {}

  override def enterSubtype_declaration(ctx: Subtype_declarationContext): Unit = {}

  override def exitEntity_header(ctx: Entity_headerContext): Unit = {}

  override def exitAssociation_element(ctx: Association_elementContext): Unit = {}

  override def exitRelational_operator(ctx: Relational_operatorContext): Unit = {}

  override def exitElement_subtype_definition(ctx: Element_subtype_definitionContext): Unit = {}

  override def exitAllocator(ctx: AllocatorContext): Unit = {}

  override def enterConcurrent_assertion_statement(ctx: Concurrent_assertion_statementContext): Unit = {}

  override def enterAssertion(ctx: AssertionContext): Unit = {}

  override def enterName_part(ctx: Name_partContext): Unit = {}

  override def enterAlias_declaration(ctx: Alias_declarationContext): Unit = {}

  override def exitPackage_declarative_item(ctx: Package_declarative_itemContext): Unit = {}

  override def enterGeneric_clause(ctx: Generic_clauseContext): Unit = {}

  override def exitFunction_specification(ctx: Function_specificationContext): Unit = {}

  override def exitSimultaneous_case_statement(ctx: Simultaneous_case_statementContext): Unit = {}

  override def enterDiscrete_range(ctx: Discrete_rangeContext): Unit = {}

  override def exitFile_open_information(ctx: File_open_informationContext): Unit = {}

  override def exitSimultaneous_procedural_statement(ctx: Simultaneous_procedural_statementContext): Unit = {}

  override def exitGeneric_clause(ctx: Generic_clauseContext): Unit = {}

  override def exitIndex_subtype_definition(ctx: Index_subtype_definitionContext): Unit = {}

  override def exitBlock_configuration(ctx: Block_configurationContext): Unit = {}

  override def exitSubprogram_specification(ctx: Subprogram_specificationContext): Unit = {}

  override def enterComposite_type_definition(ctx: Composite_type_definitionContext): Unit = {}

  override def exitAlias_declaration(ctx: Alias_declarationContext): Unit = {}

  override def exitDiscrete_range(ctx: Discrete_rangeContext): Unit = {}

  override def enterSequence_of_statements(ctx: Sequence_of_statementsContext): Unit = {}

  override def enterSignal_mode(ctx: Signal_modeContext): Unit = {}

  override def exitGeneric_list(ctx: Generic_listContext): Unit = {}

  override def enterConditional_signal_assignment(ctx: Conditional_signal_assignmentContext): Unit = {}

  override def exitChoice(ctx: ChoiceContext): Unit = {}

  override def enterAttribute_declaration(ctx: Attribute_declarationContext): Unit = {}

  override def exitFormal_parameter_list(ctx: Formal_parameter_listContext): Unit = {}

  override def enterWait_statement(ctx: Wait_statementContext): Unit = {}

  override def exitCase_statement_alternative(ctx: Case_statement_alternativeContext): Unit = {}

  override def enterPhysical_literal(ctx: Physical_literalContext): Unit = {}

  override def enterBlock_declarative_part(ctx: Block_declarative_partContext): Unit = {}

  override def enterSignal_list(ctx: Signal_listContext): Unit = {}

  override def enterName(ctx: NameContext): Unit = {}

  override def exitBlock_header(ctx: Block_headerContext): Unit = {}

  override def enterFormal_part(ctx: Formal_partContext): Unit = {}

  override def enterPrimary_unit(ctx: Primary_unitContext): Unit = {}

  override def enterComponent_declaration(ctx: Component_declarationContext): Unit = {}

  override def enterEntity_name_list(ctx: Entity_name_listContext): Unit = {}

  override def enterName_attribute_part(ctx: Name_attribute_partContext): Unit = {}

  override def enterSecondary_unit_declaration(ctx: Secondary_unit_declarationContext): Unit = {}

  override def exitSequence_of_statements(ctx: Sequence_of_statementsContext): Unit = {}

  override def enterExpression(ctx: ExpressionContext): Unit = {}

  override def enterGroup_constituent_list(ctx: Group_constituent_listContext): Unit = {}

  override def exitIf_statement(ctx: If_statementContext): Unit = {}

  override def enterSubprogram_declarative_part(ctx: Subprogram_declarative_partContext): Unit = {}

  override def exitEntity_class(ctx: Entity_classContext): Unit = {}

  override def enterPort_clause(ctx: Port_clauseContext): Unit = {}

  override def exitNumeric_literal(ctx: Numeric_literalContext): Unit = {}

  override def enterSubprogram_body(ctx: Subprogram_bodyContext): Unit = {}

  override def enterUse_clause(ctx: Use_clauseContext): Unit = {}

  override def exitIdentifier(ctx: IdentifierContext): Unit = {}

  override def enterConcurrent_break_statement(ctx: Concurrent_break_statementContext): Unit = {}

  override def enterInstantiation_list(ctx: Instantiation_listContext): Unit = {}

  override def enterRelation(ctx: RelationContext): Unit = {}

  override def enterArchitecture_statement(ctx: Architecture_statementContext): Unit = {}

  override def exitSubprogram_declarative_part(ctx: Subprogram_declarative_partContext): Unit = {}

  override def enterSelected_waveforms(ctx: Selected_waveformsContext): Unit = {}

  override def enterPrimary(ctx: PrimaryContext): Unit = {}

  override def enterGenerate_statement(ctx: Generate_statementContext): Unit = {}

  override def enterProcedural_declarative_part(ctx: Procedural_declarative_partContext): Unit = {}

  override def exitNature_element_declaration(ctx: Nature_element_declarationContext): Unit = {}

  override def exitBlock_declarative_part(ctx: Block_declarative_partContext): Unit = {}

  override def exitConcurrent_assertion_statement(ctx: Concurrent_assertion_statementContext): Unit = {}

  override def enterDesignator(ctx: DesignatorContext): Unit = {}

  override def exitInterface_file_declaration(ctx: Interface_file_declarationContext): Unit = {}

  override def exitEntity_name_list(ctx: Entity_name_listContext): Unit = {}

  override def exitComposite_nature_definition(ctx: Composite_nature_definitionContext): Unit = {}

  override def enterName_function_call_or_indexed_part(ctx: Name_function_call_or_indexed_partContext): Unit = {}

  override def exitAttribute_specification(ctx: Attribute_specificationContext): Unit = {}

  override def exitGroup_constituent_list(ctx: Group_constituent_listContext): Unit = {}

  override def enterGroup_template_declaration(ctx: Group_template_declarationContext): Unit = {}

  override def enterType_definition(ctx: Type_definitionContext): Unit = {}

  override def enterInterface_port_list(ctx: Interface_port_listContext): Unit = {}

  override def exitSignal_list(ctx: Signal_listContext): Unit = {}

  override def exitProcess_declarative_part(ctx: Process_declarative_partContext): Unit = {}

  override def exitPackage_body_declarative_part(ctx: Package_body_declarative_partContext): Unit = {}

  override def enterGeneric_map_aspect(ctx: Generic_map_aspectContext): Unit = {}

  override def exitConfiguration_declaration(ctx: Configuration_declarationContext): Unit = {}

  override def enterGroup_constituent(ctx: Group_constituentContext): Unit = {}

  override def exitShift_operator(ctx: Shift_operatorContext): Unit = {}

  override def exitSignal_assignment_statement(ctx: Signal_assignment_statementContext): Unit = {}

  override def exitPackage_body(ctx: Package_bodyContext): Unit = {}

  override def enterArray_nature_definition(ctx: Array_nature_definitionContext): Unit = {}

  override def enterTerminal_aspect(ctx: Terminal_aspectContext): Unit = {}

  override def enterNature_declaration(ctx: Nature_declarationContext): Unit = {}

  override def exitArchitecture_statement(ctx: Architecture_statementContext): Unit = {}

  override def enterComponent_configuration(ctx: Component_configurationContext): Unit = {}

  override def exitIdentifier_list(ctx: Identifier_listContext): Unit = {}

  override def enterEntity_declaration(ctx: Entity_declarationContext): Unit = {}

  override def exitGroup_constituent(ctx: Group_constituentContext): Unit = {}

  override def enterPort_list(ctx: Port_listContext): Unit = {
    for {
      port_declaration <- ctx.interface_port_list().interface_port_declaration()
      interfacePortDecl = VInterfacePortDecl(port_declaration)
      id <- interfacePortDecl.idList
    } {
      val mode = PortMode.withName(s"mode_${interfacePortDecl.mode}")
      val sti = interfacePortDecl.subtypeInd
      val expOption = interfacePortDecl.vExp
      genIPort(id, expOption, sti, mode, PortConn.connected)
    }
  }

  override def enterCondition(ctx: ConditionContext): Unit = {}

  override def exitSimultaneous_if_statement(ctx: Simultaneous_if_statementContext): Unit = {}

  override def exitEntity_class_entry_list(ctx: Entity_class_entry_listContext): Unit = {}

  override def exitVariable_declaration(ctx: Variable_declarationContext): Unit = {}

  override def exitChoices(ctx: ChoicesContext): Unit = {}

  override def exitDelay_mechanism(ctx: Delay_mechanismContext): Unit = {}

  override def exitWait_statement(ctx: Wait_statementContext): Unit = {}

  override def exitQuantity_declaration(ctx: Quantity_declarationContext): Unit = {}

  override def enterParameter_specification(ctx: Parameter_specificationContext): Unit = {}

  override def exitSignature(ctx: SignatureContext): Unit = {}

  override def enterInterface_variable_declaration(ctx: Interface_variable_declarationContext): Unit = {}

  override def exitDesign_file(ctx: Design_fileContext): Unit = {
    if (vInfo.isDefined) {
      val iEnv = IEnv(defInfo)
      entity = IEntity(definedEntities.head, iEnv, IResFn(), conc_stmt_complexes.toList)
    } else {
//      logger.info(s"${typeInfo.typeDeclTbl.mkString("[", ",\n\n", "]")}")
    }
  }

  override def exitBreak_statement(ctx: Break_statementContext): Unit = {}

  override def enterTerminal_declaration(ctx: Terminal_declarationContext): Unit = {}

  override def exitSubprogram_kind(ctx: Subprogram_kindContext): Unit = {}

  override def enterPackage_body_declarative_part(ctx: Package_body_declarative_partContext): Unit = {}

  override def enterTerm(ctx: TermContext): Unit = {}

  override def enterMultiplying_operator(ctx: Multiplying_operatorContext): Unit = {}

  override def exitSubnature_declaration(ctx: Subnature_declarationContext): Unit = {}

  override def enterFile_logical_name(ctx: File_logical_nameContext): Unit = {}

  override def enterNature_definition(ctx: Nature_definitionContext): Unit = {}

  override def exitSecondary_unit_declaration(ctx: Secondary_unit_declarationContext): Unit = {}

  override def exitEntity_tag(ctx: Entity_tagContext): Unit = {}

  override def enterSubprogram_declarative_item(ctx: Subprogram_declarative_itemContext): Unit = {}

  override def enterProcedure_call_statement(ctx: Procedure_call_statementContext): Unit = {}

  override def enterLabel_colon(ctx: Label_colonContext): Unit = {}

  override def exitName_slice_part(ctx: Name_slice_partContext): Unit = {}

  override def exitSignal_mode(ctx: Signal_modeContext): Unit = {}

  override def exitPort_map_aspect(ctx: Port_map_aspectContext): Unit = {}

  override def exitNature_definition(ctx: Nature_definitionContext): Unit = {}

  override def enterSubnature_declaration(ctx: Subnature_declarationContext): Unit = {}

  override def enterFormal_parameter_list(ctx: Formal_parameter_listContext): Unit = {}

  override def exitConcurrent_break_statement(ctx: Concurrent_break_statementContext): Unit = {}

  override def exitBlock_specification(ctx: Block_specificationContext): Unit = {}

  override def exitProcedural_declarative_part(ctx: Procedural_declarative_partContext): Unit = {}

  override def exitStep_limit_specification(ctx: Step_limit_specificationContext): Unit = {}

  override def enterSelected_signal_assignment(ctx: Selected_signal_assignmentContext): Unit = {}

  override def exitBase_unit_declaration(ctx: Base_unit_declarationContext): Unit = {}

  override def enterFile_open_information(ctx: File_open_informationContext): Unit = {}

  override def enterBlock_declarative_item(ctx: Block_declarative_itemContext): Unit = {}

  override def exitComposite_type_definition(ctx: Composite_type_definitionContext): Unit = {}

  override def enterQualified_expression(ctx: Qualified_expressionContext): Unit = {}

  override def exitConcurrent_procedure_call_statement(ctx: Concurrent_procedure_call_statementContext): Unit = {}

  override def exitSubprogram_declarative_item(ctx: Subprogram_declarative_itemContext): Unit = {}

  override def enterBreak_element(ctx: Break_elementContext): Unit = {}

  override def exitFile_logical_name(ctx: File_logical_nameContext): Unit = {}

  override def enterType_declaration(ctx: Type_declarationContext): Unit = {}

  override def enterProcedural_declarative_item(ctx: Procedural_declarative_itemContext): Unit = {}

  override def enterFactor(ctx: FactorContext): Unit = {}

  override def exitBlock_statement(ctx: Block_statementContext): Unit = {}

  override def exitPhysical_literal(ctx: Physical_literalContext): Unit = {}

  override def exitConfiguration_item(ctx: Configuration_itemContext): Unit = {}

  override def enterFree_quantity_declaration(ctx: Free_quantity_declarationContext): Unit = {}

  override def enterOpts(ctx: OptsContext): Unit = {}

  override def exitObject_declaration(ctx: Object_declarationContext): Unit = {}

  override def enterFile_type_definition(ctx: File_type_definitionContext): Unit = {}

  override def enterThrough_aspect(ctx: Through_aspectContext): Unit = {}

  override def enterBase_unit_declaration(ctx: Base_unit_declarationContext): Unit = {}

  override def exitElement_declaration(ctx: Element_declarationContext): Unit = {}

  override def enterLibrary_unit(ctx: Library_unitContext): Unit = {}

  override def enterRange(ctx: RangeContext): Unit = {}

  override def exitPort_clause(ctx: Port_clauseContext): Unit = {}

  override def enterSimultaneous_alternative(ctx: Simultaneous_alternativeContext): Unit = {}

  override def exitInterface_variable_declaration(ctx: Interface_variable_declarationContext): Unit = {}

  override def enterTimeout_clause(ctx: Timeout_clauseContext): Unit = {}

  override def enterProcedure_call(ctx: Procedure_callContext): Unit = {}

  override def exitTimeout_clause(ctx: Timeout_clauseContext): Unit = {}

  override def enterLiteral(ctx: LiteralContext): Unit = {}

  override def exitGroup_template_declaration(ctx: Group_template_declarationContext): Unit = {}

  override def enterSimultaneous_statement(ctx: Simultaneous_statementContext): Unit = {}

  override def exitBinding_indication(ctx: Binding_indicationContext): Unit = {}

  override def enterAllocator(ctx: AllocatorContext): Unit = {}

  override def exitProcess_declarative_item(ctx: Process_declarative_itemContext): Unit = {}

  override def enterElement_association(ctx: Element_associationContext): Unit = {}

  override def enterEntity_declarative_part(ctx: Entity_declarative_partContext): Unit = {}

  override def exitFormal_part(ctx: Formal_partContext): Unit = {}

  override def exitBlock_declarative_item(ctx: Block_declarative_itemContext): Unit = {}

  override def exitPrimary(ctx: PrimaryContext): Unit = {}

  override def enterLoop_statement(ctx: Loop_statementContext): Unit = {}

  override def exitEntity_class_entry(ctx: Entity_class_entryContext): Unit = {}

  override def enterComponent_instantiation_statement(ctx: Component_instantiation_statementContext): Unit = {}

  override def exitPackage_declaration(ctx: Package_declarationContext): Unit = {}

  override def exitType_declaration(ctx: Type_declarationContext): Unit = {}

  override def enterAdding_operator(ctx: Adding_operatorContext): Unit = {}

  override def exitSimultaneous_statement(ctx: Simultaneous_statementContext): Unit = {}

  override def exitComponent_configuration(ctx: Component_configurationContext): Unit = {}

  override def enterEntity_designator(ctx: Entity_designatorContext): Unit = {}

  override def enterConstant_declaration(ctx: Constant_declarationContext): Unit = {}

  override def exitSubnature_indication(ctx: Subnature_indicationContext): Unit = {}

  override def enterIndex_specification(ctx: Index_specificationContext): Unit = {}

  override def enterSuffix(ctx: SuffixContext): Unit = {}

  override def enterSubprogram_declaration(ctx: Subprogram_declarationContext): Unit = {}

  override def enterDisconnection_specification(ctx: Disconnection_specificationContext): Unit = {}

  override def enterAssertion_statement(ctx: Assertion_statementContext): Unit = {}

  override def exitEnumeration_literal(ctx: Enumeration_literalContext): Unit = {}

  override def exitSubprogram_body(ctx: Subprogram_bodyContext): Unit = {}

  override def enterRecord_nature_definition(ctx: Record_nature_definitionContext): Unit = {}

  override def exitArray_nature_definition(ctx: Array_nature_definitionContext): Unit = {}

  override def exitQuantity_list(ctx: Quantity_listContext): Unit = {}

  override def enterWaveform(ctx: WaveformContext): Unit = {}

  override def exitSecondary_unit(ctx: Secondary_unitContext): Unit = {}

  override def enterPhysical_type_definition(ctx: Physical_type_definitionContext): Unit = {}

  override def enterPackage_body_declarative_item(ctx: Package_body_declarative_itemContext): Unit = {}

  override def enterInterface_quantity_declaration(ctx: Interface_quantity_declarationContext): Unit = {}

  override def enterQuantity_list(ctx: Quantity_listContext): Unit = {}

  override def enterComponent_specification(ctx: Component_specificationContext): Unit = {}

  override def exitGuarded_signal_specification(ctx: Guarded_signal_specificationContext): Unit = {}

  override def enterBlock_configuration(ctx: Block_configurationContext): Unit = {}

  override def exitEntity_designator(ctx: Entity_designatorContext): Unit = {}

  override def enterSecondary_unit(ctx: Secondary_unitContext): Unit = {}

  override def exitFactor(ctx: FactorContext): Unit = {}

  override def exitDisconnection_specification(ctx: Disconnection_specificationContext): Unit = {}

  override def enterBreak_list(ctx: Break_listContext): Unit = {}

  override def enterActual_part(ctx: Actual_partContext): Unit = {}

  override def enterScalar_nature_definition(ctx: Scalar_nature_definitionContext): Unit = {}

  override def exitConstraint(ctx: ConstraintContext): Unit = {}

  override def enterReport_statement(ctx: Report_statementContext): Unit = {}

  override def exitBreak_element(ctx: Break_elementContext): Unit = {}

  override def exitType_definition(ctx: Type_definitionContext): Unit = {}

  override def enterNext_statement(ctx: Next_statementContext): Unit = {}

  override def enterAssociation_list(ctx: Association_listContext): Unit = {}

  override def exitSelected_signal_assignment(ctx: Selected_signal_assignmentContext): Unit = {}

  override def exitUnconstrained_nature_definition(ctx: Unconstrained_nature_definitionContext): Unit = {}

  override def exitPrimary_unit(ctx: Primary_unitContext): Unit = {}

  override def enterCondition_clause(ctx: Condition_clauseContext): Unit = {}

  override def exitThrough_aspect(ctx: Through_aspectContext): Unit = {}

  override def exitParameter_specification(ctx: Parameter_specificationContext): Unit = {}

  override def enterAttribute_designator(ctx: Attribute_designatorContext): Unit = {}

  override def enterGeneric_list(ctx: Generic_listContext): Unit = {}

  override def enterEntity_statement(ctx: Entity_statementContext): Unit = {}

  override def enterQuantity_specification(ctx: Quantity_specificationContext): Unit = {}

  override def exitTerminal_aspect(ctx: Terminal_aspectContext): Unit = {}

  override def exitSubprogram_declaration(ctx: Subprogram_declarationContext): Unit = {}

  override def exitInterface_signal_declaration(ctx: Interface_signal_declarationContext): Unit = {}

  override def enterUnconstrained_array_definition(ctx: Unconstrained_array_definitionContext): Unit = {}

  override def exitAbstract_literal(ctx: Abstract_literalContext): Unit = {}

  override def exitProcedural_declarative_item(ctx: Procedural_declarative_itemContext): Unit = {}

  override def enterPackage_declarative_part(ctx: Package_declarative_partContext): Unit = {}

  override def enterFile_declaration(ctx: File_declarationContext): Unit = {}

  override def enterGuarded_signal_specification(ctx: Guarded_signal_specificationContext): Unit = {}

  override def exitDirection(ctx: DirectionContext): Unit = {}

  override def enterSource_quantity_declaration(ctx: Source_quantity_declarationContext): Unit = {}

  override def exitFree_quantity_declaration(ctx: Free_quantity_declarationContext): Unit = {}

  override def enterSensitivity_list(ctx: Sensitivity_listContext): Unit = {}

  override def enterSequential_statement(ctx: Sequential_statementContext): Unit = {}

  override def enterBlock_header(ctx: Block_headerContext): Unit = {}

  override def enterInterface_signal_declaration(ctx: Interface_signal_declarationContext): Unit = {}

  override def enterBlock_statement_part(ctx: Block_statement_partContext): Unit = {}

  override def exitReport_statement(ctx: Report_statementContext): Unit = {}

  override def exitEntity_aspect(ctx: Entity_aspectContext): Unit = {}

  override def exitName_attribute_part(ctx: Name_attribute_partContext): Unit = {}

  override def exitSimple_simultaneous_statement(ctx: Simple_simultaneous_statementContext): Unit = {}

  override def exitAssociation_list(ctx: Association_listContext): Unit = {}

  override def enterCase_statement(ctx: Case_statementContext): Unit = {}

  override def exitQuantity_specification(ctx: Quantity_specificationContext): Unit = {}

  override def enterArchitecture_declarative_part(ctx: Architecture_declarative_partContext): Unit = {}

  override def exitLoop_statement(ctx: Loop_statementContext): Unit = {}

  override def enterProcedural_statement_part(ctx: Procedural_statement_partContext): Unit = {}

  override def exitCondition_clause(ctx: Condition_clauseContext): Unit = {}

  override def enterScalar_type_definition(ctx: Scalar_type_definitionContext): Unit = {}

  override def exitSignal_declaration(ctx: Signal_declarationContext): Unit = {}

  override def exitInterface_quantity_declaration(ctx: Interface_quantity_declarationContext): Unit = {}

  override def enterSimultaneous_statement_part(ctx: Simultaneous_statement_partContext): Unit = {}

  override def exitEntity_statement(ctx: Entity_statementContext): Unit = {}

  override def enterIf_statement(ctx: If_statementContext): Unit = {}

  override def exitPackage_body_declarative_item(ctx: Package_body_declarative_itemContext): Unit = {}

  override def enterEntity_declarative_item(ctx: Entity_declarative_itemContext): Unit = {}

  override def exitSensitivity_clause(ctx: Sensitivity_clauseContext): Unit = {}

  override def enterEntity_specification(ctx: Entity_specificationContext): Unit = {}

  override def enterSubprogram_statement_part(ctx: Subprogram_statement_partContext): Unit = {}

  override def exitSuffix(ctx: SuffixContext): Unit = {}

  override def exitSubtype_indication(ctx: Subtype_indicationContext): Unit = {}

  override def enterSignature(ctx: SignatureContext): Unit = {}

  override def exitSimultaneous_statement_part(ctx: Simultaneous_statement_partContext): Unit = {}

  override def exitFile_declaration(ctx: File_declarationContext): Unit = {}

  override def enterEntity_aspect(ctx: Entity_aspectContext): Unit = {}

  override def enterAlias_indication(ctx: Alias_indicationContext): Unit = {}

  override def exitProcedure_call(ctx: Procedure_callContext): Unit = {}

  override def exitActual_part(ctx: Actual_partContext): Unit = {}

  override def exitLabel_colon(ctx: Label_colonContext): Unit = {}

  override def enterProcess_statement_part(ctx: Process_statement_partContext): Unit = {}

  override def enterLogical_name(ctx: Logical_nameContext): Unit = {}

  override def enterSimple_simultaneous_statement(ctx: Simple_simultaneous_statementContext): Unit = {}

  override def enterAlias_designator(ctx: Alias_designatorContext): Unit = {}

  override def enterUnconstrained_nature_definition(ctx: Unconstrained_nature_definitionContext): Unit = {}

  override def exitSubtype_declaration(ctx: Subtype_declarationContext): Unit = {}

  override def exitAggregate(ctx: AggregateContext): Unit = {}

  override def exitInterface_port_list(ctx: Interface_port_listContext): Unit = {}

  override def enterAssociation_element(ctx: Association_elementContext): Unit = {}

  override def exitUnconstrained_array_definition(ctx: Unconstrained_array_definitionContext): Unit = {}

  override def enterConfiguration_item(ctx: Configuration_itemContext): Unit = {}

  override def enterIdentifier(ctx: IdentifierContext): Unit = {}

  override def enterObject_declaration(ctx: Object_declarationContext): Unit = {}

  override def enterEntity_tag(ctx: Entity_tagContext): Unit = {}

  override def enterRelational_operator(ctx: Relational_operatorContext): Unit = {}

  override def exitEntity_declarative_part(ctx: Entity_declarative_partContext): Unit = {}

  override def exitProcess_statement_part(ctx: Process_statement_partContext): Unit = {}

  override def exitAlias_indication(ctx: Alias_indicationContext): Unit = {}

  override def exitScalar_type_definition(ctx: Scalar_type_definitionContext): Unit = {
    val scalarTypeDef = VScalarTypeDef(ctx)
    // TODO
  }

  override def exitRange(ctx: RangeContext): Unit = {}

  override def exitInstantiated_unit(ctx: Instantiated_unitContext): Unit = {}

  override def enterContext_item(ctx: Context_itemContext): Unit = {}

  override def enterSimultaneous_procedural_statement(ctx: Simultaneous_procedural_statementContext): Unit = {}

  override def enterExit_statement(ctx: Exit_statementContext): Unit = {}

  override def exitEntity_statement_part(ctx: Entity_statement_partContext): Unit = {}

  override def exitEnumeration_type_definition(ctx: Enumeration_type_definitionContext): Unit = {}

  override def exitName(ctx: NameContext): Unit = {}

  override def enterEnumeration_type_definition(ctx: Enumeration_type_definitionContext): Unit = {}

  override def exitConcurrent_signal_assignment_statement(ctx: Concurrent_signal_assignment_statementContext): Unit = {
    val concurrentSignalAssign = VConcSignalAssignStat(ctx)
    concurrentSignalAssign match {
      case csa@VConcSignalAssignStatC(labelColon, _, condSignAssign) => {
        conc_stmt_complexes += csa.toI(defInfo)
        //        logger.info(s"${csa.toI(defInfo)}")
      }
      case VConcSignalAssignStatS(_, _, selectSignalAssign) => {
      }
    }
  }

  override def exitInterface_element(ctx: Interface_elementContext): Unit = {}

  override def exitSource_quantity_declaration(ctx: Source_quantity_declarationContext): Unit = {}

  override def exitAdding_operator(ctx: Adding_operatorContext): Unit = {}

  override def exitArchitecture_declarative_part(ctx: Architecture_declarative_partContext): Unit = {}

  override def enterInterface_element(ctx: Interface_elementContext): Unit = {}

  override def enterIndex_subtype_definition(ctx: Index_subtype_definitionContext): Unit = {}

  override def enterAccess_type_definition(ctx: Access_type_definitionContext): Unit = {}

  override def enterEntity_class_entry(ctx: Entity_class_entryContext): Unit = {}

  override def enterConfiguration_declarative_part(ctx: Configuration_declarative_partContext): Unit = {}

  override def enterConfiguration_specification(ctx: Configuration_specificationContext): Unit = {}

  override def enterPackage_declarative_item(ctx: Package_declarative_itemContext): Unit = {}

  override def enterIteration_scheme(ctx: Iteration_schemeContext): Unit = {}

  override def enterInterface_terminal_declaration(ctx: Interface_terminal_declarationContext): Unit = {}

  override def enterBinding_indication(ctx: Binding_indicationContext): Unit = {}

  override def exitNext_statement(ctx: Next_statementContext): Unit = {}

  override def exitTerminal_declaration(ctx: Terminal_declarationContext): Unit = {}

  override def exitComponent_specification(ctx: Component_specificationContext): Unit = {}

  override def enterSubprogram_specification(ctx: Subprogram_specificationContext): Unit = {}

  override def exitProcedure_call_statement(ctx: Procedure_call_statementContext): Unit = {}

  override def exitArchitecture_body(ctx: Architecture_bodyContext): Unit = {}

  override def enterTarget(ctx: TargetContext): Unit = {}

  override def exitSimple_expression(ctx: Simple_expressionContext): Unit = {}

  override def enterTolerance_aspect(ctx: Tolerance_aspectContext): Unit = {}

  override def enterShift_expression(ctx: Shift_expressionContext): Unit = {}

  override def enterArchitecture_body(ctx: Architecture_bodyContext): Unit = {}

  override def enterSimple_expression(ctx: Simple_expressionContext): Unit = {}

  override def exitMultiplying_operator(ctx: Multiplying_operatorContext): Unit = {}

  override def enterConstrained_nature_definition(ctx: Constrained_nature_definitionContext): Unit = {}

  override def exitGroup_declaration(ctx: Group_declarationContext): Unit = {}

  override def exitName_part(ctx: Name_partContext): Unit = {}

  override def enterArchitecture_statement_part(ctx: Architecture_statement_partContext): Unit = {}

  override def exitBreak_selector_clause(ctx: Break_selector_clauseContext): Unit = {}

  override def enterInterface_list(ctx: Interface_listContext): Unit = {}

  override def exitLibrary_clause(ctx: Library_clauseContext): Unit = {}

  override def exitLibrary_unit(ctx: Library_unitContext): Unit = {}

  override def enterEntity_class(ctx: Entity_classContext): Unit = {}

  override def enterProcedure_specification(ctx: Procedure_specificationContext): Unit = {}

  override def exitAssertion(ctx: AssertionContext): Unit = {}

  override def enterConcurrent_signal_assignment_statement(ctx: Concurrent_signal_assignment_statementContext): Unit = {}

  override def exitFile_type_definition(ctx: File_type_definitionContext): Unit = {}

  override def exitAccess_type_definition(ctx: Access_type_definitionContext): Unit = {}

  override def enterSelected_name(ctx: Selected_nameContext): Unit = {}

  override def exitInterface_declaration(ctx: Interface_declarationContext): Unit = {}

  override def enterConcurrent_procedure_call_statement(ctx: Concurrent_procedure_call_statementContext): Unit = {}

  override def enterRange_constraint(ctx: Range_constraintContext): Unit = {}

  override def exitCase_statement(ctx: Case_statementContext): Unit = {}

  override def exitArchitecture_statement_part(ctx: Architecture_statement_partContext): Unit = {}

  override def exitConstrained_array_definition(ctx: Constrained_array_definitionContext): Unit = {}

  override def exitShift_expression(ctx: Shift_expressionContext): Unit = {}

  override def enterLibrary_clause(ctx: Library_clauseContext): Unit = {}

  override def enterContext_clause(ctx: Context_clauseContext): Unit = {}

  override def exitActual_parameter_part(ctx: Actual_parameter_partContext): Unit = {}

  override def enterProcess_statement(ctx: Process_statementContext): Unit = {}

  override def exitTolerance_aspect(ctx: Tolerance_aspectContext): Unit = {}

  override def exitLogical_name_list(ctx: Logical_name_listContext): Unit = {}

  override def exitProcess_statement(ctx: Process_statementContext): Unit = {
    val procStat = VProcStat(ctx)
    val isar = procStat.toI(defInfo)
    conc_stmt_complexes += isar
    //    logger.info(s"${isar}")
  }

  override def enterGroup_declaration(ctx: Group_declarationContext): Unit = {}

  override def exitArray_type_definition(ctx: Array_type_definitionContext): Unit = {}

  override def exitQualified_expression(ctx: Qualified_expressionContext): Unit = {}

  override def exitSimultaneous_alternative(ctx: Simultaneous_alternativeContext): Unit = {}

  override def visitTerminal(node: TerminalNode): Unit = {}

  override def visitErrorNode(node: ErrorNode): Unit = {}

  override def exitEveryRule(ctx: ParserRuleContext): Unit = {}

  override def enterEveryRule(ctx: ParserRuleContext): Unit = {}
}
