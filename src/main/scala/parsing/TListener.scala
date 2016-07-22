package parsing

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, TerminalNode}
import org.slf4j.LoggerFactory
import sg.edu.ntu.hchen.VHDLBaseListener
import scala.collection.JavaConversions._
import sg.edu.ntu.hchen.VHDLParser.{Context_itemContext, Interface_listContext, Sensitivity_clauseContext, Shift_operatorContext, _}

class TListener extends VHDLBaseListener{
  val logger = LoggerFactory.getLogger(classOf[TListener])
  override def enterAbstract_literal(ctx: Abstract_literalContext): Unit = super.enterAbstract_literal(ctx)

  override def exitAbstract_literal(ctx: Abstract_literalContext): Unit = super.exitAbstract_literal(ctx)

  override def enterAccess_type_definition(ctx: Access_type_definitionContext): Unit = super.enterAccess_type_definition(ctx)

  override def exitAccess_type_definition(ctx: Access_type_definitionContext): Unit = super.exitAccess_type_definition(ctx)

  override def enterAcross_aspect(ctx: Across_aspectContext): Unit = super.enterAcross_aspect(ctx)

  override def exitAcross_aspect(ctx: Across_aspectContext): Unit = super.exitAcross_aspect(ctx)

  override def enterActual_designator(ctx: Actual_designatorContext): Unit = super.enterActual_designator(ctx)

  override def exitActual_designator(ctx: Actual_designatorContext): Unit = super.exitActual_designator(ctx)

  override def enterActual_parameter_part(ctx: Actual_parameter_partContext): Unit = super.enterActual_parameter_part(ctx)

  override def exitActual_parameter_part(ctx: Actual_parameter_partContext): Unit = super.exitActual_parameter_part(ctx)

  override def enterActual_part(ctx: Actual_partContext): Unit = super.enterActual_part(ctx)

  override def exitActual_part(ctx: Actual_partContext): Unit = super.exitActual_part(ctx)

  override def enterAdding_operator(ctx: Adding_operatorContext): Unit = super.enterAdding_operator(ctx)

  override def exitAdding_operator(ctx: Adding_operatorContext): Unit = super.exitAdding_operator(ctx)

  override def enterAggregate(ctx: AggregateContext): Unit = super.enterAggregate(ctx)

  override def exitAggregate(ctx: AggregateContext): Unit = super.exitAggregate(ctx)

  override def enterAlias_declaration(ctx: Alias_declarationContext): Unit = super.enterAlias_declaration(ctx)

  override def exitAlias_declaration(ctx: Alias_declarationContext): Unit = super.exitAlias_declaration(ctx)

  override def enterAlias_designator(ctx: Alias_designatorContext): Unit = super.enterAlias_designator(ctx)

  override def exitAlias_designator(ctx: Alias_designatorContext): Unit = super.exitAlias_designator(ctx)

  override def enterAlias_indication(ctx: Alias_indicationContext): Unit = super.enterAlias_indication(ctx)

  override def exitAlias_indication(ctx: Alias_indicationContext): Unit = super.exitAlias_indication(ctx)

  override def enterAllocator(ctx: AllocatorContext): Unit = super.enterAllocator(ctx)

  override def exitAllocator(ctx: AllocatorContext): Unit = super.exitAllocator(ctx)

  override def enterArchitecture_body(ctx: Architecture_bodyContext): Unit = super.enterArchitecture_body(ctx)

  override def exitArchitecture_body(ctx: Architecture_bodyContext): Unit = super.exitArchitecture_body(ctx)

  override def enterArchitecture_declarative_part(ctx: Architecture_declarative_partContext): Unit = super.enterArchitecture_declarative_part(ctx)

  override def exitArchitecture_declarative_part(ctx: Architecture_declarative_partContext): Unit = super.exitArchitecture_declarative_part(ctx)

  override def enterArchitecture_statement(ctx: Architecture_statementContext): Unit = super.enterArchitecture_statement(ctx)

  override def exitArchitecture_statement(ctx: Architecture_statementContext): Unit = super.exitArchitecture_statement(ctx)

  override def enterArchitecture_statement_part(ctx: Architecture_statement_partContext): Unit = super.enterArchitecture_statement_part(ctx)

  override def exitArchitecture_statement_part(ctx: Architecture_statement_partContext): Unit = super.exitArchitecture_statement_part(ctx)

  override def enterArray_nature_definition(ctx: Array_nature_definitionContext): Unit = super.enterArray_nature_definition(ctx)

  override def exitArray_nature_definition(ctx: Array_nature_definitionContext): Unit = super.exitArray_nature_definition(ctx)

  override def enterArray_type_definition(ctx: Array_type_definitionContext): Unit = super.enterArray_type_definition(ctx)

  override def exitArray_type_definition(ctx: Array_type_definitionContext): Unit = super.exitArray_type_definition(ctx)

  override def enterAssertion(ctx: AssertionContext): Unit = super.enterAssertion(ctx)

  override def exitAssertion(ctx: AssertionContext): Unit = super.exitAssertion(ctx)

  override def enterAssertion_statement(ctx: Assertion_statementContext): Unit = super.enterAssertion_statement(ctx)

  override def exitAssertion_statement(ctx: Assertion_statementContext): Unit = super.exitAssertion_statement(ctx)

  override def enterAssociation_element(ctx: Association_elementContext): Unit = super.enterAssociation_element(ctx)

  override def exitAssociation_element(ctx: Association_elementContext): Unit = super.exitAssociation_element(ctx)

  override def enterAssociation_list(ctx: Association_listContext): Unit = super.enterAssociation_list(ctx)

  override def exitAssociation_list(ctx: Association_listContext): Unit = super.exitAssociation_list(ctx)

  override def enterAttribute_declaration(ctx: Attribute_declarationContext): Unit = super.enterAttribute_declaration(ctx)

  override def exitAttribute_declaration(ctx: Attribute_declarationContext): Unit = super.exitAttribute_declaration(ctx)

  override def enterAttribute_designator(ctx: Attribute_designatorContext): Unit = super.enterAttribute_designator(ctx)

  override def exitAttribute_designator(ctx: Attribute_designatorContext): Unit = super.exitAttribute_designator(ctx)

  override def enterAttribute_specification(ctx: Attribute_specificationContext): Unit = super.enterAttribute_specification(ctx)

  override def exitAttribute_specification(ctx: Attribute_specificationContext): Unit = super.exitAttribute_specification(ctx)

  override def enterBase_unit_declaration(ctx: Base_unit_declarationContext): Unit = super.enterBase_unit_declaration(ctx)

  override def exitBase_unit_declaration(ctx: Base_unit_declarationContext): Unit = super.exitBase_unit_declaration(ctx)

  override def enterBinding_indication(ctx: Binding_indicationContext): Unit = super.enterBinding_indication(ctx)

  override def exitBinding_indication(ctx: Binding_indicationContext): Unit = super.exitBinding_indication(ctx)

  override def enterBlock_configuration(ctx: Block_configurationContext): Unit = super.enterBlock_configuration(ctx)

  override def exitBlock_configuration(ctx: Block_configurationContext): Unit = super.exitBlock_configuration(ctx)

  override def enterBlock_declarative_item(ctx: Block_declarative_itemContext): Unit = super.enterBlock_declarative_item(ctx)

  override def exitBlock_declarative_item(ctx: Block_declarative_itemContext): Unit = super.exitBlock_declarative_item(ctx)

  override def enterBlock_declarative_part(ctx: Block_declarative_partContext): Unit = super.enterBlock_declarative_part(ctx)

  override def exitBlock_declarative_part(ctx: Block_declarative_partContext): Unit = super.exitBlock_declarative_part(ctx)

  override def enterBlock_header(ctx: Block_headerContext): Unit = super.enterBlock_header(ctx)

  override def exitBlock_header(ctx: Block_headerContext): Unit = super.exitBlock_header(ctx)

  override def enterBlock_specification(ctx: Block_specificationContext): Unit = super.enterBlock_specification(ctx)

  override def exitBlock_specification(ctx: Block_specificationContext): Unit = super.exitBlock_specification(ctx)

  override def enterBlock_statement(ctx: Block_statementContext): Unit = super.enterBlock_statement(ctx)

  override def exitBlock_statement(ctx: Block_statementContext): Unit = super.exitBlock_statement(ctx)

  override def enterBlock_statement_part(ctx: Block_statement_partContext): Unit = super.enterBlock_statement_part(ctx)

  override def exitBlock_statement_part(ctx: Block_statement_partContext): Unit = super.exitBlock_statement_part(ctx)

  override def enterBranch_quantity_declaration(ctx: Branch_quantity_declarationContext): Unit = super.enterBranch_quantity_declaration(ctx)

  override def exitBranch_quantity_declaration(ctx: Branch_quantity_declarationContext): Unit = super.exitBranch_quantity_declaration(ctx)

  override def enterBreak_element(ctx: Break_elementContext): Unit = super.enterBreak_element(ctx)

  override def exitBreak_element(ctx: Break_elementContext): Unit = super.exitBreak_element(ctx)

  override def enterBreak_list(ctx: Break_listContext): Unit = super.enterBreak_list(ctx)

  override def exitBreak_list(ctx: Break_listContext): Unit = super.exitBreak_list(ctx)

  override def enterBreak_selector_clause(ctx: Break_selector_clauseContext): Unit = super.enterBreak_selector_clause(ctx)

  override def exitBreak_selector_clause(ctx: Break_selector_clauseContext): Unit = super.exitBreak_selector_clause(ctx)

  override def enterBreak_statement(ctx: Break_statementContext): Unit = super.enterBreak_statement(ctx)

  override def exitBreak_statement(ctx: Break_statementContext): Unit = super.exitBreak_statement(ctx)

  override def enterCase_statement(ctx: Case_statementContext): Unit = super.enterCase_statement(ctx)

  override def exitCase_statement(ctx: Case_statementContext): Unit = super.exitCase_statement(ctx)

  override def enterCase_statement_alternative(ctx: Case_statement_alternativeContext): Unit = super.enterCase_statement_alternative(ctx)

  override def exitCase_statement_alternative(ctx: Case_statement_alternativeContext): Unit = super.exitCase_statement_alternative(ctx)

  override def enterChoice(ctx: ChoiceContext): Unit = super.enterChoice(ctx)

  override def exitChoice(ctx: ChoiceContext): Unit = super.exitChoice(ctx)

  override def enterChoices(ctx: ChoicesContext): Unit = super.enterChoices(ctx)

  override def exitChoices(ctx: ChoicesContext): Unit = super.exitChoices(ctx)

  override def enterComponent_configuration(ctx: Component_configurationContext): Unit = super.enterComponent_configuration(ctx)

  override def exitComponent_configuration(ctx: Component_configurationContext): Unit = super.exitComponent_configuration(ctx)

  override def enterComponent_declaration(ctx: Component_declarationContext): Unit = super.enterComponent_declaration(ctx)

  override def exitComponent_declaration(ctx: Component_declarationContext): Unit = super.exitComponent_declaration(ctx)

  override def enterComponent_instantiation_statement(ctx: Component_instantiation_statementContext): Unit = super.enterComponent_instantiation_statement(ctx)

  override def exitComponent_instantiation_statement(ctx: Component_instantiation_statementContext): Unit = super.exitComponent_instantiation_statement(ctx)

  override def enterComponent_specification(ctx: Component_specificationContext): Unit = super.enterComponent_specification(ctx)

  override def exitComponent_specification(ctx: Component_specificationContext): Unit = super.exitComponent_specification(ctx)

  override def enterComposite_nature_definition(ctx: Composite_nature_definitionContext): Unit = super.enterComposite_nature_definition(ctx)

  override def exitComposite_nature_definition(ctx: Composite_nature_definitionContext): Unit = super.exitComposite_nature_definition(ctx)

  override def enterComposite_type_definition(ctx: Composite_type_definitionContext): Unit = super.enterComposite_type_definition(ctx)

  override def exitComposite_type_definition(ctx: Composite_type_definitionContext): Unit = super.exitComposite_type_definition(ctx)

  override def enterConcurrent_assertion_statement(ctx: Concurrent_assertion_statementContext): Unit = super.enterConcurrent_assertion_statement(ctx)

  override def exitConcurrent_assertion_statement(ctx: Concurrent_assertion_statementContext): Unit = super.exitConcurrent_assertion_statement(ctx)

  override def enterConcurrent_break_statement(ctx: Concurrent_break_statementContext): Unit = super.enterConcurrent_break_statement(ctx)

  override def exitConcurrent_break_statement(ctx: Concurrent_break_statementContext): Unit = super.exitConcurrent_break_statement(ctx)

  override def enterConcurrent_procedure_call_statement(ctx: Concurrent_procedure_call_statementContext): Unit = super.enterConcurrent_procedure_call_statement(ctx)

  override def exitConcurrent_procedure_call_statement(ctx: Concurrent_procedure_call_statementContext): Unit = super.exitConcurrent_procedure_call_statement(ctx)

  override def enterConcurrent_signal_assignment_statement(ctx: Concurrent_signal_assignment_statementContext): Unit = super.enterConcurrent_signal_assignment_statement(ctx)

  override def exitConcurrent_signal_assignment_statement(ctx: Concurrent_signal_assignment_statementContext): Unit = super.exitConcurrent_signal_assignment_statement(ctx)

  override def enterCondition(ctx: ConditionContext): Unit = super.enterCondition(ctx)

  override def exitCondition(ctx: ConditionContext): Unit = super.exitCondition(ctx)

  override def enterCondition_clause(ctx: Condition_clauseContext): Unit = super.enterCondition_clause(ctx)

  override def exitCondition_clause(ctx: Condition_clauseContext): Unit = super.exitCondition_clause(ctx)

  override def enterConditional_signal_assignment(ctx: Conditional_signal_assignmentContext): Unit = super.enterConditional_signal_assignment(ctx)

  override def exitConditional_signal_assignment(ctx: Conditional_signal_assignmentContext): Unit = super.exitConditional_signal_assignment(ctx)

  override def enterConditional_waveforms(ctx: Conditional_waveformsContext): Unit = super.enterConditional_waveforms(ctx)

  override def exitConditional_waveforms(ctx: Conditional_waveformsContext): Unit = super.exitConditional_waveforms(ctx)

  override def enterConfiguration_declaration(ctx: Configuration_declarationContext): Unit = super.enterConfiguration_declaration(ctx)

  override def exitConfiguration_declaration(ctx: Configuration_declarationContext): Unit = super.exitConfiguration_declaration(ctx)

  override def enterConfiguration_declarative_item(ctx: Configuration_declarative_itemContext): Unit = super.enterConfiguration_declarative_item(ctx)

  override def exitConfiguration_declarative_item(ctx: Configuration_declarative_itemContext): Unit = super.exitConfiguration_declarative_item(ctx)

  override def enterConfiguration_declarative_part(ctx: Configuration_declarative_partContext): Unit = super.enterConfiguration_declarative_part(ctx)

  override def exitConfiguration_declarative_part(ctx: Configuration_declarative_partContext): Unit = super.exitConfiguration_declarative_part(ctx)

  override def enterConfiguration_item(ctx: Configuration_itemContext): Unit = super.enterConfiguration_item(ctx)

  override def exitConfiguration_item(ctx: Configuration_itemContext): Unit = super.exitConfiguration_item(ctx)

  override def enterConfiguration_specification(ctx: Configuration_specificationContext): Unit = super.enterConfiguration_specification(ctx)

  override def exitConfiguration_specification(ctx: Configuration_specificationContext): Unit = super.exitConfiguration_specification(ctx)

  override def enterConstant_declaration(ctx: Constant_declarationContext): Unit = super.enterConstant_declaration(ctx)

  override def exitConstant_declaration(ctx: Constant_declarationContext): Unit = super.exitConstant_declaration(ctx)

  override def enterConstrained_array_definition(ctx: Constrained_array_definitionContext): Unit = super.enterConstrained_array_definition(ctx)

  override def exitConstrained_array_definition(ctx: Constrained_array_definitionContext): Unit = super.exitConstrained_array_definition(ctx)

  override def enterConstrained_nature_definition(ctx: Constrained_nature_definitionContext): Unit = super.enterConstrained_nature_definition(ctx)

  override def exitConstrained_nature_definition(ctx: Constrained_nature_definitionContext): Unit = super.exitConstrained_nature_definition(ctx)

  override def enterConstraint(ctx: ConstraintContext): Unit = super.enterConstraint(ctx)

  override def exitConstraint(ctx: ConstraintContext): Unit = super.exitConstraint(ctx)

  override def enterContext_clause(ctx: Context_clauseContext): Unit = super.enterContext_clause(ctx)

  override def exitContext_clause(ctx: Context_clauseContext): Unit = super.exitContext_clause(ctx)

  override def enterContext_item(ctx: Context_itemContext): Unit = super.enterContext_item(ctx)

  override def exitContext_item(ctx: Context_itemContext): Unit = super.exitContext_item(ctx)

  override def enterDelay_mechanism(ctx: Delay_mechanismContext): Unit = super.enterDelay_mechanism(ctx)

  override def exitDelay_mechanism(ctx: Delay_mechanismContext): Unit = super.exitDelay_mechanism(ctx)

  override def enterDesign_file(ctx: Design_fileContext): Unit = super.enterDesign_file(ctx)

  override def exitDesign_file(ctx: Design_fileContext): Unit = super.exitDesign_file(ctx)

  override def enterDesign_unit(ctx: Design_unitContext): Unit = super.enterDesign_unit(ctx)

  override def exitDesign_unit(ctx: Design_unitContext): Unit = super.exitDesign_unit(ctx)

  override def enterDesignator(ctx: DesignatorContext): Unit = super.enterDesignator(ctx)

  override def exitDesignator(ctx: DesignatorContext): Unit = super.exitDesignator(ctx)

  override def enterDirection(ctx: DirectionContext): Unit = super.enterDirection(ctx)

  override def exitDirection(ctx: DirectionContext): Unit = super.exitDirection(ctx)

  override def enterDisconnection_specification(ctx: Disconnection_specificationContext): Unit = super.enterDisconnection_specification(ctx)

  override def exitDisconnection_specification(ctx: Disconnection_specificationContext): Unit = super.exitDisconnection_specification(ctx)

  override def enterDiscrete_range(ctx: Discrete_rangeContext): Unit = super.enterDiscrete_range(ctx)

  override def exitDiscrete_range(ctx: Discrete_rangeContext): Unit = super.exitDiscrete_range(ctx)

  override def enterElement_association(ctx: Element_associationContext): Unit = super.enterElement_association(ctx)

  override def exitElement_association(ctx: Element_associationContext): Unit = super.exitElement_association(ctx)

  override def enterElement_declaration(ctx: Element_declarationContext): Unit = super.enterElement_declaration(ctx)

  override def exitElement_declaration(ctx: Element_declarationContext): Unit = super.exitElement_declaration(ctx)

  override def enterElement_subnature_definition(ctx: Element_subnature_definitionContext): Unit = super.enterElement_subnature_definition(ctx)

  override def exitElement_subnature_definition(ctx: Element_subnature_definitionContext): Unit = super.exitElement_subnature_definition(ctx)

  override def enterElement_subtype_definition(ctx: Element_subtype_definitionContext): Unit = super.enterElement_subtype_definition(ctx)

  override def exitElement_subtype_definition(ctx: Element_subtype_definitionContext): Unit = super.exitElement_subtype_definition(ctx)

  override def enterEntity_aspect(ctx: Entity_aspectContext): Unit = super.enterEntity_aspect(ctx)

  override def exitEntity_aspect(ctx: Entity_aspectContext): Unit = super.exitEntity_aspect(ctx)

  override def enterEntity_class(ctx: Entity_classContext): Unit = super.enterEntity_class(ctx)

  override def exitEntity_class(ctx: Entity_classContext): Unit = super.exitEntity_class(ctx)

  override def enterEntity_class_entry(ctx: Entity_class_entryContext): Unit = super.enterEntity_class_entry(ctx)

  override def exitEntity_class_entry(ctx: Entity_class_entryContext): Unit = super.exitEntity_class_entry(ctx)

  override def enterEntity_class_entry_list(ctx: Entity_class_entry_listContext): Unit = super.enterEntity_class_entry_list(ctx)

  override def exitEntity_class_entry_list(ctx: Entity_class_entry_listContext): Unit = {
  }

  override def enterEntity_declaration(ctx: Entity_declarationContext): Unit = {
    logger.info("-->")
    for(c<- ctx.children){
      println(c)
    }
  }

  override def exitEntity_declaration(ctx: Entity_declarationContext): Unit = super.exitEntity_declaration(ctx)

  override def enterEntity_declarative_item(ctx: Entity_declarative_itemContext): Unit = super.enterEntity_declarative_item(ctx)

  override def exitEntity_declarative_item(ctx: Entity_declarative_itemContext): Unit = super.exitEntity_declarative_item(ctx)

  override def enterEntity_declarative_part(ctx: Entity_declarative_partContext): Unit = super.enterEntity_declarative_part(ctx)

  override def exitEntity_declarative_part(ctx: Entity_declarative_partContext): Unit = super.exitEntity_declarative_part(ctx)

  override def enterEntity_designator(ctx: Entity_designatorContext): Unit = super.enterEntity_designator(ctx)

  override def exitEntity_designator(ctx: Entity_designatorContext): Unit = super.exitEntity_designator(ctx)

  override def enterEntity_header(ctx: Entity_headerContext): Unit = super.enterEntity_header(ctx)

  override def exitEntity_header(ctx: Entity_headerContext): Unit = super.exitEntity_header(ctx)

  override def enterEntity_name_list(ctx: Entity_name_listContext): Unit = super.enterEntity_name_list(ctx)

  override def exitEntity_name_list(ctx: Entity_name_listContext): Unit = super.exitEntity_name_list(ctx)

  override def enterEntity_specification(ctx: Entity_specificationContext): Unit = super.enterEntity_specification(ctx)

  override def exitEntity_specification(ctx: Entity_specificationContext): Unit = super.exitEntity_specification(ctx)

  override def enterEntity_statement(ctx: Entity_statementContext): Unit = super.enterEntity_statement(ctx)

  override def exitEntity_statement(ctx: Entity_statementContext): Unit = super.exitEntity_statement(ctx)

  override def enterEntity_statement_part(ctx: Entity_statement_partContext): Unit = super.enterEntity_statement_part(ctx)

  override def exitEntity_statement_part(ctx: Entity_statement_partContext): Unit = super.exitEntity_statement_part(ctx)

  override def enterEntity_tag(ctx: Entity_tagContext): Unit = super.enterEntity_tag(ctx)

  override def exitEntity_tag(ctx: Entity_tagContext): Unit = super.exitEntity_tag(ctx)

  override def enterEnumeration_literal(ctx: Enumeration_literalContext): Unit = super.enterEnumeration_literal(ctx)

  override def exitEnumeration_literal(ctx: Enumeration_literalContext): Unit = super.exitEnumeration_literal(ctx)

  override def enterEnumeration_type_definition(ctx: Enumeration_type_definitionContext): Unit = super.enterEnumeration_type_definition(ctx)

  override def exitEnumeration_type_definition(ctx: Enumeration_type_definitionContext): Unit = super.exitEnumeration_type_definition(ctx)

  override def enterExit_statement(ctx: Exit_statementContext): Unit = super.enterExit_statement(ctx)

  override def exitExit_statement(ctx: Exit_statementContext): Unit = super.exitExit_statement(ctx)

  override def enterExpression(ctx: ExpressionContext): Unit = super.enterExpression(ctx)

  override def exitExpression(ctx: ExpressionContext): Unit = super.exitExpression(ctx)

  override def enterFactor(ctx: FactorContext): Unit = super.enterFactor(ctx)

  override def exitFactor(ctx: FactorContext): Unit = super.exitFactor(ctx)

  override def enterFile_declaration(ctx: File_declarationContext): Unit = super.enterFile_declaration(ctx)

  override def exitFile_declaration(ctx: File_declarationContext): Unit = super.exitFile_declaration(ctx)

  override def enterFile_logical_name(ctx: File_logical_nameContext): Unit = super.enterFile_logical_name(ctx)

  override def exitFile_logical_name(ctx: File_logical_nameContext): Unit = super.exitFile_logical_name(ctx)

  override def enterFile_open_information(ctx: File_open_informationContext): Unit = super.enterFile_open_information(ctx)

  override def exitFile_open_information(ctx: File_open_informationContext): Unit = super.exitFile_open_information(ctx)

  override def enterFile_type_definition(ctx: File_type_definitionContext): Unit = super.enterFile_type_definition(ctx)

  override def exitFile_type_definition(ctx: File_type_definitionContext): Unit = super.exitFile_type_definition(ctx)

  override def enterFormal_parameter_list(ctx: Formal_parameter_listContext): Unit = super.enterFormal_parameter_list(ctx)

  override def exitFormal_parameter_list(ctx: Formal_parameter_listContext): Unit = super.exitFormal_parameter_list(ctx)

  override def enterFormal_part(ctx: Formal_partContext): Unit = super.enterFormal_part(ctx)

  override def exitFormal_part(ctx: Formal_partContext): Unit = super.exitFormal_part(ctx)

  override def enterFree_quantity_declaration(ctx: Free_quantity_declarationContext): Unit = super.enterFree_quantity_declaration(ctx)

  override def exitFree_quantity_declaration(ctx: Free_quantity_declarationContext): Unit = super.exitFree_quantity_declaration(ctx)

  override def enterGenerate_statement(ctx: Generate_statementContext): Unit = super.enterGenerate_statement(ctx)

  override def exitGenerate_statement(ctx: Generate_statementContext): Unit = super.exitGenerate_statement(ctx)

  override def enterGeneration_scheme(ctx: Generation_schemeContext): Unit = super.enterGeneration_scheme(ctx)

  override def exitGeneration_scheme(ctx: Generation_schemeContext): Unit = super.exitGeneration_scheme(ctx)

  override def enterGeneric_clause(ctx: Generic_clauseContext): Unit = super.enterGeneric_clause(ctx)

  override def exitGeneric_clause(ctx: Generic_clauseContext): Unit = super.exitGeneric_clause(ctx)

  override def enterGeneric_list(ctx: Generic_listContext): Unit = super.enterGeneric_list(ctx)

  override def exitGeneric_list(ctx: Generic_listContext): Unit = super.exitGeneric_list(ctx)

  override def enterGeneric_map_aspect(ctx: Generic_map_aspectContext): Unit = super.enterGeneric_map_aspect(ctx)

  override def exitGeneric_map_aspect(ctx: Generic_map_aspectContext): Unit = super.exitGeneric_map_aspect(ctx)

  override def enterGroup_constituent(ctx: Group_constituentContext): Unit = super.enterGroup_constituent(ctx)

  override def exitGroup_constituent(ctx: Group_constituentContext): Unit = super.exitGroup_constituent(ctx)

  override def enterGroup_constituent_list(ctx: Group_constituent_listContext): Unit = super.enterGroup_constituent_list(ctx)

  override def exitGroup_constituent_list(ctx: Group_constituent_listContext): Unit = super.exitGroup_constituent_list(ctx)

  override def enterGroup_declaration(ctx: Group_declarationContext): Unit = super.enterGroup_declaration(ctx)

  override def exitGroup_declaration(ctx: Group_declarationContext): Unit = super.exitGroup_declaration(ctx)

  override def enterGroup_template_declaration(ctx: Group_template_declarationContext): Unit = super.enterGroup_template_declaration(ctx)

  override def exitGroup_template_declaration(ctx: Group_template_declarationContext): Unit = super.exitGroup_template_declaration(ctx)

  override def enterGuarded_signal_specification(ctx: Guarded_signal_specificationContext): Unit = super.enterGuarded_signal_specification(ctx)

  override def exitGuarded_signal_specification(ctx: Guarded_signal_specificationContext): Unit = super.exitGuarded_signal_specification(ctx)

  override def enterIdentifier(ctx: IdentifierContext): Unit = super.enterIdentifier(ctx)

  override def exitIdentifier(ctx: IdentifierContext): Unit = super.exitIdentifier(ctx)

  override def enterIdentifier_list(ctx: Identifier_listContext): Unit = super.enterIdentifier_list(ctx)

  override def exitIdentifier_list(ctx: Identifier_listContext): Unit = super.exitIdentifier_list(ctx)

  override def enterIf_statement(ctx: If_statementContext): Unit = super.enterIf_statement(ctx)

  override def exitIf_statement(ctx: If_statementContext): Unit = super.exitIf_statement(ctx)

  override def enterIndex_constraint(ctx: Index_constraintContext): Unit = super.enterIndex_constraint(ctx)

  override def exitIndex_constraint(ctx: Index_constraintContext): Unit = super.exitIndex_constraint(ctx)

  override def enterIndex_specification(ctx: Index_specificationContext): Unit = super.enterIndex_specification(ctx)

  override def exitIndex_specification(ctx: Index_specificationContext): Unit = super.exitIndex_specification(ctx)

  override def enterIndex_subtype_definition(ctx: Index_subtype_definitionContext): Unit = super.enterIndex_subtype_definition(ctx)

  override def exitIndex_subtype_definition(ctx: Index_subtype_definitionContext): Unit = super.exitIndex_subtype_definition(ctx)

  override def enterInstantiated_unit(ctx: Instantiated_unitContext): Unit = super.enterInstantiated_unit(ctx)

  override def exitInstantiated_unit(ctx: Instantiated_unitContext): Unit = super.exitInstantiated_unit(ctx)

  override def enterInstantiation_list(ctx: Instantiation_listContext): Unit = super.enterInstantiation_list(ctx)

  override def exitInstantiation_list(ctx: Instantiation_listContext): Unit = super.exitInstantiation_list(ctx)

  override def enterInterface_constant_declaration(ctx: Interface_constant_declarationContext): Unit = super.enterInterface_constant_declaration(ctx)

  override def exitInterface_constant_declaration(ctx: Interface_constant_declarationContext): Unit = super.exitInterface_constant_declaration(ctx)

  override def enterInterface_declaration(ctx: Interface_declarationContext): Unit = super.enterInterface_declaration(ctx)

  override def exitInterface_declaration(ctx: Interface_declarationContext): Unit = super.exitInterface_declaration(ctx)

  override def enterInterface_element(ctx: Interface_elementContext): Unit = super.enterInterface_element(ctx)

  override def exitInterface_element(ctx: Interface_elementContext): Unit = super.exitInterface_element(ctx)

  override def enterInterface_file_declaration(ctx: Interface_file_declarationContext): Unit = super.enterInterface_file_declaration(ctx)

  override def exitInterface_file_declaration(ctx: Interface_file_declarationContext): Unit = super.exitInterface_file_declaration(ctx)

  override def enterInterface_signal_list(ctx: Interface_signal_listContext): Unit = super.enterInterface_signal_list(ctx)

  override def exitInterface_signal_list(ctx: Interface_signal_listContext): Unit = super.exitInterface_signal_list(ctx)

  override def enterInterface_port_list(ctx: Interface_port_listContext): Unit = super.enterInterface_port_list(ctx)

  override def exitInterface_port_list(ctx: Interface_port_listContext): Unit = super.exitInterface_port_list(ctx)

  override def enterInterface_list(ctx: Interface_listContext): Unit = super.enterInterface_list(ctx)

  override def exitInterface_list(ctx: Interface_listContext): Unit = super.exitInterface_list(ctx)

  override def enterInterface_quantity_declaration(ctx: Interface_quantity_declarationContext): Unit = super.enterInterface_quantity_declaration(ctx)

  override def exitInterface_quantity_declaration(ctx: Interface_quantity_declarationContext): Unit = super.exitInterface_quantity_declaration(ctx)

  override def enterInterface_port_declaration(ctx: Interface_port_declarationContext): Unit = super.enterInterface_port_declaration(ctx)

  override def exitInterface_port_declaration(ctx: Interface_port_declarationContext): Unit = super.exitInterface_port_declaration(ctx)

  override def enterInterface_signal_declaration(ctx: Interface_signal_declarationContext): Unit = super.enterInterface_signal_declaration(ctx)

  override def exitInterface_signal_declaration(ctx: Interface_signal_declarationContext): Unit = super.exitInterface_signal_declaration(ctx)

  override def enterInterface_terminal_declaration(ctx: Interface_terminal_declarationContext): Unit = super.enterInterface_terminal_declaration(ctx)

  override def exitInterface_terminal_declaration(ctx: Interface_terminal_declarationContext): Unit = super.exitInterface_terminal_declaration(ctx)

  override def enterInterface_variable_declaration(ctx: Interface_variable_declarationContext): Unit = super.enterInterface_variable_declaration(ctx)

  override def exitInterface_variable_declaration(ctx: Interface_variable_declarationContext): Unit = super.exitInterface_variable_declaration(ctx)

  override def enterIteration_scheme(ctx: Iteration_schemeContext): Unit = super.enterIteration_scheme(ctx)

  override def exitIteration_scheme(ctx: Iteration_schemeContext): Unit = super.exitIteration_scheme(ctx)

  override def enterLabel_colon(ctx: Label_colonContext): Unit = super.enterLabel_colon(ctx)

  override def exitLabel_colon(ctx: Label_colonContext): Unit = super.exitLabel_colon(ctx)

  override def enterLibrary_clause(ctx: Library_clauseContext): Unit = super.enterLibrary_clause(ctx)

  override def exitLibrary_clause(ctx: Library_clauseContext): Unit = super.exitLibrary_clause(ctx)

  override def enterLibrary_unit(ctx: Library_unitContext): Unit = super.enterLibrary_unit(ctx)

  override def exitLibrary_unit(ctx: Library_unitContext): Unit = super.exitLibrary_unit(ctx)

  override def enterLiteral(ctx: LiteralContext): Unit = super.enterLiteral(ctx)

  override def exitLiteral(ctx: LiteralContext): Unit = super.exitLiteral(ctx)

  override def enterLogical_name(ctx: Logical_nameContext): Unit = super.enterLogical_name(ctx)

  override def exitLogical_name(ctx: Logical_nameContext): Unit = super.exitLogical_name(ctx)

  override def enterLogical_name_list(ctx: Logical_name_listContext): Unit = super.enterLogical_name_list(ctx)

  override def exitLogical_name_list(ctx: Logical_name_listContext): Unit = super.exitLogical_name_list(ctx)

  override def enterLogical_operator(ctx: Logical_operatorContext): Unit = super.enterLogical_operator(ctx)

  override def exitLogical_operator(ctx: Logical_operatorContext): Unit = super.exitLogical_operator(ctx)

  override def enterLoop_statement(ctx: Loop_statementContext): Unit = super.enterLoop_statement(ctx)

  override def exitLoop_statement(ctx: Loop_statementContext): Unit = super.exitLoop_statement(ctx)

  override def enterSignal_mode(ctx: Signal_modeContext): Unit = super.enterSignal_mode(ctx)

  override def exitSignal_mode(ctx: Signal_modeContext): Unit = super.exitSignal_mode(ctx)

  override def enterMultiplying_operator(ctx: Multiplying_operatorContext): Unit = super.enterMultiplying_operator(ctx)

  override def exitMultiplying_operator(ctx: Multiplying_operatorContext): Unit = super.exitMultiplying_operator(ctx)

  override def enterName(ctx: NameContext): Unit = super.enterName(ctx)

  override def exitName(ctx: NameContext): Unit = super.exitName(ctx)

  override def enterName_part(ctx: Name_partContext): Unit = super.enterName_part(ctx)

  override def exitName_part(ctx: Name_partContext): Unit = super.exitName_part(ctx)

  override def enterName_attribute_part(ctx: Name_attribute_partContext): Unit = super.enterName_attribute_part(ctx)

  override def exitName_attribute_part(ctx: Name_attribute_partContext): Unit = super.exitName_attribute_part(ctx)

  override def enterName_function_call_or_indexed_part(ctx: Name_function_call_or_indexed_partContext): Unit = super.enterName_function_call_or_indexed_part(ctx)

  override def exitName_function_call_or_indexed_part(ctx: Name_function_call_or_indexed_partContext): Unit = super.exitName_function_call_or_indexed_part(ctx)

  override def enterName_slice_part(ctx: Name_slice_partContext): Unit = super.enterName_slice_part(ctx)

  override def exitName_slice_part(ctx: Name_slice_partContext): Unit = super.exitName_slice_part(ctx)

  override def enterSelected_name(ctx: Selected_nameContext): Unit = super.enterSelected_name(ctx)

  override def exitSelected_name(ctx: Selected_nameContext): Unit = super.exitSelected_name(ctx)

  override def enterNature_declaration(ctx: Nature_declarationContext): Unit = super.enterNature_declaration(ctx)

  override def exitNature_declaration(ctx: Nature_declarationContext): Unit = super.exitNature_declaration(ctx)

  override def enterNature_definition(ctx: Nature_definitionContext): Unit = super.enterNature_definition(ctx)

  override def exitNature_definition(ctx: Nature_definitionContext): Unit = super.exitNature_definition(ctx)

  override def enterNature_element_declaration(ctx: Nature_element_declarationContext): Unit = super.enterNature_element_declaration(ctx)

  override def exitNature_element_declaration(ctx: Nature_element_declarationContext): Unit = super.exitNature_element_declaration(ctx)

  override def enterNext_statement(ctx: Next_statementContext): Unit = super.enterNext_statement(ctx)

  override def exitNext_statement(ctx: Next_statementContext): Unit = super.exitNext_statement(ctx)

  override def enterNumeric_literal(ctx: Numeric_literalContext): Unit = super.enterNumeric_literal(ctx)

  override def exitNumeric_literal(ctx: Numeric_literalContext): Unit = super.exitNumeric_literal(ctx)

  override def enterObject_declaration(ctx: Object_declarationContext): Unit = super.enterObject_declaration(ctx)

  override def exitObject_declaration(ctx: Object_declarationContext): Unit = super.exitObject_declaration(ctx)

  override def enterOpts(ctx: OptsContext): Unit = super.enterOpts(ctx)

  override def exitOpts(ctx: OptsContext): Unit = super.exitOpts(ctx)

  override def enterPackage_body(ctx: Package_bodyContext): Unit = super.enterPackage_body(ctx)

  override def exitPackage_body(ctx: Package_bodyContext): Unit = super.exitPackage_body(ctx)

  override def enterPackage_body_declarative_item(ctx: Package_body_declarative_itemContext): Unit = super.enterPackage_body_declarative_item(ctx)

  override def exitPackage_body_declarative_item(ctx: Package_body_declarative_itemContext): Unit = super.exitPackage_body_declarative_item(ctx)

  override def enterPackage_body_declarative_part(ctx: Package_body_declarative_partContext): Unit = super.enterPackage_body_declarative_part(ctx)

  override def exitPackage_body_declarative_part(ctx: Package_body_declarative_partContext): Unit = super.exitPackage_body_declarative_part(ctx)

  override def enterPackage_declaration(ctx: Package_declarationContext): Unit = super.enterPackage_declaration(ctx)

  override def exitPackage_declaration(ctx: Package_declarationContext): Unit = super.exitPackage_declaration(ctx)

  override def enterPackage_declarative_item(ctx: Package_declarative_itemContext): Unit = super.enterPackage_declarative_item(ctx)

  override def exitPackage_declarative_item(ctx: Package_declarative_itemContext): Unit = super.exitPackage_declarative_item(ctx)

  override def enterPackage_declarative_part(ctx: Package_declarative_partContext): Unit = super.enterPackage_declarative_part(ctx)

  override def exitPackage_declarative_part(ctx: Package_declarative_partContext): Unit = super.exitPackage_declarative_part(ctx)

  override def enterParameter_specification(ctx: Parameter_specificationContext): Unit = super.enterParameter_specification(ctx)

  override def exitParameter_specification(ctx: Parameter_specificationContext): Unit = super.exitParameter_specification(ctx)

  override def enterPhysical_literal(ctx: Physical_literalContext): Unit = super.enterPhysical_literal(ctx)

  override def exitPhysical_literal(ctx: Physical_literalContext): Unit = super.exitPhysical_literal(ctx)

  override def enterPhysical_type_definition(ctx: Physical_type_definitionContext): Unit = super.enterPhysical_type_definition(ctx)

  override def exitPhysical_type_definition(ctx: Physical_type_definitionContext): Unit = super.exitPhysical_type_definition(ctx)

  override def enterPort_clause(ctx: Port_clauseContext): Unit = super.enterPort_clause(ctx)

  override def exitPort_clause(ctx: Port_clauseContext): Unit = super.exitPort_clause(ctx)

  override def enterPort_list(ctx: Port_listContext): Unit = super.enterPort_list(ctx)

  override def exitPort_list(ctx: Port_listContext): Unit = super.exitPort_list(ctx)

  override def enterPort_map_aspect(ctx: Port_map_aspectContext): Unit = super.enterPort_map_aspect(ctx)

  override def exitPort_map_aspect(ctx: Port_map_aspectContext): Unit = super.exitPort_map_aspect(ctx)

  override def enterPrimary(ctx: PrimaryContext): Unit = super.enterPrimary(ctx)

  override def exitPrimary(ctx: PrimaryContext): Unit = super.exitPrimary(ctx)

  override def enterPrimary_unit(ctx: Primary_unitContext): Unit = super.enterPrimary_unit(ctx)

  override def exitPrimary_unit(ctx: Primary_unitContext): Unit = super.exitPrimary_unit(ctx)

  override def enterProcedural_declarative_item(ctx: Procedural_declarative_itemContext): Unit = super.enterProcedural_declarative_item(ctx)

  override def exitProcedural_declarative_item(ctx: Procedural_declarative_itemContext): Unit = super.exitProcedural_declarative_item(ctx)

  override def enterProcedural_declarative_part(ctx: Procedural_declarative_partContext): Unit = super.enterProcedural_declarative_part(ctx)

  override def exitProcedural_declarative_part(ctx: Procedural_declarative_partContext): Unit = super.exitProcedural_declarative_part(ctx)

  override def enterProcedural_statement_part(ctx: Procedural_statement_partContext): Unit = super.enterProcedural_statement_part(ctx)

  override def exitProcedural_statement_part(ctx: Procedural_statement_partContext): Unit = super.exitProcedural_statement_part(ctx)

  override def enterProcedure_call(ctx: Procedure_callContext): Unit = super.enterProcedure_call(ctx)

  override def exitProcedure_call(ctx: Procedure_callContext): Unit = super.exitProcedure_call(ctx)

  override def enterProcedure_call_statement(ctx: Procedure_call_statementContext): Unit = super.enterProcedure_call_statement(ctx)

  override def exitProcedure_call_statement(ctx: Procedure_call_statementContext): Unit = super.exitProcedure_call_statement(ctx)

  override def enterProcess_declarative_item(ctx: Process_declarative_itemContext): Unit = super.enterProcess_declarative_item(ctx)

  override def exitProcess_declarative_item(ctx: Process_declarative_itemContext): Unit = super.exitProcess_declarative_item(ctx)

  override def enterProcess_declarative_part(ctx: Process_declarative_partContext): Unit = super.enterProcess_declarative_part(ctx)

  override def exitProcess_declarative_part(ctx: Process_declarative_partContext): Unit = super.exitProcess_declarative_part(ctx)

  override def enterProcess_statement(ctx: Process_statementContext): Unit = super.enterProcess_statement(ctx)

  override def exitProcess_statement(ctx: Process_statementContext): Unit = super.exitProcess_statement(ctx)

  override def enterProcess_statement_part(ctx: Process_statement_partContext): Unit = super.enterProcess_statement_part(ctx)

  override def exitProcess_statement_part(ctx: Process_statement_partContext): Unit = super.exitProcess_statement_part(ctx)

  override def enterQualified_expression(ctx: Qualified_expressionContext): Unit = super.enterQualified_expression(ctx)

  override def exitQualified_expression(ctx: Qualified_expressionContext): Unit = super.exitQualified_expression(ctx)

  override def enterQuantity_declaration(ctx: Quantity_declarationContext): Unit = super.enterQuantity_declaration(ctx)

  override def exitQuantity_declaration(ctx: Quantity_declarationContext): Unit = super.exitQuantity_declaration(ctx)

  override def enterQuantity_list(ctx: Quantity_listContext): Unit = super.enterQuantity_list(ctx)

  override def exitQuantity_list(ctx: Quantity_listContext): Unit = super.exitQuantity_list(ctx)

  override def enterQuantity_specification(ctx: Quantity_specificationContext): Unit = super.enterQuantity_specification(ctx)

  override def exitQuantity_specification(ctx: Quantity_specificationContext): Unit = super.exitQuantity_specification(ctx)

  override def enterRange(ctx: RangeContext): Unit = super.enterRange(ctx)

  override def exitRange(ctx: RangeContext): Unit = super.exitRange(ctx)

  override def enterExplicit_range(ctx: Explicit_rangeContext): Unit = super.enterExplicit_range(ctx)

  override def exitExplicit_range(ctx: Explicit_rangeContext): Unit = super.exitExplicit_range(ctx)

  override def enterRange_constraint(ctx: Range_constraintContext): Unit = super.enterRange_constraint(ctx)

  override def exitRange_constraint(ctx: Range_constraintContext): Unit = super.exitRange_constraint(ctx)

  override def enterRecord_nature_definition(ctx: Record_nature_definitionContext): Unit = super.enterRecord_nature_definition(ctx)

  override def exitRecord_nature_definition(ctx: Record_nature_definitionContext): Unit = super.exitRecord_nature_definition(ctx)

  override def enterRecord_type_definition(ctx: Record_type_definitionContext): Unit = super.enterRecord_type_definition(ctx)

  override def exitRecord_type_definition(ctx: Record_type_definitionContext): Unit = super.exitRecord_type_definition(ctx)

  override def enterRelation(ctx: RelationContext): Unit = super.enterRelation(ctx)

  override def exitRelation(ctx: RelationContext): Unit = super.exitRelation(ctx)

  override def enterRelational_operator(ctx: Relational_operatorContext): Unit = super.enterRelational_operator(ctx)

  override def exitRelational_operator(ctx: Relational_operatorContext): Unit = super.exitRelational_operator(ctx)

  override def enterReport_statement(ctx: Report_statementContext): Unit = super.enterReport_statement(ctx)

  override def exitReport_statement(ctx: Report_statementContext): Unit = super.exitReport_statement(ctx)

  override def enterReturn_statement(ctx: Return_statementContext): Unit = super.enterReturn_statement(ctx)

  override def exitReturn_statement(ctx: Return_statementContext): Unit = super.exitReturn_statement(ctx)

  override def enterScalar_nature_definition(ctx: Scalar_nature_definitionContext): Unit = super.enterScalar_nature_definition(ctx)

  override def exitScalar_nature_definition(ctx: Scalar_nature_definitionContext): Unit = super.exitScalar_nature_definition(ctx)

  override def enterScalar_type_definition(ctx: Scalar_type_definitionContext): Unit = super.enterScalar_type_definition(ctx)

  override def exitScalar_type_definition(ctx: Scalar_type_definitionContext): Unit = super.exitScalar_type_definition(ctx)

  override def enterSecondary_unit(ctx: Secondary_unitContext): Unit = super.enterSecondary_unit(ctx)

  override def exitSecondary_unit(ctx: Secondary_unitContext): Unit = super.exitSecondary_unit(ctx)

  override def enterSecondary_unit_declaration(ctx: Secondary_unit_declarationContext): Unit = super.enterSecondary_unit_declaration(ctx)

  override def exitSecondary_unit_declaration(ctx: Secondary_unit_declarationContext): Unit = super.exitSecondary_unit_declaration(ctx)

  override def enterSelected_signal_assignment(ctx: Selected_signal_assignmentContext): Unit = super.enterSelected_signal_assignment(ctx)

  override def exitSelected_signal_assignment(ctx: Selected_signal_assignmentContext): Unit = super.exitSelected_signal_assignment(ctx)

  override def enterSelected_waveforms(ctx: Selected_waveformsContext): Unit = super.enterSelected_waveforms(ctx)

  override def exitSelected_waveforms(ctx: Selected_waveformsContext): Unit = super.exitSelected_waveforms(ctx)

  override def enterSensitivity_clause(ctx: Sensitivity_clauseContext): Unit = super.enterSensitivity_clause(ctx)

  override def exitSensitivity_clause(ctx: Sensitivity_clauseContext): Unit = super.exitSensitivity_clause(ctx)

  override def enterSensitivity_list(ctx: Sensitivity_listContext): Unit = super.enterSensitivity_list(ctx)

  override def exitSensitivity_list(ctx: Sensitivity_listContext): Unit = super.exitSensitivity_list(ctx)

  override def enterSequence_of_statements(ctx: Sequence_of_statementsContext): Unit = super.enterSequence_of_statements(ctx)

  override def exitSequence_of_statements(ctx: Sequence_of_statementsContext): Unit = super.exitSequence_of_statements(ctx)

  override def enterSequential_statement(ctx: Sequential_statementContext): Unit = super.enterSequential_statement(ctx)

  override def exitSequential_statement(ctx: Sequential_statementContext): Unit = super.exitSequential_statement(ctx)

  override def enterShift_expression(ctx: Shift_expressionContext): Unit = super.enterShift_expression(ctx)

  override def exitShift_expression(ctx: Shift_expressionContext): Unit = super.exitShift_expression(ctx)

  override def enterShift_operator(ctx: Shift_operatorContext): Unit = super.enterShift_operator(ctx)

  override def exitShift_operator(ctx: Shift_operatorContext): Unit = super.exitShift_operator(ctx)

  override def enterSignal_assignment_statement(ctx: Signal_assignment_statementContext): Unit = super.enterSignal_assignment_statement(ctx)

  override def exitSignal_assignment_statement(ctx: Signal_assignment_statementContext): Unit = super.exitSignal_assignment_statement(ctx)

  override def enterSignal_declaration(ctx: Signal_declarationContext): Unit = super.enterSignal_declaration(ctx)

  override def exitSignal_declaration(ctx: Signal_declarationContext): Unit = super.exitSignal_declaration(ctx)

  override def enterSignal_kind(ctx: Signal_kindContext): Unit = super.enterSignal_kind(ctx)

  override def exitSignal_kind(ctx: Signal_kindContext): Unit = super.exitSignal_kind(ctx)

  override def enterSignal_list(ctx: Signal_listContext): Unit = super.enterSignal_list(ctx)

  override def exitSignal_list(ctx: Signal_listContext): Unit = super.exitSignal_list(ctx)

  override def enterSignature(ctx: SignatureContext): Unit = super.enterSignature(ctx)

  override def exitSignature(ctx: SignatureContext): Unit = super.exitSignature(ctx)

  override def enterSimple_expression(ctx: Simple_expressionContext): Unit = super.enterSimple_expression(ctx)

  override def exitSimple_expression(ctx: Simple_expressionContext): Unit = super.exitSimple_expression(ctx)

  override def enterSimple_simultaneous_statement(ctx: Simple_simultaneous_statementContext): Unit = super.enterSimple_simultaneous_statement(ctx)

  override def exitSimple_simultaneous_statement(ctx: Simple_simultaneous_statementContext): Unit = super.exitSimple_simultaneous_statement(ctx)

  override def enterSimultaneous_alternative(ctx: Simultaneous_alternativeContext): Unit = super.enterSimultaneous_alternative(ctx)

  override def exitSimultaneous_alternative(ctx: Simultaneous_alternativeContext): Unit = super.exitSimultaneous_alternative(ctx)

  override def enterSimultaneous_case_statement(ctx: Simultaneous_case_statementContext): Unit = super.enterSimultaneous_case_statement(ctx)

  override def exitSimultaneous_case_statement(ctx: Simultaneous_case_statementContext): Unit = super.exitSimultaneous_case_statement(ctx)

  override def enterSimultaneous_if_statement(ctx: Simultaneous_if_statementContext): Unit = super.enterSimultaneous_if_statement(ctx)

  override def exitSimultaneous_if_statement(ctx: Simultaneous_if_statementContext): Unit = super.exitSimultaneous_if_statement(ctx)

  override def enterSimultaneous_procedural_statement(ctx: Simultaneous_procedural_statementContext): Unit = super.enterSimultaneous_procedural_statement(ctx)

  override def exitSimultaneous_procedural_statement(ctx: Simultaneous_procedural_statementContext): Unit = super.exitSimultaneous_procedural_statement(ctx)

  override def enterSimultaneous_statement(ctx: Simultaneous_statementContext): Unit = super.enterSimultaneous_statement(ctx)

  override def exitSimultaneous_statement(ctx: Simultaneous_statementContext): Unit = super.exitSimultaneous_statement(ctx)

  override def enterSimultaneous_statement_part(ctx: Simultaneous_statement_partContext): Unit = super.enterSimultaneous_statement_part(ctx)

  override def exitSimultaneous_statement_part(ctx: Simultaneous_statement_partContext): Unit = super.exitSimultaneous_statement_part(ctx)

  override def enterSource_aspect(ctx: Source_aspectContext): Unit = super.enterSource_aspect(ctx)

  override def exitSource_aspect(ctx: Source_aspectContext): Unit = super.exitSource_aspect(ctx)

  override def enterSource_quantity_declaration(ctx: Source_quantity_declarationContext): Unit = super.enterSource_quantity_declaration(ctx)

  override def exitSource_quantity_declaration(ctx: Source_quantity_declarationContext): Unit = super.exitSource_quantity_declaration(ctx)

  override def enterStep_limit_specification(ctx: Step_limit_specificationContext): Unit = super.enterStep_limit_specification(ctx)

  override def exitStep_limit_specification(ctx: Step_limit_specificationContext): Unit = super.exitStep_limit_specification(ctx)

  override def enterSubnature_declaration(ctx: Subnature_declarationContext): Unit = super.enterSubnature_declaration(ctx)

  override def exitSubnature_declaration(ctx: Subnature_declarationContext): Unit = super.exitSubnature_declaration(ctx)

  override def enterSubnature_indication(ctx: Subnature_indicationContext): Unit = super.enterSubnature_indication(ctx)

  override def exitSubnature_indication(ctx: Subnature_indicationContext): Unit = super.exitSubnature_indication(ctx)

  override def enterSubprogram_body(ctx: Subprogram_bodyContext): Unit = super.enterSubprogram_body(ctx)

  override def exitSubprogram_body(ctx: Subprogram_bodyContext): Unit = super.exitSubprogram_body(ctx)

  override def enterSubprogram_declaration(ctx: Subprogram_declarationContext): Unit = super.enterSubprogram_declaration(ctx)

  override def exitSubprogram_declaration(ctx: Subprogram_declarationContext): Unit = super.exitSubprogram_declaration(ctx)

  override def enterSubprogram_declarative_item(ctx: Subprogram_declarative_itemContext): Unit = super.enterSubprogram_declarative_item(ctx)

  override def exitSubprogram_declarative_item(ctx: Subprogram_declarative_itemContext): Unit = super.exitSubprogram_declarative_item(ctx)

  override def enterSubprogram_declarative_part(ctx: Subprogram_declarative_partContext): Unit = super.enterSubprogram_declarative_part(ctx)

  override def exitSubprogram_declarative_part(ctx: Subprogram_declarative_partContext): Unit = super.exitSubprogram_declarative_part(ctx)

  override def enterSubprogram_kind(ctx: Subprogram_kindContext): Unit = super.enterSubprogram_kind(ctx)

  override def exitSubprogram_kind(ctx: Subprogram_kindContext): Unit = super.exitSubprogram_kind(ctx)

  override def enterSubprogram_specification(ctx: Subprogram_specificationContext): Unit = super.enterSubprogram_specification(ctx)

  override def exitSubprogram_specification(ctx: Subprogram_specificationContext): Unit = super.exitSubprogram_specification(ctx)

  override def enterProcedure_specification(ctx: Procedure_specificationContext): Unit = super.enterProcedure_specification(ctx)

  override def exitProcedure_specification(ctx: Procedure_specificationContext): Unit = super.exitProcedure_specification(ctx)

  override def enterFunction_specification(ctx: Function_specificationContext): Unit = super.enterFunction_specification(ctx)

  override def exitFunction_specification(ctx: Function_specificationContext): Unit = super.exitFunction_specification(ctx)

  override def enterSubprogram_statement_part(ctx: Subprogram_statement_partContext): Unit = super.enterSubprogram_statement_part(ctx)

  override def exitSubprogram_statement_part(ctx: Subprogram_statement_partContext): Unit = super.exitSubprogram_statement_part(ctx)

  override def enterSubtype_declaration(ctx: Subtype_declarationContext): Unit = super.enterSubtype_declaration(ctx)

  override def exitSubtype_declaration(ctx: Subtype_declarationContext): Unit = super.exitSubtype_declaration(ctx)

  override def enterSubtype_indication(ctx: Subtype_indicationContext): Unit = super.enterSubtype_indication(ctx)

  override def exitSubtype_indication(ctx: Subtype_indicationContext): Unit = super.exitSubtype_indication(ctx)

  override def enterSuffix(ctx: SuffixContext): Unit = super.enterSuffix(ctx)

  override def exitSuffix(ctx: SuffixContext): Unit = super.exitSuffix(ctx)

  override def enterTarget(ctx: TargetContext): Unit = super.enterTarget(ctx)

  override def exitTarget(ctx: TargetContext): Unit = super.exitTarget(ctx)

  override def enterTerm(ctx: TermContext): Unit = super.enterTerm(ctx)

  override def exitTerm(ctx: TermContext): Unit = super.exitTerm(ctx)

  override def enterTerminal_aspect(ctx: Terminal_aspectContext): Unit = super.enterTerminal_aspect(ctx)

  override def exitTerminal_aspect(ctx: Terminal_aspectContext): Unit = super.exitTerminal_aspect(ctx)

  override def enterTerminal_declaration(ctx: Terminal_declarationContext): Unit = super.enterTerminal_declaration(ctx)

  override def exitTerminal_declaration(ctx: Terminal_declarationContext): Unit = super.exitTerminal_declaration(ctx)

  override def enterThrough_aspect(ctx: Through_aspectContext): Unit = super.enterThrough_aspect(ctx)

  override def exitThrough_aspect(ctx: Through_aspectContext): Unit = super.exitThrough_aspect(ctx)

  override def enterTimeout_clause(ctx: Timeout_clauseContext): Unit = super.enterTimeout_clause(ctx)

  override def exitTimeout_clause(ctx: Timeout_clauseContext): Unit = super.exitTimeout_clause(ctx)

  override def enterTolerance_aspect(ctx: Tolerance_aspectContext): Unit = super.enterTolerance_aspect(ctx)

  override def exitTolerance_aspect(ctx: Tolerance_aspectContext): Unit = super.exitTolerance_aspect(ctx)

  override def enterType_declaration(ctx: Type_declarationContext): Unit = super.enterType_declaration(ctx)

  override def exitType_declaration(ctx: Type_declarationContext): Unit = super.exitType_declaration(ctx)

  override def enterType_definition(ctx: Type_definitionContext): Unit = super.enterType_definition(ctx)

  override def exitType_definition(ctx: Type_definitionContext): Unit = super.exitType_definition(ctx)

  override def enterUnconstrained_array_definition(ctx: Unconstrained_array_definitionContext): Unit = super.enterUnconstrained_array_definition(ctx)

  override def exitUnconstrained_array_definition(ctx: Unconstrained_array_definitionContext): Unit = super.exitUnconstrained_array_definition(ctx)

  override def enterUnconstrained_nature_definition(ctx: Unconstrained_nature_definitionContext): Unit = super.enterUnconstrained_nature_definition(ctx)

  override def exitUnconstrained_nature_definition(ctx: Unconstrained_nature_definitionContext): Unit = super.exitUnconstrained_nature_definition(ctx)

  override def enterUse_clause(ctx: Use_clauseContext): Unit = super.enterUse_clause(ctx)

  override def exitUse_clause(ctx: Use_clauseContext): Unit = super.exitUse_clause(ctx)

  override def enterVariable_assignment_statement(ctx: Variable_assignment_statementContext): Unit = super.enterVariable_assignment_statement(ctx)

  override def exitVariable_assignment_statement(ctx: Variable_assignment_statementContext): Unit = super.exitVariable_assignment_statement(ctx)

  override def enterVariable_declaration(ctx: Variable_declarationContext): Unit = super.enterVariable_declaration(ctx)

  override def exitVariable_declaration(ctx: Variable_declarationContext): Unit = super.exitVariable_declaration(ctx)

  override def enterWait_statement(ctx: Wait_statementContext): Unit = super.enterWait_statement(ctx)

  override def exitWait_statement(ctx: Wait_statementContext): Unit = super.exitWait_statement(ctx)

  override def enterWaveform(ctx: WaveformContext): Unit = super.enterWaveform(ctx)

  override def exitWaveform(ctx: WaveformContext): Unit = super.exitWaveform(ctx)

  override def enterWaveform_element(ctx: Waveform_elementContext): Unit = super.enterWaveform_element(ctx)

  override def exitWaveform_element(ctx: Waveform_elementContext): Unit = super.exitWaveform_element(ctx)

  override def enterEveryRule(ctx: ParserRuleContext): Unit = super.enterEveryRule(ctx)

  override def exitEveryRule(ctx: ParserRuleContext): Unit = super.exitEveryRule(ctx)

  override def visitTerminal(node: TerminalNode): Unit = super.visitTerminal(node)

  override def visitErrorNode(node: ErrorNode): Unit = super.visitErrorNode(node)
}
