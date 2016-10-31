package core.vhdlsyntax

import core._
import core.isabellesyntax._
import sg.edu.ntu.vhdl2isabelle.VHDLParser._
import scala.collection.JavaConversions._

/**
  * Created by Hongxu Chen.
  */
case class VLabelColon(id: String)

object VLabelColon {
  def apply(ctx: Label_colonContext): VLabelColon = {
    VLabelColon(ctx.identifier().getText)
  }
}

//********************************************************************************************************************//

import core.V2IUtils._

case class VConstantDeclaration(idList: List[String], subtypeIndication: VSubtypeIndication, vExp: Option[VExpression]) extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VConstantDeclaration {
  def apply(ctx: Constant_declarationContext): VConstantDeclaration = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExpression(_))
    VConstantDeclaration(idList, subtypeIndication, vExp)
  }
}


//********************************************************************************************************************//

case class VSignalDeclaration(idList: List[String], subtypeInd: VSubtypeIndication,
                              signalKind: Option[String], exp: Option[VExpression]) extends VBlockDeclarativeItem

object VSignalDeclaration {
  def apply(ctx: Signal_declarationContext): VSignalDeclaration = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeIndication(ctx.subtype_indication())
    val signalKind = Option(ctx.signal_kind()).map(_.getText.toLowerCase)
    val exp = Option(ctx.expression()).map(VExpression(_))
    VSignalDeclaration(idList, subtypeInd, signalKind, exp)
  }
}

//********************************************************************************************************************//

sealed abstract class VInterfaceDeclaration extends VInterfaceElement {
  val idList: List[String]
  val vSubtypeIndication: VSubtypeIndication
}

object VInterfaceDeclaration {
  def apply(ctx: Interface_declarationContext): VInterfaceDeclaration = {
    if (ctx.interface_constant_declaration() != null) VInterfaceConstantDeclaration(ctx.interface_constant_declaration())
    else if (ctx.interface_signal_declaration() != null) VInterfaceSignalDeclaration(ctx.interface_signal_declaration())
    else if (ctx.interface_variable_declaration() != null) VInterfaceVariableDeclaration(ctx.interface_variable_declaration())
    else if (ctx.interface_file_declaration() != null) VInterfaceFileDeclaration(ctx.interface_file_declaration())
    else if (ctx.interface_terminal_declaration() != null) VInterfaceTerminalDeclaration(ctx.interface_terminal_declaration())
    else VInterfaceQuantityDeclaration(ctx.interface_quantity_declaration())
  }
}

case class VInterfaceConstantDeclaration(idList: List[String], vSubtypeIndication: VSubtypeIndication, vExp: Option[VExpression]) extends VInterfaceDeclaration

object VInterfaceConstantDeclaration {
  def apply(ctx: Interface_constant_declarationContext): VInterfaceConstantDeclaration = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExpression(_))
    VInterfaceConstantDeclaration(idList, subtypeIndication, vExp)
  }
}

case class VInterfaceSignalDeclaration(idList: List[String], vSubtypeIndication: VSubtypeIndication, vExpressionOption: Option[VExpression]) extends VInterfaceDeclaration

object VInterfaceSignalDeclaration {
  def apply(ctx: Interface_signal_declarationContext): VInterfaceSignalDeclaration = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExpressionOption = Option(ctx.expression()).map(VExpression(_))
    VInterfaceSignalDeclaration(idList, subtypeIndication, vExpressionOption)
  }
}

case class VInterfaceVariableDeclaration(idList: List[String], signalMode: VSignalMode.Value, vSubtypeIndication: VSubtypeIndication, vExpressionOption: Option[VExpression]) extends VInterfaceDeclaration

object VInterfaceVariableDeclaration {
  def apply(ctx: Interface_variable_declarationContext): VInterfaceVariableDeclaration = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val signalMode = VSignalMode(ctx.signal_mode())
    val vExpressionOption = Option(ctx.expression()).map(VExpression(_))
    VInterfaceVariableDeclaration(idList, signalMode, subtypeIndication, vExpressionOption)
  }
}

case class VInterfaceFileDeclaration(idList: List[String], vSubtypeIndication: VSubtypeIndication) extends VInterfaceDeclaration

object VInterfaceFileDeclaration {
  def apply(ctx: Interface_file_declarationContext): VInterfaceFileDeclaration = ???
}

case class VInterfaceTerminalDeclaration(idList: List[String], vSubtypeIndication: VSubtypeIndication) extends VInterfaceDeclaration

object VInterfaceTerminalDeclaration {
  def apply(ctx: Interface_terminal_declarationContext): VInterfaceTerminalDeclaration = ???
}

case class VInterfaceQuantityDeclaration (idList: List[String], vSubtypeIndication: VSubtypeIndication) extends VInterfaceDeclaration

object VInterfaceQuantityDeclaration {
  def apply(ctx: Interface_quantity_declarationContext): VInterfaceQuantityDeclaration = ???
}

case class VInterfacePortDeclaration(idList: List[String], mode: String,
                                     subtypeInd: VSubtypeIndication, vExp: Option[VExpression])

object VInterfacePortDeclaration {
  def apply(ctx: Interface_port_declarationContext): VInterfacePortDeclaration = {
    val idList = getIdList(ctx.identifier_list())
    val mode = ctx.signal_mode().getText
    val subtypeInd = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExpression(_))
    VInterfacePortDeclaration(idList, mode, subtypeInd, vExp)
  }
}

object VSignalMode extends Enumeration {
  type VSignalMode = Value
  val IN = Value
  val OUT = Value
  val INOUT = Value
  val BUFFER = Value
  val LINKAGE = Value

  def apply(ctx: Signal_modeContext): VSignalMode.Value = {
    if (ctx.IN() != null) VSignalMode.IN
    else if (ctx.OUT() != null) VSignalMode.OUT
    else if (ctx.INOUT() != null) VSignalMode.INOUT
    else if (ctx.BUFFER() != null) VSignalMode.BUFFER
    else VSignalMode.LINKAGE
  }
}

//********************************************************************************************************************//

abstract class VTarget {

  def getInfo(defInfo: DefInfo): (VSelectedName, Option[IDiscrete_range]) = this match {
    case name: VName => name.getVSelectedName(defInfo)
    case aggregate: VAggregate => handler(s"${toString}")
  }

  def lhs_V_IDef(defInfo: DefInfo): V_IDef = {
    val (selectedName, _) = getInfo(defInfo)
    defInfo.getVDef(selectedName)
  }

  def lhs_SP_IDef(defInfo: DefInfo): SP_IDef = {
    val (selectedName, _) = getInfo(defInfo)
    defInfo.getSPDef(selectedName)
  }

  def toISp_clhs(defInfo: DefInfo): ISp_clhs = {
    val (selectedName, discreteRangeOption) = getInfo(defInfo)
    val iDef = defInfo.getSPDef(selectedName)
    ISp_clhs(iDef, selectedName, discreteRangeOption)
  }

  def toIV_clhs(defInfo: DefInfo): IV_clhs = {
    val (selectedName, discreteRangeOption) = getInfo(defInfo)
    val iDef = defInfo.getVDef(selectedName)
    IV_clhs(iDef, selectedName, discreteRangeOption)
  }

}

object VTarget {
  def apply(ctx: TargetContext): VTarget = {
    if (ctx.name() != null) VName(ctx.name())
    else if (ctx.aggregate() != null) VAggregate(ctx.aggregate())
    else throw VIError
  }
}

//********************************************************************************************************************//

sealed abstract class VDelay

object VDelay {
  def apply(ctx: Delay_mechanismContext): VDelay = {
    val transport = ctx.TRANSPORT()
    val inertial = ctx.INERTIAL()
    if (transport != null) {
      VDelayT
    } else if (inertial != null) {
      val exp = Option(ctx.expression()).map(VExpression(_))
      VDelayE(exp)
    } else throw VIError
  }
}

object VDelayT extends VDelay

case class VDelayE(vExp: Option[VExpression]) extends VDelay

case class VOpts(guarded: Boolean, delay: Option[VDelay])

object VOpts {
  def apply(ctx: OptsContext): VOpts = {
    val guarded = ctx.GUARDED() != null
    val delay = Option(ctx.delay_mechanism()).map(VDelay(_))
    VOpts(guarded, delay)
  }
}

//********************************************************************************************************************//
/**
  * 2nd exp is "after", not useful?
  */
case class VWaveFormElem(exp: VExpression, expOption: Option[VExpression])

object VWaveFormElem {
  def apply(ctx: Waveform_elementContext): VWaveFormElem = {
    val exprs = ctx.expression().map(VExpression(_))
    val exp = exprs.head
    val expOption = exprs.lift(1)
    VWaveFormElem(exp, expOption)
  }
}

sealed abstract class VWaveForm {

  def getSpecialVExp: VExpression = this match {
    case e: VWaveFormE => e.elems.head.exp
    case VWaveFormU => handler(s"${VWaveFormU}")
  }

  def getSpecialIExp(defInfo: DefInfo): IExpression = getSpecialVExp.toIExp(defInfo)
}

object VWaveForm {
  def apply(ctx: WaveformContext): VWaveForm = {
    val unaffected = ctx.UNAFFECTED()
    val waveFormElemList = ctx.waveform_element()
    if (unaffected != null) {
      VWaveFormU
    } else if (waveFormElemList != null) {
      VWaveFormE(waveFormElemList.map(VWaveFormElem(_)).toList)
    } else throw VIError
  }
}

case class VWaveFormE(elems: List[VWaveFormElem]) extends VWaveForm {
  require(elems.nonEmpty, "VWaveFormE")
}

case object VWaveFormU extends VWaveForm

case class VCondWaveForms(whenWaveForm: VWaveForm, cond: Option[VExpression], elseCond: Option[VCondWaveForms]) {
  // NOTE: isar definition has many limitations, but may be enough
  def toI(defInfo: DefInfo)(lhs_idef:IDef): (List[As_when], IAsmt_rhs) = {
    // whenWaveForm => as_whenList.head.crhs, cond => as_whenList.head.IExp
    // elseCond build the as_whenList(1) (last one)
    import VCondWaveForms.genAsWhen
    val as_when1 = genAsWhen(whenWaveForm, cond)(defInfo)
    val as_when2 = elseCond match {
      case Some(waveForms) => genAsWhen(waveForms.whenWaveForm, waveForms.cond)(defInfo)
      case None => handler(s"${toString}")
    }
    val as_whenList = List(as_when1, as_when2)
    val else_asmt_rhs: IAsmt_rhs = elseCond match {
      case Some(VCondWaveForms(_, _, finalElseOpt)) => finalElseOpt match {
        case Some(finalElse) => {
          val vExpression = finalElse.whenWaveForm.getSpecialVExp
          // TODO check whether consider "OTHERS"
          val iExpression = vExpression.toIExp(defInfo)
          IAsmt_rhs(iExpression)
        }
        case None => handler(s"${toString}")
      }
      case None => handler(s"${toString}")
    }
    (as_whenList, else_asmt_rhs)
  }
}

object VCondWaveForms {
  def apply(ctx: Conditional_waveformsContext): VCondWaveForms = {
    val waveForm = VWaveForm(ctx.waveform())
    val cond = Option(ctx.condition()).map(c => VExpression(c.expression()))
    val condWaves = Option(ctx.conditional_waveforms()).map(VCondWaveForms(_))
    VCondWaveForms(waveForm, cond, condWaves)
  }

  def genAsWhen(waveForm: VWaveForm, cond: Option[VExpression])(defInfo: DefInfo): As_when = {
    val when_asmt_rhs: IAsmt_rhs = {
      val exp = waveForm.getSpecialVExp
      // TODO check whether consider "OTHERS"
      IAsmt_rhs(exp.toIExp(defInfo))
    }
    val when_cond = cond match {
      case Some(c) => c.toIExp(defInfo)
      case None => handler(s"${toString}")
    }
    As_when(when_asmt_rhs, when_cond)
  }

}

//********************************************************************************************************************//

case class VSelectedWaveForm(waveForm: VWaveForm, choices: VChoices,
                             waveFormOpt: Option[VWaveForm], choicesOpt: Option[VChoices])

object VSelectedWaveForm {
  def apply(ctx: Selected_waveformsContext): VSelectedWaveForm = {
    val waveFormList = ctx.waveform().map(VWaveForm(_))
    val choicesList = ctx.choices().map(VChoices(_))
    VSelectedWaveForm(waveFormList.head, choicesList.head, waveFormList.lift(1), choicesList.lift(1))
  }
}

case class VSelectedSignalAssignment(exp: VExpression, target: VTarget, opts: VOpts, selectedWaveForm: VSelectedWaveForm)

object VSelectedSignalAssignment {
  def apply(ctx: Selected_signal_assignmentContext): VSelectedSignalAssignment = {
    val exp = VExpression(ctx.expression())
    val target = VTarget(ctx.target())
    val opts = VOpts(ctx.opts())
    val selectedWaveForm = VSelectedWaveForm(ctx.selected_waveforms())
    VSelectedSignalAssignment(exp, target, opts, selectedWaveForm)
  }
}

//********************************************************************************************************************//

case class VConditionalSignalAssignment(vTarget: VTarget, opts: VOpts, conditionalWaveforms: VCondWaveForms) {
  def toI(defInfo: DefInfo): (ISp_clhs, List[As_when], IAsmt_rhs) = {
    val sp_chls = vTarget.toISp_clhs(defInfo)
    val lhs_idef = vTarget.lhs_SP_IDef(defInfo)
    val (as_whenList, iAsmt_rhs) = conditionalWaveforms.toI(defInfo)(lhs_idef)
    (sp_chls, as_whenList, iAsmt_rhs)
  }
}

object VConditionalSignalAssignment {
  def apply(ctx: Conditional_signal_assignmentContext): VConditionalSignalAssignment = {
    val target = VTarget(ctx.target())
    val opts = VOpts(ctx.opts())
    val condWaveForms = VCondWaveForms(ctx.conditional_waveforms())
    VConditionalSignalAssignment(target, opts, condWaveForms)
  }
}

//********************************************************************************************************************//

sealed abstract class VConcurrentSignalAssignmentStatement {
  def toI(defInfo: DefInfo): IConc_stmt_complex_Csc_ca
}

object VConcurrentSignalAssignmentStatement {
  def apply(ctx: Concurrent_signal_assignment_statementContext): VConcurrentSignalAssignmentStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val isPostponded = ctx.POSTPONED() != null
    if (ctx.conditional_signal_assignment() != null) {
      VConcurrentSignalAssignmentStatementConditional(labelColon, isPostponded, VConditionalSignalAssignment(ctx.conditional_signal_assignment()))
    } else if (ctx.selected_signal_assignment() != null) {
      VConcurrentSignalAssignmentStatementSelected(labelColon, isPostponded, VSelectedSignalAssignment(ctx.selected_signal_assignment()))
    } else throw VIError
  }
}

case class VConcurrentSignalAssignmentStatementConditional(labelColon: Option[String],
                                                           isPostponded: Boolean,
                                                           conditionalSignalAssignment: VConditionalSignalAssignment) extends VConcurrentSignalAssignmentStatement {
  def toI(defInfo: DefInfo): IConc_stmt_complex_Csc_ca = {
    val id = labelColon.getOrElse(defaultId)
    val (sp_clhs, casmt_rhsList, iAsmt_rhs) = conditionalSignalAssignment.toI(defInfo)
    IConc_stmt_complex_Csc_ca(id, sp_clhs, casmt_rhsList, iAsmt_rhs)
  }
}

case class VConcurrentSignalAssignmentStatementSelected(labelColon: Option[String],
                                                        postPonded: Boolean,
                                                        selectSignalAssign: VSelectedSignalAssignment) extends VConcurrentSignalAssignmentStatement {
  override def toI(defInfo: DefInfo): IConc_stmt_complex_Csc_ca = ???
}


//********************************************************************************************************************//

case class VVariableDeclaration(shared: Boolean, idList: List[String],
                                subtypeInd: VSubtypeIndication, vExp: Option[VExpression]) extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VVariableDeclaration {
  def apply(ctx: Variable_declarationContext): VVariableDeclaration = {
    val shared = ctx.SHARED() != null
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExpression(_))
    VVariableDeclaration(shared, idList, subtypeInd, vExp)
  }
}

//********************************************************************************************************************//

case class VSubprogramDeclaration(vSubProgSpec: VSubprogramSpecification) extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VSubprogramDeclaration {
  def apply(ctx: Subprogram_declarationContext): VSubprogramDeclaration = {
    val spec = ctx.subprogram_specification()
    VSubprogramDeclaration(VSubprogramSpecification(spec))
  }
}

//********************************************************************************************************************//

// TODO: Put this somewhere else in the order
case class VDesignator(id: String)

object VDesignator {
  def apply(ctx: DesignatorContext): VDesignator = {
    if (ctx.identifier() != null) VDesignator(ctx.identifier().getText)
    else VDesignator(ctx.STRING_LITERAL().getText)
  }
}

sealed abstract class VFormalParameterList

object VFormalParameterList {
  def apply(ctx: Formal_parameter_listContext): VFormalParameterList = {
    VInterfaceList(ctx.interface_list())
  }
}

case class VInterfaceList(interfaceElementList : List[VInterfaceElement]) extends VFormalParameterList

object VInterfaceList {
  def apply(ctx: Interface_listContext): VInterfaceList = {
    VInterfaceList(ctx.interface_element().map(interfaceElementCtx => VInterfaceElement(interfaceElementCtx)).toList)
  }
}

sealed abstract class VInterfaceElement

object VInterfaceElement {
  def apply(ctx: Interface_elementContext): VInterfaceElement = {
    VInterfaceDeclaration(ctx.interface_declaration())
  }
}

//********************************************************************************************************************//

sealed abstract class VSubprogramSpecification {
  val designator: VDesignator
  val formalParameterList: VFormalParameterList
  def getInterfaceElementList(): List[VInterfaceElement] = {
    formalParameterList.asInstanceOf[VInterfaceList].interfaceElementList
  }
}

object VSubprogramSpecification {
  def apply(ctx: Subprogram_specificationContext): VSubprogramSpecification = {
    if (ctx.function_specification() != null) VFunctionSpecification(ctx.function_specification())
    else VProcedureSpecification(ctx.procedure_specification())
  }
}

case class VProcedureSpecification(designator: VDesignator, formalParameterList: VFormalParameterList) extends VSubprogramSpecification

object VProcedureSpecification {
  def apply(ctx: Procedure_specificationContext): VProcedureSpecification = {
    val designator = VDesignator(ctx.designator())
    val formalParameterList = VFormalParameterList(ctx.formal_parameter_list())
    VProcedureSpecification(designator, formalParameterList)
  }
}

case class VFunctionSpecification(subtypeIndication : VSubtypeIndication, designator: VDesignator, formalParameterList : VFormalParameterList) extends VSubprogramSpecification

object VFunctionSpecification {
  def apply(ctx: Function_specificationContext): VFunctionSpecification = {
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val designator = VDesignator(ctx.designator())
    val formalParameterList = VFormalParameterList(ctx.formal_parameter_list())
    VFunctionSpecification(subtypeIndication, designator, formalParameterList)
  }
}

//********************************************************************************************************************//

case class VSubprogramStatementPart(sequentialStatementList: List[VSequentialStatement])

object VSubprogramStatementPart {
  def apply(ctx: Subprogram_statement_partContext): VSubprogramStatementPart ={
    VSubprogramStatementPart(ctx.sequential_statement().map(sequentialStatementCtx => VSequentialStatement(sequentialStatementCtx)).toList)
  }
}

case class VSubprogramBody(subprogramSpecification: VSubprogramSpecification, subprogramStatementPart: VSubprogramStatementPart) extends VBlockDeclarativeItem with VProceduralDeclarativeItem {
  def getDesignator : VDesignator = {
    subprogramSpecification match {
      case functionSpecification: VFunctionSpecification => functionSpecification.designator
      case procedureSpecification: VProcedureSpecification => procedureSpecification.designator
    }
  }
}

object VSubprogramBody {
  def apply(ctx: Subprogram_bodyContext): VSubprogramBody = {
    val subprogramSpecification = VSubprogramSpecification(ctx.subprogram_specification())
    val subprogramStatementPart = VSubprogramStatementPart(ctx.subprogram_statement_part())
    VSubprogramBody(subprogramSpecification, subprogramStatementPart)
  }
}

//********************************************************************************************************************//

case class VTypeDeclaration(id: String, vTypeDef: Option[VTypeDef]) extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VTypeDeclaration {
  def apply(ctx: Type_declarationContext): VTypeDeclaration = {
    val id = ctx.identifier().getText
    val typeDef = Option(ctx.type_definition()).map(VTypeDef(_))
    VTypeDeclaration(id, typeDef)
  }
}

//********************************************************************************************************************//

case class VFileDeclaration() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VFileDeclaration {
  def apply(ctx: File_declarationContext): VFileDeclaration = {
    ???
  }
}

//********************************************************************************************************************//

case class VAliasDeclaration() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VAliasDeclaration {
  def apply(ctx: Alias_declarationContext): VAliasDeclaration = {
    ???
  }
}

//********************************************************************************************************************//

case class VAttributeDeclaration() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VAttributeDeclaration {
  def apply(ctx: Attribute_declarationContext): VAttributeDeclaration = {
    ???
  }
}

//********************************************************************************************************************//

case class VAttributeSpecification() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VAttributeSpecification {
  def apply(ctx: Attribute_specificationContext): VAttributeSpecification = {
    ???
  }
}

//********************************************************************************************************************//

case class VUseClause() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VUseClause {
  def apply(ctx: Use_clauseContext): VUseClause = ???
}

//********************************************************************************************************************//

case class VGroupTemplateDeclaration() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VGroupTemplateDeclaration {
  def apply(ctx: Group_template_declarationContext): VGroupTemplateDeclaration = ???
}

//********************************************************************************************************************//

case class VGroupDeclaration() extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VGroupDeclaration {
  def apply(ctx: Group_declarationContext): VGroupDeclaration = ???
}

//********************************************************************************************************************//

trait VProceduralDeclarativeItem

object VProceduralDeclarativeItem {
  def apply(ctx: Process_declarative_itemContext): VProceduralDeclarativeItem = {
    if (ctx.subprogram_declaration() != null) VSubprogramDeclaration(ctx.subprogram_declaration())
    else if (ctx.subprogram_body() != null) VSubprogramBody(ctx.subprogram_body())
    else if (ctx.type_declaration() != null) VTypeDeclaration(ctx.type_declaration())
    else if (ctx.subtype_declaration() != null) VSubtypeDeclaration(ctx.subtype_declaration())
    else if (ctx.constant_declaration() != null) VConstantDeclaration(ctx.constant_declaration())
    else if (ctx.variable_declaration() != null) VVariableDeclaration(ctx.variable_declaration())
    else if (ctx.file_declaration() != null) VFileDeclaration(ctx.file_declaration())
    else if (ctx.alias_declaration() != null) VAliasDeclaration(ctx.alias_declaration())
    else if (ctx.attribute_declaration() != null) VAttributeDeclaration(ctx.attribute_declaration())
    else if (ctx.attribute_specification() != null) VAttributeSpecification(ctx.attribute_specification())
    else if (ctx.use_clause() != null) VUseClause(ctx.use_clause())
    else if (ctx.group_template_declaration() != null) VGroupTemplateDeclaration(ctx.group_template_declaration())
    else if (ctx.group_declaration() != null) VGroupDeclaration(ctx.group_declaration())
    else throw VIError
  }
}

//********************************************************************************************************************//

sealed abstract class VSequentialStatement {
  def toI(defInfo: DefInfo): ISeq_stmt_complex
}

object VSequentialStatement {
  def apply(ctx: Sequential_statementContext): VSequentialStatement = {
    val nullNode = ctx.NULL()

    if (ctx.wait_statement() != null) VWaitStatement(ctx.wait_statement())
    else if (ctx.assertion_statement() != null) VAssertStatement(ctx.assertion_statement())
    else if (ctx.report_statement() != null) VReportStatement(ctx.report_statement())
    else if (ctx.signal_assignment_statement() != null) VSignalAssignmentStatement(ctx.signal_assignment_statement())
    else if (ctx.variable_assignment_statement() != null) VVariableAssignmentStatement(ctx.variable_assignment_statement())
    else if (ctx.if_statement() != null) VIfStatement(ctx.if_statement())
    else if (ctx.case_statement() != null) VCaseStatement(ctx.case_statement())
    else if (ctx.loop_statement() != null) VLoopStatement(ctx.loop_statement())
    else if (ctx.next_statement() != null) VNextStatement(ctx.next_statement())
    else if (ctx.exit_statement() != null) VExitStatement(ctx.exit_statement())
    else if (ctx.return_statement() != null) VReturnStatement(ctx.return_statement())
    else if (nullNode != null) {
      val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
      VNullStat(labelColon)
    }
    else if (ctx.break_statement() != null) VBreakStatement(ctx.break_statement())
    else if (ctx.procedure_call_statement() != null) VProcedureCallStatement(ctx.procedure_call_statement())
    else throw VIError
  }
}

//********************************************************************************************************************//

case class VSensitiveList(nameList: List[VName]) {
  // from list, checked from table
  def toI(defInfo: DefInfo): ISensitiveList = {
    val simpleNames = nameList.map(_.asInstanceOf[VSelectedName])
    val sp_idefs = simpleNames.map(n => defInfo.getSPDef(n)).toList
    ISensitiveList(sp_idefs)
  }
}

object VSensitiveList {
  def apply(ctx: Sensitivity_listContext): VSensitiveList = {
    val nameList = ctx.name().map(VName(_)).toList
    VSensitiveList(nameList)
  }
}


case class VAssert(cond: VExpression, report: Option[VExpression], severity: Option[VExpression])

object VAssert {
  def apply(ctx: AssertionContext): VAssert = {
    val exps = ctx.expression().map(VExpression(_))
    val exp = exps.head
    val report = exps.lift(1)
    val severity = exps.lift(2)
    VAssert(exp, report, severity)
  }
}

case class VTimeOutClause(cond: VExpression)

object VTimeOutClause {
  def apply(ctx: Timeout_clauseContext): VTimeOutClause = {
    val cond = VExpression(ctx.expression())
    VTimeOutClause(cond)
  }
}

case class VCondClause(cond: VExpression)

object VCondClause {
  def apply(ctx: Condition_clauseContext): VCondClause = {
    val cond = VExpression(ctx.condition().expression())
    VCondClause(cond)
  }
}

// no sensitivity_clause
case class VWaitStatement(labelColon: Option[String], sensitiveList: Option[VSensitiveList],
                          condClause: Option[VCondClause], toClause: Option[VTimeOutClause]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VWaitStatement {
  def apply(ctx: Wait_statementContext): VWaitStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val sensitiveList = Option(ctx.sensitivity_clause().sensitivity_list()).map(VSensitiveList(_))
    val condClause = Option(ctx.condition_clause()).map(VCondClause(_))
    val toClause = Option(ctx.timeout_clause()).map(VTimeOutClause(_))
    VWaitStatement(labelColon, sensitiveList, condClause, toClause)
  }
}

case class VAssertStatement(labelColon: Option[String], vAssert: VAssert) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VAssertStatement {
  def apply(ctx: Assertion_statementContext): VAssertStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val vAssert = VAssert(ctx.assertion())
    VAssertStatement(labelColon, vAssert)
  }
}

case class VReportStatement(labelColon: Option[String], exp: VExpression, otherExp: Option[VExpression]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VReportStatement {
  def apply(ctx: Report_statementContext): VReportStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val exps = ctx.expression().map(VExpression(_))
    val exp = exps.head
    val otherExp = exps.lift(1)
    VReportStatement(labelColon, exp, otherExp)
  }
}

case class VSignalAssignmentStatement(labelColon: Option[String], target: VTarget, delay: Option[VDelay], waveForm: VWaveForm) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = {
    val id = labelColon.getOrElse(defaultId)
    val s_clhs = target.toISp_clhs(defInfo)
    val iExpression = waveForm.getSpecialVExp.toIExp(defInfo)
    ISeq_stmt_complex_Ssc_sa(id, s_clhs, IAsmt_rhs(iExpression))
  }
}

object VSignalAssignmentStatement {
  def apply(ctx: Signal_assignment_statementContext): VSignalAssignmentStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val target = VTarget(ctx.target())
    val delay = Option(ctx.delay_mechanism()).map(VDelay(_))
    val waveForm = VWaveForm(ctx.waveform())
    VSignalAssignmentStatement(labelColon, target, delay, waveForm)
  }
}

case class VVariableAssignmentStatement(labelColon: Option[String], target: VTarget, vExpression: VExpression) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = {
    val id = labelColon.getOrElse(defaultId)
    val iV_clhs = target.toIV_clhs(defInfo)
    ISeq_stmt_complex_Ssc_va(id, iV_clhs, IAsmt_rhs(vExpression.toIExp(defInfo)))
  }
}

object VVariableAssignmentStatement {
  def apply(ctx: Variable_assignment_statementContext): VVariableAssignmentStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val target = VTarget(ctx.target())
    val expression = VExpression(ctx.expression())
    VVariableAssignmentStatement(labelColon, target, expression)
  }
}

case class VSequenceOfStatements(sequentialStatementList: List[VSequentialStatement])

object VSequenceOfStatements {
  def apply(ctx: Sequence_of_statementsContext): VSequenceOfStatements = {
    val sequentialStatementList = ctx.sequential_statement().map(VSequentialStatement(_)).toList
    VSequenceOfStatements(sequentialStatementList)
  }
}

case class VIfStatement(labelColon: Option[String],
                        ifVCond: VExpression,
                        ifSeqOfStats: VSequenceOfStatements,
                        elifConds: List[VExpression],
                        elifSeqofStats: List[VSequenceOfStatements],
                        elseSeqOfStats: Option[VSequenceOfStatements],
                        vId: Option[String]) extends VSequentialStatement {

  override def toI(defInfo: DefInfo): ISeq_stmt_complex = {
    import VIfStatement._
    val name = labelColon.orElse(vId).getOrElse(defaultId)
    val ifCond = ifVCond.toIExp(defInfo)
    val if_stmt_complexList = seq_stmt_complex_list(ifSeqOfStats)(defInfo)
    val elif_complexList = elif_complex_list(elifConds, elifSeqofStats)(defInfo)
    val else_stmt_complexList = elseSeqOfStats match {
      case Some(s) => seq_stmt_complex_list(s)(defInfo)
      case None => List.empty
    }
    ISeq_stmt_complex_Ssc_if(name, ifCond, if_stmt_complexList, elif_complexList, else_stmt_complexList)
  }
}

object VIfStatement {
  def apply(ctx: If_statementContext): VIfStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val conds = ctx.condition().map(c => VExpression(c.expression()))
    val seqOfStatsList = ctx.sequence_of_statements().map(VSequenceOfStatements(_))
    val elifLength = ctx.ELSIF().length
    require(conds.length <= seqOfStatsList.length && elifLength + 1 <= conds.length)
    val ifCond = conds.head
    val ifSeqOfStats = seqOfStatsList.head
    val elifConds = conds.slice(1, 1 + elifLength).toList
    val elifSeqOfStatsList = seqOfStatsList.slice(1, 1 + elifLength).toList
    val elseSeqOfStats = seqOfStatsList.lift(1 + elifLength)
    val id = Option(ctx.identifier()).map(_.getText)
    VIfStatement(labelColon, ifCond, ifSeqOfStats, elifConds, elifSeqOfStatsList, elseSeqOfStats, id)
  }

  def seq_stmt_complex_list(seqOfStats: VSequenceOfStatements)(defInfo: DefInfo): List[ISeq_stmt_complex] = {
    seqOfStats.sequentialStatementList.map(_.toI(defInfo))
  }

  def elif_complex_list(cl: List[VExpression], sl: List[VSequenceOfStatements])(defInfo: DefInfo): List[Ssc_elif] = {
    val ssll = sl.map(s => seq_stmt_complex_list(s)(defInfo))
    cl.zip(ssll).map {
      case (exp, ssl) => Ssc_elif(exp.toIExp(defInfo), ssl)
    }
  }

}

//********************************************************************************************************************//

case class VCaseStatAlt(choices: VChoices, seqOfStats: VSequenceOfStatements) {
  def toI(defInfo: DefInfo): Ssc_when = {
    val exps = choices.vChoiceList.map(_.getSimpleExpression.toIExp(defInfo))
    val sstList = seqOfStats.sequentialStatementList.map(_.toI(defInfo))
    Ssc_when(IChoices(exps), sstList)
  }
}

object VCaseStatAlt {
  def apply(ctx: Case_statement_alternativeContext): VCaseStatAlt = {
    val choices = VChoices(ctx.choices())
    val seqOfStats = VSequenceOfStatements(ctx.sequence_of_statements())
    VCaseStatAlt(choices, seqOfStats)
  }
}

case class VCaseStatement(labelColon: Option[String],
                          exp: VExpression, caseStatAltList: List[VCaseStatAlt],
                          vId: Option[String]) extends VSequentialStatement {
  require(caseStatAltList.nonEmpty, "caseStatAltList")

  override def toI(defInfo: DefInfo): ISeq_stmt_complex = {
    val id = labelColon.orElse(vId).getOrElse(defaultId)
    val cond = exp.toIExp(defInfo)
    // TODO: may need to ensure default case only deal with one "choice"
    val lastCase = caseStatAltList.last
    val (when_complexList: List[Ssc_when], defaultList: List[ISeq_stmt_complex]) =
      lastCase.choices.vChoiceList.head match {
        case VChoiceOthers => {
          val initCases = caseStatAltList.init
          (initCases.map(_.toI(defInfo)), lastCase.seqOfStats.sequentialStatementList.map(_.toI(defInfo)))
        }
        case _ => {
          (caseStatAltList.map(_.toI(defInfo)), List.empty)
        }
      }
    val refined: List[Ssc_when] = when_complexList.map { w =>
      val expList = w.choices.expList.map(e => refine__valType(cond, e))
      val choices = w.choices.copy(expList = expList)
      w.copy(choices = choices)
    }
    ISeq_stmt_complex_Ssc_case(id, cond, refined, defaultList)
  }
}

object VCaseStatement {
  def apply(ctx: Case_statementContext): VCaseStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val exp = VExpression(ctx.expression())
    val caseStatAltList = ctx.case_statement_alternative().map(VCaseStatAlt(_)).toList
    val id = Option(ctx.identifier()).map(_.getText)
    VCaseStatement(labelColon, exp, caseStatAltList, id)

  }
}

//********************************************************************************************************************//

sealed abstract class VIterScheme

case class VIterSchemeW(cond: VExpression) extends VIterScheme

case class VIterSchemeFor(paramSpec: VParameterSpecification) extends VIterScheme

case class VIterSchemeWhile(cond: VExpression) extends VIterScheme

case class VLoopStatement(labelColon: Option[String], seqOfStats: VSequenceOfStatements, id: Option[String]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VLoopStatement {
  def apply(ctx: Loop_statementContext): VLoopStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val seqOfStats = VSequenceOfStatements(ctx.sequence_of_statements())
    val id = Option(ctx.identifier()).map(_.getText)
    VLoopStatement(labelColon, seqOfStats, id)
  }
}

//********************************************************************************************************************//

case class VNextStatement(labelColon: Option[String], id: Option[String], cond: Option[VExpression]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VNextStatement {
  def apply(ctx: Next_statementContext): VNextStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val id = Option(ctx.identifier()).map(_.getText)
    val cond = Option(ctx.condition()).map(c => VExpression(c.expression()))
    VNextStatement(labelColon, id, cond)
  }
}


case class VExitStatement(labelColon: Option[String], id: Option[String], cond: Option[VExpression]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VExitStatement {
  def apply(ctx: Exit_statementContext): VExitStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val id = Option(ctx.identifier()).map(_.getText)
    val cond = Option(ctx.condition()).map(c => VExpression(c.expression()))
    VExitStatement(labelColon, id, cond)
  }
}

case class VReturnStatement(expression: VExpression) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = {
    ISeq_stmt_complex_Ssc_rt("", IAsmt_rhs_Rhs_e(expression.toIExp(defInfo)))
  }
}

object VReturnStatement {
  def apply(ctx: Return_statementContext): VReturnStatement = {
    val expression = VExpression(ctx.expression())
    VReturnStatement(expression)
  }
}

case class VNullStat(labelColon: Option[String]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

//********************************************************************************************************************//

case class VBreakSelectorClause(name: VName)

object VBreakSelectorClause {
  def apply(ctx: Break_selector_clauseContext): VBreakSelectorClause = {
    val name = VName(ctx.name())
    VBreakSelectorClause(name)
  }
}

case class VBreakElem(breakSelectorClause: Option[VBreakSelectorClause], name: VName, exp: VExpression)

object VBreakElem {
  def apply(ctx: Break_elementContext): VBreakElem = {
    val breakSelectorContext = Option(ctx.break_selector_clause()).map(VBreakSelectorClause(_))
    val name = VName(ctx.name())
    val exp = VExpression(ctx.expression())
    VBreakElem(breakSelectorContext, name, exp)
  }
}

case class VBreakList(breakElemList: List[VBreakElem]) {
  require(breakElemList.nonEmpty)
}

object VBreakList {
  def apply(ctx: Break_listContext): VBreakList = {
    val breakElemList = ctx.break_element().map(VBreakElem(_)).toList
    VBreakList(breakElemList)
  }
}

case class VBreakStatement(labelColon: Option[String], breakList: Option[VBreakList], cond: Option[VExpression]) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = ???
}

object VBreakStatement {
  def apply(ctx: Break_statementContext): VBreakStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val breakList = Option(ctx.break_list()).map(VBreakList(_))
    val cond = Option(ctx.condition()).map(c => VExpression(c.expression()))
    VBreakStatement(labelColon, breakList, cond)
  }
}

//********************************************************************************************************************//

case class VProcedureCallStatement(labelColon: Option[String], vProcedureCall: VProcedureCall) extends VSequentialStatement {
  override def toI(defInfo: DefInfo): ISeq_stmt_complex = {
    val id = labelColon.getOrElse(defaultId)
    ISeq_stmt_complex_Ssc_pc(id, ISubproccall_complex(vProcedureCall)(defInfo))
  }
}

object VProcedureCallStatement {
  def apply(ctx: Procedure_call_statementContext): VProcedureCallStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val vProcedureCall = VProcedureCall(ctx.procedure_call())
    VProcedureCallStatement(labelColon, vProcedureCall)
  }
}

//********************************************************************************************************************//

case class VProcessStatementPart(sequentialStatementList: List[VSequentialStatement]) {
  def toI(defInfo: DefInfo): List[ISeq_stmt_complex] = {
    sequentialStatementList.map(_.toI(defInfo))
  }
}

object VProcessStatementPart {
  def apply(ctx: Process_statement_partContext): VProcessStatementPart = {
    val seqStatList = ctx.sequential_statement().map(VSequentialStatement(_)).toList
    VProcessStatementPart(seqStatList)
  }
}

case class VProcessDeclarativePart(processDeclarativeItem: List[VProceduralDeclarativeItem])

object VProcessDeclarativePart {
  def apply(ctx: Process_declarative_partContext): VProcessDeclarativePart = {
    val processDeclItemList = ctx.process_declarative_item().map(VProceduralDeclarativeItem(_)).toList
    VProcessDeclarativePart(processDeclItemList)
  }
}

case class VProcessStatement(labelColon: Option[String],
                             p1: Boolean,
                             sensitivitylist: Option[VSensitiveList],
                             is: Boolean,
                             procDeclPart: VProcessDeclarativePart,
                             processStatementPart: VProcessStatementPart,
                             p2: Boolean,
                             identifier: Option[String]) extends VArchitectureStatement {
  override def toI(defInfo: DefInfo): IConc_stmt_complex_Csc_ps = {
    // [HC] guess
    val id = labelColon.orElse(identifier).getOrElse(defaultId)
    val iSensitiveList = sensitivitylist.map(_.toI(defInfo))
    // [HC] don't care about processDeclarativePart
    // [HC] processStatementPart => seq_stmt_complexList
    val seq_stmt_complexList: List[ISeq_stmt_complex] = processStatementPart.toI(defInfo)
    IConc_stmt_complex_Csc_ps(id, iSensitiveList, seq_stmt_complexList)
  }

}

object VProcessStatement {
  def apply(ctx: Process_statementContext): VProcessStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val posponeds = ctx.POSTPONED()
    val (p1, p2) = if (posponeds.length == 2) {
      (true, true)
    } else if (posponeds.isEmpty) {
      (false, false)
    } else if (posponeds.length == 1) {
      // TODO not sure, however may be insignificant
      (true, false)
    } else throw VIError
    val sensitiviList = Option(ctx.sensitivity_list()).map(VSensitiveList(_))
    val is = ctx.IS() != null
    val procDeclPart = VProcessDeclarativePart(ctx.process_declarative_part())
    val procStatPart = VProcessStatementPart(ctx.process_statement_part())
    val id = Option(ctx.identifier()).map(_.getText)
    VProcessStatement(labelColon, p1, sensitiviList, is, procDeclPart, procStatPart, p2, id)
  }
}

//********************************************************************************************************************//

sealed abstract class VFormalPart

object VFormalPart {
  def apply(ctx: Formal_partContext): VFormalPart = {
    val id = ctx.identifier().getText
    val explicit_rangeContext = ctx.explicit_range()
    if (explicit_rangeContext != null) {
      VFormalPartR(id, VExplicitRange(explicit_rangeContext))
    } else {
      VFormalPartI(id)
    }
  }
}

case class VFormalPartI(id: String) extends VFormalPart

case class VFormalPartR(id: String, explicitRange: VExplicitRange) extends VFormalPart

sealed abstract class VActualDesignator

object VActualDesignator {
  def apply(ctx: Actual_designatorContext): VActualDesignator = {
    val expression = ctx.expression()
    if (expression != null) {
      VActualDesignatorExpression(VExpression(expression))
    } else {
      VActualDesignatorOpen
    }
  }
}

case class VActualDesignatorExpression(vExpression: VExpression) extends VActualDesignator

case object VActualDesignatorOpen extends VActualDesignator

sealed abstract class VActualPart {
  val vActualDesignator: VActualDesignator
}

object VActualPart {
  def apply(ctx: Actual_partContext): VActualPart = {
    val name = ctx.name()
    val actualDesignator = VActualDesignator(ctx.actual_designator())
    if (name != null) {
      VActualPartN(VName(name), actualDesignator)
    } else {
      VActualPartD(actualDesignator)
    }
  }
}

case class VActualPartN(name: VName, vActualDesignator: VActualDesignator) extends VActualPart

case class VActualPartD(vActualDesignator: VActualDesignator) extends VActualPart

case class VAssociationElement(formalPart: Option[VFormalPart], actualPart: VActualPart)

object VAssociationElement {
  def apply(ctx: Association_elementContext): VAssociationElement = {
    val formalPart = Option(ctx.formal_part()).map(VFormalPart(_))
    val actualPart = VActualPart(ctx.actual_part())
    VAssociationElement(formalPart, actualPart)
  }
}

case class VAssociationList(vAssociationElementList: List[VAssociationElement]) {
  require(vAssociationElementList.nonEmpty, "vAssociationElementList")
}

object VAssociationList {
  def apply(ctx: Association_listContext): VAssociationList = {
    val assocElemList = ctx.association_element().map(VAssociationElement(_)).toList
    VAssociationList(assocElemList)
  }
}

// [HC] No actual_parameter_part
case class VProcedureCall(selectedName: String, vAssociationList: VAssociationList)

object VProcedureCall {
  def apply(ctx: Procedure_callContext): VProcedureCall = {
    val selectedName = ctx.selected_name().getText
    val vAssociationList = VAssociationList(ctx.actual_parameter_part().association_list())
    VProcedureCall(selectedName, vAssociationList)
  }
}

//********************************************************************************************************************//

sealed abstract class VGenerationScheme {
  def toI(defInfo: DefInfo): Gen_type = this match {
    case VGenerationSchemeFor(paramSpec) => {
      val exp = defInfo.getDef(paramSpec.id)
      ???
    }
    case VGenerationSchemeIf(cond) => {
      If_gen(cond.toIExp(defInfo))
    }
  }
}

object VGenerationScheme {
  def apply(ctx: Generation_schemeContext): VGenerationScheme = {
    val cond = ctx.condition()
    val parameter_specificationContext = ctx.parameter_specification()
    if (cond != null) {
      VGenerationSchemeIf(VExpression(cond.expression()))
    } else if (parameter_specificationContext != null) {
      VGenerationSchemeFor(VParameterSpecification(parameter_specificationContext))
    } else throw VIError
  }
}

case class VParameterSpecification(id: String, discrete_range: VDiscreteRange)

object VParameterSpecification {
  def apply(ctx: Parameter_specificationContext): VParameterSpecification = {
    val id = ctx.identifier().getText
    val vDiscreteRange = VDiscreteRange(ctx.discrete_range())
    VParameterSpecification(id, vDiscreteRange)
  }
}

case class VGenerationSchemeFor(paramSpec: VParameterSpecification) extends VGenerationScheme

case class VGenerationSchemeIf(cond: VExpression) extends VGenerationScheme

//********************************************************************************************************************//

case class VComponentDeclaration() extends VBlockDeclarativeItem

object VComponentDeclaration {
  def apply(ctx: Component_declarationContext): VComponentDeclaration = {
    ???
  }
}

case class VConfigurationSpecification() extends VBlockDeclarativeItem

object VConfigurationSpecification {
  def apply(ctx: Configuration_specificationContext): VConfigurationSpecification = {
    ???
  }
}

case class VDisconnectionSpecification() extends VBlockDeclarativeItem

object VDisconnectionSpecification {
  def apply(ctx: Disconnection_specificationContext): VDisconnectionSpecification = {
    ???
  }
}

case class VStepLimitSpecification() extends VBlockDeclarativeItem

object VStepLimitSpecification {
  def apply(ctx: Step_limit_specificationContext): VStepLimitSpecification = {
    ???
  }
}

case class VNatureDeclaration() extends VBlockDeclarativeItem

object VNatureDeclaration {
  def apply(ctx: Nature_declarationContext): VNatureDeclaration = {
    ???
  }
}

case class VSubnatureDeclaration() extends VBlockDeclarativeItem

object VSubnatureDeclaration {
  def apply(ctx: Subnature_declarationContext): VSubnatureDeclaration = {
    ???
  }
}

case class VQuantityDeclaration() extends VBlockDeclarativeItem

object VQuantityDeclaration {
  def apply(ctx: Quantity_declarationContext): VQuantityDeclaration = {
    ???
  }
}

case class VTerminalDeclaration() extends VBlockDeclarativeItem

object VTerminalDeclaration {
  def apply(ctx: Terminal_declarationContext): VTerminalDeclaration = {
    ???
  }
}

//********************************************************************************************************************//

sealed trait VBlockDeclarativeItem {
}

object VBlockDeclarativeItem {
  def apply(ctx: Block_declarative_itemContext): VBlockDeclarativeItem = {
    if (ctx.subprogram_declaration() != null) VSubprogramDeclaration(ctx.subprogram_declaration())
    else if (ctx.subprogram_body() != null) VSubprogramBody(ctx.subprogram_body())
    else if (ctx.type_declaration() != null) VTypeDeclaration(ctx.type_declaration())
    else if (ctx.subtype_declaration() != null) VSubtypeDeclaration(ctx.subtype_declaration())
    else if (ctx.constant_declaration() != null) VConstantDeclaration(ctx.constant_declaration())
    else if (ctx.signal_declaration() != null) VSignalDeclaration(ctx.signal_declaration())
    else if (ctx.variable_declaration() != null) VVariableDeclaration(ctx.variable_declaration())
    else if (ctx.file_declaration() != null) VFileDeclaration(ctx.file_declaration())
    else if (ctx.alias_declaration() != null) VAliasDeclaration(ctx.alias_declaration())
    else if (ctx.component_declaration() != null) VComponentDeclaration(ctx.component_declaration())
    else if (ctx.attribute_declaration() != null) VAttributeDeclaration(ctx.attribute_declaration())
    else if (ctx.attribute_specification() != null) VAttributeSpecification(ctx.attribute_specification())
    else if (ctx.configuration_specification() != null) VConfigurationSpecification(ctx.configuration_specification())
    else if (ctx.disconnection_specification() != null) VDisconnectionSpecification(ctx.disconnection_specification())
    else if (ctx.step_limit_specification() != null) VStepLimitSpecification(ctx.step_limit_specification())
    else if (ctx.use_clause() != null) VUseClause(ctx.use_clause())
    else if (ctx.group_template_declaration() != null) VGroupTemplateDeclaration(ctx.group_template_declaration())
    else if (ctx.group_declaration() != null) VGroupDeclaration(ctx.group_declaration())
    else if (ctx.nature_declaration() != null) VNatureDeclaration(ctx.nature_declaration())
    else if (ctx.subnature_declaration() != null) VSubnatureDeclaration(ctx.subnature_declaration())
    else if (ctx.quantity_declaration() != null) VQuantityDeclaration(ctx.quantity_declaration())
    else if (ctx.terminal_declaration() != null) VTerminalDeclaration(ctx.terminal_declaration())
    else throw VIError
  }
}

case class VSubtypeDeclaration(id: String, subtypeInd: VSubtypeIndication) extends VBlockDeclarativeItem with VProceduralDeclarativeItem

object VSubtypeDeclaration {
  def apply(ctx: Subtype_declarationContext): VSubtypeDeclaration = {
    val id = ctx.identifier().getText
    val subtypeInd = VSubtypeIndication(ctx.subtype_indication())
    VSubtypeDeclaration(id, subtypeInd)
  }
}

//********************************************************************************************************************//

case class VGenericList(interfaceConstantDeclarationList: List[VInterfaceConstantDeclaration]) {
  require(interfaceConstantDeclarationList.nonEmpty)
}

object VGenericList {
  def apply(ctx: Generic_listContext): VGenericList = {
    val interfaceConstantDeclarationList = ctx.interface_constant_declaration().map(VInterfaceConstantDeclaration(_)).toList
    VGenericList(interfaceConstantDeclarationList)
  }
}

case class VGenericMapAspect(vAssocList: VAssociationList)

object VGenericMapAspect {
  def apply(ctx: Generic_map_aspectContext): VGenericMapAspect = {
    val assoclist = VAssociationList(ctx.association_list())
    VGenericMapAspect(assoclist)
  }
}

case class VGenericClause(genericlist: VGenericList)

object VGenericClause {
  def apply(ctx: Generic_clauseContext): VGenericClause = {
    val genericlist = VGenericList(ctx.generic_list())
    VGenericClause(genericlist)
  }
}

case class VInterfacePortList(vInterfacePortDeclList: List[VInterfacePortDeclaration]) {
  require(vInterfacePortDeclList.nonEmpty)
}

object VInterfacePortList {
  def apply(ctx: Interface_port_listContext): VInterfacePortList = {
    val vInterfacePortDeclList = ctx.interface_port_declaration().map(VInterfacePortDeclaration(_)).toList
    VInterfacePortList(vInterfacePortDeclList)
  }
}

case class VPortList(vInterfacePortList: VInterfacePortList)

object VPortList {
  def apply(ctx: Port_listContext): VPortList = {
    val vInterfacePortList = VInterfacePortList(ctx.interface_port_list())
    VPortList(vInterfacePortList)
  }
}

case class VPortClause(vPortList: VPortList)

object VPortClause {
  def apply(ctx: Port_clauseContext): VPortClause = {
    val portList = VPortList(ctx.port_list())
    VPortClause(portList)
  }
}

case class VPortMapAspect(vAssocList: VAssociationList)

object VPortMapAspect {
  def apply(ctx: Port_map_aspectContext): VPortMapAspect = {
    val assoclist = VAssociationList(ctx.association_list())
    VPortMapAspect(assoclist)
  }
}

case class VBlockHeader(genericClause: Option[VGenericClause],
                        genericMapAspect: Option[VGenericMapAspect],
                        portClause: Option[VPortClause],
                        portMapAspect: Option[VPortMapAspect])

object VBlockHeader {
  def apply(ctx: Block_headerContext): VBlockHeader = {
    val genericClause = Option(ctx.generic_clause()).map(VGenericClause(_))
    val genericMapAspect = Option(ctx.generic_map_aspect()).map(VGenericMapAspect(_))
    val portClause = Option(ctx.port_clause()).map(VPortClause(_))
    val portMapAspect = Option(ctx.port_map_aspect()).map(VPortMapAspect(_))
    VBlockHeader(genericClause, genericMapAspect, portClause, portMapAspect)
  }
}

case class VBlockDeclPart(blockDeclItemList: List[VBlockDeclarativeItem])

object VBlockDeclPart {
  def apply(ctx: Block_declarative_partContext): VBlockDeclPart = {
    val blockDeclItem = ctx.block_declarative_item().map(VBlockDeclarativeItem(_)).toList
    VBlockDeclPart(blockDeclItem)
  }
}

case class VBlockStatPart(vArchStatList: List[VArchitectureStatement])

object VBlockStatPart {
  def apply(ctx: Block_statement_partContext): VBlockStatPart = {
    val archStatList = ctx.architecture_statement().map(VArchitectureStatement(_)).toList
    VBlockStatPart(archStatList)
  }
}

case class VBlockStatement(labelColon: String,
                           exp: Option[VExpression],
                           blockHeader: VBlockHeader,
                           blockDeclPart: VBlockDeclPart,
                           blockStatPart: VBlockStatPart,
                           id: Option[String]) extends VArchitectureStatement

object VBlockStatement {
  def apply(ctx: Block_statementContext): VBlockStatement = {
    val labelColon = ctx.label_colon().identifier().getText
    val exp = Option(ctx.expression()).map(VExpression(_))
    val blockHeader = VBlockHeader(ctx.block_header())
    val blockDeclPart = VBlockDeclPart(ctx.block_declarative_part())
    val blockStatPart = VBlockStatPart(ctx.block_statement_part())
    val id = Option(ctx.identifier()).map(_.getText)
    VBlockStatement(labelColon, exp, blockHeader, blockDeclPart, blockStatPart, id)
  }
}


//********************************************************************************************************************//

case class VComponentInstantiationStatement() extends VArchitectureStatement

object VComponentInstantiationStatement {
  def apply(ctx: Component_instantiation_statementContext): VComponentInstantiationStatement = {
    ???
  }
}

case class VSimultaneousStatement() extends VArchitectureStatement

object VSimultaneousStatement {
  def apply(ctx: Simultaneous_statementContext): VSimultaneousStatement = {
    ???
  }
}

//********************************************************************************************************************//

case class VConcurrentProcedureCallStatement(labelColon: Option[String],
                                             procCall: VProcedureCall)

object VConcurrentProcedureCallStatement {
  def apply(ctx: Concurrent_procedure_call_statementContext): VConcurrentProcedureCallStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val procedureCall = VProcedureCall(ctx.procedure_call())
    VConcurrentProcedureCallStatement(labelColon, procedureCall)
  }
}

case class VConcurrentAssertionStatement(labelColon: Option[String],
                                         assertion: VAssert)

object VConcurrentAssertionStatement {
  def apply(ctx: Concurrent_assertion_statementContext): VConcurrentAssertionStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val assertion = VAssert(ctx.assertion())
    VConcurrentAssertionStatement(labelColon, assertion)
  }
}

case class VSensitivityClause(sensitivitylist: VSensitiveList)

object VSensitivityClause {
  def apply(ctx: Sensitivity_clauseContext): VSensitivityClause = {
    val sensitivitylist = VSensitiveList(ctx.sensitivity_list())
    VSensitivityClause(sensitivitylist)
  }
}

case class VConcurrentBreakStatement(labelColon: Option[String],
                                     breaklist: Option[VBreakList],
                                     sensitivityClause: Option[VSensitivityClause],
                                     whenCond: Option[VExpression]) extends VArchitectureStatement

object VConcurrentBreakStatement {
  def apply(ctx: Concurrent_break_statementContext): VConcurrentBreakStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val breaklist = Option(ctx.break_list()).map(VBreakList(_))
    val sensitivityClause = Option(ctx.sensitivity_clause()).map(VSensitivityClause(_))
    val whenCond = Option(ctx.condition()).map(c => VExpression(c.expression()))
    VConcurrentBreakStatement(labelColon, breaklist, sensitivityClause, whenCond)
  }
}

//********************************************************************************************************************//

sealed abstract class VArchitectureStatement {
  def toI(defInfo: DefInfo): IConc_stmt_complex = this match {
    case vProcessStatement: VProcessStatement => vProcessStatement.toI(defInfo)
    case VArchitectureStatementConcurrentSignalAssignmentStatement(labelColon, vConcurrentSignalAssignmentStatement) => vConcurrentSignalAssignmentStatement.toI(defInfo)
    case vGenerateStatement: VGenerateStatement => vGenerateStatement.toI(defInfo)
    case _ => ???
  }
}

object VArchitectureStatement {
  def apply(ctx: Architecture_statementContext): VArchitectureStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)

    if (ctx.block_statement() != null) VBlockStatement(ctx.block_statement())
    else if (ctx.process_statement() != null) VProcessStatement(ctx.process_statement())
    else if (ctx.concurrent_procedure_call_statement() != null) VArchitectureStatementConcurrentProcedureCallStatement(labelColon, VConcurrentProcedureCallStatement(ctx.concurrent_procedure_call_statement()))
    else if (ctx.concurrent_assertion_statement() != null) VArchitectureStatementConcurrentAssertionStatement(labelColon, VConcurrentAssertionStatement(ctx.concurrent_assertion_statement()))
    else if (ctx.concurrent_signal_assignment_statement() != null) VArchitectureStatementConcurrentSignalAssignmentStatement(labelColon, VConcurrentSignalAssignmentStatement(ctx.concurrent_signal_assignment_statement()))
    else if (ctx.component_instantiation_statement() != null) VComponentInstantiationStatement(ctx.component_instantiation_statement())
    else if (ctx.generate_statement() != null) VGenerateStatement(ctx.generate_statement())
    else if (ctx.concurrent_break_statement() != null) VConcurrentBreakStatement(ctx.concurrent_break_statement())
    else if (ctx.simultaneous_statement() != null) VSimultaneousStatement(ctx.simultaneous_statement())
    else throw VIError
  }
}

case class VArchitectureStatementConcurrentProcedureCallStatement(labelColon: Option[String],
                                                                  cpcs: VConcurrentProcedureCallStatement) extends VArchitectureStatement

case class VArchitectureStatementConcurrentAssertionStatement(labelColon: Option[String],
                                                              cas: VConcurrentAssertionStatement) extends VArchitectureStatement

case class VArchitectureStatementConcurrentSignalAssignmentStatement(labelColon: Option[String], csas: VConcurrentSignalAssignmentStatement) extends VArchitectureStatement

//********************************************************************************************************************//

case class VGenerateStatement(labelColon: Option[String],
                              genScheme: VGenerationScheme,
                              blockDeclItemList: List[VBlockDeclarativeItem],
                              archStatList: List[VArchitectureStatement],
                              vId: Option[String]) extends VArchitectureStatement{
  override def toI(defInfo: DefInfo): IConc_stmt_complex_Csc_gen = {
    val id = vId.orElse(labelColon).getOrElse(defaultId)
    val genType = genScheme.toI(defInfo)
    val conc_stmt_complexList: List[IConc_stmt_complex] = archStatList.map(_.toI(defInfo))
    IConc_stmt_complex_Csc_gen(id, genType, conc_stmt_complexList)
  }
}

object VGenerateStatement {
  def apply(ctx: Generate_statementContext): VGenerateStatement = {
    val labelColon = Option(ctx.label_colon()).map(_.identifier().getText)
    val genScheme = VGenerationScheme(ctx.generation_scheme())
    val blockDeclItemList = ctx.block_declarative_item().map(VBlockDeclarativeItem(_)).toList
    val archStatList = ctx.architecture_statement().map(VArchitectureStatement(_)).toList
    val id = Option(ctx.identifier().getText)
    VGenerateStatement(labelColon, genScheme, blockDeclItemList, archStatList, id)
  }
}