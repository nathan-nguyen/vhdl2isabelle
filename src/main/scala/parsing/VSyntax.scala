package parsing

import parsing.V2IUtils._
import sg.edu.ntu.hchen.VHDLParser._

import scala.collection.JavaConversions._

///////////////////////////////////////////////////////////////////////

case class VConstDecl(idList: List[String], subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VConstDecl {
  def apply(ctx: Constant_declarationContext): VConstDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    VConstDecl(idList, subtypeInd, vExp)
  }
}


///////////////////////////////////////////////////////////////////////

case class VSignalDecl(idList: List[String], subtypeInd: VSubtypeInd,
                       signalKind: Option[String], exp: Option[VExp])

object VSignalDecl {
  def apply(ctx: Signal_declarationContext): VSignalDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val signalKind = Option(ctx.signal_kind()).map(_.getText.toLowerCase)
    val exp = Option(ctx.expression()).map(VExp(_))
    VSignalDecl(idList, subtypeInd, signalKind, exp)
  }
}

///////////////////////////////////////////////////////////////////////

case class VInterfaceSignalDecl(idList: List[String], subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VInterfaceSignalDecl {
  def apply(ctx: Interface_signal_declarationContext): VInterfaceSignalDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    VInterfaceSignalDecl(idList, subtypeInd, vExp)
  }
}

case class VInterfaceConstDecl(idList: List[String], subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VInterfaceConstDecl {
  def apply(ctx: Interface_constant_declarationContext): VInterfaceConstDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    VInterfaceConstDecl(idList, subtypeInd, vExp)
  }
}

case class VInterfacePortDecl(idList: List[String], mode: String,
                              subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VInterfacePortDecl {
  def apply(ctx: Interface_port_declarationContext): VInterfacePortDecl = {
    val idList = getIdList(ctx.identifier_list())
    val mode = ctx.signal_mode().getText
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    VInterfacePortDecl(idList, mode, subtypeInd, vExp)
  }
}

case class VPortList(interfacePortDecls: List[VInterfacePortDecl])


///////////////////////////////////////////////////////////////

// just a hack
case class VSuffix(s: String)

// perhaps needing separation
sealed abstract class VName {

  def getSimpleNameOpt: Option[String] = this match {
    case VSelectedName(id, suffixList) => {
      if (suffixList.nonEmpty) {
        logger.warn(s"VSelectedName ${toString}")
        None
      } else
        Some(id)
    }
    case VNameParts(namePartList) => {
      logger.warn(s"VNameParts ${toString}")
      None
    }
  }

  def getSimpleName: String = getSimpleNameOpt match {
    case Some(s) => s
    case None => throw VIErrorMsg(s"${toString}")
  }

  def isarTName: String = this match {
    case VSelectedName(id, suffixList) => {
      val (sp_of_spl, extractor) = ("sp_of_spl", "s.")
      val nList = suffixList.scanLeft(id)((acc, cur) => s"${acc}_${cur}")
      nList.tail.foldLeft(nList.head)((acc, cur) => s"(${sp_of_spl} (${extractor}''${cur}''))")
    }
    case VNameParts(namePartList) => unknownString
  }

}

object VName {
  def apply(ctx: NameContext): VName = {
    val selectedName = ctx.selected_name()
    val name_partList = ctx.name_part()
    if (selectedName != null) {
      VSelectedName(selectedName)
    } else {
      VNameParts.gen(name_partList.toList)
    }
  }
}


case class VSelectedName(id: String, suffixList: List[VSuffix]) extends VName

object VSelectedName {
  def apply(ctx: Selected_nameContext): VSelectedName = {
    val id = ctx.identifier().getText
    val suffixList = ctx.suffix().map(s => VSuffix(s.getText)).toList
    VSelectedName(id, suffixList)
  }
}

// a hack
case class VNamePart(s: String)

case class VNameParts(namePartList: List[VNamePart]) extends VName {
  require(namePartList.nonEmpty, "VNameParts")
}

object VNameParts {
  //  Fxxk type erasure
  def gen(ctxList: List[Name_partContext]): VNameParts = {
    val namePartList = ctxList.map(np => VNamePart(np.getText))
    VNameParts(namePartList)
  }
}


sealed abstract class VTarget {
  def getSelectedNameOpt: Option[VSelectedName] = this match {
    case VTargetN(name) => name match {
      case s: VSelectedName => Some(s)
      case p: VNameParts => None
    }
    case VTargetAggregate(aggregate) => None
  }

  def toI_SP(defInfo: DefInfo): SP_clhs = {
    val sn = getSelectedNameOpt match {
      case Some(s) => s
      case None => throw VIErrorMsg(s"${toString}")
    }
    val idef = defInfo.getSPDef(sn)
    // FIXME
    def__sp_clhs(idef, None)
  }

  def toI_V(defInfo: DefInfo): V_IDef = {
    val sn = getSelectedNameOpt match {
      case Some(s) => s
      case None => throw VIErrorMsg(s"${toString}")
    }
    defInfo.getVDef(sn)
  }

  def toI_V_Clhs(defInfo: DefInfo): V_clhs = {
    val idef = toI_V(defInfo)
    // FIXME may not be None
    def__v_clhs(idef, None)
  }

}

object VTarget {
  def apply(ctx: TargetContext): VTarget = {
    val name = ctx.name()
    val aggregate = ctx.aggregate()
    if (name != null) {
      VTargetN(VName(name))
    } else if (aggregate != null) {
      VTargetAggregate(VAggregate(aggregate))
    } else throw VIError
  }
}

case class VTargetN(name: VName) extends VTarget

case class VTargetAggregate(aggregate: VAggregate) extends VTarget

//////////////////////////////////////////////////////////////////////////////

sealed abstract class VDelay

object VDelay {
  def apply(ctx: Delay_mechanismContext): VDelay = {
    val transport = ctx.TRANSPORT()
    val inertial = ctx.INERTIAL()
    if (transport != null) {
      VDelayT
    } else if (inertial != null) {
      val exp = Option(ctx.expression()).map(VExp(_))
      VDelayE(exp)
    } else throw VIError
  }
}

object VDelayT extends VDelay

case class VDelayE(vExp: Option[VExp]) extends VDelay

case class VOpts(guarded: Boolean, delay: Option[VDelay])

object VOpts {
  def apply(ctx: OptsContext): VOpts = {
    val guarded = ctx.GUARDED() != null
    val delay = Option(ctx.delay_mechanism()).map(VDelay(_))
    VOpts(guarded, delay)
  }
}

//////////////////////////////////////////////////////////////////////////////
/**
  * 2nd exp is "after", not useful?
  */
case class VWaveFormElem(exp: VExp, expOption: Option[VExp])

object VWaveFormElem {
  def apply(ctx: Waveform_elementContext): VWaveFormElem = {
    val exprs = ctx.expression().map(VExp(_))
    val exp = exprs.head
    val expOption = exprs.lift(1)
    VWaveFormElem(exp, expOption)
  }
}

sealed abstract class VWaveForm {
  def toI(defInfo: DefInfo): Crhs = this match {
    case vWaveFormE@VWaveFormE(elems) => {
      val iExp = vWaveFormE.getSpecialExp(defInfo)
      iExp.crhs_e()
    }
    case VWaveFormU => {
      throw VIErrorMsg(s"${toString}")
    }
  }
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

  def getSpecialExp(defInfo: DefInfo): IExp = elems.head.exp.toIExp(defInfo)
}

case object VWaveFormU extends VWaveForm

case class VCondWaveForms(whenWaveForm: VWaveForm, cond: Option[VExp], elseCond: Option[VCondWaveForms]) {
  // FIXME isar definition has many limitations here
  def toI(defInfo: DefInfo): (List[As_when], Crhs) = {
    // whenWaveForm => as_whenList.head.crhs, cond => as_whenList.head.IExp
    // elseCond build the as_whenList(1) (last one)
    import VCondWaveForms._
    val as_when1 = genAsWhen(whenWaveForm, cond)(defInfo)
    val as_when2 = elseCond match {
      case Some(waveForms) => genAsWhen(waveForms.whenWaveForm, waveForms.cond)(defInfo)
      case None => throw VIErrorMsg(s"${this.toString}")
    }
    val as_whenList = List(as_when1, as_when2)
    val else_crhs = elseCond match {
      case Some(VCondWaveForms(_, _, finalElseOpt)) => finalElseOpt match {
        case Some(finalElse) => finalElse.whenWaveForm.toI(defInfo)
        case None => throw VIErrorMsg(s"${this.toString}")
      }
      case None => throw VIErrorMsg(s"${this.toString}")
    }
    (as_whenList, else_crhs)
  }
}

object VCondWaveForms {
  def apply(ctx: Conditional_waveformsContext): VCondWaveForms = {
    val waveForm = VWaveForm(ctx.waveform())
    val cond = Option(ctx.condition()).map(c => VExp(c.expression()))
    val condWaves = Option(ctx.conditional_waveforms()).map(VCondWaveForms(_))
    VCondWaveForms(waveForm, cond, condWaves)
  }

  def genAsWhen(waveForm: VWaveForm, cond: Option[VExp])(defInfo: DefInfo): As_when = {
    val when_crhs = waveForm.toI(defInfo)
    val when_cond = cond match {
      case Some(c) => c.toIExp(defInfo)
      case None => throw VIErrorMsg(s"${toString}")
    }
    As_when(when_crhs, when_cond)
  }

}

//////////////////////////////////////////////////////////////////////////////

case class VSelectedWaveForm(waveForm: VWaveForm, choices: VChoices,
                             waveFormOpt: Option[VWaveForm], choicesOpt: Option[VChoices])

object VSelectedWaveForm {
  def apply(ctx: Selected_waveformsContext): VSelectedWaveForm = {
    val waveFormList = ctx.waveform().map(VWaveForm(_))
    val choicesList = ctx.choices().map(VChoices(_))
    VSelectedWaveForm(waveFormList.head, choicesList.head, waveFormList.lift(1), choicesList.lift(1))
  }
}

case class VSelectedSignalAssign(exp: VExp, target: VTarget, opts: VOpts, selectedWaveForm: VSelectedWaveForm)

object VSelectedSignalAssign {
  def apply(ctx: Selected_signal_assignmentContext): VSelectedSignalAssign = {
    val exp = VExp(ctx.expression())
    val target = VTarget(ctx.target())
    val opts = VOpts(ctx.opts())
    val selectedWaveForm = VSelectedWaveForm(ctx.selected_waveforms())
    VSelectedSignalAssign(exp, target, opts, selectedWaveForm)
  }
}

//////////////////////////////////////////////////////////////////////////////

case class VCondSignalAssign(vTarget: VTarget, opts: VOpts, conditionalWaveforms: VCondWaveForms) {
  def toI(defInfo: DefInfo): (SP_clhs, List[As_when], Crhs) = {
    val sp_chls = vTarget.toI_SP(defInfo)
    val (as_whenList, crhs) = conditionalWaveforms.toI(defInfo)
    (sp_chls, as_whenList, crhs)
  }
}

object VCondSignalAssign {
  def apply(ctx: Conditional_signal_assignmentContext): VCondSignalAssign = {
    val target = VTarget(ctx.target())
    val opts = VOpts(ctx.opts())
    val condWaveForms = VCondWaveForms(ctx.conditional_waveforms())
    VCondSignalAssign(target, opts, condWaveForms)
  }
}

//////////////////////////////////////////////////////////////////////////////

sealed abstract class VConcurrentSignalAssignStat

object VConcurrentSignalAssignStat {
  def apply(ctx: Concurrent_signal_assignment_statementContext): VConcurrentSignalAssignStat = {
    val condSignalAssign = ctx.conditional_signal_assignment()
    val selectedSignalAssign = ctx.selected_signal_assignment()
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val postPonded = ctx.POSTPONED() != null
    if (condSignalAssign != null) {
      VConcurrentSignalAssignStatC(labelColon, postPonded, VCondSignalAssign(condSignalAssign))
    } else if (selectedSignalAssign != null) {
      VConcurrentSignalAssignStatS(labelColon, postPonded, VSelectedSignalAssign(selectedSignalAssign))
    } else throw VIError
  }
}

case class VConcurrentSignalAssignStatC(labelColon: Option[String],
                                        postPonded: Boolean,
                                        condSignAssign: VCondSignalAssign) extends VConcurrentSignalAssignStat {
  def toI(defInfo: DefInfo): Csc_ca = {
    val id = labelColon.getOrElse(defaultId)
    val (sp_clhs, casmt_rhsList, crhs) = condSignAssign.toI(defInfo)
    Csc_ca(id, sp_clhs, casmt_rhsList, crhs)
  }
}

case class VConcurrentSignalAssignStatS(labelColon: Option[String],
                                        postPonded: Boolean,
                                        selectSignalAssign: VSelectedSignalAssign) extends VConcurrentSignalAssignStat


///////////////////////////////////////////////////////////////////////////////////
case class VVarDecl(shared: Boolean, idList: List[String],
                    subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VVarDecl {
  def apply(ctx: Variable_declarationContext): VVarDecl = {
    val shared = ctx.SHARED() != null
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    VVarDecl(shared, idList, subtypeInd, vExp)
  }
}

////////////////////////////////////////////////////////////////////////////////

case class VSubProgDecl(vSubProgSpec: VSubProgSpec)

object VSubProgDecl {
  def apply(ctx: Subprogram_declarationContext): VSubProgDecl = {
    val spec = ctx.subprogram_specification()
    VSubProgDecl(VSubProgSpec(spec))
  }
}

////////////////////////////////////////////////////////////////////////////////

sealed abstract class VSubProgSpec

object VSubProgSpec {
  def apply(ctx: Subprogram_specificationContext): VSubProgSpec = ???
}

case class VProcSpec() extends VSubProgSpec

case class VFuncSpec() extends VSubProgSpec

////////////////////////////////////////////////////////////////////////////////

case class VSubProgBody()

object VSubProgBody {
  def apply(ctx: Subprogram_bodyContext): VSubProgBody = {
    ???
  }
}

////////////////////////////////////////////////////////////////////////////////

case class VTypeDecl()

////////////////////////////////////////////////////////////////////////////////

case class VFileDecl()

object VFileDecl {
  def apply(ctx: File_declarationContext): VFileDecl = {
    ???
  }
}

////////////////////////////////////////////////////////////////////////////////

case class VAliasDecl()

object VAliasDecl {
  def apply(ctx: Alias_declarationContext): VAliasDecl = {
    ???
  }
}

////////////////////////////////////////////////////////////////////////////////
case class VAttrDecl()

object VAttrDecl {
  def apply(ctx: Attribute_declarationContext): VAttrDecl = {
    ???
  }
}

////////////////////////////////////////////////////////////////////////////////
case class VAttrSpec()

object VAttrSpec {
  def apply(ctx: Attribute_specificationContext): VAttrSpec = {
    ???
  }
}

////////////////////////////////////////////////////////////////////////////////
case class VUseClause()

object VUseClause {
  def apply(ctx: Use_clauseContext): VUseClause = ???
}

////////////////////////////////////////////////////////////////////////////////
case class VGrpTempDecl()

object VGrpTempDecl {
  def apply(ctx: Group_template_declarationContext): VGrpTempDecl = ???
}

////////////////////////////////////////////////////////////////////////////////

case class VGrpDecl()

object VGrpDecl {
  def apply(ctx: Group_declarationContext): VGrpDecl = ???
}

////////////////////////////////////////////////////////////////////////////////
sealed abstract class VProcDeclItem

object VProcDeclItem {
  def apply(ctx: Process_declarative_itemContext): VProcDeclItem = {
    val spDecl = ctx.subprogram_declaration()
    val spBody = ctx.subprogram_body()
    val typeDecl = ctx.type_declaration()
    val subtypeDecl = ctx.subtype_declaration()
    val constDecl = ctx.constant_declaration()
    val varDecl = ctx.variable_declaration()
    val fileDecl = ctx.file_declaration()
    val aliasDecl = ctx.alias_declaration()
    val attrDecl = ctx.attribute_declaration()
    val attrSpec = ctx.attribute_specification()
    val useClause = ctx.use_clause()
    val grpTempDecl = ctx.group_template_declaration()
    val grpDecl = ctx.group_declaration()
    if (spDecl != null) {
      VProcDeclItemSPD(VSubProgDecl(spDecl))
    } else if (spBody != null) {
      VProcDeclItemSPB(VSubProgBody(spBody))
    } else if (typeDecl != null) {
      VProcDeclItemSTD(VSubtypeDecl(subtypeDecl))
    } else if (constDecl != null) {
      VProcDeclItemCD(VConstDecl(constDecl))
    } else if (varDecl != null) {
      VProcDeclItemVD(VVarDecl(varDecl))
    } else if (fileDecl != null) {
      VProcDeclItemFD(VFileDecl(fileDecl))
    } else if (aliasDecl != null) {
      VProcDeclItemAliasD(VAliasDecl(aliasDecl))
    } else if (attrDecl != null) {
      VProcDeclItemAttrD(VAttrDecl(attrDecl))
    } else if (attrSpec != null) {
      VProcDeclItemAttrS(VAttrSpec(attrSpec))
    } else if (useClause != null) {
      VProcDeclItemUC(VUseClause(useClause))
    } else if (grpTempDecl != null) {
      VProcDeclGTD(VGrpTempDecl(grpTempDecl))
    } else if (grpDecl != null) {
      VProcDeclGD(VGrpDecl(grpDecl))
    } else throw VIError
  }
}

// subprogram_declaration not defined
case class VProcDeclItemSPD(subProgDecl: VSubProgDecl) extends VProcDeclItem

case class VProcDeclItemSPB(subProgBody: VSubProgBody) extends VProcDeclItem

case class VProcDeclItemTD(typeDecl: VTypeDecl) extends VProcDeclItem

case class VProcDeclItemSTD(subtypeDecl: VSubtypeDecl) extends VProcDeclItem

case class VProcDeclItemCD(constDecl: VConstDecl) extends VProcDeclItem

case class VProcDeclItemVD(varDecl: VVarDecl) extends VProcDeclItem

case class VProcDeclItemFD(fileDecl: VFileDecl) extends VProcDeclItem

case class VProcDeclItemAliasD(aliasDecl: VAliasDecl) extends VProcDeclItem

case class VProcDeclItemAttrD(attrDecl: VAttrDecl) extends VProcDeclItem

case class VProcDeclItemAttrS(attrSpec: VAttrSpec) extends VProcDeclItem

case class VProcDeclItemUC(useClause: VUseClause) extends VProcDeclItem

case class VProcDeclGTD(grpTemplDecl: VGrpTempDecl) extends VProcDeclItem

case class VProcDeclGD(grpDecl: VGrpDecl) extends VProcDeclItem

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class VSeqStat {
  def toI(defInfo: DefInfo): Seq_stmt_complex
}

object VSeqStat {
  def apply(ctx: Sequential_statementContext): VSeqStat = {
    val wait_statementContext = ctx.wait_statement()
    val assertion_statementContext = ctx.assertion_statement()
    val report_statementContext = ctx.report_statement()
    val signal_assignal_statement = ctx.signal_assignment_statement()
    val variable_assignment_statement = ctx.variable_assignment_statement()
    val if_statement = ctx.if_statement()
    val case_statement = ctx.case_statement()
    val loop_statementContext = ctx.loop_statement()
    val next_statement = ctx.next_statement()
    val exit_statement = ctx.exit_statement()
    val nullNode = ctx.NULL()
    val break_statementContext = ctx.break_statement()
    val procedure_call_statementContext = ctx.procedure_call_statement()
    if (wait_statementContext != null) {
      VWaitStat(wait_statementContext)
    } else if (assertion_statementContext != null) {
      VAssertStat(assertion_statementContext)
    } else if (report_statementContext != null) {
      VReportStat(report_statementContext)
    } else if (signal_assignal_statement != null) {
      VSignalAssignStat(signal_assignal_statement)
    } else if (variable_assignment_statement != null) {
      VVarAssignStat(variable_assignment_statement)
    } else if (if_statement != null) {
      VIfStat(if_statement)
    } else if (case_statement != null) {
      VCaseStat(case_statement)
    } else if (loop_statementContext != null) {
      VLoopStat(loop_statementContext)
    } else if (next_statement != null) {
      VNextStat(next_statement)
    } else if (exit_statement != null) {
      VExitStat(exit_statement)
    } else if (nullNode != null) {
      val labelColon = Option(ctx.label_colon()).map(_.getText)
      VNullStat(labelColon)
    } else if (break_statementContext != null) {
      VBreakStat(break_statementContext)
    } else if (procedure_call_statementContext != null) {
      VProcCallStat(procedure_call_statementContext)
    } else throw VIError
  }
}

/////////////////////////////////////////////////////////////////////////////////

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


case class VAssert(cond: VExp, report: Option[VExp], severity: Option[VExp])

object VAssert {
  def apply(ctx: AssertionContext): VAssert = {
    val exps = ctx.expression().map(VExp(_))
    val exp = exps.head
    val report = exps.lift(1)
    val severity = exps.lift(2)
    VAssert(exp, report, severity)
  }
}

case class VTimeOutClause(cond: VExp)

object VTimeOutClause {
  def apply(ctx: Timeout_clauseContext): VTimeOutClause = {
    val cond = VExp(ctx.expression())
    VTimeOutClause(cond)
  }
}

case class VCondClause(cond: VExp)

object VCondClause {
  def apply(ctx: Condition_clauseContext): VCondClause = {
    val cond = VExp(ctx.condition().expression())
    VCondClause(cond)
  }
}

// no sensitivity_clause
case class VWaitStat(labelColon: Option[String], sensitiveList: Option[VSensitiveList],
                     condClause: Option[VCondClause], toClause: Option[VTimeOutClause]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VWaitStat {
  def apply(ctx: Wait_statementContext): VWaitStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val sensitiveList = Option(ctx.sensitivity_clause().sensitivity_list()).map(VSensitiveList(_))
    val condClause = Option(ctx.condition_clause()).map(VCondClause(_))
    val toClause = Option(ctx.timeout_clause()).map(VTimeOutClause(_))
    VWaitStat(labelColon, sensitiveList, condClause, toClause)
  }
}

case class VAssertStat(labelColon: Option[String], vAssert: VAssert) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VAssertStat {
  def apply(ctx: Assertion_statementContext): VAssertStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val vAssert = VAssert(ctx.assertion())
    VAssertStat(labelColon, vAssert)
  }
}

case class VReportStat(labelColon: Option[String], exp: VExp, otherExp: Option[VExp]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VReportStat {
  def apply(ctx: Report_statementContext): VReportStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val exps = ctx.expression().map(VExp(_))
    val exp = exps.head
    val otherExp = exps.lift(1)
    VReportStat(labelColon, exp, otherExp)
  }
}

case class VSignalAssignStat(labelColon: Option[String], target: VTarget, delay: Option[VDelay], waveForm: VWaveForm) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VSignalAssignStat {
  def apply(ctx: Signal_assignment_statementContext): VSignalAssignStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val target = VTarget(ctx.target())
    val delay = Option(ctx.delay_mechanism()).map(VDelay(_))
    val waveForm = VWaveForm(ctx.waveform())
    VSignalAssignStat(labelColon, target, delay, waveForm)
  }
}

case class VVarAssignStat(labelColon: Option[String], target: VTarget, exp: VExp) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = {
    val id = labelColon.getOrElse(defaultId)
    //////////////////////////////////////
    val v_clhs = target.toI_V_Clhs(defInfo)
    // TODO
    // rhs may still have inconsistencies from vhdl to isar
    // currently suppose lhs, rhs must be consistent as to [Scalar, Record]
    val crhs: Crhs = v_clhs match {
      case _: Clhs_v => exp.toIExp(defInfo).crhs_e()
      case _: Clhs_vr => {
        val literal = exp.getLiteral match {
          case Some(l) => l
          case None => throw VIErrorMsg(s"${exp}")
        }
        val identifier = literal.asInstanceOf[VLiteralEnumId].identifier
        val rhs_def = defInfo.getDef(identifier)
        rhs_def match {
          case _: Variable | _: Signal | _: Port => exp.toIExp(defInfo).crhs_e()
          case spl: SPl => Crhs_r(Rl_spl(spl))
          case vl: Vl => Crhs_r(Rl_vl(vl))
        }
      }
    }
    Ssc_va(id, v_clhs, crhs)

  }
}

object VVarAssignStat {
  def apply(ctx: Variable_assignment_statementContext): VVarAssignStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val target = VTarget(ctx.target())
    val exp = VExp(ctx.expression())
    VVarAssignStat(labelColon, target, exp)
  }
}

case class VSeqOfStats(seqElem: List[VSeqStat])

object VSeqOfStats {
  def apply(ctx: Sequence_of_statementsContext): VSeqOfStats = {
    val seqStatList = ctx.sequential_statement().map(VSeqStat(_)).toList
    VSeqOfStats(seqStatList)
  }
}

case class VIfStat(labelColon: Option[String],
                   ifVCond: VExp, ifSeqOfStats: VSeqOfStats,
                   elifConds: List[VExp], elifSeqofStats: List[VSeqOfStats],
                   elseSeqOfStats: Option[VSeqOfStats],
                   vId: Option[String]) extends VSeqStat {

  override def toI(defInfo: DefInfo): Seq_stmt_complex = {
    import VIfStat._
    val id = labelColon.orElse(vId).getOrElse(defaultId)
    val ifCond = ifVCond.toIExp(defInfo)
    val if_stmt_complexList = seq_stmt_complex_list(ifSeqOfStats)(defInfo)
    val elif_complexList = elif_complex_list(elifConds, elifSeqofStats)(defInfo)
    val else_stmt_complexList = elseSeqOfStats match {
      case Some(s) => seq_stmt_complex_list(s)(defInfo)
      // FIXME
      case None => ???
    }
    Ssc_if(id, ifCond, if_stmt_complexList, elif_complexList, else_stmt_complexList)
  }
}

object VIfStat {
  def apply(ctx: If_statementContext): VIfStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val conds = ctx.condition().map(c => VExp(c.expression()))
    val seqOfStatsList = ctx.sequence_of_statements().map(VSeqOfStats(_))
    val elifLength = ctx.ELSIF().length
    require(conds.length <= seqOfStatsList.length && elifLength + 1 <= conds.length)
    val ifCond = conds.head
    val ifSeqOfStats = seqOfStatsList.head
    val elifConds = conds.slice(1, 1 + elifLength).toList
    val elifSeqOfStatsList = seqOfStatsList.slice(1, 1 + elifLength).toList
    val elseSeqOfStats = seqOfStatsList.lift(1 + elifLength)
    val id = Option(ctx.identifier()).map(_.getText)
    VIfStat(labelColon, ifCond, ifSeqOfStats, elifConds, elifSeqOfStatsList, elseSeqOfStats, id)
  }

  def seq_stmt_complex_list(seqOfStats: VSeqOfStats)(defInfo: DefInfo): List[Seq_stmt_complex] = {
    seqOfStats.seqElem.map(_.toI(defInfo))
  }

  def elif_complex_list(cl: List[VExp], sl: List[VSeqOfStats])(defInfo: DefInfo): List[Ssc_elif] = {
    val ssll = sl.map(s => seq_stmt_complex_list(s)(defInfo))
    cl.zip(ssll).map {
      case (exp, ssl) => Ssc_elif(exp.toIExp(defInfo), ssl)
    }
  }

}

/////////////////////////////////////////////////////////////////////////////////
case class VCaseStatAlt(choices: VChoices, seqOfStats: VSeqOfStats) {
  def toI(defInfo:DefInfo):Ssc_when = {
    val exps = choices.choiceList.map(_.getSimplExp.toIExp(defInfo))
    val sstList = seqOfStats.seqElem.map(_.toI(defInfo))
    Ssc_when(IChoices(exps), sstList)
  }
}

object VCaseStatAlt {
  def apply(ctx: Case_statement_alternativeContext): VCaseStatAlt = {
    val choices = VChoices(ctx.choices())
    val seqOfStats = VSeqOfStats(ctx.sequence_of_statements())
    VCaseStatAlt(choices, seqOfStats)
  }
}

case class VCaseStat(labelColon: Option[String],
                     exp: VExp, caseStatAltList: List[VCaseStatAlt],
                     vId: Option[String]) extends VSeqStat {
  require(caseStatAltList.nonEmpty, "caseStatAltList")

  override def toI(defInfo: DefInfo): Seq_stmt_complex = {
    val id = labelColon.orElse(vId).getOrElse(defaultId)
    val cond = exp.toIExp(defInfo)
    /// TODO may need to ensure default case only deal with one "choice"
    val lastCase = caseStatAltList.last
    val (when_complexList: List[Ssc_when], defaultList: List[Seq_stmt_complex]) =
      lastCase.choices.choiceList.head match {
        case VChoiceOthers => {
          val initCases = caseStatAltList.init
          (initCases.map(_.toI(defInfo)), lastCase.seqOfStats.seqElem.map(_.toI(defInfo)))
        }
        case _ => ???

      }

    Ssc_case(id, cond, when_complexList, defaultList)
  }
}

object VCaseStat {
  def apply(ctx: Case_statementContext): VCaseStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val exp = VExp(ctx.expression())
    val caseStatAltList = ctx.case_statement_alternative().map(VCaseStatAlt(_)).toList
    val id = Option(ctx.identifier()).map(_.getText)
    VCaseStat(labelColon, exp, caseStatAltList, id)

  }
}

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class VIterScheme

case class VIterSchemeW(cond: VExp) extends VIterScheme

case class VParamSpec(id: String, discreteRange: VDiscreteRange)

case class VIterSchemeF(paramSpec: VParamSpec) extends VIterScheme

case class VLoopStat(labelColon: Option[String], seqOfStats: VSeqOfStats, id: Option[String]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VLoopStat {
  def apply(ctx: Loop_statementContext): VLoopStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val seqOfStats = VSeqOfStats(ctx.sequence_of_statements())
    val id = Option(ctx.identifier()).map(_.getText)
    VLoopStat(labelColon, seqOfStats, id)
  }
}

/////////////////////////////////////////////////////////////////////////////////

case class VNextStat(labelColon: Option[String], id: Option[String], cond: Option[VExp]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VNextStat {
  def apply(ctx: Next_statementContext): VNextStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val id = Option(ctx.identifier()).map(_.getText)
    val cond = Option(ctx.condition()).map(c => VExp(c.expression()))
    VNextStat(labelColon, id, cond)
  }
}


case class VExitStat(labelColon: Option[String], id: Option[String], cond: Option[VExp]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VExitStat {
  def apply(ctx: Exit_statementContext): VExitStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val id = Option(ctx.identifier()).map(_.getText)
    val cond = Option(ctx.condition()).map(c => VExp(c.expression()))
    VExitStat(labelColon, id, cond)
  }
}

case class VRetStat(labelColon: Option[String], exp: Option[VExp]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

case class VNullStat(labelColon: Option[String]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

/////////////////////////////////////////////////////////////////////////////////

case class VBreakSelectorClause(name: VName)

object VBreakSelectorClause {
  def apply(ctx: Break_selector_clauseContext): VBreakSelectorClause = {
    val name = VName(ctx.name())
    VBreakSelectorClause(name)
  }
}

case class VBreakElem(breakSelectorClause: Option[VBreakSelectorClause], name: VName, exp: VExp)

object VBreakElem {
  def apply(ctx: Break_elementContext): VBreakElem = {
    val breakSelectorContext = Option(ctx.break_selector_clause()).map(VBreakSelectorClause(_))
    val name = VName(ctx.name())
    val exp = VExp(ctx.expression())
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

case class VBreakStat(labelColon: Option[String], breakList: Option[VBreakList], cond: Option[VExp]) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VBreakStat {
  def apply(ctx: Break_statementContext): VBreakStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val breakList = Option(ctx.break_list()).map(VBreakList(_))
    val cond = Option(ctx.condition()).map(c => VExp(c.expression()))
    VBreakStat(labelColon, breakList, cond)
  }
}

/////////////////////////////////////////////////////////////////////////////////
case class VProcCallStat(labelColon: Option[String], procCall: VProcCall) extends VSeqStat {
  override def toI(defInfo: DefInfo): Seq_stmt_complex = ???
}

object VProcCallStat {
  def apply(ctx: Procedure_call_statementContext): VProcCallStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val procCall = VProcCall(ctx.procedure_call())
    VProcCallStat(labelColon, procCall)
  }
}

/////////////////////////////////////////////////////////////////////////////////
case class VProcStatPart(seqStatList: List[VSeqStat]) {
  def toI(defInfo: DefInfo): List[Seq_stmt_complex] = {
    seqStatList.map(_.toI(defInfo))
  }
}

object VProcStatPart {
  def apply(ctx: Process_statement_partContext): VProcStatPart = {
    val seqStatList = ctx.sequential_statement().map(VSeqStat(_)).toList
    VProcStatPart(seqStatList)
  }
}

case class VProcDeclPart(procDeclItem: List[VProcDeclItem])

object VProcDeclPart {
  def apply(ctx: Process_declarative_partContext): VProcDeclPart = {
    val processDeclItemList = ctx.process_declarative_item().map(VProcDeclItem(_)).toList
    VProcDeclPart(processDeclItemList)
  }
}

case class VProcStat(labelColon: Option[String],
                     p1: Boolean,
                     sensitivitylist: Option[VSensitiveList],
                     is: Boolean,
                     procDeclPart: VProcDeclPart,
                     procStatPart: VProcStatPart,
                     p2: Boolean,
                     identifier: Option[String]) {
  def toI(defInfo: DefInfo): Csc_ps = {
    // guess
    val id = labelColon.orElse(identifier).getOrElse(defaultId)
    val iSensitiveList = sensitivitylist.map(_.toI(defInfo))
    logger.info(s"${iSensitiveList}")
    // don't care about procDeclPart
    // procStatPart => seq_stmt_complexList
    val seq_stmt_complexList: List[Seq_stmt_complex] = procStatPart.toI(defInfo)
    Csc_ps(id, iSensitiveList, seq_stmt_complexList)
  }

}

object VProcStat {
  def apply(ctx: Process_statementContext): VProcStat = {
    val labelColon = Option(ctx.label_colon()).map(_.getText)
    val posponds = ctx.POSTPONED()
    val (p1, p2) = if (posponds.length == 2) {
      (true, true)
    } else if (posponds.isEmpty) {
      (false, false)
    } else if (posponds.length == 1) {
      // FIXME don't know
      (true, false)
    } else throw VIError
    val sensitivilist = Option(ctx.sensitivity_list()).map(VSensitiveList(_))
    val is = ctx.IS() != null
    val procDeclPart = VProcDeclPart(ctx.process_declarative_part())
    val procStatPart = VProcStatPart(ctx.process_statement_part())
    val id = Option(ctx.identifier()).map(_.getText)
    VProcStat(labelColon, p1, sensitivilist, is, procDeclPart, procStatPart, p2, id)
  }
}

/////////////////////////////////////////////////////////////////////////////////

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
    val exp = ctx.expression()
    if (exp != null) {
      VActualDesignatorE(VExp(exp))
    } else {
      VActualDesignatorO
    }
  }
}

case class VActualDesignatorE(vExp: VExp) extends VActualDesignator

case object VActualDesignatorO extends VActualDesignator

sealed abstract class VActualPart

object VActualPart {
  def apply(ctx: Actual_partContext): VActualPart = {
    val name = VName(ctx.name())
    val actualDesignator = VActualDesignator(ctx.actual_designator())
    if (name != null) {
      VActualPartN(name, actualDesignator)
    } else {
      VActualPartD(actualDesignator)
    }
  }
}

case class VActualPartN(name: VName, actualDesignator: VActualDesignator) extends VActualPart

case class VActualPartD(actualDesignator: VActualDesignator) extends VActualPart

case class VAssocElem(formalPart: Option[VFormalPart], ActualPart: VActualPart)

object VAssocElem {
  def apply(ctx: Association_elementContext): VAssocElem = {
    val formalPart = Option(ctx.formal_part()).map(VFormalPart(_))
    val actualPart = VActualPart(ctx.actual_part())
    VAssocElem(formalPart, actualPart)
  }
}

case class VAssocList(assocElemList: List[VAssocElem]) {
  require(assocElemList.nonEmpty, "assocElemList")
}

object VAssocList {
  def apply(ctx: Association_listContext): VAssocList = {
    val assocElemList = ctx.association_element().map(VAssocElem(_)).toList
    VAssocList(assocElemList)
  }
}

// no actual_parameter_part
case class VProcCall(selectedName: String, assocList: VAssocList)

object VProcCall {
  def apply(ctx: Procedure_callContext): VProcCall = {
    val selectedname = ctx.selected_name().getText
    val assocList = VAssocList(ctx.actual_parameter_part().association_list())
    VProcCall(selectedname, assocList)
  }
}

/////////////////////////////////////////////////////////////////////////////////