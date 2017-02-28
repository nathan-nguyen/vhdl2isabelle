package core.isabellesyntax

import core._
import core.vhdlsyntax._
import scala.language.implicitConversions

/**
  * Created by Hongxu Chen.
  */
case class IEnv_sp(signalList: List[Signal], portList: List[Port], spnlList: List[ISpl_Spnl]) {

  override def toString: String = {
    val signalListNotEmpty  = signalList.map(_.as_list).size  > 0
    val portListNotEmpty    = portList.map(_.as_list).size    > 0
    val spnlListNotEmpty    = spnlList.map(_.as_list).size    > 0
    val firstSeparator     = if (signalListNotEmpty && (portListNotEmpty || spnlListNotEmpty)) "@" else ""
    val secondSeparator    = if ((signalListNotEmpty || spnlListNotEmpty) && spnlListNotEmpty) "@" else ""
    if (signalListNotEmpty || portListNotEmpty || spnlListNotEmpty) {
      s"""${signalList.map(_.as_list).mkString("@")}${firstSeparator}${portList.map(_.as_list).mkString("@")}${secondSeparator}${spnlList.map(_.as_list).mkString("@")}""".stripMargin
    }
    else s"[]"
  }
}

case class IEnv_v(variableList: List[IVariable], vnlList: List[IVl_Vnl]) {
  override def toString: String = {
    val separatorString = if (variableList.map(_.as_list).size > 0 && vnlList.map(_.as_list).size > 0) "@" else ""
    s"""${variableList.map(_.as_list).mkString("@")}${separatorString}${vnlList.map(_.as_list) mkString ("@")}""".stripMargin
  }
}

// reserved as class
case class IEnv_t() {
  override def toString: String = "[]"
}

case class IEnv(env_sp: IEnv_sp, env_v: IEnv_v, env_t: IEnv_t) {
  override def toString =
    s"""⦇
        |    env_sp = ${env_sp},
        |    env_v = ${env_v},
        |    env_t = ${env_t}
        |  ⦈""".stripMargin
}

object IEnv {
  def apply(di: DefInfo): IEnv = {
    implicit def toSecond[T](s: Seq[T]): List[T] = s.toList
    val env_sp = IEnv_sp(di.s_raw, di.p_raw, di.spnl_raw)
    val env_v = IEnv_v(di.v_raw, di.vnl_raw)
    val env_t = IEnv_t()
    new IEnv(env_sp, env_v, env_t)
  }
}

// reserved as class
case class IResFn() {
  override def toString = "λx.(None)"
}

//********************************************************************************************************************//

sealed abstract class ISp_clhs {
  override def toString = this match {
    case ISp_clhs_Clhs_sp(sp_clhs) => s"(clhs_sp ${sp_clhs})"
    case ISp_clhs_Clhs_spr(spl) => s"(clhs_spr ${spl.getName})"
  }
}

object ISp_clhs {
  def apply(iDef: IDef, selectedName: VSelectedName,
            discreteRangeOption: Option[IDiscrete_range]): ISp_clhs = iDef match {
    case signal: Signal => {
      val sp_s = SP_s(signal, selectedName)
      val sp_lhs = discreteRangeOption match {
        case None => Lhs_s(sp_s, selectedName)
        case Some(discrete_range) => Lhs_sa(sp_s, discrete_range, selectedName)
      }
      ISp_clhs_Clhs_sp(sp_lhs)
    }
    case port: Port => {
      val sp_p = SP_p(port, selectedName)
      val sp_lhs = discreteRangeOption match {
        case None => Lhs_s(sp_p, selectedName)
        case Some(discrete_range) => Lhs_sa(sp_p, discrete_range, selectedName)
      }
      ISp_clhs_Clhs_sp(sp_lhs)
    }
    case spl: ISpl => ISp_clhs_Clhs_spr(spl)
    case _ => handler(s"${iDef}")
  }
}

case class ISp_clhs_Clhs_sp(sp_clhs: SP_lhs) extends ISp_clhs

case class ISp_clhs_Clhs_spr(spl: ISpl) extends ISp_clhs

//********************************************************************************************************************//

case class Sensitivity_list()

//********************************************************************************************************************//

/** In Isabelle:
  * datatype discrete_range =
  * vhdl_dis_to expression expression (infixl "TO" 60)
  * | vhdl_dis_downto expression expression (infixl "DOWNTO" 60)
  */

sealed abstract class IDiscrete_range {
  override def toString = this match {
    case IVhdl_dis_to(l, r) => s"(${l} TO ${r})"
    case IVhdl_dis_downto(l, r) => s"(${l} DOWNTO ${r})"
  }
}

case class IVhdl_dis_to(l: IExpression, r: IExpression) extends IDiscrete_range

case class IVhdl_dis_downto(l: IExpression, r: IExpression) extends IDiscrete_range

//********************************************************************************************************************//

sealed abstract class SP_lhs {
  // since "Nil" case is wrapped inside "SigPrt", it cannot be directly used with "sn.isa_sp"
  override def toString = this match {
    case Lhs_s(sigPrt, sn) => sn.suffixList match {
      case Nil => s"(lhs_s ${sigPrt})"
      case _ => s"(lhs_s (sp_of_spl ${sn.isa_sp}))"
    }
    case Lhs_sa(sigPrt, discreteRange, sn) => sn.suffixList match{
      case Nil => s"(lhs_sa ${sigPrt} ${discreteRange})"
      case _ => s"(lhs_sa (sp_of_spl ${sn.isa_sp}) ${discreteRange})"
    }
  }
}

case class Lhs_s(sigPrt: SigPrt, sn: VSelectedName) extends SP_lhs

case class Lhs_sa(sigPrt: SigPrt, discreteRange: IDiscrete_range, sn: VSelectedName) extends SP_lhs

//********************************************************************************************************************//

sealed abstract class IAsmt_rhs {
  override def toString = this match {
    case IAsmt_rhs_Rhs_e(iExpression) => s"(rhs_e ${iExpression})"
    case IAsmt_rhs_Rhs_o(iExpression) => s"(OTHERS => ${iExpression})"
  }
}

object IAsmt_rhs {
  def apply(iExpression: IExpression): IAsmt_rhs = {
    // TODO: Other case is not captured
    IAsmt_rhs_Rhs_e(iExpression)
  }
}

case class IAsmt_rhs_Rhs_e(iExpression: IExpression) extends IAsmt_rhs

case class IAsmt_rhs_Rhs_o(iExpression: IExpression) extends IAsmt_rhs

//********************************************************************************************************************//

sealed abstract class Seq_stmt {
  override def toString = this match {
    case Sst_sa(id, sP_lhs, asmt_rhs) => s"(sst_sa ${id} ${sP_lhs} ${asmt_rhs})"
    case Sst_va(id, v_lhs, asmt_rhs) => s"(sst_va ${id} ${v_lhs} ${asmt_rhs})"
    case Sst_if(id, cond, then_seq_stmtList, else_seq_stmtList) =>
      s"(sst_if ${id} ${cond} ${then_seq_stmtList} ${else_seq_stmtList})"
    case Sst_l(id, cond, body_seq_stmtList) => s"(sst_l ${id} ${cond} ${body_seq_stmtList})"
    case Sst_n(id, cond) => s"(sst_n ${id} ${cond})"
    case Sst_e(id, cond) => s"(sst_e ${id} ${cond})"
    case Sst_nl => "sst_nl"
  }
}

case class Sst_sa(id: String, sP_lhs: SP_lhs, asmt_rhs: IAsmt_rhs) extends Seq_stmt

case class Sst_va(id: String, v_lhs: IV_lhs, asmt_rhs: IAsmt_rhs) extends Seq_stmt

case class Sst_if(id: String, cond: IExpression, then_seq_stmtList: List[Seq_stmt], else_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_l(id: String, cond: IExpression, body_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_n(id: String, cond: IExpression) extends Seq_stmt

case class Sst_e(id: String, cond: IExpression) extends Seq_stmt

case object Sst_nl extends Seq_stmt

//********************************************************************************************************************//

sealed abstract class Rhsl {
  override def toString = this match {
    case Rl_s(s) => s"(rl_s ${s})"
    case Rl_p(p) => s"(rl_p ${p})"
    case Rl_v(v) => s"(rl_v ${v})"
    // FIXME: should also accept xxx evaluated to spl or vl
    // NOTE: these two are simulated for function
    //    case Rnl(rhslList) => s"(rnl ${rhslList.ISAR})"
    case Rl_spl(spl) => s"(rhsl_of_spl ${spl.getName})"
    case Rl_vl(vl) => s"(rhsl_of_vl ${vl.getName})"
  }
}

case class Rl_s(signal: Signal) extends Rhsl

case class Rl_p(port: Port) extends Rhsl

case class Rl_v(variable: IVariable) extends Rhsl

// use "spl", "rhsl_of_spl"
case class Rl_spl(spl: ISpl) extends Rhsl

// use "vl", "rhsl_of_vl"
case class Rl_vl(vl: IVl) extends Rhsl

//case class Rnl(rhslList: List[Rhsl]) extends Rhsl

//********************************************************************************************************************//
sealed abstract class IV_clhs {
  override def toString = this match {
    case IV_clhs_Clhs_v(iV_lhs) => s"(clhs_v ${iV_lhs})"
    case IV_clhs_Clhs_vr(iVl) => s"(clhs_vr ${iVl.getName})"
  }
}

object IV_clhs {
  def apply (iDef: IDef, selectedName: VSelectedName,
             discreteRangeOption: Option[IDiscrete_range]): IV_clhs = iDef match {
    case iVariable: IVariable => {
      val iV_lhs = discreteRangeOption match {
        case None => IV_lhs_Lhs_v(iVariable, selectedName)
        case Some(discrete_range) => IV_lhs_Lhs_va(iVariable, discrete_range, selectedName)
      }
      IV_clhs_Clhs_v(iV_lhs)
    }
    case iVl: IVl => IV_clhs_Clhs_vr(iVl)
    case _ => handler(s"${iDef}")
  }

  def apply (selectedName: VSelectedName, recordArrayIndex: IExpression,
             selectedNameSuffix: VSelectedName, discreteRangeOptionSuffix: Option[IDiscrete_range]): IV_clhs =  {
    IV_clhs_Clhs_v(IV_lhs_Lhs_var(selectedName, recordArrayIndex, selectedNameSuffix))
  }
}

case class IV_clhs_Clhs_v(iV_lhs: IV_lhs) extends IV_clhs

case class IV_clhs_Clhs_vr(vl: IVl) extends IV_clhs

//********************************************************************************************************************//

case class IChoices(expList: List[IExpression])

case class Ssc_when(choices: IChoices, Seq_stmt_complexList: List[ISeq_stmt_complex]) {
  override def toString = s"(WHEN ${choices.expList.ISABELLE} => ${Seq_stmt_complexList.ISABELLE})"
}

case class Ssc_elif(cond: IExpression, ISeq_stmt_complexList: List[ISeq_stmt_complex]) {
  override def toString = s"(ELSIF ${cond} THEN ${ISeq_stmt_complexList.ISABELLE})"
}

//********************************************************************************************************************//

sealed abstract class Gen_type {
  override def toString = this match {
    case For_gen(exp, discrete_range) => s"FOR ${exp} IN ${discrete_range} GENERATE"
    case If_gen(exp) => s"IF ${exp} GENERATE"
  }
}

// TODO [HC] Test definition in Isabelle
case class For_gen(exp: IExpression, discrete_range: IDiscrete_range) extends Gen_type

case class If_gen(exp: IExpression) extends Gen_type

//********************************************************************************************************************//

case class As_when(iAsmt_rhs: IAsmt_rhs, iCondition: IExpression) {
  override def toString = s"(${iAsmt_rhs} WHEN ${iCondition} ELSE)"
}

case class ISensitiveList(sp_IDefList: List[SP_IDef]) {
  override def toString = sp_IDefList.map(_.as_list).mkString("", "@", "")
}

sealed abstract class IConc_stmt_complex {
  override def toString = this match {
    case IConc_stmt_complex_Csc_ps(name, iSensitivilistOpt, seq_stmt_complexList) => {
      val iSensitivilistRepr = iSensitivilistOpt.map(_.toString).getOrElse("[]")
      s"(''${name}'': PROCESS (${iSensitivilistRepr}) BEGIN \n${seq_stmt_complexList.ISABELLE_conc} \nEND PROCESS)"
    }
    case IConc_stmt_complex_Csc_ca(id, sp_clhs, casmt_rhsList, iAsmt_rhs) => {
      s"(''${id}'': ${sp_clhs} <= <${casmt_rhsList.ISABELLE_conc}> ${iAsmt_rhs})"
    }
    case IConc_stmt_complex_Csc_gen(id, gen_type, conc_stmt_complexList) => {
      s"(''${id}'': ${gen_type} BEGIN \n${conc_stmt_complexList.ISABELLE_conc} \nEND GENERATE)"
    }
  }
}

// [HC] Have to make iSensitivilist option
case class IConc_stmt_complex_Csc_ps(name: String, iSensitivilist: Option[ISensitiveList],
                                     seq_stmt_complexList: List[ISeq_stmt_complex]) extends IConc_stmt_complex

case class IConc_stmt_complex_Csc_ca(name: String, sp_clhs: ISp_clhs, casmt_rhsList: List[As_when],
                                     iAsmt_rhs: IAsmt_rhs) extends IConc_stmt_complex

case class IConc_stmt_complex_Csc_gen(name: String, gen_type: Gen_type,
                                      conc_stmt_complexList: List[IConc_stmt_complex]) extends IConc_stmt_complex

//********************************************************************************************************************//

// [HC]: It is an IDef however not treated so
case class IEntity(id: String, env: IEnv, resFn: IResFn, conc_stmt_complexList: List[IConc_stmt_complex]) {
  val subprogramComplexList = IdentifierMap.getSubprogramComplexList

  override def toString =
    s"""definition ${id}:: \"vhdl_desc_complex\" where
        |"${id} ≡
        |  let env = ${env};
        |  resfn = ${resFn};
        |  cst_list = ${conc_stmt_complexList.ISABELLE};
        |  spc_list = ${subprogramComplexList.mkString("[", "]@[", "]")}
        |in (env, resfn, cst_list, spc_list)
        |"
     """.stripMargin
}