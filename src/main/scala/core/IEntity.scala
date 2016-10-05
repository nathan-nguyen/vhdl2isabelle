package core

import scala.language.implicitConversions

case class IEnv_sp(signalList: List[Signal], portList: List[Port], spnlList: List[SPnl]) {

  override def toString: String = {
    val signalListNotEmpty  = signalList.map(_.as_list).size  > 0
    val portListNotEmpty    = portList.map(_.as_list).size    > 0
    val spnlListNotEmpty    = spnlList.map(_.as_list).size    > 0
    val firstSeparator     = if (signalListNotEmpty && (portListNotEmpty || spnlListNotEmpty)) "@" else ""
    val secondSeparator    = if ((signalListNotEmpty || spnlListNotEmpty) && spnlListNotEmpty) "@" else ""
    s"""${signalList.map(_.as_list).mkString("@")}${firstSeparator}
        ${portList.map(_.as_list).mkString("@")}${secondSeparator}
        ${spnlList.map(_.as_list).mkString("@")}""".stripMargin
  }
}

case class IEnv_v(variableList: List[Variable], vnlList: List[Vnl]) {
  override def toString: String = {
    val separatorString = if (variableList.map(_.as_list).size > 0 && vnlList.map(_.as_list).size > 0) "@" else ""
    s"""${variableList.map(_.as_list).mkString("@")}${separatorString}
        ${vnlList.map(_.as_list) mkString ("@")}""".stripMargin
  }
}

// reserved as class
case class IEnv_t() {
  override def toString: String = "[]"
}

case class IEnv(env_sp: IEnv_sp, env_v: IEnv_v, env_t: IEnv_t) {
  override def toString =
    s"""| ⦇env_sp = ${env_sp},
        | env_v = ${env_v},
        | env_t = ${env_t}⦈""".stripMargin
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

sealed abstract class SP_clhs {
  override def toString = this match {
    case Clhs_sp(sp_clhs) => s"(clhs_sp ${sp_clhs})"
    case Clhs_spr(spl) => s"(clhs_spr ${spl.getId})"
  }
}

case class Clhs_sp(sp_clhs: SP_lhs) extends SP_clhs

case class Clhs_spr(spl: SPl) extends SP_clhs

//********************************************************************************************************************//

case class Sensitivity_list()

//********************************************************************************************************************//

sealed abstract class Discrete_range {
  override def toString = this match {
    case VHDL_dis_to(l, r) => s"(${l} TO ${r})"
    case VHDL_dis_downto(l, r) => s"(${l} DOWNTO ${r})"
  }
}

case class VHDL_dis_to(l: IExp, r: IExp) extends Discrete_range

case class VHDL_dis_downto(l: IExp, r: IExp) extends Discrete_range

//********************************************************************************************************************//

sealed abstract class SP_lhs {
  // since "Nil" case is wrapped inside "SigPrt", it cannot be directly used with "sn.isar_sp"
  override def toString = this match {
    case Lhs_s(sigPrt, sn) => sn.suffixList match {
      case Nil => s"(lhs_s ${sigPrt})"
      case _ => s"(lhs_s (sp_of_spl ${sn.isar_sp}))"
    }
    case Lhs_sa(sigPrt, discreteRange, sn) => sn.suffixList match{
      case Nil => s"(lhs_sa ${sigPrt} ${discreteRange})"
      case _ => s"(lhs_sa (sp_of_spl ${sn.isar_sp}) ${discreteRange})"
    }
  }
}

case class Lhs_s(sigPrt: SigPrt, sn: VSelectedName) extends SP_lhs

case class Lhs_sa(sigPrt: SigPrt, discreteRange: Discrete_range, sn: VSelectedName) extends SP_lhs

//********************************************************************************************************************//

sealed abstract class V_lhs {
  override def toString = this match {
    case Lhs_v(v, sn) => sn.suffixList match {
      case Nil => s"(lhs_v ${sn.isar_v})"
      case _ => s"(lhs_v (var_of_vl ${sn.isar_v}))"
    }
    case Lhs_va(v, discreteRange, sn) => sn.suffixList match {
      case Nil => s"(lhs_va ${sn.isar_v} ${discreteRange})"
      case _ => s"(lhs_va (var_of_vl ${sn.isar_v}) ${discreteRange})"
    }
  }
}

case class Lhs_v(variable: Variable, sn: VSelectedName) extends V_lhs

case class Lhs_va(variable: Variable, discreteRange: Discrete_range, sn: VSelectedName) extends V_lhs

//********************************************************************************************************************//

sealed abstract class Asmt_rhs {
  override def toString = this match {
    case Rhs_e(e) => s"(rhs_e ${e})"
    case Rhs_o(e) => s"(OTHERS => ${e})"
  }
}

case class Rhs_e(exp: IExp) extends Asmt_rhs

case class Rhs_o(exp: IExp) extends Asmt_rhs

//********************************************************************************************************************//

sealed abstract class Seq_stmt {
  override def toString = this match {
    case Sst_sa(id, sP_lhs, asmt_rhs) => s"(sst_sa ${id} ${sP_lhs} ${asmt_rhs})"
    case Sst_va(id, v_lhs, asmt_rhs) => s"(sst_va ${id} ${v_lhs} ${asmt_rhs})"
    case Sst_if(id, cond, then_seq_stmtList, else_seq_stmtList) => s"(sst_if ${id} ${cond} ${then_seq_stmtList} ${else_seq_stmtList})"
    case Sst_l(id, cond, body_seq_stmtList) => s"(sst_l ${id} ${cond} ${body_seq_stmtList})"
    case Sst_n(id, cond) => s"(sst_n ${id} ${cond})"
    case Sst_e(id, cond) => s"(sst_e ${id} ${cond})"
    case Sst_nl => "sst_nl"
  }
}

case class Sst_sa(id: String, sP_lhs: SP_lhs, asmt_rhs: Asmt_rhs) extends Seq_stmt

case class Sst_va(id: String, v_lhs: V_lhs, asmt_rhs: Asmt_rhs) extends Seq_stmt

case class Sst_if(id: String, cond: IExp, then_seq_stmtList: List[Seq_stmt], else_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_l(id: String, cond: IExp, body_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_n(id: String, cond: IExp) extends Seq_stmt

case class Sst_e(id: String, cond: IExp) extends Seq_stmt

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
    case Rl_spl(spl) => s"(rhsl_of_spl ${spl.getId})"
    case Rl_vl(vl) => s"(rhsl_of_vl ${vl.getId})"
  }
}

case class Rl_s(signal: Signal) extends Rhsl

case class Rl_p(port: Port) extends Rhsl

case class Rl_v(variable: Variable) extends Rhsl

// use "spl", "rhsl_of_spl"
case class Rl_spl(spl: SPl) extends Rhsl

// use "vl", "rhsl_of_vl"
case class Rl_vl(vl: Vl) extends Rhsl

//case class Rnl(rhslList: List[Rhsl]) extends Rhsl

//********************************************************************************************************************//
sealed abstract class Crhs {
  override def toString = this match {
    case Crhs_e(asmt_rhs) => s"(crhs_e ${asmt_rhs})"
    case Crhs_r(rhsl) => s"(crhs_r ${rhsl})"
  }
}


case class Crhs_e(asmt_rhs: Asmt_rhs) extends Crhs

case class Crhs_r(rhsl: Rhsl) extends Crhs

//********************************************************************************************************************//
sealed abstract class V_clhs {
  override def toString = this match {
    case Clhs_v(v_lhs) => s"(clhs_v ${v_lhs})"
    case Clhs_vr(vl) => s"(clhs_vr ${vl.getId})"
  }
}

case class Clhs_v(v_lhs: V_lhs) extends V_clhs

case class Clhs_vr(vl: Vl) extends V_clhs

//********************************************************************************************************************//

case class IChoices(expList: List[IExp])

case class Ssc_when(choices: IChoices, Seq_stmt_complexList: List[Seq_stmt_complex]) {
  override def toString = s"(WHEN ${choices.expList.ISAR} => ${Seq_stmt_complexList.ISAR})"
}

case class Ssc_elif(cond: IExp, Seq_stmt_complexList: List[Seq_stmt_complex]) {
  override def toString = s"(ELSIF ${cond} THEN ${Seq_stmt_complexList.ISAR})"
}

sealed abstract class Seq_stmt_complex {
  override def toString = this match {
    case Ssc_sa(id, sP_clhs, crhs) => s"(''${id}'': ${sP_clhs} <= ${crhs})"
    case Ssc_va(id, v_clhs, crhs) => s"(''${id}'': ${v_clhs} := ${crhs})"
    case Ssc_if(id, cond, if_seq_stmt_complexList, elif_complexList, else_complexList) => {
      s"(''${id}'': IF ${cond} THEN ${if_seq_stmt_complexList.ISAR} ${elif_complexList.ISAR} ELSE ${else_complexList.ISAR} END IF)"
    }
    case Ssc_case(id, cond, when_complexList, defaultSeq_stmt_complexList) => {
      s"(''${id}'': CASE ${cond} IS ${when_complexList.ISAR} WHEN OTHERS => ${defaultSeq_stmt_complexList.ISAR} END CASE)"
    }
    case Ssc_while(id, cond, bodySeq_stmt_complexList) => {
      s"(''${id}'': WHILE ${cond} LOOP ${bodySeq_stmt_complexList.ISAR} END LOOP)"
    }
    case Ssc_for(id, cond, discrete_range, seq_stmt_complexList) => {
      s"(''${id}'': FOR ${cond} IN ${discrete_range} LOOP ${seq_stmt_complexList.ISAR} END LOOP)"
    }
    case Ssc_n(id, tId, cond) => s"(''${id}'': NEXT ${tId} WHEN ${cond})"
    case Ssc_e(id, tId, cond) => s"(''${id}'': EXIT ${tId} WHEN ${cond})"
    case Ssc_nl => "(NULL)"
  }
}

case class Ssc_sa(id: IdTy, sP_clhs: SP_clhs, crhs: Crhs) extends Seq_stmt_complex

case class Ssc_va(id: IdTy, v_clhs: V_clhs, crhs: Crhs) extends Seq_stmt_complex

case class Ssc_if(id: IdTy, ifCond: IExp, if_seq_stmt_complexList: List[Seq_stmt_complex],
                  elif_complexList: List[Ssc_elif],
                  else_complexList: List[Seq_stmt_complex]) extends Seq_stmt_complex

case class Ssc_case(id: IdTy, cond: IExp, when_complexList: List[Ssc_when],
                    defaultSeq_stmt_complexList: List[Seq_stmt_complex]) extends Seq_stmt_complex

case class Ssc_while(id: IdTy, cond: IExp, bodySeq_stmt_complexList: List[Seq_stmt_complex]) extends Seq_stmt_complex

case class Ssc_for(id: IdTy, cond: IExp, discrete_range: Discrete_range,
                   seq_stmt_complexList: List[Seq_stmt_complex]) extends Seq_stmt_complex

case class Ssc_n(id: IdTy, tId: IdTy, cond: IExp) extends Seq_stmt_complex

case class Ssc_e(id: IdTy, tId: IdTy, cond: IExp) extends Seq_stmt_complex

case object Ssc_nl extends Seq_stmt_complex

//********************************************************************************************************************//

sealed abstract class Gen_type {
  override def toString = this match {
    case For_gen(exp, discrete_range) => s"FOR ${exp} IN ${discrete_range} GENERATE"
    case If_gen(exp) => s"IF ${exp} GENERATE"
  }
}

// TODO test definition in isar
case class For_gen(exp: IExp, discrete_range: Discrete_range) extends Gen_type

case class If_gen(exp: IExp) extends Gen_type

//********************************************************************************************************************//

case class As_when(crhs: Crhs, cond: IExp) {
  override def toString = s"(${crhs} WHEN ${cond} ELSE)"
}

case class ISensitiveList(sp_IDefList: List[SP_IDef]) {
  override def toString = sp_IDefList.map(_.as_list).mkString("", "@", "")
}

sealed abstract class Conc_stmt_complex {
  override def toString = this match {
    case Csc_ps(id, iSensitivilistOpt, seq_stmt_complexList) => {
      val iSensitivilistRepr = iSensitivilistOpt.map(_.toString).getOrElse("[]")
      s"(''${id}'': PROCESS (${iSensitivilistRepr}) BEGIN \n${seq_stmt_complexList.ISAR_conc} \nEND PROCESS)"
    }
    case Csc_ca(id, sp_clhs, casmt_rhsList, crhs) => {
      s"(''${id}'': ${sp_clhs} <= <${casmt_rhsList.ISAR_conc}> ${crhs})"
    }
    case Csc_gen(id, gen_type, conc_stmt_complexList) => {
      s"(''${id}'': ${gen_type} BEGIN \n${conc_stmt_complexList.ISAR_conc} \nEND GENERATE)"
    }
  }
}

// have to make iSensitivilist option
case class Csc_ps(id: IdTy, iSensitivilist: Option[ISensitiveList],
                  seq_stmt_complexList: List[Seq_stmt_complex]) extends Conc_stmt_complex

case class Csc_ca(id: IdTy, sp_clhs: SP_clhs, casmt_rhsList: List[As_when], crhs: Crhs) extends Conc_stmt_complex

case class Csc_gen(id: IdTy, gen_type: Gen_type,
                   conc_stmt_complexList: List[Conc_stmt_complex]) extends Conc_stmt_complex

//********************************************************************************************************************//

// it is an IDef however not treated so
case class IEntity(id: String, env: IEnv, resFn: IResFn, conc_stmt_complexList: List[Conc_stmt_complex]) {
  override def toString =
    s"""definition ${id}:: \"vhdl_desc_complex\" where
        |"${id} ≡
        |  let env = ${env};
        |      resfn = ${resFn};
        |      cst_list = ${conc_stmt_complexList.ISAR}
        |  in (env, resfn, cst_list)
        |"
     """.stripMargin
}