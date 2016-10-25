package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Thanh Nam Nguyen on 25/10/16.
  */
sealed abstract class ISeq_stmt_complex {
  override def toString = this match {
    case ISeq_stmt_complex_Ssc_sa(name, iSp_clhs, iAsmt_rhs) => s"(''${name}'': ${iSp_clhs} <= ${iAsmt_rhs})"
    case ISeq_stmt_complex_Ssc_va(name, iV_clhs, iAsmt_rhs) => s"(''${name}'': ${iV_clhs} := ${iAsmt_rhs})"
    case ISeq_stmt_complex_Ssc_if(name, iCondition, if_seq_stmt_complexList, elif_complexList, else_complexList) => s"(''${name}'': IF ${iCondition} THEN ${if_seq_stmt_complexList.ISABELLE} ${elif_complexList.ISABELLE} ELSE ${else_complexList.ISABELLE} END IF)"
    case ISeq_stmt_complex_Ssc_case(name, cond, when_complexList, defaultSeq_stmt_complexList) => s"(''${name}'': CASE ${cond} IS ${when_complexList.ISABELLE} WHEN OTHERS => ${defaultSeq_stmt_complexList.ISABELLE} END CASE)"
    case ISeq_stmt_complex_Ssc_while(name, cond, bodySeq_stmt_complexList) => s"(''${name}'': WHILE ${cond} LOOP ${bodySeq_stmt_complexList.ISABELLE} END LOOP)"
    case ISeq_stmt_complex_Ssc_for(name, cond, discrete_range, seq_stmt_complexList) => s"(''${name}'': FOR ${cond} IN ${discrete_range} LOOP ${seq_stmt_complexList.ISABELLE} END LOOP)"
    case ISeq_stmt_complex_Ssc_fn(name, iV_clhs, iSubproccall_complex) => s"(ssc_fn ''${name}'' ${iV_clhs} ${iSubproccall_complex})"
    case ISeq_stmt_complex_Ssc_rt(name, iAsmt_rhs) => s"(ssc_rt ''${name}'' ${iAsmt_rhs})"
    case ISeq_stmt_complex_Ssc_pc(name, iSubproccall_complex) => s"(ssc_pc ''${name}'' ${iSubproccall_complex})"
    case ISeq_stmt_complex_Ssc_n(name, tId, cond) => s"(''${name}'': NEXT ${tId} WHEN ${cond})"
    case ISeq_stmt_complex_Ssc_e(name, tId, cond) => s"(''${name}'': EXIT ${tId} WHEN ${cond})"
    case ISeq_stmt_complex_Ssc_nl => "(NULL)"
  }
}

case class ISeq_stmt_complex_Ssc_sa(name: String, iSp_clhs: ISp_clhs, iAsmt_rhs: IAsmt_rhs) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_va(name: String, iV_clhs: IV_clhs, iAsmt_rhs: IAsmt_rhs) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_if(name: String, iCondition: IExpression, if_seq_stmt_complexList: List[ISeq_stmt_complex], elif_complexList: List[Ssc_elif], else_complexList: List[ISeq_stmt_complex]) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_case(name: String, cond: IExpression, when_complexList: List[Ssc_when], defaultSeq_stmt_complexList: List[ISeq_stmt_complex]) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_while(name: String, cond: IExpression, bodySeq_stmt_complexList: List[ISeq_stmt_complex]) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_for(name: String, cond: IExpression, discrete_range: IDiscrete_range, seq_stmt_complexList: List[ISeq_stmt_complex]) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_fn(name: String, iV_clhs: IV_lhs, iSubproccall_complex: ISubproccall_complex) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_rt(name: String, iAsmt_rhs: IAsmt_rhs) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_pc(name: String, iSubproccall_complex: ISubproccall_complex) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_n(name: String, tId: String, cond: IExpression) extends ISeq_stmt_complex

case class ISeq_stmt_complex_Ssc_e(name: String, tId: String, cond: IExpression) extends ISeq_stmt_complex

case object ISeq_stmt_complex_Ssc_nl extends ISeq_stmt_complex

case class ISubproccall_complex(iName: String, iAsmt_rhsList: List[IAsmt_rhs], iType: IType.Value) {
  override def toString = s"(''${iName}'', ${iAsmt_rhsList.mkString("[", "]@[", "]")}, ${iType})"
}

object ISubproccall_complex {
  def apply(vProcedureCall: VProcedureCall)(defInfo: DefInfo): ISubproccall_complex = {
    val iName = vProcedureCall.selectedName
    val iAsmt_rhsList = for {
      vAssociationElement <- vProcedureCall.vAssociationList.vAssociationElementList
      iAsmt_rhs = vAssociationElement.actualPart.vActualDesignator match {
        case VActualDesignatorExpression(vExpression) => IAsmt_rhs(vExpression.toIExp(defInfo))
        case _ => throw VIError
      }
    } yield iAsmt_rhs

    val returnType = IdentifierMap.getSubprogramReturnType(iName)
    ISubproccall_complex(iName, iAsmt_rhsList, returnType)
  }
}