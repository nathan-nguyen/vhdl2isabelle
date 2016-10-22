package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Thanh Nam Nguyen on 21/10/16.
  */

// In Isabelle: type_synonym parameter = "(v_clhs × direction × type)"
case class IParameter (iv_clhs: Iv_clhs, direction: IDirection.Value, parameterType: IType) {
  override def toString = s"(${iv_clhs}, ${direction}, ${parameterType})"
}

object IParameter {
  def apply(parameterName: String, direction: IDirection.Value, subtypeIndication: VSubtypeIndication): IParameter = {
    val parameterType = IType(subtypeIndication)
    val iv_clhs = {
      IdentifierMap.iVariableMap.get(parameterName) match {
        case Some(ivariable) => Iv_clhs(Iv_lhs(ivariable, subtypeIndication.getRange), null)
        case None => ???
      }
    }
    IParameter(iv_clhs, direction, parameterType)
  }
}

/** In Isabelle:
  * datatype v_clhs =
  *   clhs_v v_lhs
  *   | clhs_vr vl
  */
case class Iv_clhs (iv_lhs : Iv_lhs, ivl : Ivl){
  override def toString = {
    if (iv_lhs != null) s"clhs_v ${iv_lhs}"
    else s"clhs_vr ${ivl}"
  }
}

abstract class Ivl

/** In Isabelle:
  * datatype v_lhs =
  *   lhs_v variable
  *   | lhs_va variable discrete_range
  */
case class Iv_lhs (ivariable: IVariable, idiscrete_rangeOption: Option[VRangeV]){
  override def toString = {
    idiscrete_rangeOption match{
      case Some(discrete_range) => ???
      case None => s"(lhs_v ${ivariable})"
    }
  }
}

case class IVariable(name: String, variableType: IType, expression: IExpression) {
  override def toString = name

  def getDefinition: String = ???
}

object IVariable {
  def apply(name: String, subtypeIndication: VSubtypeIndication, iexpression: IExpression): IVariable ={
    val variableType = IType(subtypeIndication)
    IVariable(name, variableType, iexpression)
  }
}

sealed abstract class Idiscrete_range

object IDirection extends Enumeration {
  type IDirection = Value
  val IDirectionIn = Value("dir_in")
  val IDirectionOut = Value("dir_out")
}

sealed abstract class ISequenceStatementComplex

sealed abstract class ILocalVariable

object IDesignator extends Enumeration {
  type IDesignator = Value
  val IFunctionDesignator = Value("dn_function")
  val IProcedureDesignator = Value("dn_procedure")
}

sealed abstract class ISubprogramComplex {
  val name: String
  val designator : IDesignator.Value
  val parameterList : List[IParameter]
  val sequenceStatementComplexList : List[ISequenceStatementComplex]
  val returnType : IType
  val localVariableList : List[ILocalVariable]

  override def toString = s"(''${name}'', ${designator}, ${parameterList.mkString("[", "]@[", "]")}, ${sequenceStatementComplexList.mkString("[", "]@[", "]")}, ${returnType}, ${localVariableList.mkString("[", "]@[", "]")})"
}

case class IFunction(name: String, parameterList : List[IParameter], sequenceStatementComplexList : List[ISequenceStatementComplex], returnType: IType, localVariableList : List[ILocalVariable]) extends ISubprogramComplex {
  val designator = IDesignator.IFunctionDesignator
}

case class IProcedure(name: String, parameterList : List[IParameter], sequenceStatementComplexList : List[ISequenceStatementComplex], localVariableList : List[ILocalVariable]) extends ISubprogramComplex {
  val designator = IDesignator.IProcedureDesignator
  val returnType = IEmptyType
}