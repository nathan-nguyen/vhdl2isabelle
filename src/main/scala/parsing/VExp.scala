package parsing

import parsing.V2I._
import sg.edu.ntu.hchen.VHDLParser._

import scala.collection.JavaConversions._


object VError extends Throwable


object Antlr2VTy {

  def getIdList(ctx: Identifier_listContext) = {
    for {
      id <- ctx.identifier()
    } yield id.getText.toLowerCase
  }

  def selectedNameFromSubtypeInd(ctx: Subtype_indicationContext) = {
    val names = for {
      name <- ctx.selected_name()
    } yield name.getText
    names.head
  }

}

import parsing.Antlr2VTy._

case class VLiteral(s: String) {
  //  FIXME if s is numeric literal, it should be transfered to decimal firstly

  def asExp = s

  def asVal = s
}

//////////////////////////////////////////////////////////////

case class VBaseUnitDecl(id: String)

case class VSecondaryUnitDecl(id: String, literal: VLiteral)

//////////////////////////////////////////////////////////////
sealed trait VTypeDef

case class VAccessTypeDef(subtypeIndication: VSubtypeIndication) extends VTypeDef

case class VFileTypeDef(subtypeIndication: VSubtypeIndication) extends VTypeDef

abstract class VScalarTypeDef extends VTypeDef

case class VPhysicalTypeDef(rangeConstraint: VRangeConstraint, baseUnitDecl: VBaseUnitDecl, secondaryUnitDecl: Seq[VSecondaryUnitDecl], id: Option[String]) extends VScalarTypeDef

case class VEnumTypeDef(literal: VLiteral, others: Seq[VLiteral]) extends VScalarTypeDef

case class VRangeConstraintTypeDef(rangeConstraint: VRangeConstraint) extends VScalarTypeDef

abstract class VCompositeTypeDecl extends VTypeDef

abstract class VArrayTypeDecl extends VCompositeTypeDecl

case class VIndexSubtypeDef(name: String)

case class VUArrayDef(indexSubtypeDef: Seq[VIndexSubtypeDef], subtypeIndication: VSubtypeIndication) extends VArrayTypeDecl

case class VCArrayDef(indexConstraint: VIndexConstraint, subtypeIndication: VSubtypeIndication) extends VArrayTypeDecl

// no element_subtype_definition
case class VElementDecl(ids: Seq[String], subtypeIndication: VSubtypeIndication) {
  def flatten = for (id <- ids) yield (id -> subtypeIndication)
}

object VElementDecl {
  def apply(ctx: Element_declarationContext): VElementDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.element_subtype_definition().subtype_indication())
    new VElementDecl(idList, subtypeIndication)
  }
}

case class VRecordTypeDef(elementDecls: Seq[VElementDecl], id: Option[String]) extends VCompositeTypeDecl

object VRecordTypeDef {
  def apply(ctx: Record_type_definitionContext): VRecordTypeDef = {
    val elementDecls = for {
      ed <- ctx.element_declaration()
    } yield VElementDecl(ed)
    val id = Option(ctx.identifier()).map(_.getText)
    new VRecordTypeDef(elementDecls, id)
  }
}

//////////////////////////////////////////////////////////////

sealed trait VBlockDeclItem

case class VSubtypeDecl(id: String, subtypeIndication: VSubtypeIndication) extends VBlockDeclItem

// TODO more here

//////////////////////////////////////////////////////////////

abstract class VChoice

object VChoice {
  def apply(ctx: ChoiceContext): VChoice = {
    val id = ctx.identifier()
    val discrete_range = ctx.discrete_range()
    val simple_expression = ctx.simple_expression()
    val others = ctx.OTHERS()
    if (id != null) {
      VChoiceId(id.getText)
    } else if (discrete_range != null) {
      VChoiceR(VDiscreteRange(discrete_range))
    } else if (simple_expression != null) {
      VChoiceE(VSimpleExp(simple_expression))
    } else if (others != null) {
      VChoiceOthers
    } else throw VError
  }
}

case class VChoiceId(id: String) extends VChoice

case class VChoiceR(discreteRange: VDiscreteRange) extends VChoice

case class VChoiceE(simpleExpr: VSimpleExp) extends VChoice

case object VChoiceOthers extends VChoice

final case class VChoices(choiceList: Seq[VChoice])

object VChoices {
  def apply(ctx: ChoicesContext): VChoices = {
    val choiceList = for {
      choice <- ctx.choice()
    } yield VChoice(choice)
    new VChoices(choiceList)
  }
}

////////////////////////////////////////////////////////////

case class VElemAssoc(choices: Option[VChoices], expr: VExp)

object VElemAssoc {
  def apply(ctx: Element_associationContext): VElemAssoc = {
    val choices = Option(ctx.choices()).map(VChoices(_))
    val vExp = VExp(ctx.expression())
    new VElemAssoc(choices, vExp)
  }
}

case class VAggregate(elemAssocList: Seq[VElemAssoc]) {
  require(elemAssocList.nonEmpty, "elemAssocList")

  lazy val _getAssoc: Seq[(String, VExp)] = {
    for {
      elemAssoc <- elemAssocList
      choice <- elemAssoc.choices match {
        case Some(c) => c.choiceList
        case None => throw VError
      }
    } yield {
      val id = choice match {
        case VChoiceId(s) => s
        case _ => "???"
      }
      val vExp = elemAssoc.expr
      id -> vExp
    }
  }

  def getFirstMap = _getAssoc.head

  lazy val getAssoc: Map[String, VExp] = _getAssoc.toMap

}

object VAggregate {
  def apply(ctx: AggregateContext): VAggregate = {
    val element_assocList = for {
      elemAssoc <- ctx.element_association()
    } yield VElemAssoc(elemAssoc)
    new VAggregate(element_assocList)
  }
}

sealed trait VQExp

case class VQExpA(subtypeIndication: VSubtypeIndication, aggregate: VAggregate) extends VQExp

case class VQExpE(subtypeIndication: VSubtypeIndication, exp: VExp) extends VQExp

object VQExp {
  def apply(ctx: Qualified_expressionContext): VQExp = {
    val subtypeInd = VSubtypeIndication(ctx.subtype_indication())
    val (aggregate, expression) = (ctx.aggregate(), ctx.expression())
    if (aggregate != null) {
      val vAggregate = VAggregate(aggregate)
      VQExpA(subtypeInd, vAggregate)
    } else if (expression != null) {
      val vExp = VExp(expression)
      VQExpE(subtypeInd, vExp)
    } else throw VError
  }
}

sealed trait VAllocator

case class VallocatorE(qexp: VQExp) extends VAllocator

case class VAllocatorS(subtypeIndication: VSubtypeIndication) extends VAllocator

object VAllocator {
  def apply(ctx: AllocatorContext): VAllocator = {
    val qualified_expression = ctx.qualified_expression()
    val subtype_indication = ctx.subtype_indication()
    if (qualified_expression != null) {
      VallocatorE(VQExp(qualified_expression))
    } else if (subtype_indication != null) {
      VAllocatorS(VSubtypeIndication(subtype_indication))
    } else throw VError
  }
}

abstract class VPrimary {
  def asExp: String = this match {
    case VPrimaryLiteral(literal) => literal.asExp
    case VPrimaryQExp(vQExp) => s"(??? ${vQExp.getClass.getName})"
    case VPrimaryExpLR(vExp) => s"(??? ${vExp.getClass.getName})"
    case VPrimaryAllocator(allocator) => s"(??? ${allocator.getClass.getName})"
    case VPrimaryAggregate(aggregate) => s"(??? ${aggregate.getClass.getName})"
    case VPrimaryName(name) => s"<??? ${name}>"
  }

  def asVal: String = this match {
    case VPrimaryLiteral(literal) => literal.asVal
    case _ => s"""(??? ${this.getClass.getName})"""
  }

  def getLiteral: Option[VLiteral] = this match {
    case VPrimaryLiteral(l) => Some(l)
    case _ => None
  }

  def getAggregate: Option[VAggregate] = this match {
    case VPrimaryAggregate(aggregate) => Some(aggregate)
    case _ => None
  }

}

object VPrimary {
  def apply(ctx: PrimaryContext): VPrimary = {
    val literal = ctx.literal()
    val qualified_expression = ctx.qualified_expression()
    val expression = ctx.expression()
    val allocator = ctx.allocator()
    val aggregate = ctx.aggregate()
    val name = ctx.name()
    if (literal != null) {
      VPrimaryLiteral(VLiteral(literal.getText))
    } else if (qualified_expression != null) {
      VPrimaryQExp(VQExp(qualified_expression))
    } else if (expression != null) {
      VPrimaryExpLR(VExp(expression))
    } else if (allocator != null) {
      VPrimaryAllocator(VAllocator(allocator))
    } else if (aggregate != null) {
      VPrimaryAggregate(VAggregate(aggregate))
    } else if (name != null) {
      VPrimaryName(name.getText)
    } else throw VError
  }
}

case class VPrimaryLiteral(literal: VLiteral) extends VPrimary

case class VPrimaryQExp(qExp: VQExp) extends VPrimary

case class VPrimaryExpLR(exp: VExp) extends VPrimary

case class VPrimaryAllocator(allocator: VAllocator) extends VPrimary

case class VPrimaryAggregate(aggregate: VAggregate) extends VPrimary

case class VPrimaryName(name: String) extends VPrimary

////////////////////////////////////////////////////////////

sealed trait VAliasIndication

case class VSubtypeIndication(selectedName: String,
                              constraint: Option[VConstraint],
                              tolerance: Option[VToleranceAspect]
                             ) extends VAliasIndication {
  //  TODO currently suppose it is a (Int, Int)
  def getRange: Option[RangeTy] = constraint.map(_.getRange)
}

object VSubtypeIndication {
  def apply(ctx: Subtype_indicationContext): VSubtypeIndication = {
    val selectedName = Antlr2VTy.selectedNameFromSubtypeInd(ctx).toLowerCase
    val constraint = Option(ctx.constraint()).map(VConstraint(_))
    val tolerance = Option(ctx.tolerance_aspect()).map(VToleranceAspect(_))
    new VSubtypeIndication(selectedName, constraint, tolerance)
  }
}

case class VToleranceAspect(vExp: VExp)

object VToleranceAspect {
  def apply(ctx: Tolerance_aspectContext): VToleranceAspect = {
    val vExp = VExp(ctx.expression())
    new VToleranceAspect(vExp)
  }
}

case class VSubnatureIndication(name: String, indexConstraint: Option[VIndexConstraint], exprs: Option[(VExp, VExp)]) extends VAliasIndication


sealed trait VRange {
  def getRange: RangeTy = this match {
    case VRangeE(explicitRange) => explicitRange.getRange
    case VRangeN(name: String) => ("???", "???", "???")
  }
}

object VRange {
  def apply(ctx: RangeContext): VRange = {
    val explicit_range = ctx.explicit_range()
    val name = ctx.name()
    if (explicit_range != null) {
      VRangeE(VExplicitRange(explicit_range))
    } else if (name != null) {
      VRangeN(name.getText)
    } else throw VError
  }
}

case class VRangeE(explicitRange: VExplicitRange) extends VRange

case class VRangeN(name: String) extends VRange


////////////////////////////////////////////////////////////

sealed trait VDiscreteRange {
  def getRange: RangeTy = this match {
    case VDiscreteRangeR(range) => range.getRange
    case VDiscreteRangeSub(subtypeIndication) => subtypeIndication.getRange match {
      case Some(range) => range
      case None => ("???", "???", "???")
    }
  }
}

object VDiscreteRange {
  def apply(ctx: Discrete_rangeContext): VDiscreteRange = {
    val range = ctx.range()
    val subtype_indication = ctx.subtype_indication()
    if (range != null) {
      VDiscreteRangeR(VRange(range))
    } else if (subtype_indication != null) {
      VDiscreteRangeSub(VSubtypeIndication(subtype_indication))
    } else throw VError
  }
}

case class VDiscreteRangeR(range: VRange) extends VDiscreteRange

case class VDiscreteRangeSub(subtypeIndication: VSubtypeIndication) extends VDiscreteRange

////////////////////////////////////////////////////////////


sealed trait VConstraint {
  def getRange: RangeTy = this match {
    case VRangeConstraint(range) => range.getRange
    case VIndexConstraint(discreteRanges) => {
      val (h, t) = (discreteRanges.head, discreteRanges.tail)
      t match {
        case Nil => h.getRange
        case _ => defaultRange
      }
    }
  }
}

object VConstraint {
  def apply(ctx: ConstraintContext): VConstraint = {
    val (index_constraint, range_constraint) = (ctx.index_constraint(), ctx.range_constraint())
    if (index_constraint != null) {
      VIndexConstraint(index_constraint)
    } else if (range_constraint != null) {
      VRangeConstraint(range_constraint)
    } else throw VError
  }
}

case class VRangeConstraint(range: VRange) extends VConstraint


object VRangeConstraint {
  def apply(ctx: Range_constraintContext): VRangeConstraint = {
    val range = ctx.range()
    VRangeConstraint(VRange(range))
  }
}

case class VIndexConstraint(discreteRanges: Seq[VDiscreteRange]) extends VConstraint {
  require(discreteRanges.nonEmpty, "discrete ranges")
}

object VIndexConstraint {
  def apply(ctx: Index_constraintContext): VIndexConstraint = {
    val discrete_rangeList = for {
      discrete_range <- ctx.discrete_range()
    } yield VDiscreteRange(discrete_range)
    new VIndexConstraint(discrete_rangeList)
  }
}

case class VExplicitRange(l: VSimpleExp, d: String, r: VSimpleExp) {
  def getRange: RangeTy = (l.asVal, d.toLowerCase, r.asVal)
}

object VExplicitRange {
  def apply(ctx: Explicit_rangeContext): VExplicitRange = {
    val simplExprList = for {
      simplExpr <- ctx.simple_expression()
    } yield VSimpleExp(simplExpr)
    val direction = ctx.direction().getText.toLowerCase
    require(simplExprList.length == 2, "explicitRange")
    VExplicitRange(simplExprList.head, direction, simplExprList.last)
  }
}


////////////////////////////////////////////////////////////

abstract class VFactor {
  def asExp: String = this match {
    case VFFactor(primary, primaryOption) => {
      primaryOption match {
        case Some(p) => primary.asExp + " " + p.asExp
        case None => primary.asExp
      }
    }
    case VAbsFactor(primary) => s"abs (${primary.asExp})"
    case VNotFactor(primary) => s"not (${primary.asExp}"
  }

  def computeE(v: String, n: String): String = {
    def power(v: Int, n: Int) = Array.fill(n)(v).product
    try {
      power(v.toInt, n.toInt).toString
    } catch {
      case e: Exception => "???"
    }
  }

  def asVal: String = this match {
    case VFFactor(primary, primaryOption) => primaryOption match {
      case Some(p) => computeE(primary.asVal, p.asVal)
      case None => primary.asVal
    }
    case VAbsFactor(primary) => {
      try {
        Math.abs(primary.asVal.toInt).toString
      } catch {
        case e: Exception => "???absFactor"
      }
    }
    case VNotFactor(primary) => "???notFactor"
  }

}

object VFactor {
  def apply(ctx: FactorContext): VFactor = {
    val primaryList = for {
      primary <- ctx.primary()
    } yield VPrimary(primary)
    val (abs, not) = (ctx.ABS(), ctx.NOT())
    if (abs != null) {
      require(primaryList.length == 1, "abs")
      VAbsFactor(primaryList.head)
    } else if (not != null) {
      require(primaryList.length == 1, "not")
      VNotFactor(primaryList.head)
    } else {
      VFFactor(primaryList.head, primaryList.lift(1))
    }
  }
}

case class VFFactor(primary: VPrimary, primaryOption: Option[VPrimary]) extends VFactor

case class VAbsFactor(primary: VPrimary) extends VFactor

case class VNotFactor(primary: VPrimary) extends VFactor

case class VTerm(factor: VFactor, ops: Seq[VFactorOp.Ty], others: Seq[VFactor]) {
  def asExp: String = {
    val factorRepr = factor.asExp
    val opReprs = ops.map(_.toString)
    val othersReprs = others.map(_.asExp)
    opReprs.zip(othersReprs).foldLeft(factorRepr)((acc, cur) => s"(${acc} ${cur._1} ${cur._2})")
  }

  def asVal: String = {
    import VFactorOp._
    try {
      ops.zip(others).foldLeft(factor.asVal.toInt)((acc, cur) => {
        val f = cur._2.asVal.toInt
        cur._1 match {
          case `mul` => acc * f
          case `div` => acc / f
          case `mod` => acc % f
          //        FIXME  rem is incorrect
          case `rem` => acc % f
        }
      }).toString
    } catch {
      case e: Exception => "???"
    }
  }
}

object VTerm {
  def apply(ctx: TermContext): VTerm = {
    val ops = for {
      op <- ctx.multiplying_operator()
    } yield VFactorOp(op)
    val factors = for {
      factor <- ctx.factor()
    } yield VFactor(factor)
    new VTerm(factors.head, ops, factors.tail)
  }
}

object VTermOp extends Enumeration {
  type Ty = Value
  val plus, minus, ampersand = Value

  def apply(ctx: Adding_operatorContext): Ty = {
    if (ctx.PLUS() != null) plus
    else if (ctx.MINUS() != null) minus
    else if (ctx.AMPERSAND() != null) ampersand
    else throw VError
  }

}

object VFactorOp extends Enumeration {
  type Ty = Value
  val mul, div, mod, rem = Value

  def apply(ctx: Multiplying_operatorContext): Ty = {
    if (ctx.MUL() != null) mul
    else if (ctx.DIV() != null) div
    else if (ctx.MOD() != null) mod
    else if (ctx.REM() != null) rem
    else throw VError
  }
}

////////////////////////////////////////////////////////////

case class VSimpleExp(termSign: Option[String], term: VTerm, ops: Seq[VTermOp.Ty], others: Seq[VTerm]) {
  def asExp: String = {
    val sign = termSign match {
      case Some("-") => "-"
      case _ => ""
    }
    val termRepr = sign + term.asExp
    val opsRepr = ops.map(_.toString)
    val termsRepr = others.map(_.asExp)
    opsRepr.zip(termsRepr).foldLeft(termRepr)((acc, cur) => s"(${acc} ${cur._1} ${cur._2})")
  }

  def asVal: String = {
    import VTermOp._
    val sign = termSign match {
      case Some("-") => "-"
      case _ => ""
    }
    try {
      ops.zip(others).foldLeft((sign + term.asVal).toInt)((acc, cur) => {
        val f = cur._2.asVal.toInt
        cur._1 match {
          case `plus` => acc * f
          case `minus` => acc / f
          case `ampersand` => throw VError
        }
      }).toString
    } catch {
      case e: Throwable => s"???simpleExp"
    }
  }
}

object VSimpleExp {
  def apply(ctx: Simple_expressionContext): VSimpleExp = {
    val terms = for {
      term <- ctx.term()
    } yield VTerm(term)
    val ops = for {
      op <- ctx.adding_operator()
    } yield VTermOp(op)
    val symbol = {
      if (ctx.PLUS() != null) Some("+")
      else if (ctx.MINUS() != null) Some("-")
      else None
    }
    new VSimpleExp(symbol, terms.head, ops, terms.tail)
  }
}

///////////////////////////////////////////////////////////
object VShiftOp extends Enumeration {
  type Ty = Value
  val sll, srl, sla, rol, ror = Value

  def apply(op: Shift_operatorContext): Ty = {
    if (op.SLL() != null) sll
    else if (op.SRL() != null) srl
    else if (op.SLA() != null) sla
    else if (op.ROL() != null) rol
    else if (op.ROR() != null) ror
    else throw VError
  }
}

///////////////////////////////////////////////////////////

object VLogicOp extends Enumeration {
  type Ty = Value
  val and, or, nand, nor, xor, xnor = Value

  def apply(op: Logical_operatorContext): Ty = {
    if (op.AND() != null) and
    else if (op.OR() != null) or
    else if (op.NAND() != null) nand
    else if (op.NOR() != null) nor
    else if (op.XOR() != null) xor
    else if (op.XNOR() != null) xnor
    else throw VError
  }
}

///////////////////////////////////////////////////////////

object VRelationOp extends Enumeration {
  type Ty = Value
  val eq, neq, lt, le, gt, ge = Value

  def apply(ctx: Relational_operatorContext): Ty = {
    if (ctx.EQ() != null) eq
    else if (ctx.NEQ() != null) neq
    else if (ctx.LOWERTHAN() != null) lt
    else if (ctx.LE() != null) le
    else if (ctx.GREATERTHAN() != null) gt
    else if (ctx.GE() != null) ge
    else throw VError
  }
}

case class VShiftExp(vSimpleExp: VSimpleExp, op: Option[VShiftOp.Ty], other: Option[VSimpleExp]) {
  def repr: String = (op, other) match {
    case (Some(opV), Some(simpleExpV)) => s"(${vSimpleExp.asExp} ${op.toString} ${simpleExpV.asExp})"
    case _ => vSimpleExp.asExp
  }
}

object VShiftExp {
  def apply(ctx: Shift_expressionContext): VShiftExp = {
    val simple_expressionList = for {
      simple_expression <- ctx.simple_expression()
    } yield VSimpleExp(simple_expression)
    val op = Option(ctx.shift_operator()).map(VShiftOp(_))
    val other = simple_expressionList.lift(1)
    new VShiftExp(simple_expressionList.head, op, other)
  }
}

case class VRelation(vShiftExpr: VShiftExp, op: Option[VRelationOp.Ty], other: Option[VShiftExp]) {
  def repr: String = (op, other) match {
    case (Some(opV), Some(otherV)) => s"${vShiftExpr.repr} ${opV.toString} ${otherV.repr}"
    case _ => vShiftExpr.repr
  }
}

object VRelation {
  def apply(ctx: RelationContext): VRelation = {
    val shift_expressionList = for {
      shift_expression <- ctx.shift_expression()
    } yield VShiftExp(shift_expression)
    val op = Option(ctx.relational_operator()).map(VRelationOp(_))
    val other = shift_expressionList.lift(1)
    new VRelation(shift_expressionList.head, op, other)
  }
}

case class VExp(relation: VRelation, ops: Seq[VLogicOp.Ty], others: Seq[VRelation]) {
  def repr: String = {
    val relationRepr = relation.repr
    val opReprs = ops.map(_.toString)
    val othersReprs = others.map(_.repr)
    opReprs.zip(othersReprs).foldLeft(relationRepr)((acc, cur) => s"(${acc} ${cur._1} ${cur._2})")
  }

  def getPrimary: Option[VPrimary] = {
    val simplExp = relation.vShiftExpr.vSimpleExp
    simplExp.term.factor match {
      case VFFactor(primary, primaryOption) => Some(primary)
      case _ => None
    }
  }

  def getAggregate: Option[VAggregate] = {
    for {
      primary <- getPrimary
      aggregate <- primary.getAggregate
    } yield aggregate
  }

  def getLiteral: Option[VLiteral] = {
    for {
      primary <- getPrimary
      literal <- primary.getLiteral
    } yield literal
  }

}

object VExp {
  def apply(ctx: ExpressionContext): VExp = {
    val relationList = for {
      relation <- ctx.relation()
    } yield VRelation(relation)
    val logicalOps = for {
      op <- ctx.logical_operator()
    } yield VLogicOp(op)
    VExp(relationList.head, logicalOps, relationList.tail)
  }

}

///////////////////////////////////////////////////////////////////////

case class VConstDecl(idList: Seq[String], subtypeIndication: VSubtypeIndication, vExp: Option[VExp])

object VConstDecl {
  def apply(ctx: Constant_declarationContext): VConstDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VConstDecl(idList, subtypeIndication, vExp)
  }
}


///////////////////////////////////////////////////////////////////////

case class VSignalDecl(idList: Seq[String], subtypeIndication: VSubtypeIndication, signalKind: Option[String], exp: Option[VExp])

object VSignalDecl {
  def apply(ctx: Signal_declarationContext): VSignalDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val signalKind = Option(ctx.signal_kind()).map(_.getText.toLowerCase)
    val exp = Option(ctx.expression()).map(VExp(_))
    new VSignalDecl(idList, subtypeIndication, signalKind, exp)
  }
}

///////////////////////////////////////////////////////////////////////

case class VInterfaceSignalDecl(idList: Seq[String], subtypeIndication: VSubtypeIndication, vExp: Option[VExp])

object VInterfaceSignalDecl {
  def apply(ctx: Interface_signal_declarationContext): VInterfaceSignalDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VInterfaceSignalDecl(idList, subtypeIndication, vExp)
  }
}

case class VInterfaceConstDecl(idList: Seq[String], subtypeIndication: VSubtypeIndication, vExp: Option[VExp])

object VInterfaceConstDecl {
  def apply(ctx: Interface_constant_declarationContext): VInterfaceConstDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VInterfaceConstDecl(idList, subtypeIndication, vExp)
  }
}

case class VInterfacePortDecl(idList: Seq[String], mode: String, subtypeIndication: VSubtypeIndication, vExp: Option[VExp])

object VInterfacePortDecl {
  def apply(ctx: Interface_port_declarationContext): VInterfacePortDecl = {
    val idList = getIdList(ctx.identifier_list())
    val mode = ctx.signal_mode().getText
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VInterfacePortDecl(idList, mode, subtypeIndication, vExp)
  }
}

case class VPortList(interfacePortDecls: Seq[VInterfacePortDecl])