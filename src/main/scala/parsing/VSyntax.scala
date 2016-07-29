package parsing

import parsing.V2IUtils._
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

sealed abstract class VLiteral {
  def toIExp: IVariable = this match {
    // numeric
    // FIXME if s is numeric literal, it should be transfered to decimal firstly
    case VLiteralNumInt(s) => IVariable("val_i", s)
    case VLiteralNumReal(s) => IVariable("var_r", s)
    case VLiteralNumBase(s) => defaultScalarValue(s)
    // enumeral
    // TODO only a guess, may change later
    case VLiteralEnumId(s) => defaultScalarValue(s"VLiteralEnumChar ${s}")
    case VLiteralEnumChar(s) => IVariable("val_c", s"(CHR '${s}')")
    // other cases
    case _ => defaultScalarValue(s"VLiteral ${this}")
  }

  def asVal: String = this match {
    case VLiteralNumInt(s) => s
    case VLiteralNumReal(s) => s
    case VLiteralNumBase(s) => s
    case VLiteralEnumId(s) => s
    case VLiteralEnumChar(s) => s
    case _ => "???"
  }
}

object VLiteral {
  def apply(ctx: LiteralContext): VLiteral = {
    if (ctx.NULL() != null) {
      VLiteralNull
    } else if (ctx.BIT_STRING_LITERAL() != null) {
      VLiteralBitS(ctx.getText)
    } else if (ctx.STRING_LITERAL() != null) {
      VLiteralS(ctx.getText)
    } else if (ctx.enumeration_literal() != null) {
      VLiteralEnum(ctx.enumeration_literal())
    } else if (ctx.numeric_literal() != null) {
      VLiteralNum(ctx.numeric_literal())
    } else throw VError
  }
}

case object VLiteralNull extends VLiteral

case class VLiteralBitS(s: String) extends VLiteral

case class VLiteralS(s: String) extends VLiteral

//////////////////////////////////////////////////////////////
sealed abstract class VLiteralEnum(s: String) extends VLiteral

object VLiteralEnum {
  def apply(ctx: Enumeration_literalContext): VLiteralEnum = {
    val id = ctx.identifier()
    val characterLiteral = ctx.CHARACTER_LITERAL()
    if (id != null) {
      VLiteralEnumId(id.getText)
    } else if (characterLiteral != null) {
      VLiteralEnumChar(characterLiteral.getText)
    } else throw VError
  }
}

case class VLiteralEnumId(s: String) extends VLiteralEnum(s)

case class VLiteralEnumChar(s: String) extends VLiteralEnum(s)

///////////////////////////////////////////////////////////////

sealed abstract class VLiteralNum(s: String) extends VLiteral

object VLiteralNum {
  def apply(ctx: Numeric_literalContext): VLiteralNum = {
    val abs = ctx.abstract_literal()
    val phy = ctx.physical_literal()
    if (abs != null) {
      VLiteralNumAbs(abs)
    } else {
      VLiteralNumPhy(phy.getText)
    }
  }
}

sealed abstract class VLiteralNumAbs(s: String) extends VLiteralNum(s)

object VLiteralNumAbs {
  def apply(ctx: Abstract_literalContext): VLiteralNumAbs = {
    val intL = ctx.INTEGER()
    val realL = ctx.REAL_LITERAL()
    val baseL = ctx.BASE_LITERAL()
    if (intL != null) {
      VLiteralNumInt(ctx.getText)
    } else if (realL != null) {
      VLiteralNumReal(ctx.getText)
    } else if (baseL != null) {
      VLiteralNumBase(baseL.getText)
    } else throw VError
  }
}

case class VLiteralNumInt(s: String) extends VLiteralNumAbs(s)

case class VLiteralNumReal(s: String) extends VLiteralNumAbs(s)

case class VLiteralNumBase(s: String) extends VLiteralNumAbs(s)

case class VLiteralNumPhy(s: String) extends VLiteralNum(s)

//////////////////////////////////////////////////////////////

case class VBaseUnitDecl(id: String)

case class VSecondaryUnitDecl(id: String, literal: VLiteral)

//////////////////////////////////////////////////////////////
sealed trait VTypeDef

case class VAccessTypeDef(subtypeInd: VSubtypeInd) extends VTypeDef

case class VFileTypeDef(subtypeInd: VSubtypeInd) extends VTypeDef

abstract class VScalarTypeDef extends VTypeDef

case class VPhysicalTypeDef(rangeConstraint: VRangeConstraint, baseUnitDecl: VBaseUnitDecl,
                            secondaryUnitDecl: Seq[VSecondaryUnitDecl], id: Option[String]) extends VScalarTypeDef

case class VEnumTypeDef(literal: VLiteral, others: Seq[VLiteral]) extends VScalarTypeDef

case class VRangeConstraintTypeDef(rangeConstraint: VRangeConstraint) extends VScalarTypeDef

abstract class VCompositeTypeDecl extends VTypeDef

abstract class VArrayTypeDecl extends VCompositeTypeDecl

case class VIndexSubtypeDef(name: String)

case class VUArrayDef(indexSubtypeDef: Seq[VIndexSubtypeDef], subtypeInd: VSubtypeInd) extends VArrayTypeDecl

case class VCArrayDef(indexConstraint: VIndexConstraint, subtypeInd: VSubtypeInd) extends VArrayTypeDecl

// no element_subtype_definition
case class VElementDecl(ids: Seq[String], subtypeInd: VSubtypeInd) {
  def flatten = for (id <- ids) yield (id -> subtypeInd)
}

object VElementDecl {
  def apply(ctx: Element_declarationContext): VElementDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.element_subtype_definition().subtype_indication())
    new VElementDecl(idList, subtypeInd)
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

case class VSubtypeDecl(id: String, subtypeInd: VSubtypeInd) extends VBlockDeclItem

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
        case VChoiceOthers => "others"
        case _ => s"???${choice}"
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

case class VQExpA(subtypeInd: VSubtypeInd, aggregate: VAggregate) extends VQExp

case class VQExpE(subtypeInd: VSubtypeInd, exp: VExp) extends VQExp

object VQExp {
  def apply(ctx: Qualified_expressionContext): VQExp = {
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
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

case class VAllocatorS(subtypeInd: VSubtypeInd) extends VAllocator

object VAllocator {
  def apply(ctx: AllocatorContext): VAllocator = {
    val qualified_expression = ctx.qualified_expression()
    val subtype_indication = ctx.subtype_indication()
    if (qualified_expression != null) {
      VallocatorE(VQExp(qualified_expression))
    } else if (subtype_indication != null) {
      VAllocatorS(VSubtypeInd(subtype_indication))
    } else throw VError
  }
}

abstract class VPrimary {
  def toIExp: IExp = this match {
    case VPrimaryLiteral(literal) => literal.toIExp
    case VPrimaryQExp(vQExp) => defaultScalarValue(s"${this}")
    case VPrimaryExpLR(vExp) => defaultScalarValue(s"${this}")
    case VPrimaryAllocator(allocator) => defaultScalarValue(s"${this}")
    case VPrimaryAggregate(aggregate) => defaultScalarValue(s"${this}")
    case VPrimaryName(name) => defaultScalarValue(s"${this}")
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
      VPrimaryLiteral(VLiteral(literal))
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

case class VSubtypeInd(selectedName: String,
                       constraint: Option[VConstraint],
                       tolerance: Option[VToleranceAspect]) extends VAliasIndication {
  //  TODO currently suppose it is a (Int, Int)
  def getRange: Option[RangeTy] = constraint.map(_.getRange)
}

object VSubtypeInd {
  def apply(ctx: Subtype_indicationContext): VSubtypeInd = {
    val selectedName = Antlr2VTy.selectedNameFromSubtypeInd(ctx).toLowerCase
    val constraint = Option(ctx.constraint()).map(VConstraint(_))
    val tolerance = Option(ctx.tolerance_aspect()).map(VToleranceAspect(_))
    new VSubtypeInd(selectedName, constraint, tolerance)
  }
}

case class VToleranceAspect(vExp: VExp)

object VToleranceAspect {
  def apply(ctx: Tolerance_aspectContext): VToleranceAspect = {
    val vExp = VExp(ctx.expression())
    new VToleranceAspect(vExp)
  }
}

case class VSubnatureIndication(name: String,
                                indexConstraint: Option[VIndexConstraint],
                                exprs: Option[(VExp, VExp)]) extends VAliasIndication

sealed trait VRange {
  def getRange: RangeTy = this match {
    case VRangeE(explicitRange) => explicitRange.getRange
    case VRangeN(name: String) => defaultRange(s"VRangeN: ${this}")
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
    case VDiscreteRangeSub(subtypeInd) => subtypeInd.getRange match {
      case Some(range) => range
      case None => defaultRange(s"VDiscreteRangeSub ${subtypeInd}")
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
      VDiscreteRangeSub(VSubtypeInd(subtype_indication))
    } else throw VError
  }
}

case class VDiscreteRangeR(range: VRange) extends VDiscreteRange

case class VDiscreteRangeSub(subtypeInd: VSubtypeInd) extends VDiscreteRange

////////////////////////////////////////////////////////////


sealed trait VConstraint {
  def getRange: RangeTy = this match {
    case VRangeConstraint(range) => range.getRange
    case VIndexConstraint(discreteRanges) => {
      val (h, t) = (discreteRanges.head, discreteRanges.tail)
      t match {
        case Nil => h.getRange
        case _ => defaultRange(s"VConstraint ${discreteRanges}")
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
  def toIExp: IExp = this match {
    case VFFactor(primary, primaryOption) => {
      primaryOption match {
        case Some(p) => IBexpfa(primary.toIExp, VFactorOp.exp, p.toIExp)
        case None => primary.toIExp
      }
    }
    case VAbsFactor(primary) => IUexp(VUop.abs, primary.toIExp)
    case VNotFactor(primary) => IUexp(VUop.not, primary.toIExp)
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
  def toIExp: IExp = {
    ops.zip(others).foldLeft(factor.toIExp) {
      case (accExp, (op, curFactor)) => IBexpfa(accExp, op, curFactor.toIExp)
    }
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

///////////////////////////////////////////////////
object VUop extends Enumeration {
  type Ty = Value
  val abs = Value("[abs]")
  val not = Value("[not]")
  val neg = Value("[-:]")
  val pos = Value("[+:]")
}

///////////////////////////////////////////////////

sealed trait VAOp

object VTermOp extends Enumeration with VAOp {
  type Ty = Value
  val plus = Value("[+]")
  val minus = Value("[-]")
  val ampersand = Value("[&]")

  def apply(ctx: Adding_operatorContext): Ty = {
    if (ctx.PLUS() != null) plus
    else if (ctx.MINUS() != null) minus
    else if (ctx.AMPERSAND() != null) ampersand
    else throw VError
  }

}

object VFactorOp extends Enumeration with VAOp {
  type Ty = Value
  val mul = Value("[*]")
  val div = Value("[/=]")
  //  NOT sure which to use
  val mod = Value("[mod]")
  val rem = Value("[rem]")
  //  DOUBLESTAR
  val exp = Value("[**]")

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
  def toIExp: IExp = {
    val firstExp: IExp = termSign match {
      case Some("-") => IUexp(VUop.neg, term.toIExp)
      case Some("+") => IUexp(VUop.pos, term.toIExp)
      case _ => term.toIExp
    }
    ops.zip(others).foldLeft(firstExp) {
      case (acc, (op, curTerm)) => IBexpta(acc, op, curTerm.toIExp)
    }
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
  val sll = Value("[sll]")
  val srl = Value("[srl]")
  val sla = Value("[sla]")
  val sra = Value("[sra]")
  val rol = Value("[rol]")
  val ror = Value("[ror]")

  def apply(op: Shift_operatorContext): Ty = {
    if (op.SLL() != null) sll
    else if (op.SRL() != null) srl
    else if (op.SLA() != null) sla
    else if (op.SRA() != null) sra
    else if (op.ROL() != null) rol
    else if (op.ROR() != null) ror
    else throw VError
  }
}

///////////////////////////////////////////////////////////

object VLogicOp extends Enumeration {
  type Ty = Value
  val and = Value("[and]")
  val or = Value("[or]")
  val nand = Value("[nand]")
  val nor = Value("[nor]")
  val xor = Value("[xor]")
  val xnor = Value("[xnor]")

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
  val eq = Value("[=]")
  val neq = Value("['/=]")
  val lt = Value("[<]")
  val le = Value("[<=]")
  val gt = Value("[>]")
  val ge = Value("[>=]")

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
  def toIExp: IExp = (op, other) match {
    case (Some(opV), Some(simpleExpV)) => IBexps(vSimpleExp.toIExp, opV, simpleExpV.toIExp)
    case _ => vSimpleExp.toIExp
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
  def toIExp: IExp = (op, other) match {
    case (Some(opV), Some(otherV)) => IBexpr(vShiftExpr.toIExp, opV, otherV.toIExp)
    case _ => vShiftExpr.toIExp
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
  def toIExp: IExp = {
    ops.zip(others).foldLeft(relation.toIExp) {
      case (acc, (op, curRelation)) => IBexpl(acc, op, curRelation.toIExp)
    }
  }

  def eval = toString

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

case class VConstDecl(idList: Seq[String], subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VConstDecl {
  def apply(ctx: Constant_declarationContext): VConstDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VConstDecl(idList, subtypeInd, vExp)
  }
}


///////////////////////////////////////////////////////////////////////

case class VSignalDecl(idList: Seq[String], subtypeInd: VSubtypeInd,
                       signalKind: Option[String], exp: Option[VExp])

object VSignalDecl {
  def apply(ctx: Signal_declarationContext): VSignalDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val signalKind = Option(ctx.signal_kind()).map(_.getText.toLowerCase)
    val exp = Option(ctx.expression()).map(VExp(_))
    new VSignalDecl(idList, subtypeInd, signalKind, exp)
  }
}

///////////////////////////////////////////////////////////////////////

case class VInterfaceSignalDecl(idList: Seq[String], subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VInterfaceSignalDecl {
  def apply(ctx: Interface_signal_declarationContext): VInterfaceSignalDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VInterfaceSignalDecl(idList, subtypeInd, vExp)
  }
}

case class VInterfaceConstDecl(idList: Seq[String], subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VInterfaceConstDecl {
  def apply(ctx: Interface_constant_declarationContext): VInterfaceConstDecl = {
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VInterfaceConstDecl(idList, subtypeInd, vExp)
  }
}

case class VInterfacePortDecl(idList: Seq[String], mode: String,
                              subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VInterfacePortDecl {
  def apply(ctx: Interface_port_declarationContext): VInterfacePortDecl = {
    val idList = getIdList(ctx.identifier_list())
    val mode = ctx.signal_mode().getText
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VInterfacePortDecl(idList, mode, subtypeInd, vExp)
  }
}

case class VPortList(interfacePortDecls: Seq[VInterfacePortDecl])


///////////////////////////////////////////////////////////////

// perhaps needing separation
case class VName(s: String)

object VName {
  def apply(ctx: NameContext): VName = {
    new VName(ctx.getText)
  }
}

sealed abstract class VTarget {
  def getName: Option[String] = this match {
    case VTargetN(name) => Some(name.s)
    case VTargetAggregate(aggregate) => None
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
    } else throw VError
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
    } else throw VError
  }
}

object VDelayT extends VDelay

case class VDelayE(vExp: Option[VExp]) extends VDelay

case class VOpts(guarded: Boolean, delay: Option[VDelay])

object VOpts {
  def apply(ctx: OptsContext): VOpts = {
    val guarded = ctx.GUARDED() != null
    val delay = Option(ctx.delay_mechanism()).map(VDelay(_))
    new VOpts(guarded, delay)
  }
}

//////////////////////////////////////////////////////////////////////////////
case class VWaveFormElem(exp: VExp, expOption: Option[VExp])

object VWaveFormElem {
  def apply(ctx: Waveform_elementContext): VWaveFormElem = {
    val exprs = ctx.expression().map(VExp(_))
    val exp = exprs.head
    val expOption = exprs.lift(1)
    new VWaveFormElem(exp, expOption)
  }
}

sealed abstract class VWaveForm

object VWaveForm {
  def apply(ctx: WaveformContext): VWaveForm = {
    val unaffected = ctx.UNAFFECTED()
    val waveFormElemList = ctx.waveform_element()
    if (unaffected != null) {
      VWaveFormU
    } else if (waveFormElemList != null) {
      VWaveFormE(waveFormElemList.map(VWaveFormElem(_)))
    } else throw VError
  }
}

case class VWaveFormE(elems: Seq[VWaveFormElem]) extends VWaveForm {
  require(elems.nonEmpty, "VWaveFormE")
}

object VWaveFormU extends VWaveForm

case class VCondWaveForms(vWaveForm: VWaveForm, cond: Option[VExp], elseCond: Option[VCondWaveForms])

object VCondWaveForms {
  def apply(ctx: Conditional_waveformsContext): VCondWaveForms = {
    val waveForm = VWaveForm(ctx.waveform())
    val cond = Option(ctx.condition()).map(c => VExp(c.expression()))
    val condWaves = Option(ctx.conditional_waveforms()).map(VCondWaveForms(_))
    new VCondWaveForms(waveForm, cond, condWaves)
  }
}

//////////////////////////////////////////////////////////////////////////////

case class VSelectedWaveForm(waveForm: VWaveForm, choices: VChoices,
                             waveFormOpt: Option[VWaveForm], choicesOpt: Option[VChoices])

object VSelectedWaveForm {
  def apply(ctx: Selected_waveformsContext): VSelectedWaveForm = {
    val waveFormList = ctx.waveform().map(VWaveForm(_))
    val choicesList = ctx.choices().map(VChoices(_))
    new VSelectedWaveForm(waveFormList.head, choicesList.head, waveFormList.lift(1), choicesList.lift(1))
  }
}

case class VSelectedSignalAssign(exp: VExp, target: VTarget, opts: VOpts, selectedWaveForm: VSelectedWaveForm)

object VSelectedSignalAssign {
  def apply(ctx: Selected_signal_assignmentContext): VSelectedSignalAssign = {
    val exp = VExp(ctx.expression())
    val target = VTarget(ctx.target())
    val opts = VOpts(ctx.opts())
    val selectedWaveForm = VSelectedWaveForm(ctx.selected_waveforms())
    new VSelectedSignalAssign(exp, target, opts, selectedWaveForm)
  }
}

//////////////////////////////////////////////////////////////////////////////

case class VCondSignAssign(target: VTarget, opts: VOpts, conditionalWaveforms: VCondWaveForms)

object VCondSignAssign {
  def apply(ctx: Conditional_signal_assignmentContext): VCondSignAssign = {
    val target = VTarget(ctx.target())
    val opts = VOpts(ctx.opts())
    val condWaveForms = VCondWaveForms(ctx.conditional_waveforms())
    new VCondSignAssign(target, opts, condWaveForms)
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
      VConcurrentSignalAssignStatC(labelColon, postPonded, VCondSignAssign(condSignalAssign))
    } else if (selectedSignalAssign != null) {
      VConcurrentSignalAssignStatS(labelColon, postPonded, VSelectedSignalAssign(selectedSignalAssign))
    } else throw VError
  }
}

case class VConcurrentSignalAssignStatC(labelColon: Option[String],
                                        postPonded: Boolean,
                                        condSignAssign: VCondSignAssign) extends VConcurrentSignalAssignStat

case class VConcurrentSignalAssignStatS(labelColon: Option[String],
                                        postPonded: Boolean,
                                        selectSignalAssign: VSelectedSignalAssign) extends VConcurrentSignalAssignStat


///////////////////////////////////////////////////////////////////////////////////
case class VVarDecl(shared: Boolean, idList: Seq[String],
                    subtypeInd: VSubtypeInd, vExp: Option[VExp])

object VVarDecl {
  def apply(ctx: Variable_declarationContext): VVarDecl = {
    val shared = ctx.SHARED() != null
    val idList = getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeInd(ctx.subtype_indication())
    val vExp = Option(ctx.expression()).map(VExp(_))
    new VVarDecl(shared, idList, subtypeInd, vExp)
  }
}

////////////////////////////////////////////////////////////////////////////////

case class VSubProgSpec()

////////////////////////////////////////////////////////////////////////////////

case class VSubProgBody()

////////////////////////////////////////////////////////////////////////////////

case class VTypeDecl()

////////////////////////////////////////////////////////////////////////////////

case class VFileDecl()

////////////////////////////////////////////////////////////////////////////////

case class VAliasDecl()

////////////////////////////////////////////////////////////////////////////////
case class VAttrDecl()

////////////////////////////////////////////////////////////////////////////////
case class VAttrSpec()

////////////////////////////////////////////////////////////////////////////////
case class VUseClause()

////////////////////////////////////////////////////////////////////////////////
case class VGrpTempDecl()

////////////////////////////////////////////////////////////////////////////////

case class VGrpDecl()

////////////////////////////////////////////////////////////////////////////////
sealed abstract class VProcDeclItem

// subprogram_declaration not defined
case class VProcDeclItemSPD(subProgDecl: VSubProgSpec) extends VProcDeclItem

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

sealed abstract class VSeqStat



/////////////////////////////////////////////////////////////////////////////////
case class VProcStatPart(seqStatList: List[VSeqStat])

/////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////