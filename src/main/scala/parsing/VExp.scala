package parsing

import sg.edu.ntu.hchen.VHDLParser._

import scala.collection.JavaConversions._

object VError extends Throwable

case class VLiteral(s: String)

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

case class VUArrayDecl(indexSubtypeDecl: VIndexSubtypeDecl, others: Seq[VIndexSubtypeDecl]) extends VArrayTypeDecl

case class VCArrayDecl(indexConstraint: VIndexConstraint, subtypeIndication: VSubtypeIndication) extends VArrayTypeDecl

// no element_subtype_definition
case class VElementDecl(ids: Seq[String], subtypeIndication: VSubtypeIndication)

case class VRecordTypeDecl(elementDecls: Seq[VElementDecl], id: Option[String]) extends VCompositeTypeDecl

case class VIndexSubtypeDecl(name: String)

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
    if (id != null) {
      VChoiceId(id.getText)
    } else if (discrete_range != null) {
      VChoiceR(VDiscreteRange(discrete_range))
    } else if (simple_expression != null) {
      VChoiceE(VSimpleExp(simple_expression))
    } else throw VError
  }
}

case class VChoiceId(id: String) extends VChoice

case class VChoiceR(discreteRange: VDiscreteRange) extends VChoice

case class VChoiceE(simpleExpr: VSimpleExp) extends VChoice

case class VChoiceOthers() extends VChoice

final case class VChoices(choice: VChoice, others: Seq[VChoice])

object VChoices {
  def apply(ctx: ChoicesContext): VChoices = {
    val choiceList = for {
      choice <- ctx.choice()
    } yield VChoice(choice)
    new VChoices(choiceList.head, choiceList.tail)
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

case class VAggregate(elemAssoc: VElemAssoc, others: Seq[VElemAssoc])

object VAggregate {
  def apply(ctx: AggregateContext): VAggregate = {
    val element_assocList = for {
      elemAssoc <- ctx.element_association()
    } yield VElemAssoc(elemAssoc)
    new VAggregate(element_assocList.head, element_assocList.tail)
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

abstract class VPrimary

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

case class VSubtypeIndication(selectedName: String) extends VAliasIndication

object VSubtypeIndication {
  def apply(ctx: Subtype_indicationContext): VSubtypeIndication = {
    val selectedName = Antlr2VTy.selectedNameFromSubtypeInd(ctx)
    new VSubtypeIndication(selectedName)
  }
}

case class VSubnatureIndication(name: String, indexConstraint: Option[VIndexConstraint], exprs: Option[(VExp, VExp)]) extends VAliasIndication


sealed trait VRange

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

sealed trait VDiscreteRange

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
  def apply(ctx: ConstraintContext): VConstraint = {
    require(ctx != null, "ctx should not be null")
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

case class VIndexConstraint(discreteRange: VDiscreteRange, composite: Seq[VDiscreteRange]) extends VConstraint

object VIndexConstraint {
  def apply(ctx: Index_constraintContext): VIndexConstraint = {
    val discrete_rangeList = for {
      discrete_range <- ctx.discrete_range()
    } yield VDiscreteRange(discrete_range)
    new VIndexConstraint(discrete_rangeList.head, discrete_rangeList.tail)
  }
}

case class VExplicitRange(l: VSimpleExp, d: String, r: VSimpleExp)

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

abstract class VFactor

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
      require(ctx.DOUBLESTAR() != null, "double start")
      VFFactor(primaryList.head, primaryList.lift(1))
    }
  }
}

case class VFFactor(primary: VPrimary, primaryOption: Option[VPrimary]) extends VFactor

case class VAbsFactor(primary: VPrimary) extends VFactor

case class VNotFactor(primary: VPrimary) extends VFactor

case class VTerm(factor: VFactor, ops: Seq[VFactorOp.Ty], others: Seq[VFactor])

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

case class VSimpleExp(termOpSymbol: Option[String], term: VTerm, ops: Seq[VTermOp.Ty], tails: Seq[VTerm])

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

case class VShiftExp(vSimpleExpr: VSimpleExp, op: Option[VShiftOp.Ty], other: Option[VSimpleExp])

object VShiftExp {
  def apply(ctx: Shift_expressionContext): VShiftExp = {
    val simple_expressionList = for {
      simple_expression <- ctx.simple_expression()
    } yield VSimpleExp(simple_expression)
    val op = Option(ctx.shift_operator()).map(VShiftOp(_))
    val other = simple_expressionList.lift(1)
    VShiftExp(simple_expressionList.head, op, other)
  }
}

case class VRelation(vShiftExpr: VShiftExp, op: Option[VRelationOp.Ty], other: Option[VShiftExp])

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

case class VExp(relation: VRelation, ops: Seq[VLogicOp.Ty], others: Seq[VRelation])

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
