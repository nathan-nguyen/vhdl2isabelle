package parsing

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

case class VChoiceId(id: String) extends VChoice

case class VChoiceR(discreteRange: VDiscreteRange) extends VChoice

case class VChoiceE(simpleExpr: VSimpleExp) extends VChoice

case class VChoiceOthers() extends VChoice

final case class VChoices(choice: VChoice, others: Seq[VChoice])

////////////////////////////////////////////////////////////

case class VElemAssoc(choices: Option[VChoices], expr: VExp)

case class VAggregate(elemAssoc: VElemAssoc, others: Seq[VElemAssoc])

case class VQExp(subtypeIndication: VSubtypeIndication, either: Either[VAggregate, VExp])

case class VAllocator(either: Either[VQExp, VSubtypeIndication])

abstract class VPrimary

case class VPrimaryLiteral(literal: VLiteral) extends VPrimary

case class VPrimaryQExp(qExp: VQExp) extends VPrimary

case class VPrimaryAllocator(allocator: VAllocator) extends VPrimary

case class VPrimaryAggregate(aggregate: VAggregate) extends VPrimary

case class VPrimaryName(name: String) extends VPrimary

////////////////////////////////////////////////////////////

sealed trait VAliasIndication

case class VSubtypeIndication(selectedName: String) extends VAliasIndication

case class VSubnatureIndication(name: String, indexConstraint: Option[VIndexConstraint], exprs: Option[(VExp, VExp)]) extends VAliasIndication


sealed trait VRange

case class VRangeE(explicitRange: VExplicitRange) extends VRange

case class VRangeN(name: String) extends VRange


////////////////////////////////////////////////////////////

sealed trait VDiscreteRange

case class VDiscreteRangeR(range: VRange) extends VDiscreteRange

case class VDiscreteRangeSub(subtypeIndication: VSubtypeIndication) extends VDiscreteRange

////////////////////////////////////////////////////////////


sealed trait VConstraint

case class VRangeConstraint(range: VRange) extends VConstraint

case class VIndexConstraint(discreteRange: VDiscreteRange, composite: Seq[VDiscreteRange]) extends VConstraint

case class VExplicitRange(l: VSimpleExp, d: String, r: VSimpleExp)


////////////////////////////////////////////////////////////

abstract class VFactor

case class VFFactor(primary: VPrimary, primaryOption: Option[VPrimary]) extends VFactor

case class VAbsFactor(primary: VPrimary) extends VFactor

case class VNotFactor(primary: VPrimary) extends VFactor

case class VTerm(factor: VFactor, composite: Seq[(VFactorOp.Ty, VFactor)])

object VTermOp extends Enumeration {
  type Ty = Value
  val plus, minus, ampersand = Value
}

object VFactorOp extends Enumeration {
  type Ty = Value
  val mul, div, mod, rem = Value
}

////////////////////////////////////////////////////////////

case class VSimpleExp(termOpSymbol: Option[String], term: VTerm, composite: List[(VTermOp.Ty, VTerm)])

///////////////////////////////////////////////////////////
object VShiftOp extends Enumeration {
  type Ty = Value
  val sll, srl, sla, rol, ror = Value
}

///////////////////////////////////////////////////////////

object VLogicOp extends Enumeration {
  type Ty = Value
  val and, or, nand, nor, xor, xnor = Value
}

///////////////////////////////////////////////////////////

object VRelationOp extends Enumeration {
  type Ty = Value
  val eq, neq, lt, le, gt, ge = Value
}

case class VShiftExp(vSimpleExpr: VSimpleExp, composite: Option[(VShiftOp.Ty, VSimpleExp)])

case class VRelation(vShiftExpr: VShiftExp, composite: Option[(VRelationOp.Ty, VShiftExp)])

case class VExp(relation: VRelation, composite: Seq[(VLogicOp.Ty, VRelation)])
