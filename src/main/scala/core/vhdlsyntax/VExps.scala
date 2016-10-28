package core.vhdlsyntax

import core._
import core.isabellesyntax._
import sg.edu.ntu.vhdl2isabelle.VHDLParser._

import scala.collection.JavaConversions._

/**
  * Created by Hongxu Chen.
  */
sealed abstract class VLiteral extends VPrimary {

  val trueOrFalse = Set("true", "false")

  override def toIExp(defInfo: DefInfo): IExpression = {
    //    logger.info(s"${toString}")
    this match {
      // numeric
      // FIXME if s is numeric literal, it should be transfered to decimal firstly
      case numL: VNumericLiteral => numL match {
        case numAbsL: VAbstractLiteral => numAbsL match {
          case VIntegerLiteral(s) => VScalarType("integer").getInitValFromLiteral(s)
          case VRealLiteral(s) => VScalarType("real").getInitValFromLiteral(s)
          case VBaseLiteral(s) => numAbsL.asInstanceOf[VBaseLiteral].getValue match {
            case i : Int => VScalarType("integer").getInitValFromLiteral(i.toString)
            //case r : Float/Double => VScalarType("real").getInitValFromLiteral(r.toString)
          }
        }
        case VPhysicalLiteral(s) => handler(s)
      }
      // enumeral
      case enumL: VLiteralEnum => enumL match {
        // TODO: [HC] Check whether s has been lowercased
        case VLiteralEnumId(s) => {
          if (trueOrFalse(s)) {
            VScalarType("boolean").getInitValFromLiteral(s)
          } else {
            val idef = defInfo.getDef(s)
            val expKind = idef.getExpKind
            idef match {
              case iVariable: IVariable => IExpression_Variable(iVariable, expKind)
              case iVl: IVl => IExpression_Vl(iVl, expKind)
              case signal: Signal => IExp_signal(signal, expKind)
              case iSpl: ISpl => IExpression_Spl(iSpl, expKind)
              case port: Port => IExp_port(port, expKind)
              case _ => handler(s"${idef}, ${s}")
            }
          }
        }
        // TODO: [HC] will be refined later if is a scalar type; for other cases???
        case VLiteralEnumChar(s) => VScalarType(defaultCharType).getInitValFromLiteral(s)
      }
      case VLiteralBitS(s) => handler(s)
      case vLiteralS: VLiteralS => ???
      case VLiteralNull => ???
    }
  }

  def asRangeExp(defInfo: DefInfo): IExpression_constantBaseType = this match {
    case abstractLiteral: VAbstractLiteral => abstractLiteral match {
      case VIntegerLiteral(s) => VScalarType("natural").getInitValFromLiteral(s)
      case VRealLiteral(s) => handler(s"real as range? ${s}")
      case VBaseLiteral(s) => handler(s"base as range? ${s}")
    }
    case _ => handler(s"what as range? ${toString}")
  }

  def to_IDiscreteRange(defInfo: DefInfo): IDiscrete_range = {
    val e = asRangeExp(defInfo)
    IVhdl_dis_downto(e, e)
  }

  // [TN] Not sure this is a valid way to put get_init_val, should move to ISyntax
  override  def getStringValue: String = this match {
    case VIntegerLiteral(s) => s
    case VRealLiteral(s) => s
    case VBaseLiteral(s) => s
    case VLiteralEnumId(s) => s"get_init_val ${s}"
    case VLiteralEnumChar(s) => s
    case _ => unknownString
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
      VNumericLiteral(ctx.numeric_literal())
    } else throw VIError
  }
}

case object VLiteralNull extends VLiteral

case class VLiteralBitS(s: String) extends VLiteral

case class VLiteralS(s: String) extends VLiteral {
  def num2Exp(valType: VScalarType, vExplicitRange: VExplicitRange): IExpression_constantBaseType = {
    val ss = s.substring(1, s.length - 1)
    require(ss.forall(_.isDigit), s"${s} not all digits")
    val iConstList = ss.toList.map(c => IConstS("val_c", s"(CHR ''${c}'')"))
    val vt = valType.vectorize
    if (vExplicitRange.vDirection == VDirection.to) {
      if (ss.forall(_ == ss(0))) {
        IExpression_constantBaseType(vt, IConstL_gen(vt, ss.length.toString, ss(0)), ExpVectorKindTo)
      } else {
        IExpression_constantBaseType(vt, IConstL_raw(vt, iConstList), ExpVectorKindTo)
      }
    } else if (vExplicitRange.vDirection == VDirection.downto) {
      if (ss.forall(_ == ss(0))) {
        IExpression_constantBaseType(vt, IConstRL_gen(vt, ss.length.toString, ss(0)), ExpVectorKindDownTo)
      } else {
        IExpression_constantBaseType(vt, IConstRL_raw(vt, iConstList), ExpVectorKindDownTo)
      }
    } else throw VIError
  }

}

//********************************************************************************************************************//
sealed abstract class VLiteralEnum(s: String) extends VLiteral

object VLiteralEnum {
  def apply(ctx: Enumeration_literalContext): VLiteralEnum = {
    val id = ctx.identifier()
    val characterLiteral = ctx.CHARACTER_LITERAL()
    if (id != null) {
      VLiteralEnumId(id.getText.toLowerCase)
    } else if (characterLiteral != null) {
      VLiteralEnumChar(characterLiteral.getText)
    } else throw VIError
  }
}

case class VLiteralEnumId(identifier: String) extends VLiteralEnum(identifier)

case class VLiteralEnumChar(s: String) extends VLiteralEnum(s)

//********************************************************************************************************************//

sealed abstract class VNumericLiteral extends VLiteral {
  val s: String
}

object VNumericLiteral {
  def apply(ctx: Numeric_literalContext): VNumericLiteral = {
    val abs = ctx.abstract_literal()
    val phy = ctx.physical_literal()
    if (abs != null) {
      VAbstractLiteral(abs)
    } else {
      VPhysicalLiteral(phy.getText)
    }
  }
}

sealed abstract class VAbstractLiteral extends VNumericLiteral {
  val s: String
}

object VAbstractLiteral {
  def apply(ctx: Abstract_literalContext): VAbstractLiteral = {
    val intL = ctx.INTEGER()
    val realL = ctx.REAL_LITERAL()
    val baseL = ctx.BASE_LITERAL()
    if (intL != null) {
      VIntegerLiteral(ctx.getText)
    } else if (realL != null) {
      VRealLiteral(ctx.getText)
    } else if (baseL != null) {
      VBaseLiteral(baseL.getText)
    } else throw VIError
  }
}

case class VIntegerLiteral(s: String) extends VAbstractLiteral

case class VRealLiteral(s: String) extends VAbstractLiteral

// TODO: The value could be real number (Return double or float)
case class VBaseLiteral(s: String) extends VAbstractLiteral {
  def getValue = {
    val values = s.split("#");
    val radix = values(0).toInt
    // val baseFractionalNumber = values(1)
    // val exponent = values(2).toInt
    if (radix < 2 || radix > 16) handler(s)
    val baseInteger = values(1)
    Integer.parseInt(baseInteger, radix)
  }
}

case class VPhysicalLiteral(s: String) extends VNumericLiteral

object VPhysicalLiteral {
  def apply(ctx: Physical_literalContext): VPhysicalLiteral = {
    val s = ctx.identifier().getText
    VPhysicalLiteral(s)
  }
}

//********************************************************************************************************************//

case class VBaseUnitDecl(id: String)

object VBaseUnitDecl {
  def apply(ctx: Base_unit_declarationContext): VBaseUnitDecl = {
    VBaseUnitDecl(ctx.identifier().getText)
  }
}

case class VSecondaryUnitDecl(id: String, phyLiteral: VPhysicalLiteral)

object VSecondaryUnitDecl {
  def apply(ctx: Secondary_unit_declarationContext): VSecondaryUnitDecl = {
    val id = ctx.identifier().getText
    val phyLiteral = VPhysicalLiteral(ctx.physical_literal())
    VSecondaryUnitDecl(id, phyLiteral)
  }
}

//********************************************************************************************************************//
sealed trait VTypeDef

object VTypeDef {
  def apply(ctx: Type_definitionContext): VTypeDef = {
    val scalar_type_definitionContext = ctx.scalar_type_definition()
    val composite_type_definitionContext = ctx.composite_type_definition()
    val access_type_definition = ctx.access_type_definition()
    val file_type_definition = ctx.file_type_definition()
    if (scalar_type_definitionContext != null) {
      VScalarTypeDef(scalar_type_definitionContext)
    } else if (composite_type_definitionContext != null) {
      VCompositeTypeDef(composite_type_definitionContext)
    } else if (access_type_definition != null) {
      VAccessTypeDef(access_type_definition)
    } else if (file_type_definition != null) {
      VFileTypeDef(file_type_definition)
    } else throw VIError
  }
}

case class VAccessTypeDef(subtypeInd: VSubtypeIndication) extends VTypeDef

object VAccessTypeDef {
  def apply(ctx: Access_type_definitionContext): VAccessTypeDef = {
    ???
  }
}

case class VFileTypeDef(subtypeInd: VSubtypeIndication) extends VTypeDef

object VFileTypeDef {
  def apply(ctx: File_type_definitionContext): VFileTypeDef = {
    ???
  }
}

abstract class VScalarTypeDef extends VTypeDef

object VScalarTypeDef {
  def apply(ctx: Scalar_type_definitionContext): VScalarTypeDef = {
    val physical_type_definitionContext = ctx.physical_type_definition()
    val enumeration_type_defnition = ctx.enumeration_type_definition()
    val range_constraint = ctx.range_constraint()
    if (physical_type_definitionContext != null) {
      VScalarTypeDefP(physical_type_definitionContext)
    } else if (enumeration_type_defnition != null) {
      VScalarTypeE(enumeration_type_defnition)
    } else if (range_constraint != null) {
      VScalarTypeDefR(range_constraint)
    } else throw VIError
  }
}

case class VScalarTypeDefP(rangeConstraint: VRangeConstraint, baseUnitDecl: VBaseUnitDecl,
                           secondaryUnitDecl: List[VSecondaryUnitDecl], id: Option[String]) extends VScalarTypeDef

object VScalarTypeDefP {
  def apply(ctx: Physical_type_definitionContext): VScalarTypeDefP = {
    val rangeConstraint = VRangeConstraint(ctx.range_constraint())
    val baseUnitDecl = VBaseUnitDecl(ctx.base_unit_declaration())
    val secondaryUnitDeclList = ctx.secondary_unit_declaration().map(VSecondaryUnitDecl(_)).toList
    val id = Option(ctx.identifier()).map(_.getText)
    VScalarTypeDefP(rangeConstraint, baseUnitDecl, secondaryUnitDeclList, id)
  }
}

case class VScalarTypeE(enumLiteralList: List[VLiteralEnum]) extends VScalarTypeDef

object VScalarTypeE {
  def apply(ctx: Enumeration_type_definitionContext): VScalarTypeE = {
    val literalList = ctx.enumeration_literal().map(VLiteralEnum(_)).toList
    VScalarTypeE(literalList)
  }
}

case class VScalarTypeDefR(rangeConstraint: VRangeConstraint) extends VScalarTypeDef

object VScalarTypeDefR {
  def apply(ctx: Range_constraintContext): VScalarTypeDefR = {
    val rangeConstraint = VRangeConstraint(ctx)
    VScalarTypeDefR(rangeConstraint)
  }
}

abstract class VCompositeTypeDef extends VTypeDef

object VCompositeTypeDef {
  def apply(ctx: Composite_type_definitionContext): VCompositeTypeDef = {
    val array_type_definitionContext = ctx.array_type_definition()
    val record_type_definitionContext = ctx.record_type_definition()
    if (array_type_definitionContext != null) {
      VArrayTypeDef(array_type_definitionContext)
    } else if (record_type_definitionContext != null) {
      VRecordTypeDef(record_type_definitionContext)
    } else throw VIError
  }
}

abstract class VArrayTypeDef extends VCompositeTypeDef

object VArrayTypeDef {
  def apply(ctx: Array_type_definitionContext): VArrayTypeDef = {
    val uad = ctx.unconstrained_array_definition()
    val cad = ctx.constrained_array_definition()
    if (uad != null) {
      VUnconstrainedArrayDefinition(uad)
    } else if (cad != null) {
      VConstrainedArrayDefinition(cad)
    } else throw VIError
  }
}

case class VIndexSubtypeDef(name: String)

case class VUnconstrainedArrayDefinition(indexSubtypeDefList: List[VIndexSubtypeDef],
                                         subtypeIndication: VSubtypeIndication) extends VArrayTypeDef

object VUnconstrainedArrayDefinition {
  def apply(ctx: Unconstrained_array_definitionContext): VUnconstrainedArrayDefinition = {
    val indexSubtypeDefList = ctx.index_subtype_definition().map(d => VIndexSubtypeDef(d.name().getText)).toList
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    VUnconstrainedArrayDefinition(indexSubtypeDefList, subtypeIndication)
  }
}

case class VConstrainedArrayDefinition(indexConstraint: VIndexConstraint,
                                       vSubtypeIndication: VSubtypeIndication) extends VArrayTypeDef

object VConstrainedArrayDefinition {
  def apply(ctx: Constrained_array_definitionContext): VConstrainedArrayDefinition = {
    val indexConstraint = VIndexConstraint(ctx.index_constraint())
    val subtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    VConstrainedArrayDefinition(indexConstraint, subtypeIndication)
  }
}

// no element_subtype_definition
case class VElementDecl(ids: List[String], subtypeInd: VSubtypeIndication) {
  def flatten = for (id <- ids) yield (id, subtypeInd)
}

object VElementDecl {
  def apply(ctx: Element_declarationContext): VElementDecl = {
    val idList = V2IUtils.getIdList(ctx.identifier_list())
    val subtypeInd = VSubtypeIndication(ctx.element_subtype_definition().subtype_indication())
    VElementDecl(idList, subtypeInd)
  }
}

case class VRecordTypeDef(elementDecls: List[VElementDecl], id: Option[String]) extends VCompositeTypeDef

object VRecordTypeDef {
  def apply(ctx: Record_type_definitionContext): VRecordTypeDef = {
    val elementDecls = (for {
      ed <- ctx.element_declaration()
    } yield VElementDecl(ed)).toList
    val id = Option(ctx.identifier()).map(_.getText)
    VRecordTypeDef(elementDecls, id)
  }
}

//********************************************************************************************************************//

abstract class VChoice {
  def getSimpleExpression: VSimpleExpression = this match {
    case VChoiceSimpleExpression(vSimpleExpression) => vSimpleExpression
    case _ => ???
  }

  def getId : String = this match {
    case VChoiceIdentifier(id) => id
    case VChoiceOthers => "others"
    case _ => ???
  }
}

object VChoice {
  def apply(ctx: ChoiceContext): VChoice = {
    val others = ctx.OTHERS()
    if (ctx.identifier() != null) VChoiceIdentifier(ctx.identifier().getText)
    else if (ctx.discrete_range() != null) VChoiceDiscreteRange(VDiscreteRange(ctx.discrete_range()))
    else if (ctx.simple_expression() != null) VChoiceSimpleExpression(VSimpleExpression(ctx.simple_expression()))
    else if (others != null) VChoiceOthers
    else throw VIError
  }
}

case class VChoiceIdentifier(id: String) extends VChoice

case class VChoiceDiscreteRange(vDiscreteRange: VDiscreteRange) extends VChoice

case class VChoiceSimpleExpression(vSimpleExpression: VSimpleExpression) extends VChoice

case object VChoiceOthers extends VChoice

case class VChoices(vChoiceList: List[VChoice])

object VChoices {
  def apply(ctx: ChoicesContext): VChoices = {
    val choiceList = ctx.choice().map(VChoice(_)).toList
    VChoices(choiceList)
  }
}

//********************************************************************************************************************//

case class VElementAssociation(choices: Option[VChoices], vExpression: VExpression)

object VElementAssociation {
  def apply(ctx: Element_associationContext): VElementAssociation = {
    val choices = Option(ctx.choices()).map(VChoices(_))
    val vExpression = VExpression(ctx.expression())
    VElementAssociation(choices, vExpression)
  }
}

case class VAggregate(vElementAssociationList: List[VElementAssociation]) extends VTarget with VPrimary {
  require(vElementAssociationList.nonEmpty, "elemAssocList")
  lazy val _getAssoc: List[(String, VExpression)] = {
    for {
      elemAssoc <- vElementAssociationList
      choice <- elemAssoc.choices match {
        case Some(c) => c.vChoiceList
        case None => throw VIError
      }
    } yield {
      val id = choice match {
        case VChoiceIdentifier(s) => s
        case VChoiceOthers => "others"
        case _ => s"???${choice}"
      }
      val vExp = elemAssoc.vExpression
      id -> vExp
    }
  }

  def getFirstMap = _getAssoc.head

  lazy val getAssoc: Map[String, VExpression] = _getAssoc.toMap

}

object VAggregate {
  def apply(ctx: AggregateContext): VAggregate = {
    val element_assocList = ctx.element_association().map(VElementAssociation(_)).toList
    VAggregate(element_assocList)
  }
}

sealed trait VQualifiedExpression extends VPrimary

case class VQualifiedExpressionAggregate(vSubtypeIndication: VSubtypeIndication, aggregate: VAggregate) extends VQualifiedExpression

case class VQualifiedExpressionExpression(vSubtypeIndication: VSubtypeIndication, exp: VExpression) extends VQualifiedExpression

object VQualifiedExpression {
  def apply(ctx: Qualified_expressionContext): VQualifiedExpression = {
    val vSubtypeIndication = VSubtypeIndication(ctx.subtype_indication())
    if (ctx.aggregate() != null) {
      val vAggregate = VAggregate(ctx.aggregate())
      VQualifiedExpressionAggregate(vSubtypeIndication, vAggregate)
    } else if (ctx.expression() != null) {
      val vExp = VExpression(ctx.expression())
      VQualifiedExpressionExpression(vSubtypeIndication, vExp)
    } else throw VIError
  }
}

sealed trait VAllocator extends VPrimary

case class VAllocatorQualifiedExpression(vQualifiedExpression: VQualifiedExpression) extends VAllocator

case class VAllocatorSubtypeIndication(vSubtypeIndication: VSubtypeIndication) extends VAllocator

object VAllocator {
  def apply(ctx: AllocatorContext): VAllocator = {
    if (ctx.qualified_expression() != null) {
      VAllocatorQualifiedExpression(VQualifiedExpression(ctx.qualified_expression()))
    } else if (ctx.subtype_indication() != null) {
      VAllocatorSubtypeIndication(VSubtypeIndication(ctx.subtype_indication()))
    } else throw VIError
  }
}

sealed trait VPrimary {
  def toIExp(defInfo: DefInfo): IExpression = this match {
    case vLiteral: VLiteral => vLiteral.toIExp(defInfo)
    case vQualifiedExpression: VQualifiedExpression => ???
    case VPrimaryExpression(vExpression) => vExpression.toIExp(defInfo)
    case vAllocator: VAllocator => ???
    case vAggregate: VAggregate => vAggregate.getFirstMap._2.toIExp(defInfo)

    case vName: VName => vName.toIRhs(defInfo)
  }

  def getStringValue: String = this match {
    case vLiteral: VLiteral => vLiteral.getStringValue
    case _ => s"""(??? ${this.getClass.getName})"""
  }

  def getLiteral: Option[VLiteral] = this match {
    case vLiteral: VLiteral => Some(vLiteral)
    case _ => None
  }

  def getAggregate: Option[VAggregate] = this match {
    case vAggregate: VAggregate => Some(vAggregate)
    case _ => None
  }

}

object VPrimary {
  def apply(ctx: PrimaryContext): VPrimary = {
    if (ctx.literal() != null) VLiteral(ctx.literal())
    else if (ctx.qualified_expression() != null) VQualifiedExpression(ctx.qualified_expression())
    else if (ctx.expression() != null) VPrimaryExpression(VExpression(ctx.expression()))
    else if (ctx.allocator() != null) VAllocator(ctx.allocator())
    else if (ctx.aggregate() != null) VAggregate(ctx.aggregate())
    else if (ctx.name() != null) VName(ctx.name())
    else throw VIError
  }
}

case class VPrimaryExpression(exp: VExpression) extends VPrimary

// [HC] Just a hack
case class VSuffix(s: String) {
  override def toString = s"${s}"
}


// [HC] Perhaps needing separation
sealed abstract class VName extends VTarget with VRange with VPrimary {

  def getVSelectedName(defInfo: DefInfo): (VSelectedName, Option[IDiscrete_range])

  def getSimpleNameOption: Option[String] = this match {
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

  def getSimpleName: String = getSimpleNameOption match {
    case Some(s) => s
    case None => handler(s"${toString}")
  }

  def toIRhs(defInfo: DefInfo): IExpression

}

object VName {
  def apply(ctx: NameContext): VName = {
    if (ctx.selected_name() != null) {
      VSelectedName(ctx.selected_name())
    } else VNameParts(ctx)
  }
}


case class VSelectedName(id: String, suffixList: List[VSuffix]) extends VName {

  override def getVSelectedName(defInfo: DefInfo) = (this, None)

  def extracted(extractor: String): String = {
    val nList = suffixList.scanLeft(id)((acc, cur) => s"${acc}_${cur}")
    nList.tail.foldLeft(nList.head)((acc, cur) => s"(${acc} ${extractor}''${cur}'')")
  }

  def isa_v = extracted("v.")

  def isa_sp = extracted("s.")

  override def toIRhs(defInfo: DefInfo): IExpression = {
    val iDef = defInfo.getDef(this)
    val expKind = iDef.getExpKind
    iDef match {
      case vl: IVl => IExp_vl_rhs(vl, this, expKind)
      case v: IVariable => suffixList match {
        case Nil => IExpression_Variable(v, expKind)
        case _ => IExp_vl_rhs(v, this, expKind)
      }
      case spl: ISpl => IExp_spl_rhs(spl, this, expKind)
      case s: Signal => suffixList match {
        case Nil => IExp_signal(s, expKind)
        case _ => IExp_spl_rhs(s, this, expKind)
      }
      case p: Port => suffixList match {
        case Nil => IExp_port(p, expKind)
        case _ => IExp_spl_rhs(p, this, expKind)
      }
    }
  }

}

object VSelectedName {
  def apply(ctx: Selected_nameContext): VSelectedName = {
    val id = ctx.identifier().getText.toLowerCase
    val suffixList = ctx.suffix().map(s => VSuffix(s.getText)).toList
    VSelectedName(id, suffixList)
  }
}

sealed abstract class VAttrDesignator

object VAttrDesignator {
  def apply(ctx: Attribute_designatorContext): VAttrDesignator = {
    val id = ctx.identifier()
    val range = ctx.RANGE()
    val rrange = ctx.REVERSE_RANGE()
    val across = ctx.ACROSS()
    val through = ctx.THROUGH()
    val reference = ctx.REFERENCE()
    val tolerance = ctx.TOLERANCE()
    if (id != null) {
      VAttrDesignatorId(id.getText)
    } else if (range != null) {
      VAttrDesignatorR
    } else if (rrange != null) {
      VAttrDesignatorRR
    } else if (across != null) {
      VAttrDesignatorA
    } else if (through != null) {
      VAttrDesignatorTh
    } else if (reference != null) {
      VAttrDesignatorRef
    } else if (tolerance != null) {
      VAttrDesignatorT
    } else throw VIError
  }
}

case class VAttrDesignatorId(id: String) extends VAttrDesignator

object VAttrDesignatorR extends VAttrDesignator

object VAttrDesignatorRR extends VAttrDesignator

object VAttrDesignatorA extends VAttrDesignator

object VAttrDesignatorTh extends VAttrDesignator

object VAttrDesignatorRef extends VAttrDesignator

object VAttrDesignatorT extends VAttrDesignator

case class VNameAttributePart(attrDesignator: VAttrDesignator, exprList: List[VExpression])

object VNameAttributePart {
  def apply(ctx: Name_attribute_partContext): VNameAttributePart = {
    val attrDesignator = VAttrDesignator(ctx.attribute_designator())
    val exps = ctx.expression().map(VExpression(_)).toList
    VNameAttributePart(attrDesignator, exps)
  }
}

case class VNameFunctionCallOrIndexedPart(assocListOpt: Option[VAssociationList]) {
  def getExp: VExpression = assocListOpt match {
    case Some(al) => {
      val elem = al.vAssociationElementList.head.actualPart
      val designator = elem match {
        case VActualPartD(d) => d
        case VActualPartN(sn, d) => d
      }
      designator match {
        case VActualDesignatorExpression(vExp) => vExp
        case VActualDesignatorOpen => ???
      }
    }
    case None => handler(s"${toString}")
  }

  def toI_rhs(defInfo: DefInfo): IExpression = {
    getExp.toIExp(defInfo)
  }
}

object VNameFunctionCallOrIndexedPart {
  def apply(ctx: Name_function_call_or_indexed_partContext): VNameFunctionCallOrIndexedPart = {
    val assocList = Option(ctx.actual_parameter_part()).map(al => VAssociationList(al.association_list()))
    VNameFunctionCallOrIndexedPart(assocList)
  }
}

case class VNameSlicePart(r1: VExplicitRange, r2: Option[VExplicitRange]) {
  // TODO r2
  def toI_lhs(defInfo: DefInfo): IDiscrete_range = {
    r1.toI(defInfo)
  }

  // TODO r2
  def toI_rhs(defInfo: DefInfo): (IExpression, IExpression) = r1.vDirection match {
    case VDirection.`to` => (r1.right.toIExp(defInfo), r1.left.toIExp(defInfo))
    case VDirection.`downto` => (r1.left.toIExp(defInfo), r1.right.toIExp(defInfo))
  }
}

object VNameSlicePart {
  def apply(ctx: Name_slice_partContext): VNameSlicePart = {
    val rangeList = ctx.explicit_range().map(VExplicitRange(_))
    VNameSlicePart(rangeList.head, rangeList.lift(1))
  }
}

sealed abstract class VNamePart {
  def toILhs(defInfo: DefInfo): (VSelectedName, Option[IDiscrete_range]) = this match {
    case VNamePartAttr(selectedName, nameAttrPart) => ???
    case VNamePartFnI(selectedName, nameFnCallOrIndexPart) => {
      val literal = nameFnCallOrIndexPart.getExp.getLiteral
      val range = literal match {
        case Some(l) => Some(l.to_IDiscreteRange(defInfo))
        case None => None
      }
      (selectedName, range)
    }
    case VNamePartSlice(selectedName, nameSlicePart) => {
      (selectedName, Option(nameSlicePart.toI_lhs(defInfo)))
    }
  }

  def toI_rhs(defInfo: DefInfo): IExpression = this match {
    case VNamePartAttr(selectedName, nameAttrPart) => ???
    case VNamePartFnI(selectedName, nameFnCallOrIndexPart) => {
      //      logger.info(s"${toString}")
      val iexp = selectedName.toIRhs(defInfo)
      val nth = nameFnCallOrIndexPart.toI_rhs(defInfo)
      // always
      IExp_nth(iexp, nth)
    }
    case vNamePartSlice@VNamePartSlice(selectedName, nameSlicePart) => {
      val iexp = selectedName.toIRhs(defInfo)
      // not checked type, trust VHDL semantics
      val (e1, e2) = nameSlicePart.toI_rhs(defInfo)
      // always
      IExp_sl(iexp, e1, e2)
    }
  }
}

object VNamePart {
  def apply(ctx: Name_partContext): VNamePart = {
    val selectedName = VSelectedName(ctx.selected_name())
    if (ctx.name_attribute_part() != null) {
      VNamePartAttr(selectedName, VNameAttributePart(ctx.name_attribute_part()))
    } else if (ctx.name_function_call_or_indexed_part() != null) {
      VNamePartFnI(selectedName, VNameFunctionCallOrIndexedPart(ctx.name_function_call_or_indexed_part()))
    } else if (ctx.name_slice_part() != null) {
      VNamePartSlice(selectedName, VNameSlicePart(ctx.name_slice_part()))
    } else throw VIError
  }
}

case class VNamePartAttr(selectedName: VSelectedName,
                         nameAttrPart: VNameAttributePart) extends VNamePart

case class VNamePartFnI(selectedName: VSelectedName,
                        nameFnCallOrIndexPart: VNameFunctionCallOrIndexedPart) extends VNamePart {
}

case class VNamePartSlice(selectedName: VSelectedName,
                          nameSlicePart: VNameSlicePart) extends VNamePart


case class VNameParts(namePartList: List[VNamePart]) extends VName {
  require(namePartList.nonEmpty, "VNameParts")

  override def getVSelectedName(defInfo: DefInfo): (VSelectedName, Option[IDiscrete_range]) = {
    // TODO: Currently we only deal with head of namePartList
    namePartList.head.toILhs(defInfo)
  }

  override def toIRhs(defInfo: DefInfo): IExpression = {
    // TODO: Currently we only deal with head of namePartList
    namePartList.head.toI_rhs(defInfo)
  }
}

object VNameParts {
  def apply(ctx: NameContext): VNameParts = {
    val namePartList = ctx.name_part().map(VNamePart(_))
    VNameParts(namePartList.toList)
  }
}

//********************************************************************************************************************//

sealed trait VAliasIndication

case class VSubtypeIndication(selectedName: VSelectedName,
                              constraint: Option[VConstraint],
                              tolerance: Option[VToleranceAspect]) extends VAliasIndication with VDiscreteRange {
  def getExplicitRangeOption: Option[VExplicitRange] = constraint.map(_.getExplicitRange)

  def getSimpleName = selectedName.getSimpleNameOption.getOrElse(s"ERROR: ${toString}")
}

object VSubtypeIndication {
  def apply(ctx: Subtype_indicationContext): VSubtypeIndication = {
    val selectedName = V2IUtils.selectedNameFromSubtypeInd(ctx)
    val constraint = Option(ctx.constraint()).map(VConstraint(_))
    val tolerance = Option(ctx.tolerance_aspect()).map(VToleranceAspect(_))
    VSubtypeIndication(selectedName, constraint, tolerance)
  }
}

case class VToleranceAspect(vExp: VExpression)

object VToleranceAspect {
  def apply(ctx: Tolerance_aspectContext): VToleranceAspect = {
    val vExp = VExpression(ctx.expression())
    VToleranceAspect(vExp)
  }
}

case class VSubnatureIndication(name: String,
                                indexConstraint: Option[VIndexConstraint],
                                exprs: Option[(VExpression, VExpression)]) extends VAliasIndication

sealed trait VRange extends VDiscreteRange {
  override  def getVExplicitRange: VExplicitRange = this match {
    case vExplicitRange: VExplicitRange => vExplicitRange
    case vName: VName => ???
  }
}

object VRange {
  def apply(ctx: RangeContext): VRange = {
    if (ctx.explicit_range() != null) {
      VExplicitRange(ctx.explicit_range())
    } else if (ctx.name() != null) {
      VName(ctx.name())
    } else throw VIError
  }
}


//********************************************************************************************************************//

sealed trait VDiscreteRange {
  def getVExplicitRange: VExplicitRange = this match {
    case vRange: VRange => vRange.getVExplicitRange
    case vSubtypeIndication: VSubtypeIndication => vSubtypeIndication.getExplicitRangeOption match {
      case Some(vExplicitRange) => vExplicitRange
      case None => ???
    }
  }
}

object VDiscreteRange {
  def apply(ctx: Discrete_rangeContext): VDiscreteRange = {
    if (ctx.range() != null) VRange(ctx.range())
    else if (ctx.subtype_indication() != null) VSubtypeIndication(ctx.subtype_indication())
    else throw VIError
  }
}

//********************************************************************************************************************//


sealed trait VConstraint {
  def getExplicitRange: VExplicitRange = this match {
    case VRangeConstraint(vRange) => vRange.getVExplicitRange
    case VIndexConstraint(vDiscreteRanges) => {
      if (vDiscreteRanges.size == 1) vDiscreteRanges.head.getVExplicitRange
      else ??? // Multidimensional
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
    } else throw VIError
  }
}

case class VRangeConstraint(range: VRange) extends VConstraint

object VRangeConstraint {
  def apply(ctx: Range_constraintContext): VRangeConstraint = {
    VRangeConstraint(VRange(ctx.range()))
  }
}

case class VIndexConstraint(discreteRanges: List[VDiscreteRange]) extends VConstraint {
  require(discreteRanges.nonEmpty, "discrete ranges")
}

object VIndexConstraint {
  def apply(ctx: Index_constraintContext): VIndexConstraint = {
    val discrete_rangeList = ctx.discrete_range().map(VDiscreteRange(_)).toList
    VIndexConstraint(discrete_rangeList)
  }
}

case class VExplicitRange(left: VSimpleExpression, vDirection: VDirection.Value, right: VSimpleExpression) extends VRange{
  def toI(defInfo: DefInfo): IDiscrete_range = vDirection match {
    case VDirection.`to` =>
      IVhdl_dis_to(left.toIExp(defInfo), right.toIExp(defInfo))
    case VDirection.`downto` => IVhdl_dis_downto(left.toIExp(defInfo), right.toIExp(defInfo))
  }
}

object VExplicitRange {
  def apply(ctx: Explicit_rangeContext): VExplicitRange = {
    val simplExprList = for {
      simplExpr <- ctx.simple_expression()
    } yield VSimpleExpression(simplExpr)
    val direction = VDirection.withName(ctx.direction().getText.toUpperCase)
    require(simplExprList.length == 2, "explicitRange")
    VExplicitRange(simplExprList.head, direction, simplExprList.last)
  }
}

object VDirection extends Enumeration {
  type VDirection = Value
  val to = Value("TO")
  val downto = Value("DOWNTO")
}

//********************************************************************************************************************//

abstract class VFactor {
  def toIExp(defInfo: DefInfo): IExpression = this match {
    case VPrimaryFactor(primary, primaryOption) => {
      primaryOption match {
        case Some(p) => IBinaryArithmeticPrimaryExpression(primary.toIExp(defInfo), VDoubleStarOperator.exp, p.toIExp(defInfo))
        case None => primary.toIExp(defInfo)
      }
    }
    case VAbsFactor(primary) => IUnaryExpression(VUnaryOperator.abs, primary.toIExp(defInfo))
    case VNotFactor(primary) => IUnaryExpression(VUnaryOperator.not, primary.toIExp(defInfo))
  }

  def computeE(v: String, n: String): String = {
    def power(v: Int, n: Int) = Array.fill(n)(v).product
    try {
      power(v.toInt, n.toInt).toString
    } catch {
      case e: Exception => "???"
    }
  }

  def getStringValue: String = this match {
    case VPrimaryFactor(primary, primaryOption) => primaryOption match {
      case Some(p) => computeE(primary.getStringValue, p.getStringValue)
      case None => primary.getStringValue
    }
    case VAbsFactor(primary) => {
      try {
        Math.abs(primary.getStringValue.toInt).toString
      } catch {
        case e: Exception => "???absFactor"
      }
    }
    case VNotFactor(primary) => "???notFactor"
  }

}

object VFactor {
  def apply(ctx: FactorContext): VFactor = {
    val vPrimaryList = ctx.primary().map(VPrimary(_))
    if (ctx.ABS() != null) {
      VAbsFactor(vPrimaryList.head)
    } else if (ctx.NOT() != null) {
      VNotFactor(vPrimaryList.head)
    } else {
      VPrimaryFactor(vPrimaryList.head, vPrimaryList.lift(1))
    }
  }
}

case class VPrimaryFactor(vPrimary: VPrimary, primaryOption: Option[VPrimary]) extends VFactor

case class VAbsFactor(primary: VPrimary) extends VFactor

case class VNotFactor(primary: VPrimary) extends VFactor

case class VTerm(vFactor: VFactor, ops: List[VMultiplyingOperator.Value], others: List[VFactor]) {
  def toIExp(defInfo: DefInfo): IExpression = {
    ops.zip(others).foldLeft(vFactor.toIExp(defInfo)) {
      case (accExp, (op, curFactor)) => IBinaryArithmeticFactorExpression(accExp, op, curFactor.toIExp(defInfo))
    }
  }

  //  FIXME [HC] asVal should not be used
  def getStringValue: String = {
    import VMultiplyingOperator._
    try {
      ops.zip(others).foldLeft(vFactor.getStringValue.toInt)((acc, cur) => {
        val f = cur._2.getStringValue.toInt
        cur._1 match {
          case `mul` => acc * f
          case `div` => acc / f
          case `mod` => acc % f
          case `rem` => ???
        }
      }).toString
    } catch {
      case nfe : NumberFormatException => {
        ops.zip(others).foldLeft(vFactor.getStringValue)((acc, cur) => {
          val f = cur._2.getStringValue
          cur._1 match {
            case `mul` => s"(${acc} * ${f})"
            case `div` => ???
            case `mod` => ???
            case `rem` => ???
          }
        }).toString
      }
      case e: Exception => ???
    }
  }
}

object VTerm {
  def apply(ctx: TermContext): VTerm = {
    val ops = ctx.multiplying_operator().map(VMultiplyingOperator(_)).toList
    val factors = ctx.factor().map(VFactor(_)).toList
    VTerm(factors.head, ops, factors.tail)
  }
}

//********************************************************************************************************************//

object VUnaryOperator extends Enumeration {
  type VUnaryOperator = Value
  val abs = Value("[abs]")
  val not = Value("[not]")
  val neg = Value("[-:]")
  val pos = Value("[+:]")
}

//********************************************************************************************************************//
sealed trait VArithmeticOperator

object VAddingOperator extends Enumeration with VArithmeticOperator {
  // TODO VHDL overloads this, should make it homomorphic
  type VAddingOperator = Value
  val plus = Value("[+]")
  val minus = Value("[-]")
  val ampersand = Value("[&]")

  def apply(ctx: Adding_operatorContext): VAddingOperator.Value = {
    if (ctx.PLUS() != null) plus
    else if (ctx.MINUS() != null) minus
    else if (ctx.AMPERSAND() != null) ampersand
    else throw VIError
  }

}

object VMultiplyingOperator extends Enumeration with VArithmeticOperator {
  type VMultiplyingOperator = Value
  val mul = Value("[*]")
  val div = Value("[/]")
  val mod = Value("[mod]")
  val rem = Value("[rem]")

  def apply(ctx: Multiplying_operatorContext): VMultiplyingOperator = {
    if (ctx.MUL() != null) mul
    else if (ctx.DIV() != null) div
    else if (ctx.MOD() != null) mod
    else if (ctx.REM() != null) rem
    else throw VIError
  }
}

object VDoubleStarOperator extends Enumeration with VArithmeticOperator {
  type VDoubleStarOperator = Value
  val exp = Value("[**]")
}

//********************************************************************************************************************//

case class VSimpleExpression(termSign: Option[String], vTerm: VTerm, ops: List[VAddingOperator.Value], others: List[VTerm]) {

  def refine__tl_trl(tKind: ExpKind, srcIExp: IExpression): IExpression = {
    require(tKind.isV && srcIExp.expKind == ExpScalarKind)
    tKind match {
      case ExpVectorKindTo => IExp_tl(srcIExp)
      case ExpVectorKindDownTo => IExp_trl(srcIExp)
      case _ => srcIExp
    }
  }

  def toIExp(defInfo: DefInfo): IExpression = {
    val firstExp: IExpression = termSign match {
      case Some("+") => IUnaryExpression(VUnaryOperator.pos, vTerm.toIExp(defInfo))
      case Some("-") => IUnaryExpression(VUnaryOperator.neg, vTerm.toIExp(defInfo))
      case _ => vTerm.toIExp(defInfo)
    }
    // FIXME this is perhaps still WRONG since VHDL may allow "vt := st1 [+] st2"
    ops.zip(others).foldLeft(firstExp) {
      case (acc, (op, curTerm)) => {
        val cur = curTerm.toIExp(defInfo)
        if (acc.expKind.isV && cur.expKind == ExpScalarKind) {
          val refinedCur = refine__tl_trl(acc.expKind, cur)
          IBinaryArithmeticTermExpression(acc, op, refinedCur)
        } else if (acc.expKind == ExpScalarKind && cur.expKind.isV) {
          val refinedAcc = refine__tl_trl(cur.expKind, acc)
          IBinaryArithmeticTermExpression(refinedAcc, op, cur)
        } else {
          val refined = V2IUtils.refine__valType(acc, cur)
          IBinaryArithmeticTermExpression(acc, op, refined)
        }
      }
    }
  }

  def getStringValue: String = {
    import VAddingOperator._
    val sign = termSign match {
      case Some("-") => "-"
      case _ => ""
    }
    try {
      ops.zip(others).foldLeft((sign + vTerm.getStringValue).toInt)((acc, cur) => {
        val f = cur._2.getStringValue.toInt
        cur._1 match {
          case `plus` => acc + f
          case `minus` => acc - f
          case `ampersand` => throw VIError
        }
      }).toString
    } catch {
      case nfe : NumberFormatException => {
        // FIXME if sign is negative -> sign + term.asVal is syntax error in Isabelle
        ops.zip(others).foldLeft((sign + vTerm.getStringValue))((acc, cur) => {
          val f = cur._2.getStringValue
          cur._1 match {
            case `plus` => s"(${acc} + ${f})"
            case `minus` => s"(${acc} - ${f})"
            case `ampersand` => throw VIError
          }
        }).toString
      }
      case e: Throwable => handler(s"${e}")
    }
  }
}

object VSimpleExpression {
  def apply(ctx: Simple_expressionContext): VSimpleExpression = {
    val terms = ctx.term().map(VTerm(_)).toList
    val ops = ctx.adding_operator().map(VAddingOperator(_)).toList
    val symbol = {
      if (ctx.PLUS() != null) Some("+")
      else if (ctx.MINUS() != null) Some("-")
      else None
    }
    VSimpleExpression(symbol, terms.head, ops, terms.tail)
  }
}

//********************************************************************************************************************//

object VShiftOperator extends Enumeration {
  type VShiftOperator = Value
  val sll = Value("[sll]")
  val srl = Value("[srl]")
  val sla = Value("[sla]")
  val sra = Value("[sra]")
  val rol = Value("[rol]")
  val ror = Value("[ror]")

  def apply(op: Shift_operatorContext): VShiftOperator = {
    if (op.SLL() != null) sll
    else if (op.SRL() != null) srl
    else if (op.SLA() != null) sla
    else if (op.SRA() != null) sra
    else if (op.ROL() != null) rol
    else if (op.ROR() != null) ror
    else throw VIError
  }
}

//********************************************************************************************************************//

object VLogicalOperator extends Enumeration {
  type VLogicalOperator = Value
  val and = Value("[and]")
  val or = Value("[or]")
  val nand = Value("[nand]")
  val nor = Value("[nor]")
  val xor = Value("[xor]")
  val xnor = Value("[xnor]")

  def apply(op: Logical_operatorContext): VLogicalOperator = {
    if (op.AND() != null) and
    else if (op.OR() != null) or
    else if (op.NAND() != null) nand
    else if (op.NOR() != null) nor
    else if (op.XOR() != null) xor
    else if (op.XNOR() != null) xnor
    else throw VIError
  }
}

//********************************************************************************************************************//

object VRelationalOperator extends Enumeration {
  type VRelationalOperator = Value
  val eq = Value("[=]")
  val neq = Value("[/=]")
  val lt = Value("[<]")
  val le = Value("[<=]")
  val gt = Value("[>]")
  val ge = Value("[>=]")

  def apply(ctx: Relational_operatorContext): VRelationalOperator = {
    if (ctx.EQ() != null) eq
    else if (ctx.NEQ() != null) neq
    else if (ctx.LOWERTHAN() != null) lt
    else if (ctx.LE() != null) le
    else if (ctx.GREATERTHAN() != null) gt
    else if (ctx.GE() != null) ge
    else throw VIError
  }
}

case class VShiftExpression(vSimpleExpression: VSimpleExpression, op: Option[VShiftOperator.Value], other: Option[VSimpleExpression]) {
  def toIExp(defInfo: DefInfo): IExpression = (op, other) match {
    case (Some(opV), Some(simpleExpV)) => IBinaryShiftingExpression(vSimpleExpression.toIExp(defInfo), opV, simpleExpV.toIExp(defInfo))
    case _ => vSimpleExpression.toIExp(defInfo)
  }
}

object VShiftExpression {
  def apply(ctx: Shift_expressionContext): VShiftExpression = {
    val simple_expressionList = for {
      simple_expression <- ctx.simple_expression()
    } yield VSimpleExpression(simple_expression)
    val op = Option(ctx.shift_operator()).map(VShiftOperator(_))
    val other = simple_expressionList.lift(1)
    VShiftExpression(simple_expressionList.head, op, other)
  }
}

case class VRelation(vShiftExpression: VShiftExpression, op: Option[VRelationalOperator.Value], other: Option[VShiftExpression]) {
  // FIXME type refinement should be made here (perhaps ALL other toIExp)
  // TODO however we are not sure which should be trusted!!
  def toIExp(defInfo: DefInfo): IExpression = (op, other) match {
    case (Some(opV), Some(otherV)) => {
      val (lhs, rhs) = (vShiftExpression.toIExp(defInfo), otherV.toIExp(defInfo))
      val refinedRhs = V2IUtils.refine__valType(lhs, rhs)
      IBinaryRelationalExpression(lhs, opV, refinedRhs)
    }
    case _ => vShiftExpression.toIExp(defInfo)
  }
}

object VRelation {
  def apply(ctx: RelationContext): VRelation = {
    val shift_expressionList = for {
      shift_expression <- ctx.shift_expression()
    } yield VShiftExpression(shift_expression)
    val op = Option(ctx.relational_operator()).map(VRelationalOperator(_))
    val other = shift_expressionList.lift(1)
    VRelation(shift_expressionList.head, op, other)
  }
}

case class VExpression(vRelation: VRelation, ops: List[VLogicalOperator.Value], others: List[VRelation]) {

  def toIExp(defInfo: DefInfo): IExpression = {
    ops.zip(others).foldLeft(vRelation.toIExp(defInfo)) {
      case (acc, (op, curRelation)) => IBinaryLogicalExpression(acc, op, curRelation.toIExp(defInfo))
    }
  }

  def eval = toString

  def getPrimary: Option[VPrimary] = {
    val simplExp = vRelation.vShiftExpression.vSimpleExpression
    simplExp.vTerm.vFactor match {
      case VPrimaryFactor(primary, primaryOption) => Some(primary)
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

  def getLiteralS: VLiteralS = getLiteral match {
    case Some(l) => l match {
      case ls: VLiteralS => ls
      case _ => throw VIError
    }
    case None => throw VIError
  }

  // For rhs (exp)
  def rhs_IDef(defInfo: DefInfo): IDef = {
    val literal = getLiteral match {
      case Some(l) => l
      case None => handler(s"${toString}")
    }
    val identifier = try {
      literal.asInstanceOf[VLiteralEnumId].identifier
    } catch {
      case e: ClassCastException => {
        logger.info(s"${literal}")
        throw VIError
      }
    }
    defInfo.getDef(identifier)
  }

}

object VExpression {
  def apply(ctx: ExpressionContext): VExpression = {
    val relationList = ctx.relation().map(VRelation(_)).toList
    val logicalOps = ctx.logical_operator().map(VLogicalOperator(_)).toList
    VExpression(relationList.head, logicalOps, relationList.tail)
  }
}
