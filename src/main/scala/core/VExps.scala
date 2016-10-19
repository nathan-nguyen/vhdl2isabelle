package core

import core.V2IUtils._
import sg.edu.ntu.vhdl2isabelle.VHDLParser._
import scala.collection.JavaConversions._

/**
  * Created by Hongxu Chen.
  */
sealed abstract class VLiteral {

  val trueOrFalse = Set("true", "false")

  def toIExp(defInfo: DefInfo): IsabelleExpression = {
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
        // TODO check whether s has been lowercased
        case VLiteralEnumId(s) => {
          if (trueOrFalse(s)) {
            VScalarType("boolean").getInitValFromLiteral(s)
          } else {
            val idef = defInfo.getDef(s)
            val expKind = idef.getExpKind
            idef match {
              // NOTE: this should always use IExp_variable/IExp_signal/IExp_port
              case ivariable: Variable => IExp_variable(ivariable, expKind)
              case signal: Signal => IExp_signal(signal, expKind)
              case port: Port => IExp_port(port, expKind)
              case _ => handler(s"${idef}, ${s}")
            }
          }
        }
        // TODO will be refined later if is a scalar type; for other cases???
        case VLiteralEnumChar(s) => VScalarType(defaultCharType).getInitValFromLiteral(s)
      }
      case VLiteralBitS(s) => handler(s)
      case vLiteralS: VLiteralS => vLiteralS.num2Exp
      case VLiteralNull => handler("null")
    }
  }

  def asRangeExp(defInfo: DefInfo): IExp_baseTypeConstant = this match {
    case ln: VAbstractLiteral => ln match {
      case VIntegerLiteral(s) => VScalarType("natural").getInitValFromLiteral(s)
      case VRealLiteral(s) => handler(s"real as range? ${}")
      case VBaseLiteral(s) => handler(s"base as range? ${s}")
    }
    case _ => handler(s"what as range? ${toString}")
  }

  def to_IDiscreteRange(defInfo: DefInfo): Discrete_range = {
    val e = asRangeExp(defInfo)
    VHDL_dis_downto(e, e)
  }

  // [TN] Not sure this is a valid way to put get_init_val, should move to ISyntax
  def asVal: String = this match {
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
  def num2Exp(valType: VScalarType, rangeTy: VRangeV): IExp_baseTypeConstant = {
    val ss = s.substring(1, s.length - 1)
    require(ss.forall(_.isDigit), s"${s} not all digits")
    val iConstList = ss.toList.map(c => IConstS("val_c", s"(CHR ''${c}'')"))
    val vt = valType.vectorize
    if (rangeTy.rangeD == RangeD.to) {
      if (ss.forall(_ == ss(0))) {
        IExp_baseTypeConstant(vt, IConstL_gen(vt, ss.length.toString, ss(0)), ExpVectorKindTo)
      } else {
        IExp_baseTypeConstant(vt, IConstL_raw(vt, iConstList), ExpVectorKindTo)
      }
    } else if (rangeTy.rangeD == RangeD.downto) {
      if (ss.forall(_ == ss(0))) {
        IExp_baseTypeConstant(vt, IConstRL_gen(vt, ss.length.toString, ss(0)), ExpVectorKindDownTo)
      } else {
        IExp_baseTypeConstant(vt, IConstRL_raw(vt, iConstList), ExpVectorKindDownTo)
      }
    } else throw VIError
  }

  // FIXME this is quite wrong, but seems that we have to infer it late
  def num2Exp: IExp_baseTypeConstant = num2Exp(VScalarType(defaultCharType), VRangeV(unknownString, RangeD.to, unknownString))

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
                                       subtypeIndication: VSubtypeIndication) extends VArrayTypeDef

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
    val idList = getIdList(ctx.identifier_list())
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
  def getSimplExp: VSimpleExp = this match {
    case VChoiceE(simpleExp) => simpleExp
    case _ => ???
  }
}

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
    } else throw VIError
  }
}

case class VChoiceId(id: String) extends VChoice

case class VChoiceR(discreteRange: VDiscreteRange) extends VChoice

case class VChoiceE(simpleExp: VSimpleExp) extends VChoice

case object VChoiceOthers extends VChoice

case class VChoices(choiceList: List[VChoice])

object VChoices {
  def apply(ctx: ChoicesContext): VChoices = {
    val choiceList = ctx.choice().map(VChoice(_)).toList
    VChoices(choiceList)
  }
}

//********************************************************************************************************************//

case class VElemAssoc(choices: Option[VChoices], expr: VExp)

object VElemAssoc {
  def apply(ctx: Element_associationContext): VElemAssoc = {
    val choices = Option(ctx.choices()).map(VChoices(_))
    val vExp = VExp(ctx.expression())
    VElemAssoc(choices, vExp)
  }
}

case class VAggregate(elemAssocList: List[VElemAssoc]) {
  require(elemAssocList.nonEmpty, "elemAssocList")
  lazy val _getAssoc: List[(String, VExp)] = {
    for {
      elemAssoc <- elemAssocList
      choice <- elemAssoc.choices match {
        case Some(c) => c.choiceList
        case None => throw VIError
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
    val element_assocList = ctx.element_association().map(VElemAssoc(_)).toList
    VAggregate(element_assocList)
  }
}

sealed trait VQExp

case class VQExpA(subtypeInd: VSubtypeIndication, aggregate: VAggregate) extends VQExp

case class VQExpE(subtypeInd: VSubtypeIndication, exp: VExp) extends VQExp

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
    } else throw VIError
  }
}

sealed trait VAllocator

case class VallocatorE(qexp: VQExp) extends VAllocator

case class VAllocatorS(subtypeInd: VSubtypeIndication) extends VAllocator

object VAllocator {
  def apply(ctx: AllocatorContext): VAllocator = {
    val qualified_expression = ctx.qualified_expression()
    val subtype_indication = ctx.subtype_indication()
    if (qualified_expression != null) {
      VallocatorE(VQExp(qualified_expression))
    } else if (subtype_indication != null) {
      VAllocatorS(VSubtypeIndication(subtype_indication))
    } else throw VIError
  }
}

abstract class VPrimary {
  def toIExp(defInfo: DefInfo): IsabelleExpression = this match {
    case VPrimaryLiteral(literal) => literal.toIExp(defInfo)
    case VPrimaryQExp(vQExp) => handler(s"${toString}")
    case VPrimaryExpLR(vExp) => vExp.toIExp(defInfo)
    case VPrimaryAllocator(allocator) => handler(s"${toString}")
    case VPrimaryAggregate(aggregate) => {
      aggregate.getFirstMap._2.toIExp(defInfo)
    }
    case VPrimaryName(name) => name.toI_rhs(defInfo)
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
      VPrimaryName(VName(name))
    } else throw VIError
  }
}

case class VPrimaryLiteral(literal: VLiteral) extends VPrimary

case class VPrimaryQExp(qExp: VQExp) extends VPrimary

case class VPrimaryExpLR(exp: VExp) extends VPrimary

case class VPrimaryAllocator(allocator: VAllocator) extends VPrimary

case class VPrimaryAggregate(aggregate: VAggregate) extends VPrimary

case class VPrimaryName(name: VName) extends VPrimary

// [HC] Just a hack
case class VSuffix(s: String) {
  override def toString = s"${s}"
}


// [HC] Perhaps needing separation
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
    case None => handler(s"${toString}")
  }

  def toI_rhs(defInfo: DefInfo): IsabelleExpression

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


case class VSelectedName(id: String, suffixList: List[VSuffix]) extends VName {

  def extracted(extractor: String): String = {
    val nList = suffixList.scanLeft(id)((acc, cur) => s"${acc}_${cur}")
    nList.tail.foldLeft(nList.head)((acc, cur) => s"(${acc} ${extractor}''${cur}'')")
  }

  def isa_v = extracted("v.")

  def isa_sp = extracted("s.")

  override def toI_rhs(defInfo: DefInfo): IsabelleExpression = {
    //    logger.info(s"${toString}")
    val idef = defInfo.getDef(this)
    val expKind = idef.getExpKind
    idef match {
      case vl: Vl => IExp_vl_rhs(vl, this, expKind)
      case v: Variable => suffixList match {
        case Nil => IExp_variable(v, expKind)
        case _ => IExp_vl_rhs(v, this, expKind)
      }
      case spl: SPl => IExp_spl_rhs(spl, this, expKind)
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

case class VNameAttrPart(attrDesignator: VAttrDesignator, exprList: List[VExp])

object VNameAttrPart {
  def apply(ctx: Name_attribute_partContext): VNameAttrPart = {
    val attrDesignator = VAttrDesignator(ctx.attribute_designator())
    val exps = ctx.expression().map(VExp(_)).toList
    VNameAttrPart(attrDesignator, exps)
  }
}

case class VNameFnCallOrIndexPart(assocListOpt: Option[VAssocList]) {
  def getExp: VExp = assocListOpt match {
    case Some(al) => {
      val elem = al.assocElemList.head.actualPart
      val designator = elem match {
        case VActualPartD(d) => d
        case VActualPartN(sn, d) => d
      }
      designator match {
        case VActualDesignatorE(vExp) => vExp
        case VActualDesignatorO => ???
      }
    }
    case None => handler(s"${toString}")
  }

  def toI_rhs(defInfo: DefInfo): IsabelleExpression = {
    getExp.toIExp(defInfo)
  }
}

object VNameFnCallOrIndexPart {
  def apply(ctx: Name_function_call_or_indexed_partContext): VNameFnCallOrIndexPart = {
    val assocList = Option(ctx.actual_parameter_part()).map(al => VAssocList(al.association_list()))
    VNameFnCallOrIndexPart(assocList)
  }
}

case class VNameSlicePart(r1: VExplicitRange, r2: Option[VExplicitRange]) {
  // TODO r2
  def toI_lhs(defInfo: DefInfo): Discrete_range = {
    r1.toI(defInfo)
  }

  // TODO r2
  def toI_rhs(defInfo: DefInfo): (IsabelleExpression, IsabelleExpression) = r1.d match {
    case RangeD.`to` => (r1.r.toIExp(defInfo), r1.l.toIExp(defInfo))
    case RangeD.`downto` => (r1.l.toIExp(defInfo), r1.r.toIExp(defInfo))
    case RangeD.`unkown` => throw VIError
  }
}

object VNameSlicePart {
  def apply(ctx: Name_slice_partContext): VNameSlicePart = {
    val rangeList = ctx.explicit_range().map(VExplicitRange(_))
    VNameSlicePart(rangeList.head, rangeList.lift(1))
  }
}

sealed abstract class VNamePart {
  def toI_lhs(defInfo: DefInfo): (VSelectedName, Option[Discrete_range]) = this match {
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

  def toI_rhs(defInfo: DefInfo): IsabelleExpression = this match {
    case VNamePartAttr(selectedName, nameAttrPart) => ???
    case VNamePartFnI(selectedName, nameFnCallOrIndexPart) => {
      //      logger.info(s"${toString}")
      val iexp = selectedName.toI_rhs(defInfo)
      val nth = nameFnCallOrIndexPart.toI_rhs(defInfo)
      // always
      IExp_nth(iexp, nth)
    }
    case vNamePartSlice@VNamePartSlice(selectedName, nameSlicePart) => {
      val iexp = selectedName.toI_rhs(defInfo)
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
    val nameAttrPart = ctx.name_attribute_part()
    val nameFnCallOrIndexPart = ctx.name_function_call_or_indexed_part()
    val nameSlicePart = ctx.name_slice_part()
    if (nameAttrPart != null) {
      VNamePartAttr(selectedName, VNameAttrPart(nameAttrPart))
    } else if (nameFnCallOrIndexPart != null) {
      VNamePartFnI(selectedName, VNameFnCallOrIndexPart(nameFnCallOrIndexPart))
    } else if (nameSlicePart != null) {
      VNamePartSlice(selectedName, VNameSlicePart(nameSlicePart))
    } else throw VIError
  }
}

case class VNamePartAttr(selectedName: VSelectedName,
                         nameAttrPart: VNameAttrPart) extends VNamePart

case class VNamePartFnI(selectedName: VSelectedName,
                        nameFnCallOrIndexPart: VNameFnCallOrIndexPart) extends VNamePart {
}

case class VNamePartSlice(selectedName: VSelectedName,
                          nameSlicePart: VNameSlicePart) extends VNamePart


case class VNameParts(namePartList: List[VNamePart]) extends VName {
  require(namePartList.nonEmpty, "VNameParts")

  def toI_lhs(defInfo: DefInfo): (VSelectedName, Option[Discrete_range]) = {
    namePartList.head.toI_lhs(defInfo)
  }

  // TODO only deal with ONE
  override def toI_rhs(defInfo: DefInfo): IsabelleExpression = {
    namePartList.head.toI_rhs(defInfo)
  }
}

object VNameParts {
  //  Fxxk type erasure
  def gen(ctxList: List[Name_partContext]): VNameParts = {
    val namePartList = ctxList.map(np => VNamePart(np))
    VNameParts(namePartList)
  }
}

//********************************************************************************************************************//

sealed trait VAliasIndication

case class VSubtypeIndication(selectedName: VSelectedName,
                              constraint: Option[VConstraint],
                              tolerance: Option[VToleranceAspect]) extends VAliasIndication {
  def getRange: Option[VRangeV] = constraint.map(_.getRange)

  def getSimpleName = selectedName.getSimpleNameOpt.getOrElse(s"ERROR: ${toString}")
}

object VSubtypeIndication {
  def apply(ctx: Subtype_indicationContext): VSubtypeIndication = {
    val selectedName = selectedNameFromSubtypeInd(ctx)
    val constraint = Option(ctx.constraint()).map(VConstraint(_))
    val tolerance = Option(ctx.tolerance_aspect()).map(VToleranceAspect(_))
    VSubtypeIndication(selectedName, constraint, tolerance)
  }
}

case class VToleranceAspect(vExp: VExp)

object VToleranceAspect {
  def apply(ctx: Tolerance_aspectContext): VToleranceAspect = {
    val vExp = VExp(ctx.expression())
    VToleranceAspect(vExp)
  }
}

case class VSubnatureIndication(name: String,
                                indexConstraint: Option[VIndexConstraint],
                                exprs: Option[(VExp, VExp)]) extends VAliasIndication

sealed trait VRange {
  def getRV: VRangeV = this match {
    case VRangeE(explicitRange) => explicitRange.getRange
    case VRangeN(name: String) => defaultRangeV(s"VRangeN: ${this}")
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
    } else throw VIError
  }
}

case class VRangeE(explicitRange: VExplicitRange) extends VRange

case class VRangeN(name: String) extends VRange


//********************************************************************************************************************//

sealed trait VDiscreteRange {
  def getRange: VRangeV = this match {
    case VDiscreteRangeR(range) => range.getRV
    case VDiscreteRangeSub(subtypeInd) => subtypeInd.getRange match {
      case Some(range) => range
      case None => defaultRangeV(s"VDiscreteRangeSub ${subtypeInd}")
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
    } else throw VIError
  }
}

case class VDiscreteRangeR(range: VRange) extends VDiscreteRange

case class VDiscreteRangeSub(subtypeInd: VSubtypeIndication) extends VDiscreteRange

//********************************************************************************************************************//


sealed trait VConstraint {
  def getRange: VRangeV = this match {
    case VRangeConstraint(range) => range.getRV
    case VIndexConstraint(discreteRanges) => {
      val (h, t) = (discreteRanges.head, discreteRanges.tail)
      t match {
        case Nil => h.getRange
        case _ => defaultRangeV(s"VConstraint ${discreteRanges}")
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
    } else throw VIError
  }
}

case class VRangeConstraint(range: VRange) extends VConstraint

object VRangeConstraint {
  def apply(ctx: Range_constraintContext): VRangeConstraint = {
    val range = ctx.range()
    VRangeConstraint(VRange(range))
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

case class VExplicitRange(l: VSimpleExp, d: RangeD.Ty, r: VSimpleExp) {
  def getRange: VRangeV = VRangeV(l.asVal, d, r.asVal)

  def toI(defInfo: DefInfo): Discrete_range = d match {
    case RangeD.`to` =>
      VHDL_dis_to(l.toIExp(defInfo), r.toIExp(defInfo))
    case RangeD.`downto` => VHDL_dis_downto(l.toIExp(defInfo), r.toIExp(defInfo))
    case RangeD.`unkown` => handler(s"${toString}")
  }
}

object VExplicitRange {
  def apply(ctx: Explicit_rangeContext): VExplicitRange = {
    val simplExprList = for {
      simplExpr <- ctx.simple_expression()
    } yield VSimpleExp(simplExpr)
    val direction = RangeD.withName(ctx.direction().getText.toUpperCase)
    require(simplExprList.length == 2, "explicitRange")
    VExplicitRange(simplExprList.head, direction, simplExprList.last)
  }
}


//********************************************************************************************************************//

abstract class VFactor {
  def toIExp(defInfo: DefInfo): IsabelleExpression = this match {
    case VFFactor(primary, primaryOption) => {
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

case class VTerm(factor: VFactor, ops: List[VMultiplyingOperator.Ty], others: List[VFactor]) {
  def toIExp(defInfo: DefInfo): IsabelleExpression = {
    ops.zip(others).foldLeft(factor.toIExp(defInfo)) {
      case (accExp, (op, curFactor)) => IBinaryArithmeticFactorExpression(accExp, op, curFactor.toIExp(defInfo))
    }
  }

  //  FIXME [HC] asVal should not be used
  def asVal: String = {
    import VMultiplyingOperator._
    try {
      ops.zip(others).foldLeft(factor.asVal.toInt)((acc, cur) => {
        val f = cur._2.asVal.toInt
        cur._1 match {
          case `mul` => acc * f
          case `div` => acc / f
          case `mod` => acc % f
          case `rem` => ???
        }
      }).toString
    } catch {
      case nfe : NumberFormatException => {
        ops.zip(others).foldLeft(factor.asVal)((acc, cur) => {
          val f = cur._2.asVal
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
  type Ty = Value
  val abs = Value("[abs]")
  val not = Value("[not]")
  val neg = Value("[-:]")
  val pos = Value("[+:]")
}

//********************************************************************************************************************//
sealed trait VArithmeticOperator

object VAddingOperator extends Enumeration with VArithmeticOperator {
  // TODO VHDL overloads this, should make it homomorphic
  type Ty = Value
  val plus = Value("[+]")
  val minus = Value("[-]")
  val ampersand = Value("[&]")

  def apply(ctx: Adding_operatorContext): Ty = {
    if (ctx.PLUS() != null) plus
    else if (ctx.MINUS() != null) minus
    else if (ctx.AMPERSAND() != null) ampersand
    else throw VIError
  }

}

object VMultiplyingOperator extends Enumeration with VArithmeticOperator {
  type Ty = Value
  val mul = Value("[*]")
  val div = Value("[/]")
  val mod = Value("[mod]")
  val rem = Value("[rem]")

  def apply(ctx: Multiplying_operatorContext): Ty = {
    if (ctx.MUL() != null) mul
    else if (ctx.DIV() != null) div
    else if (ctx.MOD() != null) mod
    else if (ctx.REM() != null) rem
    else throw VIError
  }
}

object VDoubleStarOperator extends Enumeration with VArithmeticOperator {
  type Ty = Value
  val exp = Value("[**]")
}

//********************************************************************************************************************//

case class VSimpleExp(termSign: Option[String], term: VTerm, ops: List[VAddingOperator.Ty], others: List[VTerm]) {

  def refine__tl_trl(tKind: ExpKind, srcIExp: IsabelleExpression): IsabelleExpression = {
    require(tKind.isV && srcIExp.expKind == ExpScalarKind)
    tKind match {
      case ExpVectorKindTo => IExp_tl(srcIExp)
      case ExpVectorKindDownTo => IExp_trl(srcIExp)
      case _ => srcIExp
    }
  }

  def toIExp(defInfo: DefInfo): IsabelleExpression = {
    val firstExp: IsabelleExpression = termSign match {
      case Some("+") => IUnaryExpression(VUnaryOperator.pos, term.toIExp(defInfo))
      case Some("-") => IUnaryExpression(VUnaryOperator.neg, term.toIExp(defInfo))
      case _ => term.toIExp(defInfo)
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
          val refined = refine__valType(acc, cur)
          IBinaryArithmeticTermExpression(acc, op, refined)
        }
      }
    }
  }

  def asVal: String = {
    import VAddingOperator._
    val sign = termSign match {
      case Some("-") => "-"
      case _ => ""
    }
    try {
      ops.zip(others).foldLeft((sign + term.asVal).toInt)((acc, cur) => {
        val f = cur._2.asVal.toInt
        cur._1 match {
          case `plus` => acc + f
          case `minus` => acc - f
          case `ampersand` => throw VIError
        }
      }).toString
    } catch {
      case nfe : NumberFormatException => {
        // FIXME if sign is negative -> sign + term.asVal is syntax error in Isabelle
        ops.zip(others).foldLeft((sign + term.asVal))((acc, cur) => {
          val f = cur._2.asVal
          cur._1 match {
            case `plus` => s"(${acc} + ${f})"
            case `minus` => s"(${acc} - ${f})"
            case `ampersand` => throw VIError
          }
        }).toString
      }
      case e: Throwable => s"???simpleExp"
    }
  }
}

object VSimpleExp {
  def apply(ctx: Simple_expressionContext): VSimpleExp = {
    val terms = ctx.term().map(VTerm(_)).toList
    val ops = ctx.adding_operator().map(VAddingOperator(_)).toList
    val symbol = {
      if (ctx.PLUS() != null) Some("+")
      else if (ctx.MINUS() != null) Some("-")
      else None
    }
    VSimpleExp(symbol, terms.head, ops, terms.tail)
  }
}

//********************************************************************************************************************//

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
    else throw VIError
  }
}

//********************************************************************************************************************//

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
    else throw VIError
  }
}

//********************************************************************************************************************//

object VRelationOp extends Enumeration {
  type Ty = Value
  val eq = Value("[=]")
  val neq = Value("[/=]")
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
    else throw VIError
  }
}

case class VShiftExp(vSimpleExp: VSimpleExp, op: Option[VShiftOp.Ty], other: Option[VSimpleExp]) {
  def toIExp(defInfo: DefInfo): IsabelleExpression = (op, other) match {
    case (Some(opV), Some(simpleExpV)) => IBinaryShiftingExpression(vSimpleExp.toIExp(defInfo), opV, simpleExpV.toIExp(defInfo))
    case _ => vSimpleExp.toIExp(defInfo)
  }
}

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

case class VRelation(vShiftExpr: VShiftExp, op: Option[VRelationOp.Ty], other: Option[VShiftExp]) {
  // FIXME type refinement should be made here (perhaps ALL other toIExp)
  // TODO however we are not sure which should be trusted!!
  def toIExp(defInfo: DefInfo): IsabelleExpression = (op, other) match {
    case (Some(opV), Some(otherV)) => {
      val (lhs, rhs) = (vShiftExpr.toIExp(defInfo), otherV.toIExp(defInfo))
      val refinedRhs = refine__valType(lhs, rhs)
      IBinaryRelationalExpression(lhs, opV, refinedRhs)
    }
    case _ => vShiftExpr.toIExp(defInfo)
  }
}

object VRelation {
  def apply(ctx: RelationContext): VRelation = {
    val shift_expressionList = for {
      shift_expression <- ctx.shift_expression()
    } yield VShiftExp(shift_expression)
    val op = Option(ctx.relational_operator()).map(VRelationOp(_))
    val other = shift_expressionList.lift(1)
    VRelation(shift_expressionList.head, op, other)
  }
}

case class VExp(relation: VRelation, ops: List[VLogicOp.Ty], others: List[VRelation]) {
  def toIExp(defInfo: DefInfo): IsabelleExpression = {
    ops.zip(others).foldLeft(relation.toIExp(defInfo)) {
      case (acc, (op, curRelation)) => IBinaryLogicalExpression(acc, op, curRelation.toIExp(defInfo))
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

object VExp {
  def apply(ctx: ExpressionContext): VExp = {
    val relationList = ctx.relation().map(VRelation(_)).toList
    val logicalOps = ctx.logical_operator().map(VLogicOp(_)).toList
    VExp(relationList.head, logicalOps, relationList.tail)
  }
}
