package core.isabellesyntax

import core._
import core.vhdlsyntax._

import scala.collection.mutable.ListBuffer

/**
  * Created by Thanh Nam Nguyen on 21/10/16.
  */

// In Isabelle: type_synonym parameter = "(v_clhs × direction × type)"
case class IParameter (iv_clhs: IV_clhs, direction: IDirection.Value, parameterType: IType.Value) {
  override def toString = s"(${iv_clhs}, ${direction}, ${parameterType})"
}

object IParameter {
  def apply(parameterName: String, iDirection: IDirection.Value, subtypeIndication: VSubtypeIndication)(defInfo: DefInfo): IParameter = {
    val parameterType = IType(subtypeIndication)
    val iV_clhs = IV_clhs(defInfo.getVDef(VSelectedName(parameterName, Nil)), VSelectedName(parameterName, Nil), None)
    IParameter(iV_clhs, iDirection, parameterType)
  }
}

//********************************************************************************************************************//

object IDirection extends Enumeration {
  type IDirection = Value
  val IDirectionIn = Value("dir_in")
  val IDirectionOut = Value("dir_out")
  val IDirectionInOut = Value("dir_inout")

  def apply(signalMode: VSignalMode.Value): IDirection.Value = signalMode match {
    case VSignalMode.IN => IDirectionIn
    case VSignalMode.OUT => IDirectionOut
    case VSignalMode.INOUT => IDirectionInOut
    case _ => handler(s"${signalMode}")
  }
}

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
  val iSeq_stmt_complexList : List[ISeq_stmt_complex]
  val returnType : IType.Value
  val localVariableList : List[ILocalVariable]

  override def toString = s"(\n\t\t''${name}'', ${designator}, ${parameterList.mkString("[", "]@[", "]")}, ${iSeq_stmt_complexList.mkString("[\n\t\t\t", "]@[\n\t\t\t", "]")}, ${returnType}, ${localVariableList.mkString("[", "]@[", "]")})"
}

object ISubprogramComplex {
  def apply(vSubprogramBody: VSubprogramBody)(defInfo: DefInfo): ISubprogramComplex = {
    val vSubprogramSpecification = vSubprogramBody.subprogramSpecification
    val name = vSubprogramBody.getDesignator.id
    val parameterList = new ListBuffer[IParameter]()

    val interfaceElementList = vSubprogramSpecification.getInterfaceElementList()
    for (interfaceElement <- interfaceElementList){
      interfaceElement match {
        case interfaceConstantDeclaration : VInterfaceConstantDeclaration => for (id <- interfaceConstantDeclaration.idList) parameterList += IParameter(s"${name}_${id}", IDirection.IDirectionIn, interfaceConstantDeclaration.vSubtypeIndication)(defInfo)
        case interfaceVariableDeclaration : VInterfaceVariableDeclaration => for (id <- interfaceVariableDeclaration.idList) parameterList += IParameter(s"${name}_${id}", IDirection(interfaceVariableDeclaration.signalMode), interfaceVariableDeclaration.vSubtypeIndication)(defInfo)
        case _ => ???
      }
    }
    IdentifierMap.startParsingSubprogram(name)
    val iSeq_stmt_complexList = ISeq_stmt_complex(vSubprogramBody.vSubprogramStatementPart.vSequentialStatementList)(defInfo)
    IdentifierMap.finishParsingSubprogram

    vSubprogramSpecification match {
      case vFunctionSpecification: VFunctionSpecification => {
        val iFunction = IFunction(name, parameterList.toList, iSeq_stmt_complexList, IType(vFunctionSpecification.vSubtypeIndication), List.empty)
        IdentifierMap.iFunctionMap += name -> iFunction
        IdentifierMap.vFunctionMap += name -> vFunctionSpecification
        iFunction
      }
      case vProcedureSpecification: VProcedureSpecification => {
        val iProcedure = IProcedure(name, parameterList.toList, iSeq_stmt_complexList, List.empty)
        IdentifierMap.iProcedureMap += name -> iProcedure
        iProcedure
      }
    }
  }
}

case class IFunction(name: String, parameterList : List[IParameter], iSeq_stmt_complexList : List[ISeq_stmt_complex], returnType: IType.Value, localVariableList : List[ILocalVariable]) extends ISubprogramComplex {
  val designator = IDesignator.IFunctionDesignator
}

case class IProcedure(name: String, parameterList : List[IParameter], iSeq_stmt_complexList : List[ISeq_stmt_complex], localVariableList : List[ILocalVariable]) extends ISubprogramComplex {
  val designator = IDesignator.IProcedureDesignator
  val returnType = IType.IEmptyType
}