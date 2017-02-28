package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Thanh Nam Nguyen on 21/10/16.
  */
sealed abstract class IType {
}

object IType extends Enumeration{
  type IType = Value
  val IEmptyType = Value("emptype")
  val IInteger = Value("vhdl_integer")
  val IBoolean = Value("vhdl_boolean")
  val IStd_logic = Value("vhdl_std_logic")
  val IStd_ulogic = Value("vhdl_std_ulogic")
  val INatural = Value("vhdl_natural")
  val IArray = Value("vhdl_array")
  val IRecord = Value("vhdl_record")

  def apply(subtypeIndication: VSubtypeIndication): IType ={
    val typeDefinition = VVariableType(subtypeIndication.getSimpleName)
    IType(typeDefinition)
  }

  def apply(vVariableType : VVariableType): IType = {
    vVariableType match {
      case vScalarType : VScalarType => getScalarType(vVariableType.s)
      case vVectorType : VVectorType => IArray
      case vArrayType : VArrayType => IArray
      case vRecordType : VRecordType => IRecord
      case vSubType : VSubtype => IType(vSubType.getOriginalType(IdentifierMap.tListener.typeInfo))
      case _ => handler(s"${vVariableType}")
    }
  }

  def getScalarType(scalarType: String) : IType = {
    scalarType match {
      case "integer" => IInteger
      case "real" => ???
      case "character" => ???
      case "boolean" => IBoolean
      case "std_ulogic" => IStd_logic
      case "std_logic" => IStd_ulogic
      case "natural" => INatural
    }
  }

}