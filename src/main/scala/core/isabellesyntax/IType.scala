package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Thanh Nam Nguyen on 21/10/16.
  */
sealed abstract class IType {

}

object IType {
  def apply(subtypeIndication: VSubtypeIndication): IType ={
    val typeDefinition = VTypeDefinition(subtypeIndication.getSimpleName)
    typeDefinition match {
      case scalaType : VScalarType => getScalarType(subtypeIndication.getSimpleName)
      case vectorType : VVectorType => ???
      case customizedType : VCustomizedType => ???
    }
  }

  def getScalarType(scalarType: String) : IType = {
    scalarType match {
      case "integer" => IInteger()
      case "real" => ???
      case "character" => ???
      case "boolean" => ???
      case "std_ulogic" => ???
      case "std_logic" => ???
    }
  }
}

case class IInteger() extends IType{
  override def toString = "vhdl_integer"
}

case object IEmptyType extends IType {
  override def toString = "emptype"
}