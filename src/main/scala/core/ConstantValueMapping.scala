package core

import scala.collection.mutable

/**
  * Created by Thanh Nam Nguyen on 11/10/16.
  *
  * This class is used to store the value of VHDL constants
  * These values could be used in type declaration that does not allow variable as size
  */
object ConstantValueMapping {

  val integer_constant_mapping = mutable.Map.empty[String, String];

  def getIntegerConstant(id : String, scalaType : VScalarType, expOption: Option[VExp], defInfo : DefInfo): Unit ={
    if (!scalaType.s.equals("integer")) return

    if (!expOption.isEmpty){
      (expOption.get.toIExp(defInfo)) match {
        case iexp_con : IExp_con => {
          iexp_con.const match{
            case IConstS(isaType, initVal) => integer_constant_mapping += (id -> initVal)
            case _ => ???
          }
        }
        case _ => ???
      }
    }
  }
}
