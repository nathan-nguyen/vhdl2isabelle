package core.isabellesyntax

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Thanh Nam Nguyen on 22/10/16.
  */
object IdentifierMap {
  val iVariableMap = mutable.Map.empty[String, IVariable]
  val iFunctionMap = mutable.Map.empty[String, IFunction]
  val iProcedureMap = mutable.Map.empty[String, IProcedure]

  def getSubprogramComplexList : List[ISubprogramComplex] = {
    val subprogramComplexList = new ListBuffer[ISubprogramComplex]
    for (functionMap <- iFunctionMap) subprogramComplexList += functionMap._2
    for (procedureMap <- iProcedureMap) subprogramComplexList += procedureMap._2
    subprogramComplexList.toList
  }
}
