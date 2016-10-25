package core.isabellesyntax

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Thanh Nam Nguyen on 22/10/16.
  */
object IdentifierMap {
  val iFunctionMap = mutable.Map.empty[String, IFunction]
  val iProcedureMap = mutable.Map.empty[String, IProcedure]

  // TODO: Not an appropriate implementation
  var subprogramName = ""
  var isParsingSubprogram = false

  def startParsingSubprogram(subprogramName: String) = {
    isParsingSubprogram = true
    this.subprogramName = subprogramName
  }
  def finishParsingSubprogram = {
    isParsingSubprogram = false
  }

  def getSubprogramComplexList : List[ISubprogramComplex] = {
    val subprogramComplexList = new ListBuffer[ISubprogramComplex]
    for (functionMap <- iFunctionMap) subprogramComplexList += functionMap._2
    for (procedureMap <- iProcedureMap) subprogramComplexList += procedureMap._2
    subprogramComplexList.toList
  }

  def getSubprogramReturnType(name: String): IType.Value = {
    iFunctionMap.get(name) match {
      case Some(iFunction) => iFunction.returnType
      case None => IType.IEmptyType
    }
  }
}
