package core.isabellesyntax

import core.{DefInfo, Keeper}
import core.vhdlsyntax._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Thanh Nam Nguyen on 22/10/16.
  */
object IdentifierMap {
  val iFunctionMap = mutable.Map.empty[String, IFunction]
  val iProcedureMap = mutable.Map.empty[String, IProcedure]
  val vFunctionMap = mutable.Map.empty[String, VFunctionSpecification]

  var tListener: Keeper = _

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

//********************************************************************************************************************//

  val iArrayVariableMap = mutable.Map.empty[String, mutable.Map[String, Int]]
  def getArrayIndex(id: String, attribute: String): Int = iArrayVariableMap(id)(attribute)


//********************************************************************************************************************//

  var iTemporaryVariableIndex = 0
  val iTemporaryVariableMap = mutable.Map.empty[List[VActualPart], (String, String)]
  val iTemporaryVariableQueue = new ListBuffer[(List[VActualPart], String, String)]

  def updateITemporaryVariableValue(functionName: String, parameterList: List[VActualPart]){
    val iType = iFunctionMap(functionName).returnType
    val temporaryVariableName = s"return_${iType}_${iTemporaryVariableIndex}"
    generateTemporaryVariable(temporaryVariableName, functionName)
    iTemporaryVariableIndex += 1
    iTemporaryVariableMap += parameterList -> (functionName, temporaryVariableName)
    iTemporaryVariableQueue ++= List((parameterList , functionName, temporaryVariableName))
  }

  def getITemporaryVariableName(vActualPartList: List[VActualPart]): String = iTemporaryVariableMap(vActualPartList)._2

  def startNewITemporaryVariableMap(): Unit = {
    iTemporaryVariableIndex = 0;
    iTemporaryVariableMap.clear
    iTemporaryVariableQueue.clear
  }

  def generateTemporaryVariable(temporaryVariableName:String, functionName:String) : Unit = {
    // TODO: Check if the iTemporaryVariable is already defined
    // Or keep all the temporary variables
    try {
      tListener.defInfo.getDef(temporaryVariableName)
    } catch {
      case e: Exception => {
        val vSubtypeIndication = IdentifierMap.vFunctionMap(functionName).vSubtypeIndication
        tListener.generateIVariable(temporaryVariableName, None, vSubtypeIndication)
      }
    }
  }

  def generateTemporarySequentialStatement(defInfo: DefInfo): List[ISeq_stmt_complex_Ssc_fn] = {
    for {
      iTemporaryVariable <- iTemporaryVariableQueue.toList
      functionName = iTemporaryVariable._2
      temporaryVariableName = iTemporaryVariable._3
      iV_clhs = IV_clhs(defInfo.getVDef(VSelectedName(temporaryVariableName, Nil)), VSelectedName(temporaryVariableName, Nil), None)
      iSubproccall_complex = ISubproccall_complex(functionName, iTemporaryVariable._1, iFunctionMap(functionName).returnType)(defInfo: DefInfo)
      iSeq_stmt_complex_Ssc_fn = ISeq_stmt_complex_Ssc_fn("", iV_clhs, iSubproccall_complex)
    } yield iSeq_stmt_complex_Ssc_fn
  }

  def traverseVNamePartNameFunctionCallOrIndexedPart(vNamePartNameFunctionCallOrIndexedPart: VNamePartNameFunctionCallOrIndexedPart): Unit = {
    val functionName = vNamePartNameFunctionCallOrIndexedPart.vSelectedName.getSimpleName
    val vActualPartList = vNamePartNameFunctionCallOrIndexedPart.vNameFunctionCallOrIndexedPart.getVActualPartList

    for (vActualPart <- vActualPartList) {
      vActualPart match {
        case vActualPartNameAndDesignator: VActualPartNameAndDesignator => {
          traverseVActualPart(vActualPart)
          // Mark the inside temporary variable by double the size of vActualPartList
          // This helps to distinguish inside temporary variable with its container
          IdentifierMap.updateITemporaryVariableValue(vActualPartNameAndDesignator.vName.getSimpleName, List(vActualPart, vActualPart))
        }
        case _ => traverseVActualPart(vActualPart)
      }
    }
    IdentifierMap.updateITemporaryVariableValue(functionName, vActualPartList)
  }

  def traverseVActualPart(vActualPart: VActualPart): Unit = {
    val vNamePartNameFunctionCallOrIndexedPartsList = vActualPart.vActualDesignator.getVExpression.getVNamePartNameFunctionCallOrIndexedPartsList
    for (vNamePartNameFunctionCallOrIndexedPart <- vNamePartNameFunctionCallOrIndexedPartsList)
      traverseVNamePartNameFunctionCallOrIndexedPart(vNamePartNameFunctionCallOrIndexedPart)
  }

  def traverseVNamePartNameFunctionCallOrIndexedPartsList(vNamePartNameFunctionCallOrIndexedPartsList: List[VNamePartNameFunctionCallOrIndexedPart]): Unit ={
    for (vNamePartNameFunctionCallOrIndexedPart <- vNamePartNameFunctionCallOrIndexedPartsList)
      IdentifierMap.traverseVNamePartNameFunctionCallOrIndexedPart(vNamePartNameFunctionCallOrIndexedPart)
  }

  def traverseVExpressionList(vExpressionList: List[VExpression]): Unit ={
    val vNamePartNameFunctionCallOrIndexedPartsListBuffer = new ListBuffer[VNamePartNameFunctionCallOrIndexedPart]
    for (vExpression <- vExpressionList) {
      vNamePartNameFunctionCallOrIndexedPartsListBuffer ++= vExpression.getVNamePartNameFunctionCallOrIndexedPartsList
    }
    traverseVNamePartNameFunctionCallOrIndexedPartsList(vNamePartNameFunctionCallOrIndexedPartsListBuffer.toList)
  }

//********************************************************************************************************************//


}
