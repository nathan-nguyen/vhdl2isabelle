package parsing

import sg.edu.ntu.hchen.VHDLParser._

import scala.collection.JavaConversions._

object Antlr2VTy {

  def getIdList(ctx: Identifier_listContext) = {
    for {
      id <- ctx.identifier()
    } yield id.getText
  }

  def selectedNameFromSubtypeInd(ctx: Subtype_indicationContext) = {
    val names = for {
      name <- ctx.selected_name()
    } yield name.getText
    names.head
  }



}
