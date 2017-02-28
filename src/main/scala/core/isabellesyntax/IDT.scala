package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Hongxu Chen.
  */


// [HC] It's only used for IDef generation
final case class MetaData(itemId: String, vTypeDefinition: VVariableType, iExpression: IExpression)

//********************************************************************************************************************//

sealed trait V_IDef extends IDef

case class IVariable(name: String, valType: VBaseType, iExp: IExpression) extends V_IDef {
    override def toString = s"""(''${name}'', ${IType(valType)}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${name}:: \"variable\" where
        |\"${name} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[${name}]"

  override def getName = name

  override def getExpKind = iExp.expKind

}

sealed trait IVl extends V_IDef {

  override def toString = this match {
    case IVl_Vl_v(iVariable) => s"(vl_v ${iVariable})"
    case IVl_Vnl(id, vlList) => s"(vnl ('''', ${vlList.ISABELLE}))"
    case IVl_Vl_m(iVl,iExpression) => ???
  }

  def as_list: String = this match {
    case IVl_Vl_v(iVariable) => iVariable.name
    case IVl_Vnl(id, _) => s"(vlist_of_vl ${id})"
    case IVl_Vl_m(_,_) => ???
  }

  def as_definition: String = this match {
    // "variable", this case should never be called
    case IVl_Vl_v(v) => v.as_definition
    // becomes "vl"
    case IVl_Vnl(id, _) => {
      s"""definition ${id}:: \"vl\" where
          |\"${id} ≡ ${toString}\"""".stripMargin
    }
    case IVl_Vl_m(_,_) => ???
  }

  override def getName = this match {
    case IVl_Vl_v(v) => v.name
    case IVl_Vnl(id, vlList) => id
    case IVl_Vl_m(_,_) => ???
  }

  override def getExpKind: ExpKind = this match {
    case IVl_Vl_v(v) => v.getExpKind
    case IVl_Vnl(id, vlList) => ExpUnknownKind
    case IVl_Vl_m(_,_) => ???
  }


  // [HC] It may return a "variable" or a "vl" (which is vnl-generated)
  def get(subprogramName: String, nameList: List[String]): Option[V_IDef] = {
    def aux(list: List[String], iVlOption: Option[IVl]): Option[IVl] = list match {
      case h :: t => iVlOption match {
        case Some(vl) => vl match {
          // [HC] vl_v contains
          case vl_v: IVl_Vl_v => Some(vl)
          // [HC] vnl not sure, recursive
          case vnl: IVl_Vnl => aux(t, vnl.vlList.find(_.getName == s"${subprogramName}${h}"))
          case _ => ???
        }
        // [HC] No
        case None => None
      }
      // [HC] No more recursive
      case Nil => iVlOption
    }
    aux(nameList, Some(this)) match {
      case Some(IVl_Vl_v(v)) => Option(v)
      case other => other
    }
  }
}

case class IVl_Vl_v(iVariable: IVariable) extends IVl

case class IVl_Vnl(id: String, vlList: List[IVl]) extends IVl

case class IVl_Vl_m(iVl: IVl, iExpression: IExpression) extends IVl

object IVl_Vnl {
  def generate(id: String, dataList: List[MetaData])(typeInfo: VTypeInfo, defInfo: DefInfo): IVl_Vnl = {
    val iVlList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      // [TN] VRecordType and VArrayType should have vExpressionOption
      // Currently not able to initialize VRecordType and VArrayType with initial values
      data.vTypeDefinition match {
        case vBaseType: VBaseType => IVl_Vl_v(IVariable(itemId, data.vTypeDefinition.asInstanceOf[VBaseType], data.iExpression))
        case vRecordType: VRecordType => IVl_Vnl.generate(itemId, vRecordType.getInitialValue(typeInfo, None)(defInfo))(typeInfo, defInfo)
        case vArrayType: VArrayType => vArrayType.getInitialValue(itemId, typeInfo, None)(defInfo) match {
          case iVariable : IVariable => IVl_Vl_v(iVariable)
          case iVl_Vnl : IVl_Vnl => iVl_Vnl
          case _ => throw handler(s"${vArrayType}")
        }
        case _ => handler(s"${data.vTypeDefinition}")
      }
    }
    IVl_Vnl(id, iVlList)
  }
}

//********************************************************************************************************************//

sealed trait IDef {
  def as_definition: String

  def as_list: String

  // [HC] Only the "final" classes have name
  def getName: String

  def getVType: VVariableType = this match {
    case v: IVariable => v.valType
    case s: Signal => s.signalType
    case p: Port => p.portType
    case vl: IVl => vl match {
      case vl_v: IVl_Vl_v => vl_v.iVariable.valType
      case vnl: IVl_Vnl => handler(s"${vnl}")
      case _ => ???
    }
    case spl: ISpl => spl match {
      case spl_s: ISPl_s => spl_s.iSignal.signalType
      case spl_p: ISpl_p => spl_p.iPort.portType
      case spnl: ISpl_Spnl => handler(s"${spnl}")
    }
  }

  def getExpKind: ExpKind
}

// [HC] Only a trait
sealed trait SP_IDef extends IDef

sealed abstract class ISpl extends SP_IDef {

  override def toString = this match {
    case ISPl_s(s) => s"  spl_s ${s}"
    case ISpl_p(p) => s"(spl_p ${p})"
    case spnl@ISpl_Spnl(id, splList) => s"(spnl ('''', ${splList.ISABELLE}))"
  }

  override def as_definition: String = this match {
    // [HC] : This case should never be called
    case ISPl_s(s) => s.as_definition
    // [HC] : This case should never be called
    case ISpl_p(p) => p.as_definition
    case ISpl_Spnl(id, splList) => {
      s"""definition ${id}:: \"spl\" where
          |\"${id} ≡ ${toString}\"""".stripMargin
    }
  }

  override def as_list = this match {
    case ISPl_s(s) => s.id
    case ISpl_p(p) => p.id
    case ISpl_Spnl(id, _) => s"(splist_of_spl ${id})"
  }

  override def getName = this match {
    case ISPl_s(s) => s.id
    case ISpl_p(p) => p.id
    case ISpl_Spnl(id, splList) => id
  }

  override def getExpKind = this match {
    case ISPl_s(s) => s.getExpKind
    case ISpl_p(p) => p.getExpKind
    case ISpl_Spnl(id, splList) => ExpUnknownKind
  }


  // [HC] Return "signal", "port" or "spl" ("spnl" generated)
  def get(subprogramName: String, nameList: List[String]): Option[SP_IDef] = {
    def aux(list: List[String], iVlOption: Option[ISpl]): Option[ISpl] = list match {
      case h :: t => iVlOption match {
        case Some(spl) => spl match {
          case _: ISPl_s => Some(spl)
          case _: ISpl_p => Some(spl)
          case spnl: ISpl_Spnl => aux(t, spnl.splList.find(_.getName == s"${subprogramName}${h}"))
        }
        case None => None
      }
      case Nil => iVlOption
    }
    aux(nameList, Some(this)) match {
      case Some(ISPl_s(spl_s)) => Some(spl_s)
      case Some(ISpl_p(spl_p)) => Some(spl_p)
      case other => other
    }
  }
}

case class ISPl_s(iSignal: Signal) extends ISpl

case class ISpl_p(iPort: Port) extends ISpl

case class ISpl_Spnl(id: String, splList: List[ISpl]) extends ISpl {
  var isNested : Boolean = false
}

object ISpl_Spnl {
  def apply(id: String, dataList: List[MetaData], signalKind: SignalKind.Value): ISpl_Spnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      ISPl_s(Signal(itemId, data.vTypeDefinition.asInstanceOf[VBaseType], data.iExpression, signalKind))
    }
    ISpl_Spnl(id, splList)
  }

  def apply(id: String, dataList: List[MetaData], portMode: PortMode.Value, portConnection: PortConnection.Value)(typeInfo: VTypeInfo, defInfo: DefInfo): ISpl_Spnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      // [TN] VRecordType and VArrayType should have vExpressionOption
      // Currently not able to initialize VRecordType and VArrayType with initial values
      data.vTypeDefinition match {
        case baseType : VBaseType => ISpl_p(Port_baseType(itemId, baseType, data.iExpression, portMode, portConnection))
        case vRecordType: VRecordType => ISpl_Spnl(itemId, vRecordType.getInitialValue(typeInfo, None)(defInfo), portMode, portConnection)(typeInfo, defInfo)
        case arrayType : VArrayType => ISpl_p(Port_baseType(itemId, arrayType.getElementType(typeInfo).asInstanceOf[VBaseType], data.iExpression, portMode, portConnection))
        case subtype: VSubtype => handler(s"${data.vTypeDefinition}")
      }
    }
    ISpl_Spnl(id, splList)
  }

}

//********************************************************************************************************************//

object SignalKind extends Enumeration {
  type SignalKind = Value
  val register, bus = Value
}

case class Signal(id: String, signalType: VBaseType, iExp: IExpression, signalKind: SignalKind.Value) extends SP_IDef {
  override def toString = s"""(''${id}'', ${IType(signalType)}, ${signalKind}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"signal\" where
        |\"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[(sp_s ${id})]"

  override def getName = id

  override def getExpKind = iExp.expKind

}

object PortMode extends Enumeration {
  type PortMode = Value
  val in = Value("mode_in")
  val out = Value("mode_out")
  val inout = Value("mode_inout")
  val linkage = Value("mode_linkage")
}

object PortConnection extends Enumeration {
  type PortConnection = Value
  val connected, unconnected = Value
}

sealed abstract class Port extends SP_IDef {
  val id: String
  val portType : VVariableType
  val iExp: IExpression

  def as_definition: String = {
    s"""definition ${id}:: \"port\" where
        |\"${id} ≡ ${toString}\"""".stripMargin
  }

  def getExpKind = iExp.expKind

  def as_list: String = s"[sp_p ${id}]"

  def getName = id
}

case class Port_baseType(id: String, portType: VBaseType, iExp: IExpression, mode: PortMode.Value, conn: PortConnection.Value) extends Port {
  override def toString = s"(''${id}'', ${IType(portType)}, ${mode}, ${conn}, ${iExp})"
}

//********************************************************************************************************************//

sealed abstract class SigPrt {
  override def toString = this match {
    case SP_s(s, sn) => sn.suffixList match {
      case Nil => s"(sp_s ${s.getName})"
      case _ => s"(sp_s ${sn.isa_sp})"
    }
    case SP_p(p, sn) => sn.suffixList match {
      case Nil => s"(sp_p ${p.getName})"
      case _ => s"(sp_p ${sn.isa_sp})"
    }
    // shouldn't be called
    case SP_spl(spl) => handler(s"${spl}")
  }
}

// [HC] "up cast: Signal=>SigPrt"
case class SP_s(iSignal: Signal, sn: VSelectedName) extends SigPrt

// [HC] "up cast: Port=>SigPrt"
case class SP_p(iPort: Port, sn: VSelectedName) extends SigPrt

// [HC] Fake sp_of_spl
case class SP_spl(spl: ISpl) extends SigPrt

//********************************************************************************************************************//
