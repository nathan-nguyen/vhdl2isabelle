package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Hongxu Chen.
  */


// [HC] It's only used for IDef generation
final case class MetaData(itemId: String, valType: VTypeDefinition, initVal: IExpression)

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
    case Vl_v(iVariable) => s"(vl_v ${iVariable})"
    case Vnl(id, vlList) => s"(vnl ('''', ${vlList.ISABELLE}))"
  }

  def as_list: String = this match {
    case Vl_v(iVariable) => iVariable.name
    case Vnl(id, _) => s"(vlist_of_vl ${id})"
  }

  def as_definition: String = this match {
    // "variable", this case should never be called
    case Vl_v(v) => v.as_definition
    // becomes "vl"
    case Vnl(id, _) => {
      s"""definition ${id}:: \"vl\" where
          |\"${id} ≡ ${toString}\"""".stripMargin
    }
  }

  override def getName = this match {
    case Vl_v(v) => v.name
    case Vnl(id, vlList) => id
  }

  override def getExpKind: ExpKind = this match {
    case Vl_v(v) => v.getExpKind
    case Vnl(id, vlList) => ExpUnknownKind
  }


  // [HC] It may return a "variable" or a "vl" (which is vnl-generated)
  def get(nList: List[String]): Option[V_IDef] = {
    def aux(l: List[String], cur: Option[IVl]): Option[IVl] = l match {
      case h :: t => cur match {
        case Some(vl) => vl match {
          // vl_v contains
          case vl_v: Vl_v => Some(vl)
          // vnl not sure, recursive
          case vnl: Vnl => aux(t, vnl.vlList.find(_.getName == h))
        }
        // no
        case None => None
      }
      // no more recursive
      case Nil => cur
    }
    aux(nList, Some(this)) match {
      case Some(Vl_v(v)) => Option(v)
      case other => other
    }
  }
}

case class Vl_v(iVariable: IVariable) extends IVl


case class Vnl(id: String, vlList: List[IVl]) extends IVl

object Vnl {
  // FIXME: this gen is TOO specific!
  // NOTE: currently valType is the EXACTLY type from VHDL without prefix!!!
  def gen(id: String, dataList: List[MetaData])(implicit s: DummyImplicit): Vnl = {
    val vlList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      Vl_v(IVariable(itemId, data.valType.asInstanceOf[VBaseType], data.initVal))
    }
    Vnl(id, vlList)
  }
}

//********************************************************************************************************************//

sealed trait IDef {
  def as_definition: String

  def as_list: String

  // [HC] Only the "final" classes have name
  def getName: String

  def getVType: VTypeDefinition = this match {
    case v: IVariable => v.valType
    case s: Signal => s.signalType
    case p: Port => p.portType
    case vl: IVl => vl match {
        case vl_v: Vl_v => vl_v.iVariable.valType
      case vnl: Vnl => handler(s"${vnl}")
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
    case ISpl_p(p) => p match {
      case pbt : Port_baseType => s"(spl_p ${p})"
      case pct : Port_recordType => s"(spnl ${p})"
      case pat : Port_arrayType => s"(spl_p ${p})"
    }
    case spnl@ISpl_Spnl(id, splList) => {
      if (spnl.isNested) s"${splList.ISABELLE}"
      else s"(spnl ('''', ${splList.ISABELLE}))"
    }
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

  /**
    * return "signal", "port" or "spl" ("spnl" generated)
    */
  def get(nList: List[String]): Option[SP_IDef] = {
    def aux(l: List[String], cur: Option[ISpl]): Option[ISpl] = l match {
      case h :: t => cur match {
        case Some(spl) => spl match {
          case _: ISPl_s => Some(spl)
          case _: ISpl_p => Some(spl)
          case spnl: ISpl_Spnl => aux(t, spnl.splList.find(_.getName == h))
        }
        case None => None
      }
      case Nil => cur
    }
    aux(nList, Some(this)) match {
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
      ISPl_s(Signal(itemId, data.valType.asInstanceOf[VBaseType], data.initVal, signalKind))
    }
    ISpl_Spnl(id, splList)
  }

  def apply(id: String, dataList: List[MetaData], portMode: PortMode.Value, portConnection: PortConnection.Value, typeInfo : VTypeInfo): ISpl_Spnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      data.valType match {
        case baseType : VBaseType => ISpl_p(Port_baseType(itemId, baseType, data.initVal, portMode, portConnection))
        case recordType : VRecordType => ISpl_p(Port_recordType(itemId, recordType, data.initVal, portMode, portConnection, typeInfo))
        case arrayType : VArrayType => ISpl_p(Port_arrayType(itemId, arrayType, data.initVal, portMode, portConnection, typeInfo))
        case subtype: VSubtype => handler(s"${data.valType}")
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
  val portType : VTypeDefinition
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

sealed abstract class Port_customizedType extends Port

case class Port_recordType(id: String, portType: VRecordType, iExp: IExpression, portMode: PortMode.Value, portConnection: PortConnection.Value, typeInfo : VTypeInfo) extends Port_customizedType {
  override def toString = {
    val initVals = portType.guessInitVals(typeInfo)
    val iSpnl_nested = ISpl_Spnl(id, initVals, portMode, portConnection, typeInfo)
    iSpnl_nested.isNested = true
    s"(''${id}'', ${iSpnl_nested})"
  }
}

case class Port_arrayType(id: String, portType: VArrayType, iExp: IExpression, portMode: PortMode.Value, portConnection: PortConnection.Value, typeInfo : VTypeInfo) extends Port_customizedType {
  override def toString = s"(''${id}'', ${IType(portType)}, ${portMode}, ${portConnection}, ${iExp})"
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
