package core.isabellesyntax

import core._
import core.vhdlsyntax._

/**
  * Created by Hongxu Chen.
  */

/**
  * It's only used for idef generation
  */
final case class MetaData(itemId: String, valType: VTypeDefinition, initVal: IExpression)

//********************************************************************************************************************//

sealed trait V_IDef extends IDef

case class IVariable_old(id: String, valType: VBaseType, iExp: IExpression) extends V_IDef {
  override def toString = s"""(''${id}'', ${VHDLize(valType)}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"variable\" where
        |\"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[${id}]"

  override def getId = id

  override def getExpKind = iExp.expKind

}

sealed trait Ivl_old extends V_IDef {

  override def toString = this match {
    case Vl_v(iVariable) => s"(vl_v ${iVariable})"
    case Vnl(id, vlList) => s"(vnl ('''', ${vlList.ISABELLE}))"
  }

  def as_list: String = this match {
    case Vl_v(iVariable) => iVariable.id
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

  override def getId = this match {
    case Vl_v(v) => v.id
    case Vnl(id, vlList) => id
  }

  override def getExpKind: ExpKind = this match {
    case Vl_v(v) => v.getExpKind
    case Vnl(id, vlList) => ExpUnknownKind
  }


  // [HC] It may return a "variable" or a "vl" (which is vnl-generated)
  def get(nList: List[String]): Option[V_IDef] = {
    def aux(l: List[String], cur: Option[Ivl_old]): Option[Ivl_old] = l match {
      case h :: t => cur match {
        case Some(vl) => vl match {
          // vl_v contains
          case vl_v: Vl_v => Some(vl)
          // vnl not sure, recursive
          case vnl: Vnl => aux(t, vnl.vlList.find(_.getId == h))
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

case class Vl_v(iVariable: IVariable_old) extends Ivl_old


case class Vnl(id: String, vlList: List[Ivl_old]) extends Ivl_old

object Vnl {
  // FIXME: this gen is TOO specific!
  // NOTE: currently valType is the EXACTLY type from VHDL without prefix!!!
  def gen(id: String, dataList: List[MetaData])(implicit s: DummyImplicit): Vnl = {
    val vlList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      Vl_v(IVariable_old(itemId, data.valType.asInstanceOf[VBaseType], data.initVal))
    }
    Vnl(id, vlList)
  }
}

//********************************************************************************************************************//

sealed trait IDef {
  def as_definition: String

  def as_list: String

  // [HC] Only the "final" classes have the id
  def getId: IdTy

  def getVType: VTypeDefinition = this match {
    case v: IVariable_old => v.valType
    case s: Signal => s.valType
    case p: Port => p.valType
    case vl: Ivl_old => vl match {
      case vl_v: Vl_v => vl_v.iVariable.valType
      case vnl: Vnl => handler(s"${vnl}")
    }
    case spl: SPl => spl match {
      case spl_s: SPl_signal => spl_s.iSignal.valType
      case spl_p: Spl_port => spl_p.iPort.valType
      case spnl: Spnl => handler(s"${spnl}")
    }
  }

  def getExpKind: ExpKind
}

// [HC] Only a trait
sealed trait SP_IDef extends IDef

sealed abstract class SPl extends SP_IDef {

  override def toString = this match {
    case SPl_signal(s) => s"(spl_s ${s})"
    case Spl_port(p) => p match {
      case pbt : Port_baseType => s"(spl_p ${p})"
      case pct : Port_recordType => s"(spnl ${p})"
      case pat : Port_arrayType => s"(spl_p ${p})"
    }
    case spnl@Spnl(id, splList) => {
      if (spnl.isNestedList) s"${splList.ISABELLE}"
      else s"(spnl ('''', ${splList.ISABELLE}))"
    }
  }

  override def as_definition: String = this match {
    // [HC] : This case should never be called
    case SPl_signal(s) => s.as_definition
    // [HC] : This case should never be called
    case Spl_port(p) => p.as_definition
    case Spnl(id, splList) => {
      s"""definition ${id}:: \"spl\" where
          |\"${id} ≡ ${toString}\"""".stripMargin
    }
  }

  override def as_list = this match {
    case SPl_signal(s) => s.id
    case Spl_port(p) => p.id
    case Spnl(id, _) => s"(splist_of_spl ${id})"
  }

  override def getId = this match {
    case SPl_signal(s) => s.id
    case Spl_port(p) => p.id
    case Spnl(id, splList) => id
  }

  override def getExpKind = this match {
    case SPl_signal(s) => s.getExpKind
    case Spl_port(p) => p.getExpKind
    case Spnl(id, splList) => ExpUnknownKind
  }

  /**
    * return "signal", "port" or "spl" ("spnl" generated)
    */
  def get(nList: List[String]): Option[SP_IDef] = {
    def aux(l: List[String], cur: Option[SPl]): Option[SPl] = l match {
      case h :: t => cur match {
        case Some(spl) => spl match {
          case _: SPl_signal => Some(spl)
          case _: Spl_port => Some(spl)
          case spnl: Spnl => aux(t, spnl.splList.find(_.getId == h))
        }
        case None => None
      }
      case Nil => cur
    }
    aux(nList, Some(this)) match {
      case Some(SPl_signal(spl_s)) => Some(spl_s)
      case Some(Spl_port(spl_p)) => Some(spl_p)
      case other => other
    }
  }

}

case class SPl_signal(iSignal: Signal) extends SPl

case class Spl_port(iPort: Port) extends SPl

case class Spnl(id: String, splList: List[SPl]) extends SPl {
  var isNestedList : Boolean = false
}
// TODO: [HC] Rewrite to make it more general
object Spnl {
  def genFromSignal(id: String, dataList: List[MetaData], signalKind: SignalKind.Ty): Spnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      SPl_signal(Signal(itemId, data.valType.asInstanceOf[VBaseType], data.initVal, signalKind))
    }
    Spnl(id, splList)
  }

  def generateFromPort(id: String, dataList: List[MetaData], mode: PortMode.Ty, conn: PortConn.Ty, typeInfo : VTypeInfo): Spnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      data.valType match {
        case baseType : VBaseType => Spl_port(Port_baseType(itemId, baseType, data.initVal, mode, conn))
        case recordType : VRecordType => Spl_port(Port_recordType(itemId, recordType, data.initVal, mode, conn, typeInfo))
        case arrayType : VArrayType => Spl_port(Port_arrayType(itemId, arrayType, data.initVal, mode, conn, typeInfo))
        case subtype: VSubtype => handler(s"${data.valType}")
      }
    }
    Spnl(id, splList)
  }

}

//********************************************************************************************************************//

object SignalKind extends Enumeration {
  type Ty = Value
  val register, bus = Value
}

case class Signal(id: String, valType: VBaseType, iExp: IExpression, signalKind: SignalKind.Ty) extends SP_IDef {
  override def toString = s"""(''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"signal\" where
        |\"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[(sp_s ${id})]"

  override def getId = id

  override def getExpKind = iExp.expKind

}

object PortMode extends Enumeration {
  type Ty = Value
  val in = Value("mode_in")
  val out = Value("mode_out")
  val inout = Value("mode_inout")
  val linkage = Value("mode_linkage")
}

object PortConn extends Enumeration {
  type Ty = Value
  val connected, unconnected = Value
}

sealed abstract class Port extends SP_IDef {
  val id: String
  val valType : VTypeDefinition
  val iExp: IExpression

  def as_definition: String = {
    s"""definition ${id}:: \"port\" where
        |\"${id} ≡ ${toString}\"""".stripMargin
  }

  def getExpKind = iExp.expKind

  def as_list: String = s"[sp_p ${id}]"

  def getId = id
}

case class Port_baseType(id: String, valType: VBaseType, iExp: IExpression, mode: PortMode.Ty, conn: PortConn.Ty) extends Port {
  override def toString = s"(''${id}'', ${VHDLize(valType)}, ${mode}, ${conn}, ${iExp})"
}

sealed abstract class Port_customizedType extends Port

case class Port_recordType(id: String, valType: VRecordType, iExp: IExpression, mode: PortMode.Ty, conn: PortConn.Ty, typeInfo : VTypeInfo) extends Port_customizedType {
  override def toString = {
    val initVals = valType.guessInitVals(typeInfo)
    val spnlNestedList = Spnl.generateFromPort(id, initVals, mode, conn, typeInfo)
    spnlNestedList.isNestedList = true
    s"(''${id}'', ${spnlNestedList})"
  }
}

case class Port_arrayType(id: String, valType: VArrayType, iExp: IExpression, mode: PortMode.Ty, conn: PortConn.Ty, typeInfo : VTypeInfo) extends Port_customizedType {
  override def toString = s"(''${id}'', vhdl_array, ${mode}, ${conn}, ${iExp})"
}

//********************************************************************************************************************//

sealed abstract class SigPrt {
  override def toString = this match {
    case SP_s(s, sn) => sn.suffixList match {
      case Nil => s"(sp_s ${s.getId})"
      case _ => s"(sp_s ${sn.isa_sp})"
    }
    case SP_p(p, sn) => sn.suffixList match {
      case Nil => s"(sp_p ${p.getId})"
      case _ => s"(sp_p ${sn.isa_sp})"
    }
    // shouldn't be called
    case SP_spl(spl) => handler(s"${spl}")
  }
}

// "up cast: Signal=>SigPrt"
case class SP_s(iSignal: Signal, sn: VSelectedName) extends SigPrt

// "up cast: Port=>SigPrt"
case class SP_p(iPort: Port, sn: VSelectedName) extends SigPrt

// FAKE sp_of_spl
case class SP_spl(spl: SPl) extends SigPrt

//********************************************************************************************************************//
