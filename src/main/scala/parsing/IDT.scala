package parsing

import parsing.V2IUtils.VHDLize

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SP_clhs

case class Clhs_sp(sp_clhs: SP_lhs) extends SP_clhs

case class Clhs_spr(spl: SPl) extends SP_clhs

/////////////////////////////////////////////////////////////////////////////////

case class Sensitivity_list()

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Discrete_range

case class VHDL_dis_to(l: IExp, r: IExp) extends Discrete_range

case class VHDL_dis_downto(l: IExp, r: IExp) extends Discrete_range

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SP_lhs

case class Lhs_s(sigPrt: SigPrt) extends SP_lhs

case class Lhs_sa(sigPrt: SigPrt, discreteRange: Discrete_range) extends SP_lhs

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class V_lhs

case class Lhs_v(variable: IValue) extends V_lhs

case class LHs_va(variable: IValue, discreteRange: Discrete_range) extends V_lhs

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Asmt_rhs

case class Rhs_e(exp: IExp) extends Asmt_rhs

case class Rhs_o(exp: IExp, discreteRange: Discrete_range) extends Asmt_rhs

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Seq_stmt

case class Sst_sa(id: String, sP_lhs: SP_lhs, asmt_rhs: Asmt_rhs) extends Seq_stmt

case class Sst_va(id: String, v_lhs: V_lhs, asmt_rhs: Asmt_rhs) extends Seq_stmt

case class Sst_if(id: String, cond: IExp, then_seq_stmtList: List[Seq_stmt], else_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_l(id: String, cond: IExp, body_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_n(id: String, cond: IExp) extends Seq_stmt

case class Sst_e(id: String, cond: IExp) extends Seq_stmt

case object Sst_nl extends Seq_stmt


/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

case class IVariable(id: String, valType: String, iExp: IExp) extends IDef {
  override def toString = s"""(''${id}'', ${VHDLize(valType)}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[${id}]"
}

sealed abstract class Vl extends IDef {

  override def toString = this match {
    case Vl_v(iVariable) => s"(vl_v ${iVariable})"
    case Vnl(id, vlList) => s"(vnl ('''', ${vlList.mkString("[\n ", ",\n ", "\n]")}))"
  }

  def as_list: String = this match {
    case Vl_v(iVariable) => iVariable.id
    case Vnl(id, _) => s"(vlist_of_v ${id})"
  }

  def as_definition: String = this match {
    /// "variable"
    case Vl_v(v) => v.as_definition
    /// becomes "vl"
    case Vnl(id, _) => {
      s"""definition ${id}:: \"vl\" where
          |\"${id} ≡ ${toString}\"""".stripMargin
    }
  }

}

case class Vl_v(iVariable: IVariable) extends Vl

case class Vnl(id: String, vlList: List[Vl]) extends Vl

object Vnl {
  //  FIXME: this is TOO specific!
  // TODO: currently valType is the EXACTLY type from VHDL without prefix!!!
  def apply(id: String, dataList: List[IData])(implicit s: DummyImplicit): Vnl = {
    val vlList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      Vl_v(IVariable(itemId, data.valType, data.initVal))
    }
    Vnl(id, vlList)
  }
}


/////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////

trait IDef {
  def as_definition: String

  def as_list: String
}

sealed abstract class SPl extends IDef {

  override def toString = this match {
    case SPl_s(s) => s"(spl_s ${s})"
    case SPl_p(p) => s"(spl_p ${p})"
    case SPnl(id, splList) => s"(spnl ('''', ${splList.mkString("[\n ", ",\n ", "\n]")}))"
  }

  def as_definition: String = this match {
    case SPl_s(s) => s.as_definition
    case SPl_p(p) => p.as_definition
    case SPnl(id, splList) => {
      s"""definition ${id}:: \"spl\" where
          | \"${id} ≡ ${toString}\"""".stripMargin
    }
  }

  def as_list = this match {
    case SPl_s(s) => s.id
    case SPl_p(p) => p.id
    case SPnl(id, _) => s"(splist_of_spl ${id})"
  }
}

case class SPl_s(iSignal: Signal) extends SPl

case class SPl_p(iPortDef: Port) extends SPl

case class SPnl(id: String, splList: List[SPl]) extends SPl

object SPnl {
  def apply(id: String, dataList: List[IData], signalKind: SignalKind.Ty): SPnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      SPl_s(Signal(itemId, data.valType, data.initVal, signalKind))
    }
    SPnl(id, splList)
  }

  def apply(id: String, dataList: List[IData], mode: PortMode.Ty, conn: PortConn.Ty): SPnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      SPl_p(Port(itemId, data.valType, data.initVal, mode, conn))
    }
    SPnl(id, splList)
  }
}

//////////////////////////////////////////////////////////////////////////////////

object SignalKind extends Enumeration {
  type Ty = Value
  val register, bus = Value
}

case class Signal(id: String, valType: String, iExp: IExp, signalKind: SignalKind.Ty) extends IDef {
  override def toString = s"""(''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"signal\" where
        | \"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[${SP_s(this)}]"
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

case class Port(id: String, valType: String, iExp: IExp, mode: PortMode.Ty, conn: PortConn.Ty) extends IDef {
  override def toString = s"(''${id}'', ${VHDLize(valType)}, ${mode}, ${conn}, ${iExp})"

  override def as_definition: String = {
    s"""definition ${id}:: \"port\" where
        | \"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"${SP_p(this)}"
}

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SigPrt {
  override def toString = this match {
    case SP_s(s) => s"(sp_s ${s.id})"
    case SP_p(p) => s"(sp_p ${p.id})"
  }
}

// "up cast: Signal=>SigPrt"
case class SP_s(iSignal: Signal) extends SigPrt

// "up cast: Port=>SigPrt"
case class SP_p(iPort: Port) extends SigPrt

/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
