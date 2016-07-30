package parsing

import scala.language.implicitConversions

case class IEnv_sp(signalList: List[Signal], portList: List[Port], spnlList: List[SPnl]) {

  override def toString: String = {
    s"""${signalList.map(_.as_list).mkString("@")}
       |@${portList.map(_.as_list).mkString("@")}
       |@${spnlList.map(_.as_list).mkString("@")}""".stripMargin
  }
}

case class IEnv_v(variableList: List[IVariable], vnlList: List[Vnl]) {
  override def toString: String = {
    s"""${variableList.map(_.as_list).mkString("@")}
       |@${vnlList.map(_.as_list) mkString ("@")}""".stripMargin
  }
}

// reserved as class
case class IEnv_t() {
  override def toString: String = "[]"
}


case class IEnv(env_sp: IEnv_sp, env_v: IEnv_v, env_t: IEnv_t) {
  def repr =
    s"""|(env_sp = ${env_sp},
        | env_v = ${env_v},
        | env_t = ${env_t})""".stripMargin
}

object IEnv {
  def apply(di: DefInfo): IEnv = {
    implicit def toSecond[T](s: Seq[T]): List[T] = s.toList
    val env_sp = IEnv_sp(di.s_raw, di.p_raw, di.spnl_raw)
    val env_v = IEnv_v(di.v_raw, di.vnl_raw)
    val env_t = IEnv_t()
    new IEnv(env_sp, env_v, env_t)
  }
}

// reserved as class
case class IResFn() {
  def repr = "λx.(None)"
}

case class IConcStatComplex() {
  def repr = "[]"
}

// it is an IDef however not treated so
case class IEntity(id: String, env: IEnv, resFn: IResFn, complex: IConcStatComplex) {
  def repr =
    s"""definition ${id}:: \"vhdl_desc_complex\" where
        |"${id} ≡
        |  let env = ${env.repr};
        |      resfn = ${resFn.repr};
        |      cst_list = ${complex.repr}
        |  in (env, resfn, cst_list)
        |"
     """.stripMargin
}