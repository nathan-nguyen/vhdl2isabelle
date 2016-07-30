package parsing

import scala.language.implicitConversions

case class IEnv_sp(ss: List[ISignalScalarDef], sl: List[ISignalListDef],
                   ps: List[IPortScalarDef], pl: List[IPortListDef]) {

  override def toString: String = {
    s"""${ss.map(_.as_sigprt).mkString("[", ",", "]")}
       |@${sl.map(_.as_sigprt).mkString("@")}
       |@${ps.map(_.as_sigprt).mkString("[", ",", "]")}
       |@${pl.map(_.as_sigprt).mkString("@")}""".stripMargin
  }
}

case class IEnv_v(vs: List[IVarScalarDef], vl: List[IVarListDef]) {
  override def toString: String = {
    s"""${vs.map(_.as_v).mkString("[", ",", "]")}
       |@${vl.map(_.as_vlist).mkString("@")}""".stripMargin
  }
}

// reserved as class
case class IEnv_t() {
  override def toString: String = "[]"
}


case class IEnv(env_sp: IEnv_sp, env_v: IEnv_v, env_t: IEnv_t) {
  def repr =
    s"""|(env_sp = [${env_sp}],
        | env_v = [${env_v}],
        | env_t = ${env_t})""".stripMargin
}

object IEnv {
  def apply(di: DefInfo): IEnv = {
    implicit def m2l[K, V <: IDef](m: scala.collection.mutable.Map[K, V]): List[V] = m.values.toList
    val env_sp = IEnv_sp(di.ss, di.sl, di.ps, di.pl)
    val env_v = IEnv_v(di.vs, di.vl)
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