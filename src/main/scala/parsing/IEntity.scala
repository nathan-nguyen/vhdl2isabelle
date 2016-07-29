package parsing

case class IEnv_sp(signalList: List[ISignalScalarDef])

case class IEnv() {
  def repr =
    """
      |(env_sp = [xxx],
      | env_v = [xxx],
      | env_t = [])""".stripMargin.stripLineEnd
}

class IResFn {
  def repr = "λx.(None)"
}

case class IConcStatComplex() {
  def repr = "[]"
}

// it is an IDef however not treated soe
case class IEntity(id:String, env: IEnv, resFn: IResFn, complex: IConcStatComplex) {
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