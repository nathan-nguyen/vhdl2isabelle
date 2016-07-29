package parsing

class EntitySpec extends BaseSpec {
  "entity" should "transfer" in {
    val env = IEnv()
    val resFn = new IResFn
    val cstList = IConcStatComplex()
    val entity = IEntity("div32", env, resFn, cstList)
    println(entity.repr)
  }

}
