import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, ParallelTestExecution}

package object core {

  abstract class BaseSpec extends FlatSpec with BeforeAndAfterAll with Matchers with ParallelTestExecution

}
