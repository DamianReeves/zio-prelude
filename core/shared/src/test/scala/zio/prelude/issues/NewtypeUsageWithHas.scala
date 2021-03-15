package zio.prelude.issues
import zio.prelude._
import zio.test._
import zio.{Has, Tag}

object NewtypeUsageWithHas extends DefaultRunnableSpec {
  def spec = suite("NewtypeUsageWithHas Spec")(
    test("It should be possible to combine 2 Data instances") {
      val stringData: Data[String] = Data("Server")
      val portData: Data[Port]     = Data(Port(80))
      val combined                 = stringData ++ (portData)
      val actualPort: Port         = combined.getData[Port]
      val actualString             = combined.getData[String]
      assert(actualPort.value)(equalTo(80)) && assert(actualString)(equalTo("Server"))
    } /*,
    test("It should be possible to combine a Has and a Data") {
      val dataString                              = Data("Data String")
      val hasString                               = Has("Has String")
      val combined: Data[String] with Has[String] = dataString ++ hasString
      assert(combined.get[String])(equalTo("Data-String"))
    }*/
  )
  final case class Port(value: Int)

  implicit final class DataSyntax[Self <: Data[_]](private val self: Self) extends AnyVal {
    def getData[B](implicit ev: Self <:< Data[B], tagged: Tag[B]): B =
      self.get[DataValue[B]].value
  }

  object Data extends SubtypeSmartF[HasData](DataAssertion.anything) {
    def apply[A](value: A)(implicit ev: Tag[A]): Data[A] = super.apply(Has(DataValue(value)))

  }
  type Data[A] = Data.Type[A]

  final case class DataValue[A](value: A)
  type HasData[A] = Has[DataValue[A]]

  object DataAssertion {
    val anything = new AssertionF[HasData] {
      def apply[A]: Assertion[Has[DataValue[A]]] = Assertion.assertion("anything")()(_ => true)
    }
  }
}
