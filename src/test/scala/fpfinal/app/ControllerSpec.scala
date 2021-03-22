package fpfinal.app

import fpfinal.FpFinalSpec
import org.scalacheck.{Arbitrary, Gen}

class ControllerSpec extends FpFinalSpec {
  val controller: LiveController#Service = new LiveController {}.controller

  test("getCommandByNumber returns Some for a valid index") {
    val g: Gen[Int] = Gen.choose(0, controller.getAllCommands.length - 1)
    forAll(g) { (i: Int) =>
      assert(controller.getCommandByNumber(i).nonEmpty)
    }
  }

  test("getCommandByNumber returns None for invalid index") {
    val g: Gen[Int] = Arbitrary
      .arbitrary[Int]
      .suchThat(i => i < 0 || i >= controller.getAllCommands.length)
    forAll(g) { (i: Int) =>
      assert(controller.getCommandByNumber(i).isEmpty)
    }
  }
}
