package www

import www.Main
import www.tw.*

import utest.*

object FooTests extends TestSuite {
  def tests = Tests {
    test("flex") {
      val result = tw.flex.css
      assert(result == "flex")
      result
    }
    test("flex better") {
      val result = tw.flex.css
      assert(result == "flex")
      result
    }
  }
}
