package www

import www.Main
import www.tw.*

import utest.*

object FooTests extends TestSuite {
  def tests = Tests {
    test("hello") {
      val result = tw.flex.css
      assert(result == "flex")
      result
    }
  }
}
