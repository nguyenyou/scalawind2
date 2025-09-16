package www

import org.scalajs.dom
import com.raquo.laminar.api.L.*
import www.tw.*

case class App() {
  def apply(): HtmlElement = {
    div(
      tw.flex,
      h1("Hello World!")
    )
  }
}
