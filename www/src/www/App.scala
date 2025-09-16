package www

import org.scalajs.dom
import com.raquo.laminar.api.L.*

case class App() {
  def apply(): HtmlElement = {
    div(
      h1("Hello World!")
    )
  }
}
