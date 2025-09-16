package www.tw

import com.raquo.laminar.api.{L, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement

import scala.language.implicitConversions
import scala.quoted.*

extension (inline tailwind: Tailwind) {

  inline def toHtmlMod: L.HtmlMod =
    ${ laminarTailwindImpl('tailwind) }

  inline def toSvgMod: L.SvgMod =
    ${ laminarSvgTailwindImpl('tailwind) }

  inline def <--(inline boolSignal: L.Signal[Boolean]): L.HtmlMod =
    ${ boolSignalClsImpl('tailwind, 'boolSignal) }

  inline def :=(inline bool: Boolean): L.HtmlMod =
    ${ boolClsImpl('tailwind, 'bool) }

}

def boolSignalClsImpl(
    tailwindExpr: Expr[Tailwind],
    boolSignal: Expr[L.Signal[Boolean]]
)(using
    Quotes
): Expr[L.HtmlMod] = {
  val value = builderImpl(tailwindExpr).valueOrAbort
  '{ L.cls(${ Expr(value) }) <-- ${ boolSignal } }
}

def boolClsImpl(
    tailwindExpr: Expr[Tailwind],
    bool: Expr[Boolean]
)(using
    Quotes
): Expr[L.HtmlMod] = {
  val value = builderImpl(tailwindExpr).valueOrAbort
  '{ L.cls(${ Expr(value) }) := ${ bool } }
}

implicit inline def laminarTailwind(inline tailwind: Tailwind): L.HtmlMod = {
  ${ laminarTailwindImpl('tailwind) }
}

def laminarTailwindImpl(
    tailwindExpr: Expr[Tailwind]
)(using
    Quotes
): Expr[L.HtmlMod] = {
  val value = builderImpl(tailwindExpr).valueOrAbort
  '{ L.cls := ${ Expr(value) } }
}

implicit inline def laminarSvgTailwind(inline tailwind: Tailwind): L.SvgMod = {
  ${ laminarSvgTailwindImpl('tailwind) }
}

def laminarSvgTailwindImpl(
    tailwindExpr: Expr[Tailwind]
)(using
    Quotes
): Expr[L.SvgMod] = {
  val value = builderImpl(tailwindExpr).valueOrAbort
  '{ L.svg.className := ${ Expr(value) } }
}

final class SignalOfBooleanOps(underlying: L.Signal[Boolean]) {

  @inline def cls(whenTrue: => String = "", whenFalse: => String = ""): L.Binder[ReactiveHtmlElement.Base] =
    L.cls <-- underlying.map { bool =>
      Seq(
        whenTrue  -> bool,
        whenFalse -> !bool
      )
    }

  @inline def unary_! : L.Signal[Boolean] = underlying.map(!_)

  @inline def ||(r: Boolean): L.Signal[Boolean] = underlying.map { l =>
    l || r
  }

  @inline def ||(that: L.Signal[Boolean]): L.Signal[Boolean] =
    underlying.combineWithFn(that)(_ || _)

  @inline def &&(r: Boolean): L.Signal[Boolean] = underlying.map { l =>
    l && r
  }

  @inline def &&(that: L.Signal[Boolean]): L.Signal[Boolean] =
    underlying.combineWithFn(that)(_ && _)

  @inline def switch[T](whenTrue: => T, whenFalse: => T): L.Signal[T] =
    underlying.map(if (_) whenTrue else whenFalse)

}

implicit def syntaxSignalOfBoolean(s: L.Signal[Boolean]): SignalOfBooleanOps =
  new SignalOfBooleanOps(s)

final class BooleanOps(underlying: Boolean) {

  @inline def cls(whenTrue: => String = "", whenFalse: => String = ""): L.HtmlMod =
    L.cls := Seq(
      whenTrue  -> underlying,
      whenFalse -> !underlying
    )

  @inline def not: Boolean = !underlying

}

implicit def syntaxOfBoolean(s: Boolean): BooleanOps =
  new BooleanOps(s)

def whenSignal(condition: L.Signal[Boolean])(mods: L.Node*): L.DynamicInserter = {
  L.children <-- condition.map {
    if (_) {
      mods
    } else {
      Seq(L.emptyNode)
    }
  }
}
