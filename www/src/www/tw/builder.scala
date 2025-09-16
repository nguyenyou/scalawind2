package www.tw

import scala.quoted.*
import scala.language.implicitConversions

extension (inline tailwind: Tailwind) {

  inline def css: String =
    ${ builderImpl('tailwind) }

}

implicit inline def builder(inline tailwind: Tailwind): String =
  ${ builderImpl('tailwind) }

def convertCamelCaseToSnakeCase(methodName: String): String = {
  val units = List(
    "px",
    "pc",
    "vh",
    "tx"
  )

  val step1 = "[A-Z]".r.replaceAllIn(
    methodName,
    m => s"_${m.group(0).toLowerCase}"
  )

  val step2 = "([a-z]+)([0-9]+)".r.replaceAllIn(
    step1,
    m => {
      val p1 = m.group(1)
      val p2 = m.group(2)
      if (units.contains(p1)) {
        m.group(0)
      } else {
        s"${p1}_${p2}"
      }
    }
  )

  val step3 = "_([a-z]+)([0-9]+)".r.replaceAllIn(
    step2,
    m => {
      val p1 = m.group(1)
      val p2 = m.group(2)
      if (units.contains(p1)) {
        m.group(0)
      } else {
        s"_${p1}_${p2}"
      }
    }
  )

  step3.toLowerCase
}

def toSnakeCase(methodName: String): String = {
  if (methodName.matches("^px[0-9]+$")) {
    s"px_${methodName.substring(2)}"
  } else {
    convertCamelCaseToSnakeCase(methodName)
  }
}

def methodNameToTailwindClass(methodName: String) = {
  toSnakeCase(methodName).replace("_", "-")
}

def builderImpl(
    tailwindExpr: Expr[Tailwind]
)(using
    Quotes
): Expr[String] = {
  import quotes.reflect.*
  def validate(classes: List[String]): Unit = {
    checkDuplication(classes)
    val optimizationSuggestions = checkOptimization(classes)
    if (optimizationSuggestions.nonEmpty)
      report.errorAndAbort(s"[Optimization] ${optimizationSuggestions.mkString(", ")}")
  }

  def checkDuplication(classes: List[String]): Unit = {
    val duplicates = classes.groupBy(identity).collect {
      case (
            x,
            List(
              _,
              _,
              _*
            )
          ) =>
        x
    }
    if (duplicates.nonEmpty) report.errorAndAbort(s"[Duplication] ${duplicates.mkString(", ")}")
  }

  def checkOptimization(classes: List[String]): List[String] = {
    val properties = List("p", "m")

    val suggestions = properties.flatMap { property =>
      val propertySuggestions = scala.collection.mutable.ListBuffer.empty[String]

      val classMap = classes
        .map {
          case c if c.startsWith(s"${property}t-") => s"${property}t" -> c
          case c if c.startsWith(s"${property}b-") => s"${property}b" -> c
          case c if c.startsWith(s"${property}l-") => s"${property}l" -> c
          case c if c.startsWith(s"${property}r-") => s"${property}r" -> c
          case c if c.startsWith(s"${property}x-") => s"${property}x" -> c
          case c if c.startsWith(s"${property}y-") => s"${property}y" -> c
          case c if c.startsWith(s"${property}-")  => property        -> c
          case c                                   => c               -> c
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap

      def checkAndSuggest(key1: String, key2: String, combined: String): Unit = {
        (classMap.get(key1), classMap.get(key2)) match {
          case (Some(List(c1)), Some(List(c2))) if c1.substring(3) == c2.substring(3) =>
            propertySuggestions += s"Use $combined${c1.substring(3)} instead of $c1 and $c2"
          case _ => ()
        }
      }

      def checkFourWay(): Unit = {
        (
          classMap.get(s"${property}t"),
          classMap.get(s"${property}b"),
          classMap.get(s"${property}l"),
          classMap.get(s"${property}r")
        ) match {
          case (Some(List(pt)), Some(List(pb)), Some(List(pl)), Some(List(pr)))
              if pt.substring(3) == pb.substring(3) && pl.substring(3) == pr.substring(3) && pt.substring(3) == pl
                .substring(3) =>
            propertySuggestions += s"Use ${property}-${pt.substring(3)} instead of $pt, $pb, $pl and $pr"
          case _ => ()
        }
      }

      def checkPxPy(): Unit = {
        (classMap.get(s"${property}x"), classMap.get(s"${property}y")) match {
          case (Some(List(px)), Some(List(py))) if px.substring(3) == py.substring(3) =>
            propertySuggestions += s"Use ${property}-${px.substring(3)} instead of $px and $py"
          case _ => ()
        }
      }

      // Check for four-way combination first
      checkFourWay()

      // Only check for two-way combinations if no four-way combination is found
      if (propertySuggestions.isEmpty) {
        checkPxPy()
        if (propertySuggestions.isEmpty) {
          checkAndSuggest(
            s"${property}t",
            s"${property}b",
            s"${property}y-"
          )
          checkAndSuggest(
            s"${property}l",
            s"${property}r",
            s"${property}x-"
          )
        }
      }

      propertySuggestions.toList
    }

    suggestions
  }

  def extractClassNames(term: Term, prefix: String = "", important: Boolean = false): List[String] = {
    var stack      = List((term, prefix, important))
    var classNames = List.empty[String]

    while (stack.nonEmpty) {
      stack.headOption.foreach { (currentTerm, currentPrefix, currentImportant) =>
        stack = stack.drop(1)
        currentTerm match {
          case Apply(Select(inner, "important"), List(styles)) =>
            stack = (styles, currentPrefix, true) :: stack
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Inlined(
                _,
                _,
                inner
              ) =>
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Select(inner, name) =>
            val methodName = methodNameToTailwindClass(name)
            val className  = s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}"
            classNames = classNames :+ className
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Ident("tw") =>
          // No action needed, just continue processing the remaining stack
          case Apply(Ident(name), List(arg)) =>
            val methodName = methodNameToTailwindClass(name)
            val className  = s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}"
            classNames = classNames :+ className
            stack = (arg, currentPrefix, currentImportant) :: stack
          case Apply(Select(inner, name), List(Literal(StringConstant(value)))) if name == "raw" =>
            val className = s"$currentPrefix${if (currentImportant) "!" else ""}$value"
            classNames = classNames :+ className
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Apply(Select(inner, name), List(Literal(StringConstant(opacity)))) if name.endsWith("$") =>
            val methodName = methodNameToTailwindClass(name.stripSuffix("$"))
            val className  = s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}/${opacity}"
            classNames = classNames :+ className
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Apply(Select(inner, name), List(Literal(StringConstant(value)))) =>
            val methodName = methodNameToTailwindClass(name)
            val className  = s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}[$value]"
            classNames = classNames :+ className
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Apply(Apply(Ident(name), args), List(Literal(StringConstant(value)))) =>
            val methodName = methodNameToTailwindClass(name)
            val className  = s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}[$value]"
            classNames = classNames :+ className
            stack = args.map(arg => (arg, currentPrefix, currentImportant)) ++ stack
          case Apply(Select(Ident("tw"), name), List(inner)) =>
            val methodName = methodNameToTailwindClass(name)
            stack = (inner, s"$currentPrefix${methodName}:", currentImportant) :: stack
          case Apply(Select(inner, "variant"), List(Literal(StringConstant(selector)), styles)) =>
            val variantPrefix = s"$currentPrefix[$selector]:" // Use the selector as provided
            val styleClasses = extractClassNames(
              styles,
              variantPrefix,
              currentImportant
            ) // Extract classes with the variant prefix
            classNames = classNames ++ styleClasses
            stack = (inner, currentPrefix, currentImportant) :: stack
          case Apply(Select(inner, name), args) =>
            val methodName   = methodNameToTailwindClass(name)
            val innerClasses = args.flatMap(arg => extractClassNames(arg, s"$currentPrefix${methodName}:"))
            classNames = classNames ++ innerClasses
            stack = (inner, currentPrefix, currentImportant) :: stack
          case unexpectedTerm =>
            report.errorAndAbort(
              s"[Tailwind] Ops! You might need to explicitly declare type for this, either HtmlMod, SvgMod, TagMod, String or call .toHtmlMod, .toSvgMod, .toTagMod, .css methods. Unexpected term: $unexpectedTerm"
            )
        }
      }
    }

    classNames
  }

  val term = tailwindExpr.asTerm
  val classList =
    extractClassNames(term).reverse // e.g. tw.negativeM8.maxHInherit -> List("negative-m-8", "max-h-inherit")
  validate(classList)
  val combinedClasses = classList
    .mkString(" ")           // e.g. List("negative-m-8", "max-h-inherit") -> "negative-m-8 max-h-inherit"
    .replace("negative", "") // e.g. "negative-m-8 max-h-inherit" -> "-m-8 max-h-inherit"
  Expr(combinedClasses)
}
