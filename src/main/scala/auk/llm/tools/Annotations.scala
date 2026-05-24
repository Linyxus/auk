package auk.llm.tools

import scala.quoted.*

/** Compile-time extraction of [[desc]] annotations.
  *
  * Scala 3 `Mirror`s expose field names and types but not annotations, so we
  * reach for the reflection API to read them. We inspect both the primary
  * constructor parameters and the case accessors, because the compiler may
  * attach a parameter annotation to either symbol.
  */
private[tools] object Annotations:
  /** Map from field name to its `@desc` text, for fields that carry one. */
  inline def fieldDescriptions[A]: Map[String, String] =
    ${ fieldDescriptionsImpl[A] }

  /** The `@desc` text attached to the type itself, if any. */
  inline def typeDescription[A]: Option[String] =
    ${ typeDescriptionImpl[A] }

  private def descText(using Quotes)(term: quotes.reflect.Term): Option[String] =
    import quotes.reflect.*
    term match
      case Apply(Select(New(tpt), _), args) if tpt.tpe <:< TypeRepr.of[desc] =>
        args.collectFirst { case Literal(StringConstant(s)) => s }
      case _ => None

  private def fieldDescriptionsImpl[A: Type](using Quotes): Expr[Map[String, String]] =
    import quotes.reflect.*
    val sym = TypeRepr.of[A].typeSymbol
    val ctorParams = sym.primaryConstructor.paramSymss.flatten.filterNot(_.isType)
    val symsByName: Map[String, List[Symbol]] =
      (ctorParams ++ sym.caseFields).groupBy(_.name)
    val pairs =
      symsByName.toList.flatMap { (name, syms) =>
        syms.flatMap(_.annotations).flatMap(descText).headOption.map(d => name -> d)
      }
    val entries: List[Expr[(String, String)]] =
      pairs.map { (n, d) => '{ (${ Expr(n) }, ${ Expr(d) }) } }
    '{ Map(${ Varargs(entries) }*) }

  private def typeDescriptionImpl[A: Type](using Quotes): Expr[Option[String]] =
    import quotes.reflect.*
    val sym = TypeRepr.of[A].typeSymbol
    sym.annotations.flatMap(descText).headOption match
      case Some(s) => '{ Some(${ Expr(s) }) }
      case None    => '{ None }
