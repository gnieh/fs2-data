package fix

import scalafix.v1._
import scala.meta._

class JsonParse extends SemanticRule("json-parse") {

  private val TokensMatcher = SymbolMatcher.normalized("fs2/data/json/package.tokens().")
  private val ValuesMatcher = SymbolMatcher.normalized("fs2/data/json/ast/package.values().")
  private val tokensSymbol = Symbol("fs2/data/json/package.tokens().")
  private val valuesSymbol = Symbol("fs2/data/json/ast/package.values().")

  private def containsImport(importer: Importer)(implicit doc: SemanticDocument): Boolean =
    doc.tree
      .collect {
        case i: Importer if i.importees.intersect(importer.importees) == importer.importees =>
          true
        case _ =>
          false
      }
      .exists(identity)

  private def addFs2DataJsonImport(implicit doc: SemanticDocument): Patch =
    if (containsImport(importer"fs2.data.json._"))
      Patch.empty
    else
      Patch.addGlobalImport(importer"fs2.data.json._")

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      // case t @ q"$_.through($tokens).through($values)" if TokensMatcher.matches(tokens) && ValuesMatcher.matches(values) =>
      case t @ Term.Apply(Term.Select(Term.Apply(Term.Select(base, Term.Name("through")), List(TokensMatcher(_))),
                                      Term.Name("through")),
                          List(ValuesMatcher(_))) =>
        Patch.removeGlobalImport(tokensSymbol) +
          Patch.removeGlobalImport(valuesSymbol) +
          addFs2DataJsonImport + Patch.replaceTree(t, s"""$base.through(ast.parse)""")
    }.asPatch

}
