package viper.silver.parser

import viper.silver.ast.FilePosition
import viper.silver.ast.pretty.{Call, FastPrettyPrinterBase}
import viper.silver.parser.PSym.{LBrace, LParen, RBrace}
import viper.silver.parser.PSymOp.RParen

object ReformatPrettyPrinter extends FastPrettyPrinterBase {
  override val defaultIndent = 2

  def reformat(n: Reformattable): String = {
    super.pretty(defaultWidth, show(n))
  }

  def show(n: Reformattable): Cont = {
    n match {
      case p@PProgram(_, _, _) => {
        println(p)
        val elements = (p.comments ++ p.members).sortBy(el => el.pos match {
          case (slc: FilePosition, _) => (slc.line, slc.column)
          case _ => (0, 0)
        });
        elements.map(show).foldLeft(nil)(_ <> linebreak <> linebreak <> _)
      }
      case PMethod(annotations, keyword, idndef, args, returns, pres, posts, body) => {
        var cont = nil
        cont = cont <> annotations.map(show).foldLeft(nil)(_ <> linebreak <> _)
        cont = cont <+> text(keyword.token) <+> text(idndef.name) <> show(args)
        cont
      }
      case p@PGrouped(left: PReserved[LBrace.type], inner: Reformattable, right: PReserved[RBrace.type]) => {
        println(s"reached first ${}, ${}", left, right);
        show(left) <> line <> nest(defaultIndent, show(inner)) <> line <> show(right)
      }
      case p@PGrouped(left, inner: Reformattable, right)  => {
        println("reached second");
        show(left) <> show(inner) <> show(right)
      }
      case p: PDelimited[Reformattable, Reformattable] => {
        println(s"delimited: ${p}");
        p.first.map(show).getOrElse(nil) <> p.inner.foldLeft(nil)((a, b) => a <> show(b._1) <> show(b._2)) <> p.end.map(show).getOrElse(nil)
      }
      case p: PReserved[_] => p.display
      case p: PIdnDef => p.display
      case l: List[Reformattable] => l.map(show).reduce(_ <> _)
      case _ => text(n.reformat)
    }
  }
}