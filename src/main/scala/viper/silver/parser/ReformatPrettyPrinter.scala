package viper.silver.parser

import viper.silver.ast.FilePosition
import viper.silver.ast.pretty.{Call, FastPrettyPrinterBase}

object ReformatPrettyPrinter extends FastPrettyPrinterBase {
  override val defaultIndent = 2

  def reformat(n: Reformattable): String = {
    super.pretty(defaultWidth, show(n))
  }

  def show(n: Reformattable): Cont = {
    n match {
      case p@PProgram(_, _, _) => {
//        println(p)
        val elements = (p.comments ++ p.members).sortBy(el => el.pos match {
          case (slc: FilePosition, _) => (slc.line, slc.column)
          case _ => (0, 0)
        });
        elements.map(show).foldLeft(nil)(_ <> linebreak <> linebreak <> _)
      }
      case PMethod(annotations, keyword, idndef, args, returns, pres, posts, body) => {
        println(args)
        var cont = nil
        cont = cont <> annotations.map(show).foldLeft(nil)(_ <> linebreak <> _)
        cont = cont <+> text(keyword.token) <+> text(idndef.name)
        cont
      }
      case p: PGrouped.Paren[Reformattable] => {
        show(p.l) <> line <> nest(defaultIndent, show(p.inner)) <> line <> show(p.r)
      }
      case p@PGrouped(left, inner: Reformattable, right)  => {
        show(left) <+> show(inner) <+> show(right)
      }
      case p: PReserved[_] => p.display
      case p: PIdnDef => p.display
      case l: List[Reformattable] => l.map(show).reduce(_ <> _)
      case _ => text(n.reformat)
    }
  }
}