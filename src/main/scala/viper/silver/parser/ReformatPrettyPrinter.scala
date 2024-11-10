package viper.silver.parser

import fastparse.Parsed
import viper.silver.ast.pretty.FastPrettyPrinter.Cont
import viper.silver.ast.{FilePosition, HasLineColumn, LineColumnPosition, Position}
import viper.silver.ast.pretty.FastPrettyPrinterBase
import viper.silver.parser.FastParserCompanion.programComments
import viper.silver.parser.PSym.Brace
import viper.silver.plugin.standard.adt.{PAdt, PAdtConstructor, PAdtFieldDecl, PAdtSeq}

import scala.runtime.Nothing$

trait Reformattable extends FastPrettyPrinterBase {
  def reformat(ctx: ReformatterContext): Cont
}

class ReformatterContext(val program: String, val offsets: Seq[Int]) {
  var currentPosition: (Position, Position) = (LineColumnPosition(1, 1), LineColumnPosition(1, 1))

  private def getByteOffset(p: HasLineColumn): Int = {
    val row = offsets(p.line - 1);
    row + p.column - 1
  }

  def getTrivia(pos: (Position, Position)): String = {
    (currentPosition._2, pos._1) match {
      case (c: HasLineColumn, p: HasLineColumn) => {
        val c_offset = getByteOffset(c);
        val p_offset = getByteOffset(p);
        if (c_offset <= p_offset) {
          program.substring(c_offset, p_offset).trim
        } else {
          ""
        }
      }
      case _ => ""
    }
  }
}

object ReformatPrettyPrinter extends FastPrettyPrinterBase  {
  override val defaultIndent = 4

  def reformatProgram(p: PProgram): String = {
    val ctx = new ReformatterContext(p.rawProgram, p.offsets);
    super.pretty(defaultWidth, show(p, ctx))
  }

  def showOption[T <: Any](n: Option[T], ctx: ReformatterContext): Cont = {
    n match {
      case Some(r) => showAny(r, ctx)
      case None => nil
    }
  }

  def showAnnotations(annotations: Seq[PAnnotation], ctx: ReformatterContext): Cont = {
    if (annotations.isEmpty) {nil} else {
      annotations.map(show(_, ctx)).foldLeft(nil)((acc, n) => acc <@@> n)
    }
  }

  def showReturns(returns: Option[PMethodReturns], ctx: ReformatterContext): Cont = {
    returns.map(a => nil <+> show(a, ctx)).getOrElse(nil)
  }

  def showPresPosts(pres: PDelimited[_, _], posts: PDelimited[_, _], ctx: ReformatterContext): Cont = {
    nest(defaultIndent, (if (pres.isEmpty) nil
    else line <> show(pres, ctx)) <>
      (if (posts.isEmpty) nil
      else line <> show(posts, ctx)
        )
    )
  }

  def showInvs(invs: PDelimited[_, _], ctx: ReformatterContext): Cont = {
    nest(defaultIndent, (if (invs.isEmpty) nil else line <> show(invs, ctx)))
  }

  def showBody(body: Cont, newline: Boolean): Cont = {
    if (newline) {
      linebreak <> body
    } else {
      nil <+> body
    }
  }

  def show(r: Reformattable, ctx: ReformatterContext): Cont = {
    val trivia = r match {
      case p: PLeaf => {
        val trivia = ctx.getTrivia(p.pos);
//        println(s"cur: ${ctx.currentPosition}, new: ${p.pos}")
//        println(s"trivia: ${trivia}")
        ctx.currentPosition = p.pos;
        trivia
      };
      case _ => ""
    }

    val comments: Seq[PComment] = fastparse.parse(trivia, programComments(_)) match {
      case Parsed.Success(value, _) => value
      case _: Parsed.Failure => Seq()
    }

    val formattedComments = if (comments.isEmpty) nil else comments.map(a => text(a.display))reduce(_ <> linebreak <> _)

    formattedComments <@@> r.reformat(ctx)
  }

  def showAny(n: Any, ctx: ReformatterContext): Cont = {
    n match {
      case p: Reformattable => show(p, ctx)
      case p: Option[Any] => showOption(p, ctx)
      case p: Seq[Any] => showSeq(p, ctx)
      case p: Right[Any, Any] => showAny(p.value, ctx)
      case p: Left[Any, Any] => showAny(p.value, ctx)
    }
  }

  def showSeq(l: Seq[Any], ctx: ReformatterContext): Cont = {
    if (l.isEmpty) {
      nil
    } else {
      val sep = l.head match {
        case _: PAdtConstructor => linebreak
        case _ => linebreak
      }
      l.map(showAny(_, ctx)).reduce(_ <> sep <> _)
    }
  }
}