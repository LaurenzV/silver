package viper.silver.parser

import viper.silver.ast.FilePosition
import viper.silver.ast.pretty.{Call, FastPrettyPrinterBase}
import viper.silver.parser.PSym.{Brace, LBrace, LParen, RBrace}
import viper.silver.parser.PSymOp.RParen

object ReformatPrettyPrinter extends FastPrettyPrinterBase {
  override val defaultIndent = 4

  def reformat(n: Reformattable): String = {
    super.pretty(defaultWidth, show(n))
  }

  def show_option(n: Option[AnyRef]): Cont = {
    n match {
      case Some(r) => show(r)
      case None => nil
    }
  }

  def show(n: AnyRef): Cont = {
    n match {
      case p: Option[AnyRef] => show_option(p)
      case p@PProgram(_, _, _) => {
        println(p)
        val elements = (p.comments ++ p.members).sortBy(el => el.pos match {
          case (slc: FilePosition, _) => (slc.line, slc.column)
          case _ => (0, 0)
        });
        elements.map(show).foldLeft(nil)((acc, n) => acc <@@> n)
      }
      case PMethod(annotations, keyword, idndef, args, returns, pres, posts, body) => {
        println(s"PMethod");
        println(s"---------------------------");
        println(s"args ${args}");
        println(s"returns ${returns}");
        println(s"pres ${pres}");
        println(s"posts ${posts}");
        println(s"body ${body}");
        (if (annotations.isEmpty) {nil} else {
          annotations.map(show).foldLeft(nil)((acc, n) => acc <@@> n) <+> nil
        }) <> group(
            text(keyword.token) <+> text(idndef.name) <> show(args) <>
            returns.map(a => nil <+> show(a)).getOrElse(nil)
          ) <>
        nest(defaultIndent, (if (pres.isEmpty) nil
                            else line <> show(pres)) <>
                              (if (posts.isEmpty) nil
                              else line <> show(posts)
                                )
        ) <> (if (pres.isEmpty && posts.isEmpty) {
          // If not pre- or post conditions exist, the starting brace should be on the same line and
          // just separated by a space.
          nil <+> show(body)
        } else {
          // Otherwise, show on a new line.
          nil <> linebreak <> show(body)
        })
      }
      case PFields(annotation, field, fields, s) => {
        println(s"PFields");
        println(s"---------------------------");
        println(s"annotation: ${annotation}");
        println(s"field: ${field}");
        println(s"fields: ${fields}");
        println(s"s: ${s}");
        show(field) <+> show(fields) <> show(s)
      }
      case p: PGrouped[Brace, Reformattable] if p.l.rs.isInstanceOf[Brace] => {
        println(s"PGrouped with brace");

        println(s"---------------------------");
        println(s"left: ${p.l}");
        println(s"inner: ${p.inner}");
        println(s"right: ${p.r}");
        val inner = show(p.inner);
        println(s"is nil? ${inner == nil}");
        if (inner == nil) {
          show(p.l) <> show(p.r)
        } else {
          show(p.l) <> nest(defaultIndent, line <> inner) <> line <> show(p.r)
        }
      }
      case PGrouped(left, inner: Reformattable, right)  => {
        println(s"PGrouped without brace");
        println(s"left: ${left}");
        println(s"inner: ${inner}");
        println(s"right: ${right}");
        println(s"---------------------------");
        show(left) <> nest(defaultIndent, show(inner)) <> show(right)
      }
      case p: PSeqn => show(p.ss)
      case p: PDelimited[Reformattable, Reformattable] => {
        println(s"PDelimited");
        println(s"---------------------------");
        println(s"first: ${p.first}");
        println(s"inner: ${p.inner}");
        println(s"end: ${p.end}");

        val separator = if (p.first.isInstanceOf[Option[PSpecification[_]]]) {
          line
        } else if (p.first.isInstanceOf[Option[PFormalArgDecl]]) {
          space
        } else {
          nil
        }

        p.first.map(show).getOrElse(nil) <@@@>
          p.inner.foldLeft(nil)((acc, b) => acc <@@@> show(b._1) <@@@> separator <@@@> show(b._2)) <@@@>
          p.end.map(show).getOrElse(nil)
      }
      case p: PVars => show(p.keyword) <+> show(p.vars) <> p.init.map(s => nil <+> show(s._1) <+> show(s._2)).getOrElse(nil)
      case p: PMethodReturns => show(p.k) <+> show(p.formalReturns)
      case p: PReserved[_] => text(p.token)
      case p: PSpecification[_] => show(p.k) <+> show(p.e)
      case p: PBinExp => show(p.left) <+> show(p.op) <+> show(p.right)
      case p: PFieldDecl => show(p.idndef) <> show(p.c) <+> show(p.typ)
      case p: PSym => text(p.symbol)
      case p: PReserved[_] => p.token
      case p: PIdnDef => p.name
      case p: PLocalVarDecl => show(p.idndef) <> show(p.c) <+> show(p.typ)
      case l: List[Reformattable] => l.map(show).reduce(_ <> _)
      // This should in theory never be called
      case n: Reformattable => text(n.reformat)
      case u => throw new IllegalArgumentException(s"attemted to format non-formattable type ${u}")
    }
  }
}