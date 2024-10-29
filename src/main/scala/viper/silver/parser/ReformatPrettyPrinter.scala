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

  def show(n: Option[Reformattable]): Cont = {
    n match {
      case Some(r) => show(r)
      case None => nil
    }
  }

  def show(n: AnyRef): Cont = {
    n match {
      case None => nil
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
        )
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
      case p: PGrouped[Brace, Reformattable] if p.l.isInstanceOf[Brace] => {
        show(p.l) <> line <> nest(defaultIndent, show(p.inner)) <> line <> show(p.r)
      }
      case PGrouped(left, inner: Reformattable, right)  => {
        show(left) <> show(inner) <> show(right)
      }
      case p: PDelimited[Reformattable, Reformattable] => {
        println(s"PDelimited");
        println(s"---------------------------");
        println(s"first: ${p.first}");
        println(s"inner: ${p.inner}");
        println(s"end: ${p.end}");

        p.first.map(show).getOrElse(nil) <@>
          p.inner.foldLeft(nil)((acc, b) => acc <@> show(b._1) <@> show(b._2)) <>
          p.end.map(show).getOrElse(nil)
      }
      case p: PMethodReturns => show(p.k) <+> show(p.formalReturns)
      case p: PReserved[_] => text(p.token)
      case p: PSpecification[_] => show(p.k) <+> show(p.e)
      case p: PBinExp => show(p.left) <+> show(p.op) <+> show(p.right)
      case p: PFieldDecl => show(p.idndef) <> show(p.c) <+> show(p.typ)
      case p: PSym => text(p.symbol)
      case p: PReserved[_] => p.token
      case p: PIdnDef => p.name
      case l: List[Reformattable] => l.map(show).reduce(_ <> _)
      // This should in theory never be called
      case n: Reformattable => text(n.reformat)
      case u => throw new IllegalArgumentException(s"attemted to format non-formattable type ${u}")
    }
  }
}