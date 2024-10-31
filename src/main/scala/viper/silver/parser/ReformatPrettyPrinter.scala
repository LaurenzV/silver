package viper.silver.parser

import viper.silver.ast.FilePosition
import viper.silver.ast.pretty.FastPrettyPrinterBase
import viper.silver.parser.PSym.Brace
import viper.silver.plugin.standard.adt.{PAdt, PAdtConstructor, PAdtSeq}

object ReformatPrettyPrinter extends FastPrettyPrinterBase {
  override val defaultIndent = 4

  def reformat(n: Reformattable): String = {
    super.pretty(defaultWidth, show(n))
  }

  def showOption(n: Option[AnyRef]): Cont = {
    n match {
      case Some(r) => show(r)
      case None => nil
    }
  }

  def showAnnotations(annotations: Seq[PAnnotation]): Cont = {
    if (annotations.isEmpty) {nil} else {
      annotations.map(show).foldLeft(nil)((acc, n) => acc <@@> n) <+> nil
    }
  }

  def showReturns(returns: Option[PMethodReturns]): Cont = {
    returns.map(a => nil <+> show(a)).getOrElse(nil)
  }

  def showPresPosts(pres: PDelimited[_, _], posts: PDelimited[_, _]): Cont = {
    nest(defaultIndent, (if (pres.isEmpty) nil
    else line <> show(pres)) <>
      (if (posts.isEmpty) nil
      else line <> show(posts)
        )
    )
  }

  def showBody(body: Cont, newline: Boolean): Cont = {
    if (newline) {
      linebreak <> body
    } else {
      nil <+> body
    }
  }

  def sep[U](n: Option[U]): Cont = {
    n.map(a => a match {
      case _: PSpecification[_] => line
      case _: PVars => line
      case _: PFormalArgDecl => space
      case _: PTypeVarDecl => space
      case _ => nil
    }).getOrElse(nil)
  }

  def list(n: List[AnyRef], sep: Cont): Cont = {
    n.map(show).reduce(_ <> sep <> _)
  }

  def show(n: AnyRef): Cont = {
    n match {
      case p: Option[AnyRef] => showOption(p)
      case l: List[AnyRef] => {
        if (l.isEmpty) {
          nil
        } else {
          val sep = l.head match {
            case _: PAdtConstructor => linebreak
            case _ => linebreak
          }
          list(l, sep)
        }
      }
      case p: PProgram => {
        println(p)
        val elements = (p.comments ++ p.members).sortBy(el => el.pos match {
          case (slc: FilePosition, _) => (slc.line, slc.column)
          case _ => (0, 0)
        });
        elements.map(show).foldLeft(nil)((acc, n) => acc <@@> n)
      }
      case p: PMethod => {
        // TODO: Test annotations
        println(s"PMethod");
        println(s"---------------------------");
        println(s"args ${p.args}");
        println(s"returns ${p.returns}");
        println(s"pres ${p.pres}");
        println(s"posts ${p.posts}");
        println(s"body ${p.body}");
        showAnnotations(p.annotations) <@@> text(p.keyword.token) <+> text(p.idndef.name) <> show(p.args) <> showReturns(p.returns) <>
        showPresPosts(p.pres, p.posts) <> showBody(show(p.body), !(p.pres.isEmpty && p.posts.isEmpty))
      }
      case p: PFunction => {
        // TODO: Add PFunctioNType
        showAnnotations(p.annotations) <@@> show(p.keyword) <+> show(p.idndef) <+>
          show(p.args) <+> show(p.c) <+> show(p.resultType) <>
          showPresPosts(p.pres, p.posts) <> showBody(show(p.body), !(p.pres.isEmpty && p.posts.isEmpty))
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

        val separator = sep(p.first);

        p.first.map(show).getOrElse(nil) <@@@>
          p.inner.foldLeft(nil)((acc, b) => acc <@@@> show(b._1) <@@@> separator <@@@> show(b._2)) <@@@>
          p.end.map(show).getOrElse(nil)
      }
      case p: PAdt => {
        showAnnotations(p.annotations) <@@> show(p.adt) <+>
          show(p.idndef) <> show(p.typVars) <+> show(p.c)
      }
      case p: PAdtSeq[_] => show(p.seq)
      case p: PVars => show(p.keyword) <+> show(p.vars) <> p.init.map(s => nil <+> show(s._1) <+> show(s._2)).getOrElse(nil)
      case p: PMethodReturns => show(p.k) <+> show(p.formalReturns)
      case p: PReserved[_] => text(p.token)
      case p: PSpecification[_] => show(p.k) <+> show(p.e)
      case p: PBinExp => show(p.left) <+> show(p.op) <+> show(p.right)
      case p: PFieldDecl => show(p.idndef) <> show(p.c) <+> show(p.typ)
      case p: PSym => text(p.symbol)
      case p: PIdnDef => p.name
      case p: PLocalVarDecl => show(p.idndef) <> show(p.c) <+> show(p.typ)
      case l: List[Reformattable] => l.map(show).reduce(_ <> _)
      case p: PComment => text(p.display)
      case p: PKw => text(p.keyword)
      // TODO: Support annotations
      case p: PImport => show(p.imprt) <+> show(p.file)
      case p: PStringLiteral => show(p.grouped)
      case p: PRawString => text(p.str)
      case n: Reformattable => text(n.reformat)
      case u => throw new IllegalArgumentException(s"attemted to format non-formattable type ${u}")
    }
  }
}