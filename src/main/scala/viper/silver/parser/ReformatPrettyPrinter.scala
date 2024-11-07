package viper.silver.parser

import viper.silver.ast.{FilePosition, HasLineColumn, LineColumnPosition, Position}
import viper.silver.ast.pretty.{BracketPrettyPrinter, FastPrettyPrinterBase, PrettyExpression}
import viper.silver.parser.PSym.Brace
import viper.silver.plugin.standard.adt.{PAdt, PAdtConstructor, PAdtSeq}

import scala.collection.mutable.ArrayBuffer

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
          program.substring(c_offset, p_offset)
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
    super.pretty(defaultWidth, showProgram(p, ctx))
  }

  def showOption(n: Option[AnyRef], ctx: ReformatterContext): Cont = {
    n match {
      case Some(r) => show(r, ctx)
      case None => nil
    }
  }

  def showAnnotations(annotations: Seq[PAnnotation], ctx: ReformatterContext): Cont = {
    if (annotations.isEmpty) {nil} else {
      annotations.map(show(_, ctx)).foldLeft(nil)((acc, n) => acc <@@> n) <+> nil
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

  def sep[U](n: Option[U]): Cont = {
    n.map(a => a match {
      case _: PSpecification[_] => line
      case _: PVars => line
      case _: PAssign => line
      case _: PUnfold => line
      case _: PFold => line
      case _: PInhale => line
      case _: PExhale => line
      case _: PDomainInterpretation => space
      case _: PFormalArgDecl => space
      case _: PDomainFunctionArg => space
      case _: PFormalReturnDecl => space
      case _: PTypeVarDecl => space
      case _ => nil
    }).getOrElse(nil)
  }

  def list(n: List[AnyRef], sep: Cont, ctx: ReformatterContext): Cont = {
    n.map(show(_, ctx)).reduce(_ <> sep <> _)
  }

  def showProgram(p: PProgram, ctx: ReformatterContext): Cont = {
    p.members.map(show(_, ctx)).foldLeft(nil)((acc, n) => acc <@@> n)
  }

  def show(n: AnyRef, ctx: ReformatterContext): Cont = {
    val trivia = n match {
      case p: PLeaf => {
        val trivia = ctx.getTrivia(p.pos);
        ctx.currentPosition = p.pos;
        text(trivia)
      };
      case _ => nil
    }

    val reformatted = n match {
      case p: Option[AnyRef] => showOption(p, ctx)
      case l: List[AnyRef] => {
        if (l.isEmpty) {
          nil
        } else {
          val sep = l.head match {
            case _: PAdtConstructor => linebreak
            case _ => linebreak
          }
          list(l, sep, ctx)
        }
      }
      case p: String => text(p)
      case p: PDefine => {
        showAnnotations(p.annotations, ctx) <@@> show(p.define, ctx) <+> show(p.idndef, ctx) <> show(p.parameters, ctx) <+> show(p.body, ctx)
      }
      case p: PMethod => {
        // TODO: Test annotations
//        println(s"PMethod");
//        println(s"---------------------------");
//        println(s"args ${p.args}");
//        println(s"returns ${p.returns}");
//        println(s"pres ${p.pres}");
//        println(s"posts ${p.posts}");
//        println(s"body ${p.body}");
//        println(s"keyword pos: ${p.keyword.pos}");
        showAnnotations(p.annotations, ctx) <@@> show(p.keyword, ctx) <+> show(p.idndef, ctx) <> show(p.args, ctx) <> showReturns(p.returns, ctx) <>
        showPresPosts(p.pres, p.posts, ctx) <> showBody(show(p.body, ctx), !(p.returns.isEmpty && p.pres.isEmpty && p.posts.isEmpty))
      }
      case p: PFunction => {
        // TODO: Add PFunctioNType
//        println(s"PFunction");
//        println(s"---------------------------");
//        println(s"body ${p.body}");
        showAnnotations(p.annotations, ctx) <@@> show(p.keyword, ctx) <+> show(p.idndef, ctx) <>
          show(p.args, ctx) <+> show(p.c, ctx) <+> show(p.resultType, ctx) <>
          showPresPosts(p.pres, p.posts, ctx) <> showBody(show(p.body, ctx), !(p.pres.isEmpty && p.posts.isEmpty))
      }
      case p: PDomain => {
        val interp = if (p.interpretations.isEmpty) {
          nil
        } else  {
          nest(defaultIndent, line <> show(p.interpretations, ctx))
        }
        showAnnotations(p.annotations, ctx) <@@> show(p.domain, ctx) <+>
          show(p.idndef, ctx) <> show(p.typVars, ctx) <> interp <>
          showBody(show(p.members, ctx), !p.interpretations.isEmpty)
      }
      case p: PDomainInterpretations => show(p.k, ctx) <+> show(p.m, ctx)
      case p: PDomainInterpretation => show(p.name, ctx) <> show(p.c, ctx) <+> show(p.lit, ctx)
      case p: PDomainMembers => show(p.original, ctx)
      case p: PDomainMembers1 => if (p.members.isEmpty) nil else p.members.map(m => show(m, ctx))
        .reduce(_ <> linebreak <> linebreak <> _)
      case p: PAxiom1 => {
//        println(s"axiom stuff: ${p.exp}")
        showAnnotations(p.annotations, ctx) <@@> show(p.axiom, ctx) <+@> show(p.idndef, ctx) <+@> show(p.exp, ctx) <> show(p.s, ctx)
      }
      case p: PDomainFunction1 => showAnnotations(p.annotations, ctx) <@@> show(p.unique, ctx) <+@>
        show(p.function, ctx) <+@> show(p.interpretation, ctx) <+>
        show(p.idndef, ctx) <> show(p.args, ctx) <> show(p.c, ctx) <+> show(p.typ, ctx) <+> show(p.s, ctx)
      case p: PPredicate => {
        showAnnotations(p.annotations, ctx) <@@> show(p.keyword, ctx) <+> show(p.idndef, ctx) <>
          show(p.args, ctx) <> showBody(show(p.body, ctx), false)
      }
      case PFields(annotation, field, fields, s) => {
//        println(s"PFields");
//        println(s"---------------------------");
//        println(s"annotation: ${annotation}");
//        println(s"field: ${field}");
//        println(s"fields: ${fields}");
//        println(s"s: ${s}");
        show(field, ctx) <+> show(fields, ctx) <> show(s, ctx)
      }
      case p: PGrouped[Brace, Reformattable] if p.l.rs.isInstanceOf[Brace] => {
//        println(s"PGrouped with brace");
//
//        println(s"---------------------------");
//        println(s"left: ${p.l}");
//        println(s"inner: ${p.inner}");
//        println(s"right: ${p.r}");
        val left = show(p.l, ctx);
        val inner = show(p.inner, ctx);
        val right = show(p.r, ctx);
        if (inner == nil) {
          left <> right
        } else {
          left <> nest(defaultIndent, line <> inner) <> line <> right
        }
      }
      case PGrouped(left, inner: Reformattable, right) => {
//        println(s"PGrouped without brace");
//        println(s"left: ${left}");
//        println(s"inner: ${inner}");
//        println(s"right: ${right}");
//        println(s"---------------------------");
        show(left, ctx) <> nest(defaultIndent, show(inner, ctx)) <> show(right, ctx)
      }
      case p: PTrigger => show(p.exp, ctx)
      case p: PSeqn => show(p.ss, ctx)
      case p: PDelimited[Reformattable, Reformattable] => {
//        println(s"PDelimited");
//        println(s"---------------------------");
//        println(s"first: ${p.first}");
//        println(s"inner: ${p.inner}");
//        println(s"end: ${p.end}");

        val separator = sep(p.first);

        showOption(p.first, ctx) <@@@>
          p.inner.foldLeft(nil)((acc, b) => acc <@@@> show(b._1, ctx) <@@@> separator <@@@> show(b._2, ctx)) <@@@>
          showOption(p.end, ctx)
      }
      case p: PAdt => {
        showAnnotations(p.annotations, ctx) <@@> show(p.adt, ctx) <+>
          show(p.idndef, ctx) <> show(p.typVars, ctx) <+> show(p.c, ctx)
      }
      case p: PForall => show(p.keyword, ctx) <+> show(p.vars, ctx) <+>
        show(p.c, ctx) <+> show(p.triggers, ctx) <+> show(p.body, ctx)
      case p: PAdtSeq[_] => show(p.seq, ctx)
      case p: PVars => show(p.keyword, ctx) <+> show(p.vars, ctx) <> p.init.map(s => nil <+> show(s._1, ctx) <+> show(s._2, ctx)).getOrElse(nil)
      case p: PMethodReturns => show(p.k, ctx) <+> show(p.formalReturns, ctx)
      case p: PReserved[_] => text(p.token)
      case p: PSpecification[_] => show(p.k, ctx) <+> show(p.e, ctx)
      case p: PBinExp => {
        val inner = show(p.left, ctx) <+> show(p.op, ctx) <+> show(p.right, ctx)

        if (!p.brackets.isEmpty) {
          text("(") <> inner <> text(")")
        } else  {
          inner
        }
      }
      case p: PFieldDecl => show(p.idndef, ctx) <> show(p.c, ctx) <+> show(p.typ, ctx)
      case p: PSym => text(p.symbol)
      case p: PIdnDef => text(p.name)
      case p: PAssign => show(p.targets, ctx) <+@> show(p.op, ctx) <+@> show(p.rhs, ctx)
      case p: PLocalVarDecl => show(p.idndef, ctx) <> show(p.c, ctx) <+> show(p.typ, ctx)
      case l: List[Reformattable] => l.map(show(_, ctx)).reduce(_ <> _)
      case p: PComment => text(p.display)
      case p: PKw => text(p.keyword)
      case p: PWhile => {
        show(p.keyword, ctx) <> show(p.cond, ctx) <+> showInvs(p.invs, ctx) <> showBody(show(p.body, ctx), !p.invs.isEmpty)
      }
      case p: PImport => show(p.imprt, ctx) <+> show(p.file, ctx)
      case p: PStringLiteral => show(p.grouped, ctx)
      case p: PRawString => text(p.str)
      case p: PBracedExp => show(p.e, ctx)
      // TODO: Actually implement the ones below
      case p: PExp => show(p.pretty, ctx)
      case p: PIf => show(p.keyword, ctx) <+> show(p.cond, ctx) <> showBody(show(p.thn, ctx), true) <+@> showBody(show(p.els, ctx), false)
      case p: PElse => show(p.k, ctx) <+> showBody(show(p.els, ctx), false)
      case p: PFormalArgDecl => show(p.pretty, ctx)
      case p: PPrimitiv[_] => show(p.name, ctx)
      case p: PCall => show(p.idnref, ctx) <> show(p.callArgs, ctx) <> show(p.typeAnnotated, ctx)
      case n: Reformattable => text(n.reformat)
      case u => throw new IllegalArgumentException(s"attemted to format non-formattable type ${u.getClass}")
    }

    reformatted
  }
}