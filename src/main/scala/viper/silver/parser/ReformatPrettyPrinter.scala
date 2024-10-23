package viper.silver.parser

import viper.silver.ast.pretty.FastPrettyPrinterBase

object ReformatPrettyPrinter extends FastPrettyPrinterBase {
  override val defaultIndent = 2

  def pretty(n: Reformattable): String = {
    super.pretty(defaultWidth, show(n))
  }

  def show(n: Reformattable): Cont = {
    n match {
      case PProgram(_, members, _) => {
        for (member <- members) {
          member <> linebreak
        }
      }
      case _ => text(n.reformat)
    }
  }
}