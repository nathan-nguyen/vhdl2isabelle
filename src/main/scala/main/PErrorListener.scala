package main

import java.util
import java.util.Collections

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.slf4j.LoggerFactory

/**
  * Created by Hongxu Chen.
  */
final class PErrorListener extends BaseErrorListener {

  val logger = LoggerFactory.getLogger(classOf[PErrorListener])
  override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {
    logger.debug(s"[reportContextSensitivity]: ${recognizer.getCurrentToken}")
  }

  override def reportAmbiguity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: util.BitSet, configs: ATNConfigSet): Unit = {
    logger.debug(s"[reportAmbiguity]: ${recognizer.getCurrentToken}")
  }

  override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {
    logger.debug(s"[reportAttemptingFullContext]: ${recognizer.getCurrentToken}")
  }

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    val stack = recognizer.asInstanceOf[Parser].getRuleInvocationStack
    Collections.reverse(stack)
    throw new RuntimeException(s"[syntaxError]: line=${line} ${charPositionInLine} at ${offendingSymbol} : ${msg}; rule_stack=${stack}")
  }
}
