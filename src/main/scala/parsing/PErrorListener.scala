package parsing

import java.util
import java.util.Collections

import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime.{ANTLRErrorListener, Parser, RecognitionException, Recognizer}
import org.slf4j.LoggerFactory

final class PErrorListener extends ANTLRErrorListener {
  val logger = LoggerFactory.getLogger(classOf[PErrorListener])
  override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {
    logger.error(s"[reportContextSensitivity]: ${dfa}")
  }

  override def reportAmbiguity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: util.BitSet, configs: ATNConfigSet): Unit = {
    logger.error(s"[reportAmbiguity]: ${dfa}")
  }

  override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {
    logger.error(s"[reportAttemptingFullContext]: ${dfa}")
  }

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    val stack = recognizer.asInstanceOf[Parser].getRuleInvocationStack
    Collections.reverse(stack)
    throw new RuntimeException(s"[syntaxError]: line=${line} ${charPositionInLine} at ${offendingSymbol} : ${msg}; rule_stack=${stack}")
  }
}
