package org.scalafmt

import scala.meta.tokens.Token.Comment
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class CommentTest extends FunSuite with DiffAssertions {
  val javadocStyle = ScalafmtStyle.default.copy(scalaDocs = false)

  ignore("javadoc docstrings are correct") {
    val original = """object a {
                     |/**
                     |   * Y is cool
                     |   */
                     |val y = 2
                     |}
      """.stripMargin
    val expected = """object a {
                     |
                     |  /**
                     |   * Y is cool
                     |   */
                     |  val y = 2
                     |}
      """.stripMargin
    val obtained = Scalafmt.format(original, javadocStyle).get
    assertNoDiff(obtained, expected)
  }
  import scala.meta._
  val tree = """
    |package io.buildo.baseexample
    |
    |package enums
    |
    |import ingredients.caseenum.annotations.enum
    |
    |// scalastyle:off number.of.types
    |@enum trait CampingLocation {
    |  object Seaside
    |  object Mountains
    |}
  """.stripMargin.parse[Source].get

  case class AssociatedComments(
      leading: Map[Token, Seq[Comment]],
      trailing: Map[Token, Seq[Comment]]
  )

  def getAssociatedComments(tokens: Tokens): AssociatedComments = {
    import scala.meta.tokens.Token._
    val leadingBuilder = Map.newBuilder[Token, Seq[Comment]]
    val trailingBuilder = Map.newBuilder[Token, Seq[Comment]]
    val leading = Seq.newBuilder[Comment]
    val trailing = Seq.newBuilder[Comment]
    var isLeading = true
    var lastToken: Token = tokens.head
    tokens.foreach {
      case c: Comment =>
        if (isLeading) leading += c
        else trailing += c
      case _: `\n` => isLeading = true
      case _: Trivia =>
      case currentToken =>
        val t = trailing.result()
        if (t.nonEmpty) {
          trailingBuilder += lastToken -> trailing.result()
          trailing.clear()
        }
        val l = leading.result()
        if (l.nonEmpty) {
          leadingBuilder += currentToken -> leading.result()
          leading.clear()
        }
        lastToken = currentToken
        isLeading = false
    }
    AssociatedComments(leadingBuilder.result(), trailingBuilder.result())
  }

  import org.scalafmt.util.LoggerOps._
  val t = getAssociatedComments(tree.tokens)
  logger.elem(t.leading)
  logger.elem(t.trailing)
//  println(tree.show[Structure])
}
