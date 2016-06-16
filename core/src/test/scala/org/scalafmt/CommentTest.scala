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
    |  // Foobar
    |  object Seaside // trailing
    |  object Mountains
    |}
  """.stripMargin.parse[Source].get

  case class AssociatedComments(leadingMap: Map[Token, Seq[Comment]],
                                trailingMap: Map[Token, Seq[Comment]]) {
    def leading(tree: Tree): Seq[Comment] =
      (for {
        token <- tree.tokens.headOption
        comments <- leadingMap.get(token)
      } yield comments).getOrElse(Seq.empty[Comment])

    def trailing(tree: Tree): Seq[Comment] =
      (for {
        token <- tree.tokens.lastOption
        comments <- trailingMap.get(token)
      } yield comments).getOrElse(Seq.empty[Comment])

    def contains(tree: Tree) =
      trailing(tree).nonEmpty || leading(tree).nonEmpty
  }

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
  val comments = getAssociatedComments(tree.tokens)
  tree.transform {
    case t: Tree if comments.contains(t) =>
      logger.elem(t, t.getClass, comments.leading(t), comments.trailing(t))
      t
  }
  logger.elem(comments.leadingMap)
  logger.elem(comments.trailingMap)
//  println(tree.show[Structure])
}
