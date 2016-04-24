package org.scalafmt.util

import scala.meta.internal.classifiers.classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

@classifier
trait Keyword {
  def unapply(token: Token): Boolean = {
    token.is[Abstract] || token.is[Case] || token.is[Catch] || token.is[Class] ||
    token.is[Def] || token.is[Do] || token.is[Else] || token.is[Extends] ||
    token.is[False] || token.is[Final] || token.is[Finally] || token.is[For] ||
    token.is[ForSome] || token.is[If] || token.is[Implicit] || token.is[Import] ||
    token.is[Lazy] || token.is[Match] || token.is[Macro] || token.is[New] ||
    token.is[Null] || token.is[Object] || token.is[Override] || token.is[Package] ||
    token.is[Private] || token.is[Protected] || token.is[Return] || token.is[Sealed] ||
    token.is[Super] || token.is[This] || token.is[Throw] || token.is[Trait] ||
    token.is[True] || token.is[Try] || token.is[Type] || token.is[Val] ||
    token.is[Var] || token.is[While] || token.is[With] || token.is[Yield]
  }
}

@classifier
trait Delim {
  def unapply(token: Token): Boolean = {
    token.is[Hash] || token.is[Colon] || token.is[Viewbound] || token.is[LeftArrow] ||
    token.is[Subtype] || token.is[Equals] || token.is[RightArrow] || token.is[Supertype] ||
    token.is[At] || token.is[Underscore] || token.is[LeftParen] || token.is[RightParen] ||
    token.is[Comma] || token.is[Dot] || token.is[Semicolon] || token.is[LeftBracket] ||
    token.is[RightBracket] || token.is[LeftBrace] || token.is[RightBrace]
  }
}

@classifier
trait Modifier {
  def unapply(token: Token): Boolean = {
    token.is[Abstract] || token.is[Final] ||
    token.is[Sealed] || token.is[Implicit] ||
    token.is[Lazy] || token.is[Private] ||
    token.is[Protected] || token.is[Override]
  }
}

@classifier
trait Literal {
  def unapply(token: Token): Boolean = {
    token.is[Constant.Int] || token.is[Constant.Long] ||
    token.is[Constant.Float] || token.is[Constant.Double] ||
    token.is[Constant.Char] || token.is[Constant.Symbol] ||
    token.is[Constant.String] || token.is[Null] ||
    token.is[True] || token.is[False]
  }
}

@classifier
trait Whitespace {
  def unapply(token: Token): Boolean = {
    token.is[Space] || token.is[Tab] ||
    token.is[CR] || token.is[LF] ||
    token.is[FF]
  }
}

@classifier
trait Trivia {
  def unapply(token: Token): Boolean = {
    token.is[Whitespace] || token.is[Comment]
  }
}
