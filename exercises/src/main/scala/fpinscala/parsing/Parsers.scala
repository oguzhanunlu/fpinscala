package fpinscala.parsing

import language.higherKinds
import fpinscala.testing._
import fpinscala.testing.Prop._

import scala.util.matching.Regex


case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List()) {

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest.map(_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}

trait Parsers[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(Nil)
    case _ => map2(p, listOfN(n-1, p))(_ :: _)
  }

  // 0 or more
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _).or(succeed(Nil))

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice: Parser[String] = self.slice(p)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
  }

  val numA: Parser[Int] = map(many[Char](char('a')))(_.size)

  val numA2: Parser[Int] = char('a').many.map(_.size)

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)
  }

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  val numA3: Parser[Int] = char('a').many.slice.map(_.length)

  // 1 or more
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = map2(p, p2)((_,_))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a,b)))

  // 0 or more 'a' followed by 1 or more 'b'
//  val p: Parser[(Int, Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

  implicit def regex(r: Regex): Parser[String]
}

object Parser1 {
  type Parser[+A] = String => Either[ParseError, A]

  def string(s: String): Parser[String] =
    (input: String) =>
      if (input.startsWith(s)) Right(s)
      else Left(Location(input).toError("Expected: " + s))

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object Parser2 {
  type Parser[+A] = Location => Either[ParseError, A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}