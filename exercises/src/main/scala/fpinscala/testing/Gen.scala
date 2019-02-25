package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  def &&(that: Prop): Prop =
    Prop(
      (ms: MaxSize, tc: TestCases, rng: RNG) =>
        if (this.run(ms, tc, rng).isFalsified) this.run(ms, tc, rng) else that.run(ms, tc, rng)
    )

  def ||(that: Prop): Prop =
    Prop(
      (ms: MaxSize, tc: TestCases, rng: RNG) =>
        (this.run(ms, tc, rng) , that.run(ms, tc, rng)) match {
          case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1.concat("\n"+f2), s1 + s2)
          case _ => Passed
        }
    )
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String

  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proven extends Result {
    def isFalsified: Boolean = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
      val prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: MaxSize = 10,
          testCases: TestCases = 10,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proven =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) =>
      if (p) Proven else Falsified("()", 0)
  }
}

case class SGen[+A](size: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(i => size(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => size(i).flatMap(a => f(a).toGen(i)))

  def toGen(n: Int): Gen[A] = size(n)
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(g.listOfN)
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def pick(s: RNG): (Int, RNG) = {
      val (i, s2) = s.nextInt
      if (i >= start && i < stopExclusive) (i, s2)
      else pick(s2)
    }
    Gen(State(s => pick(s)))
  }

  // always generates the value a
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(g.listOfN)

  // generates list of A using generator g
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val p1 = math.abs(g1._2) / (math.abs(g1._2) + math.abs(g2._2))
    Gen(State(RNG.double).flatMap(d => if (d < p1) g1._1.sample else g2._1.sample))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => g.listOfN(i.max(1)))

  val smallInt: Gen[Int] = choose(-10, 10)

  val sortedProp: Prop = forAll(listOf(smallInt)){ ns =>
    val sorted = ns.sorted
    ns.isEmpty || ns.tail.isEmpty || !sorted.zip(sorted.tail).exists{ case (a,b) => a > b }
  }
}
