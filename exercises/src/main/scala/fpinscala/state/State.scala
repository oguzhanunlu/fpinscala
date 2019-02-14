package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) if i == Int.MinValue => (0, r)
      case (i, r) if i < 0 => (math.abs(i), r)
      case (i, r) => (i, r)
    }

  def double(rng: RNG): (Double, RNG) =
    rng.nextInt match { case (i, r) => (math.abs((i.toDouble - 1d) / Int.MaxValue.toDouble), r) }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, r1) = rng.nextInt
    val (i2, r2) = double(r1)
    ((i1, i2), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1, i2), r)= intDouble(rng)
    ((i2, i1), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (i1, r1) = double(rng)
    val (i2, r2) = double(r1)
    val (i3, r3) = double(r2)
    ((i1, i2, i3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else
      rng.nextInt match { case (i, r) =>
        ints(count-1)(r) match { case (li, r2) =>
          (i :: li, r2)
        }
      }

  def nonNegativeEven: Rand[Int] =
    rng => {
      val (v, s) = nonNegativeInt(rng)
      (v - v % 2, s)
    }

  def nonNegativeEven2: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap(rng: RNG): (Double, RNG) = map(int)(i => math.abs((i.toDouble - 1d) / Int.MaxValue.toDouble))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (va, sa) = ra(rng)
      val (vb, sb) = rb(rng)
      (f(va, vb), sb)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft[Rand[List[A]]](unit(Nil)){ (rla, ra) => map2(ra, rla)(_ :: _) }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegativeInt)(_ % n)

  def nonNegativeLessThan2(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (v, s) = f(rng)
      g(v)(s)
    }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatMap(n)
    }

  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a =>
      flatMap(rb)(b => unit(f(a,b)))
    }

}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => (f(run(s)._1), run(s)._2))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State{ s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State((a, _))

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldLeft[State[S, List[A]]](unit(Nil))((sla, sa) => sa.map2(sla)(_ :: _))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
