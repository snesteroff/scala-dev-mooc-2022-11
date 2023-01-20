package module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap { a => b.map((a, _)) }

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap { a => b.map((a, _)) }

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap { a => b.map((a, _)) }

  def tuplef[F[_] : Bind, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    // instance ev: Bind[F,A]
    val c = implicitly[Bind[F]]
    c.flatMap((a: A) => c.map((b: B) => (a, b)))
  }

  /*
    def tuple(fa: Option[A], fb: Option[B])
   */
  trait Bind[F[_]] {
    def map[A, B](f: A => B): F[B]

    def flatMap[A, B](f: A => F[B]): F[B]

    def get[A](m: F[A]): A

    def put[A](x: A): F[A]
  }

  implicit def optBind[C](opt: Option[C]): Bind[Option] = new Bind[Option] {
    override def get[A](m: Option[A]): A = m.get

    override def put[A](x: A): Option[A] = Some(x)

    override def map[C, B](f: C => B): Option[B] = opt.map(f)

    override def flatMap[A, B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  tuplef(Some(1), Some(2))

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]

    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = {
    fa.flatMap(a => fb.map(b => (a, b)))
  }


  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r3 = println(tupleBindable(optBindable(optA), optBindable(optB)))
 // val r4 = ???

//  val r1 = println(tuplef(optA, optB))
//  val r2 = println(tuplef(list1, list2))

}