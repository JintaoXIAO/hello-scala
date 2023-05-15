package freemonad


object Main {
  sealed trait KVStoreA[A]
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Delete(key: String) extends KVStoreA[Unit]

  import cats.free.Free
  type KVStore[A] = Free[KVStoreA, A]

  import cats.free.Free.liftF
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put(key, value))

  def get[T](key: String): KVStore[Option[T]] =
    liftF[KVStoreA, Option[T]](Get(key))

  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key))

  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  import cats.arrow.FunctionK
  import cats.{ Id, ~> }
  import scala.collection.mutable

  def impureCompiler: KVStoreA ~> Id =
    new (KVStoreA ~> Id) {
      val kvs = mutable.Map.empty[String, Any]

      override def apply[A](fa: KVStoreA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            Id()
          case Get(key) =>
            println(s"get($key)")
            val a = kvs.get(key).asInstanceOf[A]
            Id(a)
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            Id()
        }
    }

  val result: Option[Int] = program.foldMap(impureCompiler)

  import cats.data.State
  type KVStoreState[A] = State[Map[String, Any], A]
  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    override def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
      fa match {
        case Put(key, value) => State.modify(_.updated(key, value))
        case Get(key) =>
          State.inspect(_.get(key).asInstanceOf[A])
        case Delete(key) => State.modify(_ - key)
      }
  }

  val pureResult: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
}