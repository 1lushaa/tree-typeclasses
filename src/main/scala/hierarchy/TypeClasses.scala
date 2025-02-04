package hierarchy

import scala.util.control.TailCalls.{TailRec, done, tailcall}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object TypeClasses {

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {

      def flatMapImpl(tree: Tree[A]): TailRec[Tree[B]] = tree match {
        case Leaf(value) => done(f(value))
        case Branch(left, right) =>
          tailcall(flatMapImpl(left)).flatMap(leftRes =>
            tailcall(flatMapImpl(right)).flatMap(rightRes => done(Branch(leftRes, rightRes)))
          )
      }

      flatMapImpl(fa).result
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def tailRecImpl(a: A): TailRec[Tree[B]] = {
        val tree = f(a)
        tree match {
          case Leaf(Right(value))  => done(Leaf(value))
          case Leaf(Left(value))   => tailcall(tailRecImpl(value))
          case Branch(left, right) => tailcall(processBranch(Branch(left, right)))
        }
      }

      def processBranch(branch: Branch[Either[A, B]]): TailRec[Tree[B]] = branch match {
        case Branch(Leaf(Right(leftVal)), Leaf(Right(rightVal))) => done(Branch(Leaf(leftVal), Leaf(rightVal)))
        case Branch(Leaf(Left(leftVal)), Leaf(Right(rightVal))) =>
          tailcall(tailRecImpl(leftVal)).flatMap(value => done(Branch(value, Leaf(rightVal))))
        case Branch(Leaf(Right(leftVal)), Leaf(Left(rightVal))) =>
          tailcall(tailRecImpl(rightVal)).flatMap(value => done(Branch(Leaf(leftVal), value)))
        case Branch(Leaf(Left(leftVal)), Leaf(Left(rightVal))) =>
          tailcall(tailRecImpl(leftVal)).flatMap(firstVal =>
            tailcall(tailRecImpl(rightVal)).flatMap(secondVal => done(Branch(firstVal, secondVal)))
          )
        case Branch(Leaf(Right(firstVal)), Branch(left, right)) =>
          tailcall(processBranch(Branch(left, right))).flatMap(secondVal => done(Branch(Leaf(firstVal), secondVal)))
        case Branch(Leaf(Left(firstVal)), Branch(left, right)) =>
          tailcall(tailRecImpl(firstVal)).flatMap(secondVal =>
            tailcall(processBranch(Branch(left, right))).flatMap(value => done(Branch(secondVal, value)))
          )
        case Branch(Branch(left, right), Leaf(Right(firstVal))) =>
          tailcall(processBranch(Branch(left, right))).flatMap(secondVal => done(Branch(secondVal, Leaf(firstVal))))
        case Branch(Branch(left, right), Leaf(Left(firstVal))) =>
          tailcall(processBranch(Branch(left, right))).flatMap(secondVal =>
            tailcall(tailRecImpl(firstVal)).flatMap(value => done(Branch(secondVal, value)))
          )
        case Branch(Branch(firstLeft, firstRight), Branch(secondLeft, secondRight)) =>
          tailcall(processBranch(Branch(firstLeft, firstRight))).flatMap(firstVal =>
            tailcall(processBranch(Branch(secondLeft, secondRight))).flatMap(secondVal =>
              done(Branch(firstVal, secondVal))
            )
          )
      }

      tailRecImpl(a).result
    }

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = (ff, fa) match {
      case (Leaf(f), tree)                                    => flatMap(tree)(value => Leaf(f(value)))
      case (Branch(left, _), Leaf(_))                         => ap(left)(fa)
      case (Branch(fLeft, fRight), Branch(valLeft, valRight)) => Branch(ap(fLeft)(valLeft), ap(fRight)(valRight))
    }

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = flatMap(fa)(value => Leaf(f(value)))
  }

}
