package hierarchy

import cats.laws.discipline.{ApplicativeTests, ApplyTests, FlatMapTests, FunctorTests, MonadTests}
import hierarchy.CatsInstances._
import hierarchy.TypeClasses._
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline


class TypeClassesSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(
    minSuccessful = PosInt(100)
  )

  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
  checkAll("Tree.ApplicativeLaws", ApplicativeTests[Tree].applicative[Int, Int, String])
  checkAll("Tree.ApplyLaws", ApplyTests[Tree].apply[Int, Int, String])
  checkAll("Tree.FlatMapLaws", FlatMapTests[Tree].flatMap[Int, Int, String])
  checkAll("Tree.MonadLaws", MonadTests[Tree].monad[Int, Int, String])
}
