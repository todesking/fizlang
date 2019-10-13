package com.todesking.fizlang

sealed abstract class Trampoline[+A] {
  import Trampoline.{Done, More, FlatMap}

  final def runT: A = resume match {
    case Right(x) => x
    case Left(k)  => k().runT
  }
  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(x) => Right(x)
    case More(k) => Left(k)
    case FlatMap(t, f) =>
      t match {
        case Done(x) => f(x).resume
        case More(k) => Left(() => k().flatMap(f))
        case FlatMap(u, g) =>
          u.flatMap((x: Any) => g(x)).flatMap(f).resume
      }
  }
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case FlatMap(t, k) =>
      FlatMap(t, (x: Any) => k(x).flatMap(f))
    case _ => FlatMap(this, f)
  }
  def map[B](f: A => B): Trampoline[B] =
    flatMap { x =>
      Done(f(x))
    }
}
object Trampoline {
  case class Done[A](value: A) extends Trampoline[A]
  case class More[A](k: () => Trampoline[A]) extends Trampoline[A]
  case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B])
      extends Trampoline[B]
}
