package org.primetalk.tuple

inline def assertSubtype[A, B <: A]: Unit = {}

inline def assertTrue[T <: true]: Unit   = assertSubtype[true, T]
inline def assertFalse[T <: false]: Unit = assertSubtype[false, T]

inline transparent def constInt[T <: Int]: Int =
  scala.compiletime.constValue[T]
