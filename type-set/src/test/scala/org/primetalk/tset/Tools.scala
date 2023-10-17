package org.primetalk.tset

inline def assertTrue[T <: true]: true =
  true
inline def assertFalse[T <: false]: false =
  false

inline transparent def constInt[T <: Int]: Int =
  scala.compiletime.constValue[T]
