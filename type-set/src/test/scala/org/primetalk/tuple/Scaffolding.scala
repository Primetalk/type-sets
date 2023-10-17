package org.primetalk.tuple

trait Abcd:
  sealed trait e
  case object a extends e
  type a = a.type
  case object b extends e
  type b = b.type
  case object c extends e
  type c = c.type
  case object d extends e
  type d = d.type


