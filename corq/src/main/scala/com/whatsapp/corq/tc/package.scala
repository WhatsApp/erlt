package com.whatsapp.corq

import erlang.CErl._
import erlang.Data._

/**
  * TODO: maybe use subtyping in CConvert and delete this
  */
package object tc {
  object LitAtom {
    def unapply(cerl: CErl): Option[String] =
      cerl match {
        case CLiteral(_, EAtom(str)) => Some(str)
        case _                       => None
      }
  }
  object ErlangCall {
    def unapply(cerl: CErl): Option[(String, List[CErl])] =
      cerl match {
        case CCall(_, LitAtom("erlang"), LitAtom(op), args) => Some(op, args)
        case _                                              => None
      }
  }
}
