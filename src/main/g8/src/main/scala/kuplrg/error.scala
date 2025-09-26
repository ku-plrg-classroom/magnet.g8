package kuplrg

case class PLError(msg: String) extends Exception(msg)

def error(): Nothing = error("")

def error(msg: String): Nothing = throw PLError(msg)
