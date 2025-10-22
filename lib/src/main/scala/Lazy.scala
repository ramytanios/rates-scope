package lib

class Lazy[V](v: => V):

  lazy val value: V = v
