object States extends Enumeration {
  type State = Value

  val Empty = Value(0, " ")
  val X = Value(1, "X")
  val O = Value(2, "O")
}
