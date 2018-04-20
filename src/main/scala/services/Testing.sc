def DoubleDigits(num: Int): List[Int] = {
  val x = (num toString).toList map(_.toString) map(_.toInt)
  x
}

val test = DoubleDigits(100)


test(2)


val z = test mkString("")

def Remove1stItem(x: List[Int]): List[Int] = {
  val z = x.tail
  z
}

val u = Remove1stItem(test)

val y = u mkString("")

y.toInt