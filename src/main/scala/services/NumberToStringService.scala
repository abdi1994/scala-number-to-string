package services

object NumberToStringService {

  def numberToString(num: Int): String =
    if(num >= 10000 || num <= -1 ) {
      "out of range"
    }else {
      val length = LengthDecider(num)
      length match {
        case 1 => SingleDigit(num)
        case 2 => upToHundred(num)
        case 3 => hundred(num)
        case 4 => thousand(num)
      }
    }

  def SingleDigit(num: Int): String = {
    val length = LengthDecider(num)
    num match {
      case 0 => if(length == 1) {
        "zero"
      }else {
        ""
      }
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
    }
  }

  def lessThanTwenty(num: Int): String = {
    num match {
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 14 => "fourteen"
      case 15 => "fifteen"
      case 16 => "sixteen"
      case 17 => "seventeen"
      case 18 => "eighteen"
      case 19 => "ninteen"
    }
  }

  def upToHundred(num: Int): String = {
    val number = numberToList(num)
    val newNumber = Remove1stItem(number)
    val newListToInt = listToInt(newNumber)

    if(newListToInt == 0) {
      upToHundredCollection(number.head)
    }else if (number(0) == 1) {
      lessThanTwenty(num)
    }else {
      upToHundredCollection(number.head) + " " + SingleDigit(newListToInt)
    }
  }

  def upToHundredCollection(num: Int): String = {
    num match {
      case 1 => lessThanTwenty(num)
      case 2 => "twenty"
      case 3 => "thirty"
      case 4 => "forty"
      case 5 => "fifty"
      case 6 => "sixty"
      case 7 => "seventy"
      case 8 => "eighty"
      case 9 => "ninety"
    }
  }


  def hundred(num: Int): String = {
    val number = numberToList(num)
    val newNumber = Remove1stItem(number)
    val newListToInt = listToInt(newNumber)
    if(newListToInt == 0) {
      SingleDigit(number.head) + " hundred"
    } else if(number(1) == 0) {
      SingleDigit(number.head) + " hundred" + " and " + SingleDigit(number(2))
    }
    else {
      SingleDigit(number.head) + " hundred" + " and " + upToHundred(newListToInt)
    }
  }

  def thousand(num: Int): String = {
    val number = numberToList(num)
    val newNumber = Remove1stItem(number)
    val newListToInt = listToInt(newNumber)
    if(newListToInt == 0) {
      SingleDigit(number.head) + " thousand"
    } else if (newListToInt < 10) {
      SingleDigit(number.head) + " thousand" + " and " + SingleDigit(newListToInt)
    } else if (newListToInt < 100){
      SingleDigit(number.head) + " thousand" + " and " + upToHundred(newListToInt)
    } else if (newListToInt < 1000){
      SingleDigit(number.head) + " thousand " + hundred(newListToInt)
    } else {
      "out of range"
    }
  }

  def LengthDecider(num: Int): Int = {
    val x = (num toString).toList map(_.toString) map(_.toInt)
    x.length
  }

  def numberToList(num: Int): List[Int] = {
    val x = (num toString).toList map(_.toString) map(_.toInt)
    x
  }

  def listToInt(listInt : List[Int]): Int = {
    val toString = listInt mkString
    val toInt = toString.toInt
    toInt
  }

  def Remove1stItem(x: List[Int]): List[Int] = {
    val z = x.tail
    z
  }

}
