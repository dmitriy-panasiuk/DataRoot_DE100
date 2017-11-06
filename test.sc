import scala.io.Source

// #1
def solution1() = {
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList
  val message = "GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM"
  val foundKey = 7
  val url = "https://sites.google.com/datarootlabs.com/dru-w3-q1/"

  def decodeChar(c: Char, key: Int) = {
    val index = alphabet indexOf c
    if (index != -1) alphabet((index + key) % alphabet.size) else c
  }

  def decodeMessage(key: Int): String =
    message.map(c => decodeChar(c, key))

  url + decodeMessage(foundKey).toLowerCase().split(" ").toList.map(s => s(0)).mkString("")
}

println(s"#1 - ${solution1()}")


def solution2() = {
  val url = "https://sites.google.com/datarootlabs.com/dru-w3-q1/"
  val fileUrl = "https://drive.google.com/uc?export=download&id=0Bw8apsd2PoTYRERMNWVGc044VW8"
  val numbers = Source.fromURL(fileUrl).getLines.toList
  val n1 = numbers(0).reverse
  val n2 = ("0" * (numbers(0).length - numbers(1).length) + numbers(1)).reverse

  def helper(pos: Int, carry: Int, acc: List[Char]): List[Char] = {
    if (pos >= n1.length) if (carry == 0) acc else '1' :: acc else {
      val f = n1(pos) - '0'
      val s = n2(pos) - '0'
      val sum = ((f + s + carry) % 2 + '0').toChar
      val new_carry = (f + s + carry) / 2
      helper(pos + 1, new_carry, sum :: acc)
    }
  }

  val sum = helper(0, 0, List())
  url + (sum.count(c => c == '1') - sum.count(c => c == '0'))
}
println(s"#2 - ${solution2()}")

// #3

def solution3() = {
  val url = "https://sites.google.com/datarootlabs.com/dru-w3-q1/"
  val count = 73

  def isPalindrome(x: Int) = x.toBinaryString == x.toBinaryString.reverse

  def helper(i: Int, counter: Int, acc: Int): Int = {
    if (counter > count) acc
    else if (isPalindrome(i)) helper(i + 1, counter + 1, acc + i)
    else helper(i + 1, counter, acc)
  }

  url + helper(1, 1, 0)
}

println(s"#3 - ${solution3()}")

// #4

def solution4() = {
  val url = "https://sites.google.com/datarootlabs.com/dru-w3-q1/"
  val arr: List[Int] = List(-1, -1, -2, -2, 1, -5, 1, 0, 1, 14, -8, 4, 5, -11, 13, 5, 7, -10, -4, 3, -6, 8, 6, 2, -9, -1, -4, 0)

  def countZeroSums(comb: List[List[Int]], counter: Int): Int = comb match {
    case List() => counter
    case x :: xs => if (x.sum == 0) countZeroSums(xs, counter + 1) else countZeroSums(xs, counter)
  }

  url + countZeroSums(arr.combinations(3).toList, 0)
}

println(s"#4 - ${solution4()}")

// #5

def solution5() = {
  val url = "https://sites.google.com/datarootlabs.com/dru-w3-q1/"
  val filename = "https://drive.google.com/uc?export=download&id=0Bw8apsd2PoTYb05lVk0tbzFlZzg"
  val numbers = Source.fromURL(filename).getLines.map(s => BigInt(s)).toList

  url + numbers.sum.toString.substring(0, 10)
}

println(s"#5 - ${solution5()}")