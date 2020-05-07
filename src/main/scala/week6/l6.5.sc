import scala.io.Source
import scala.List

/*
def invertMaping(map: (Char, String)): Map[Char, Char] = {
  val (num, chars) = map
  (for (char <- chars) yield char -> num).toMap
}
val charCode: Map[Char, Char] = mnem flatMap invertMaping
*/


val in = Source.fromURL("https://www.epfl.ch/labs/lamp/wp-content/uploads/2019/01/linuxwords.txt")
// Strings are iterators. Convert to List to gain access to more methods (groupBy)
val words = in.getLines.toList filter (word => word forall (_.isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")


val charCode: Map[Char, Char] =
  for ((num, chars) <- mnem; char <- chars) yield char -> num

def wordCode(word: String): String = // maps word (string) to integer sequence
  word.toUpperCase map charCode


val wordsForNum: Map[String, Seq[String]] = // map all integer sequences to their words
  words groupBy wordCode withDefaultValue Seq() // if an integer doesn't map, return empty sequence. Good for combining things later


def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length // split number at every possible place
      word <- wordsForNum(number take split) // every word found matching first split nums
      rest <- encode(number drop split) // remaining possible words
    } yield word :: rest
  }.toSet

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")
translate("963")



