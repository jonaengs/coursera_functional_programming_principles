package patmat

import scala.annotation.tailrec

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }
  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees
  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    @tailrec
    def timesAcc(chars: List[Char], charCounts: List[(Char, Int)]): List[(Char, Int)] = chars match {
      case Nil => charCounts
      case c :: cs => timesAcc(cs, addCount(c, charCounts))
    }
    def addCount(char: Char, charCounts: List[(Char, Int)]): List[(Char, Int)] = charCounts match {
      case (c, n) :: ccs =>
        if (c == char) (c, n+1) :: ccs
        else (c, n) :: addCount(char, ccs)
      case Nil => List((char, 1))
    }

    timesAcc(chars, List())
  }

  private def insert[T <: CodeTree](tree: T, trees: List[T]): List[T] = trees match {
    case List() => List(tree)
    case t :: ts =>
      if (weight(tree) <= weight(t)) tree :: trees
      else t :: insert(tree, ts)
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def sort(leaves: List[Leaf]): List[Leaf] = leaves match {
      case Nil => Nil
      case l :: ls => insert(l, sort(ls))
    }
    def makeLeaves(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
      case Nil => Nil
      case p :: ps => Leaf(p._1, p._2) :: makeLeaves(ps)
    }
    sort(makeLeaves(freqs))
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
      case t1 :: t2 :: ts => insert(makeCodeTree(t1, t2), ts)
      case _ => trees
  }

  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    if (done(trees)) trees
    else until(done, merge)(merge(trees))

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head


  // Part 3: Decoding

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    @tailrec
    def charAcc(_tree: CodeTree, bits: List[Bit], chars: List[Char]): List[Char] = _tree match {
      case Leaf(char, _) =>
        if (bits == Nil) chars ::: List(char)  // reached end of bit-code
        else charAcc(tree, bits, chars ::: List(char))
      case Fork(left, right, _, _) => bits match {
        case 0 :: bs => charAcc(left, bs, chars)
        case 1 :: bs => charAcc(right, bs, chars)
        case _ => throw new Error("Dangling bits or illegal bit-value in code")
      }
    }
    charAcc(tree, bits, List())
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def charInList(char: Char, charList: List[Char]): Boolean = charList match {
      case Nil => false
      case c :: cs =>
        if (c == char) true
        else charInList(char, cs)
    }
    @tailrec
    def bitsAcc(_tree: CodeTree, _text: List[Char], bits: List[Bit]): List[Bit] = _text match {
      case Nil => bits
      case c :: cs => _tree match {
        case Leaf(char, _) =>
          if (c == char) bitsAcc(tree, cs, bits)
          else throw new Error("Found wrong char when encoding")
        case Fork(left, right, _, _) =>
          if (charInList(c, chars(left)))
            bitsAcc(left, _text, bits ::: List(0))
          else if (charInList(c, chars(right)))
            bitsAcc(right, _text, bits ::: List(1))
          else
            throw new Error("character not found in code tree: " + c)
      }
    }
    bitsAcc(tree, text, List())
  }

  // Part 4b: Encoding using code table
  type CodeTable = List[(Char, List[Bit])]


  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case p :: ps =>
      if (p._1 == char) p._2
      else codeBits(ps)(char)
    case Nil => throw new Error("char not found in table: " + char)
  }

  def convert(tree: CodeTree): CodeTable = {
    def convertAcc(tree: CodeTree, bits: List[Bit]): CodeTable = tree match {
      case Fork(left, right, _, _) => convertAcc(left, bits ::: List(0)) ::: convertAcc(right, bits ::: List(1))
      case Leaf(char, _) => List((char, bits))
    }
    convertAcc(tree, List())
  }

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def converter: Char => List[Bit] = codeBits(convert(tree))
    @tailrec
    def encodings(chars: List[Char], bits: List[Bit]): List[Bit] = chars match {
      case Nil => bits
      case c :: cs => encodings(cs, bits ::: converter(c))
    }
    encodings(text, List())
  }

}

object Huffman extends Huffman
