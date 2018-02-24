package week06

import scala.io.{BufferedSource, Source}

/**
  * Created by Yuqi Li on 2018/2/24.
  */
object Mnemonics {
  val in: BufferedSource = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

  val words: List[String] = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  /**
    * Invert the mnem map to give a map from chars: letters to numbers
    */
  val charCode:Map[Char,Char] =
    for{
      (digit,string) <- mnem
      letters <- string
    }yield letters -> digit

  /**
    * Maps a word to the digit string it can represent: "Java" -> "5285"
    * charCode is a function itself
    */
  def wordCode(word:String):String = {
    word.toUpperCase.map(charCode)
  }

  /**
    * A map from digit strings to the words that represent them
    * e,g. "5282" -> List("Java", "Kata", "Lava", ...)
    * A missing number should map to the empty set  "1111" -> List()
    * groupBy takes a f that will transfer the object(List) into an another state, then combine as a Map
    * the wordCode function is "Words" -> "Number", so the result of groupBy will treat "Number" as the key
    * to generate key value pairs.
    */
  val wordsForNum:Map[String,Seq[String]] = words.groupBy(wordCode) withDefaultValue Seq()

  /**
    * returns all ways to encode a number as a list of words
    */
  def encode(number:String) :Set[List[String]] = {
    if(number.isEmpty) Set(List())
    else {
      for{
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
  }

  /**
    * translate several words into phrases
    * @param number
    * @return
    */
  def translate(number:String):Set[String] = {
    encode(number) map ( _ mkString " ")
  }
}
