package wordcounter

import java.nio.file.Paths

import scala.io.Source
import scala.util.matching.Regex

object WordCounter {

  val words: Regex = """(\w+)""".r

  def prettyPrint(m : Map[String, Int]) : Unit = {
    def format(t: (String, Int)) : Unit = t match {
      case (w, c) => println(s"$w: $c")
    }
    m.toList.sortWith(_._2 > _._2).foreach(format)
  }

  def main(args: Array[String]): Unit = {
    // could pass the file location as positional argument to main
    // but I added the source text as a resource
    val path = Paths.get(getClass.getResource("/wordcounter/sample.txt").toURI).toString

    // lazy read from the file with Source.fromFile
    val data = Source.fromFile(path).getLines().foldLeft(Map[String, Int]()) {
      (acc1, line) =>
        // findAllMatchIn returns an iterator
        words.findAllMatchIn(line.toLowerCase).foldLeft(acc1) {
          (acc2, word) =>
            val w = word.toString()
            if (acc2.contains(w)) acc2 + (w -> (acc2(w) + 1)) else acc2 + (w -> 1)
        }
    }
    prettyPrint(data)
  }
}