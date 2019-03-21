import java.io.File
import java.io.PrintStream
import java.util.Scanner
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import Constants._
import Types._

object VerbEraser {
  private def readLines(filePath: String): List[String] = {
    def readLine(list: List[String], scanner: Scanner): List[String] =
      if (scanner.hasNextLine()) readLine(scanner.nextLine() :: list, scanner)
      else list

    def flip[A](list: List[A]): List[A] = {
      def flipIter(left: List[A], right: List[A]): List[A] =
        left match {
          case head :: tail => flipIter(tail, head :: right)
          case Nil => right
        }

      flipIter(list, Nil)
    }

    val scanner = new Scanner(new File(filePath))
    flip(readLine(Nil, scanner))
  }

  private def tagLines(list: List[String]): List[String] = {
    val tagger = new MaxentTagger(taggerPath)
    list.map(line => tagger.tagString(line))
  }

  private def getPartsOfSpeech(list: List[String]): List[List[PartOfSpeech]] = {
    val linesOfStrings: List[List[String]] = list.map(line => line.split(' ').toList)
    val linesOfTokens: List[List[Token]] = linesOfStrings.map(line => line.map(word => new Token(word)))
    linesOfTokens.map(line => line.map(token => token.partOfSpeech))
  }

  private def reconstructLines(linesOfPOS: List[List[PartOfSpeech]]): List[String] = {
    def prependSpace(string: String): String = " " + string
    val blank: String = "________"

    def foldLogic(accumulator: String, partOfSpeech: PartOfSpeech): String =
      if (accumulator != "")
        partOfSpeech match {
          case NonVerb(word) => accumulator + prependSpace(word)
          case VerbConjugation(_) => accumulator + prependSpace(blank)
          case Punctuation(punc) => accumulator + punc
          case Other(raw) => accumulator + prependSpace(raw)
        }
      else
        partOfSpeech match {
          case NonVerb(word) => accumulator + word
          case VerbConjugation(_) => accumulator + blank
          case Punctuation(punc) => accumulator + punc
          case Other(raw) => accumulator + raw
        }

    linesOfPOS.map(line => line.foldLeft("")(foldLogic))
  }

  def run = {
    val lines: List[String] = readLines(inputPath)
    val taggedLines: List[String] = tagLines(lines)
    val linesOfPOS: List[List[PartOfSpeech]] = getPartsOfSpeech(taggedLines)
    val reconstructedLines: List[String] = reconstructLines(linesOfPOS)

    val printStream = new PrintStream(new File(outputPath))
    reconstructedLines.foreach(line => printStream.println(line))
  }
}
