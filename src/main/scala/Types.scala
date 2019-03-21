object Types {
  class Token(token: String) {
    private def untag: (String, Identifier) = {
      val separatorIdx = token.indexOf('_')
      if (separatorIdx == -1) (token, None())
      else {
        val word: String = token.substring(0, separatorIdx)
        val tag: String = token.substring(separatorIdx + 1)
        (word, Tag(tag))
      }
    }

    val word: String = untag._1
    val identifier: Identifier = untag._2
    val partOfSpeech: PartOfSpeech =
      identifier match {
        case Tag(tag) if tag.charAt(0) >= 33 && tag.charAt(0) <= 47 =>
          Punctuation(word)
        case Tag(tag) if tag.charAt(0) == 'V' => VerbConjugation(word)
        case Tag(tag) if tag.equals("MD") => VerbConjugation(word)
        case Tag(_) if word.length() > 3 =>
          if (word.substring(word.length() - 3).equals("ing"))
            VerbConjugation(word)
          else if (word.substring(word.length() - 2).equals("ed"))
            VerbConjugation(word)
          else NonVerb(word)
        case Tag(_) => NonVerb(word)
        case None() => Other(word)
      }
  }

  abstract class Identifier
  case class Tag(tag: String) extends Identifier
  case class None() extends Identifier

  abstract class PartOfSpeech
  case class VerbConjugation(word: String) extends PartOfSpeech
  case class Punctuation(punc: String) extends PartOfSpeech
  case class NonVerb(word: String) extends PartOfSpeech
  case class Other(raw: String) extends PartOfSpeech
}
