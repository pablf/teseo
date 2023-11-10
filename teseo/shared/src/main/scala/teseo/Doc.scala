package teseo

/**
 * Represents the documentation of an algorithm.
 */
class Doc {

  import Doc.*

  /**
   * Adds a line to last paragraph of documentation.
   */
  def addLine(line: String): Unit =
    content.lastOption.map(_.addLine(line))

  /**
   * Adds a line at the end of paragraph with given title.
   *
   * @param line
   *   String added.
   * @param nameOfParagraph
   *   Title of the paragraph.
   * @param onlyFirst
   *   `true` to add only in the first paragraph with said name, `false` to add to all paragraphs.
   */
  def addLineIn(line: String, nameOfParagraph: String, onlyFirst: Boolean = true): Unit = {

    def addAtFirst(contentAux: List[Paragraph]): Unit =
      contentAux.headOption match {
        case Some(par) if par.title == Some(nameOfParagraph) => par.addLine(line)
        case None                                            => ()
        case _                                               => addAtFirst(contentAux.tail)
      }

    if onlyFirst then addAtFirst(content)
    else
      content.map {
        case par if par.title == Some(nameOfParagraph) => par.addLine(line)
        case other                                     => other
      }
  }

  /**
   * Adds a new paragraph at the end of documentation.
   */
  def addParagraph(title: String, lines: List[String]): Unit =
    content = content ++ List(Paragraph(Some(title), lines))

  /**
   * Adds a new paragraph at the end of documentation.
   */
  def addParagraph(paragraph: Paragraph): Unit =
    content = content ++ List(paragraph)

  /**
   * Applies a function to all paragraphs.
   *
   * @return
   *   Returns the list of paragraphs with the function applied.
   */
  def map[A](f: Paragraph => A): List[A] = content.map(f)

  /**
   * Searchs first ocurrence of a paragraph named `title`.
   */
  def search(title: String): Option[Paragraph] =
    content
      .filter(_.title == Some(title))
      .headOption

  /**
   * Searchs all ocurrences of a paragraph named `title`.
   */
  def searchAllOcurrences(title: String): List[Paragraph] =
    content.filter(_.title == Some(title))

  override def equals(x: Any): Boolean = x match {
    case doc: Doc => doc.name == name && doc.content == content
    case _        => false
  }

  /**
   * Returns a `String` with the content of the `Doc`.
   */
  override def toString(): String = (name, content) match {
    case (None, Nil)    => ""
    case (Some(n), Nil) => n
    case (None, c)      => c.map(_.string("")).mkString("\n")
    case (Some(n), c)   => (n :: c.map(_.string("   "))).mkString("\n")
  }

  /**
   * Prints a `String` with the content of the `Doc`.
   */
  def printDoc(): Unit = print(toString())

  /**
   * Prints a `String` with the content of the `Doc`.
   */
  def printlnDoc(): Unit = println(toString())

  private[teseo] var name: Option[String]     = None
  private[teseo] var content: List[Paragraph] = Nil

}

object Doc {

  def apply(): Doc = new Doc

  def apply(docName: Option[String]): Doc = new Doc {

    name = docName

  }

  def apply(lines: List[String]): Doc = new Doc {

    content = List(Paragraph(None, lines))

  }

  def apply(docContent: List[Paragraph], docName: Option[String]): Doc = new Doc {

    name = docName

    content = docContent

  }

  def apply(title: String, lines: List[String]): Doc = new Doc {

    content = List(Paragraph(Some(title), lines))

  }

  class Paragraph {

    def addLine(line: String): Unit =
      content = content ++ List(line)

    private[teseo] var title: Option[String] = None
    private[teseo] var content: List[String] = Nil

    def map[A](f: String => A): List[A] = content.map(f)

    private def processBody(body: List[String], margin: String): String =
      body.map(l => margin ++ "   " ++ l).mkString("\n")

    override def equals(x: Any): Boolean = x match {
      case par: Paragraph => par.title == title && par.content == content
      case _              => false
    }

    override def toString(): String = string("")

    def string(margin: String): String =
      title match {
        case Some(title) =>
          val body = processBody(content, margin ++ "   ")
          if body == "" then margin ++ title else margin ++ title ++ "\n" ++ body

        case None => processBody(content, margin)
      }

  }

  object Paragraph {

    def apply(t: Option[String], c: List[String]) = new Paragraph {

      title = t

      content = c

    }

  }

}
