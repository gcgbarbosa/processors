package org.clulab.sequences

//import org.clulab.dynet.Utils

import java.io.{File => JFile}
import better.files.File
import File._
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ArrayBuffer

//import org.clulab.sequences.LexiconNER
import org.clulab.processors.Document
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.Sentence

object NERData extends App {
  def validMatch(sentence: Sentence, start: Int, end: Int): Boolean = {
    assert(start < end)

    // must contain at least one NN*
    // see also removeSinglePrepositions, for deprecated code
    /*
    var nouns = 0
    for(i <- start until end)
      if(sentence.tags.get(i).startsWith("NN"))
        nouns += 1
    if(nouns == 0) {
      return false
    }
     */
    // some entities end with -ing verbs (e.g., "binding")
    // do not accept them when followed by "to"
    // TODO: anything else?
    if (end < sentence.words.length) {
      val last = sentence.words(end - 1)
      val to = sentence.words(end)
      if (
        last.length > 3 && last.toLowerCase.endsWith(
          "ing"
        ) && to.toLowerCase == "to"
      ) {
        return false
      }
    }

    true
  }

  /** Receive the start index of the entity span and returns the end index */
  def findEntityEnd(startFrom: Int, list: Seq[String]) = {
    // sanity check
    assert(list(startFrom).startsWith("B-"))
    val entityType = list(startFrom).drop(2)
    //
    var i = startFrom + 1
    // assume list=O, B-foo, I-foo, I-foo, O
    // i=1
    // returns 4
    while (i < list.length && list(i) == "I-" + entityType) i = i + 1
    i
  }

  // needed assets
  //org.clulab.dynet.Utils.initializeDyNet()
  val proc = new CluProcessor()
  val ner = LexiconNER(Seq("lexicon/Gene_or_gene_product.tsv"))

  /** get a list of txtfiles recursively */
  def getTxtFiles(path: String): Iterator[File] = {
    val folder = File(path)
    folder.listRecursively.filter(_.extension.getOrElse("unknown") == ".txt")
  }

  /** get NER, where each sentence is an entire string */
  private def getNER(doc: Document): Array[Array[String]] = {
    /* FILTER SHIT HERE INSTEAD */
    /** TODO: add filter */
    doc.sentences.map(s => {
      val ann: ArrayBuffer[String] = ner.find(s).to[ArrayBuffer]
      var i = 0
      while (i < ann.size) {
        // if this is the start of a sentence
        if (ann(i).startsWith("B-")) {
          val start = i
          val end = findEntityEnd(start, ann)
          // if this match is invalid
          //
          if (!validMatch(s, start, end)) {
            for (j <- start until end) {
              // mark shit as out
              ann(j) = "O"
            }
          }
        }
        i = i + 1
      }
      ann.to[Array]
    })
  }

  val outFile: File =
    home / "repos" / "research" / "data" / "proteins-NER.conll"
  outFile.createFile
  val txtFiles = getTxtFiles("/data/nlp/corpora/pmc_openaccess/pmc_dec2019")
  //
  for (f <- txtFiles) {
    try {
      // get text
      val txt = f.lines.mkString("\n")
      // create doc + annotate
      val doc = proc.mkDocument(txt)
      proc.lemmatize(doc)
      //proc.tagPartsOfSpeech(doc)
      // get annotation
      val anns = getNER(doc)
      for ((entities, sentence) <- anns zip doc.sentences) {
        if (entities.filter(_ == "O").nonEmpty) {
          val annotations = sentence.words
            .zip(entities)
            .map(s => s._1 + " " + s._2)
            .mkString("\n") + "\n"
          outFile.appendLine(annotations)
        }
      }
    } catch {
      case e: Exception => println(s"could not process file ${f}")
    }
  }
}
