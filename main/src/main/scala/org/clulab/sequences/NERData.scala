package org.clulab.sequences

import org.clulab.dynet.Utils

import java.io.{File => JFile}
import better.files.File
import File._
import scala.util.{Try, Success, Failure}

//import org.clulab.sequences.LexiconNER
import org.clulab.processors.Document
import org.clulab.processors.clu.CluProcessor

object NERData extends App {
  // needed assets
  val proc = new CluProcessor()
  val ner = LexiconNER(Seq("lexicon/Gene_or_gene_product.tsv"))

  /** get a list of txtfiles recursively */
  def getTxtFiles(path: String): Iterator[File] = {
    val folder = File(path)
    folder.listRecursively.filter(_.extension.getOrElse("unknown") == ".txt")
  }

  /** get NER, where each sentence is an entire string */
  private def getNER(doc: Document): Array[Array[String]] = {
    doc.sentences.map(s => ner.find(s))
  }

  val outFile: File = home / "repos" / "research" / "data" / "proteins-NER.conl"
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
      // get annotation
      val anns = getNER(doc)
      for ((entities, words) <- anns zip doc.sentences.map(_.words)  ) {
        if (entities.filter(_ == "O").nonEmpty) {
          // get BIO format
          val annotations = words.zip(entities).map(s=>s._1 + " " + s._2).mkString("\n") + "\n"
          outFile.appendLine(annotations)
        }
      }
    } catch {
      case e: Exception => println(s"could not process file ${f}")
    }
  }
}
