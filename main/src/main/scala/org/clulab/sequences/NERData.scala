package org.clulab.sequences

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor

object NERData extends App {
  val proc = new CluProcessor()
  val ner = LexiconNER(Seq("lexicon/Gene_or_gene_product.tsv"))
    val doc = proc.mkDocument("whatever")
    for(sent <- doc.sentences) {
      val labels = ner.find(sent)
      println(labels.mkString(", "))
    }
}
