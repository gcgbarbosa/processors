package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestEnhancedSemanticRoles extends FlatSpec with Matchers {
  val proc = {
    Utils.initializeDyNet()
    new CluProcessor()
  }

  "CluProcessor" should "collapse prepositions with noun objects" in {
    val doc = proc.annotate("He gave the book to Jane.")
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 3, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 5, "Ax_to") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 4, "Ax") should be(false)
  }

  it should "collapse prepositions with verb objects" in {
    val doc = proc.annotate("Food diversity increased the cost of feeding children.")
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 0, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(2, 1, "A0") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(2, 4, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(4, 6, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(6, 7, "Ax") should be(true) // TODO: this should probably be A1...
  }

  it should "propagate conjoined subjects and objects to same predicate" in {
    val doc = proc.annotate("Mary and John ate cookies and icecream.")

    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(3, 0, "A0") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(3, 2, "A0") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(3, 4, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(3, 6, "A1") should be(true)
  }

  it should "propagate subjects and objects in conjoined predicates" in {
    val doc = proc.annotate("The store buys and sells cameras.")

    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(2, 1, "A0") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(2, 5, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(4, 1, "A0") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(4, 5, "A1") should be(true)
  }

  it should "apply the deterministic predicate corrections" in {
    val doc = proc.annotate("The price of water trucking.")

    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 4, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(4, 3, "A1") should be(true)
  }
}
