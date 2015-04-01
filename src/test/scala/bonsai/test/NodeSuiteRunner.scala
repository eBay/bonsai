/**
 * Copyright 2012 eBay Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package bonsai.test

import scala.io.Source

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import bonsai.Leaf
import bonsai.LessThan
import bonsai.IsIn
import bonsai.Model
import bonsai.Tree

import play.api.libs.json._

object NodeSuiteRunner {
  def main(args: Array[String]) = {
    (new NodeSuite).execute()
    (new JsonSuite).execute()
  }
}

@RunWith(classOf[JUnitRunner])
class NodeSuite extends FunSuite {
  val features = Map[String, Double]("f1" -> 0, "f2" -> 1)

  test("A leaf node returns a value") {
    val onlyLeaf = new Tree(Leaf(0.0))
    expectResult(0.0) {
      onlyLeaf.eval(features)
    }
  }

  test("A LessThan node evaluates a variable") {
    val tree = new Tree(LessThan("f1", 1.0, Leaf(1), Leaf(-1), Leaf(0)))
    expectResult(1) { tree.eval(features) }
    expectResult(-1) { tree.eval(Map("f1" -> 2)) }
  }

  test("A IsIn node evaluates a categorical variable") {
    val tree = new Tree(IsIn("f2", Set(1), Leaf(1), Leaf(-1), Leaf(0)))
    expectResult(1) { tree.eval(features) }
    expectResult(-1) { tree.eval(Map("f2" -> 2)) }
  }

  test("A tree combines multiple branches") {
    val tree = new Tree(LessThan("f1", 3, LessThan("f1", 2, LessThan("f1", 1, Leaf(1), Leaf(0), Leaf(Double.NaN)), Leaf(0), Leaf(Double.NaN)), Leaf(0), Leaf(Double.NaN)))
    expectResult(1) { tree.eval(features) }
  }
}

@RunWith(classOf[JUnitRunner])
class JsonSuite extends FunSuite {
  test("Parsing a leaf") {
    val leaf = """{ "prediction": -0.010983234 }"""
    val leafJson = Json.parse(leaf)
    val node = Model.parseNode(leafJson)
    expectResult(Leaf(-0.010983234)) { node }
  }

  test("executing a node") {
    val cond = """{
                  "cond": {
                     "var": "VariableA",
                     "op": "<",
                     "val": 33044.5
                  },
                  "if_true": {
                     "prediction": -0.010983234
                  },
                  "if_false": {
                     "prediction": -1.1031008E-4
                  },
                  "if_missing": {
                     "prediction": -0.004943939
                  }
                 }"""

    val condJson = Json.parse(cond)
    val node = Model.parseNode(condJson)

    val tree = Tree(node)
    expectResult(-0.010983234) { tree.eval(Map("VariableA" -> 0.0)) }
    expectResult(-1.1031008E-4) { tree.eval(Map("VariableA" -> 33045.0)) }
    expectResult(-0.004943939) { tree.eval(Map("VariableA" -> Double.NaN)) }

  }

  test("parsing a tree") {
    val input = Source.fromFile("testmodels/model.json").mkString
    val json = Json.parse(input)
    val trees = (json \ "trees").asInstanceOf[JsArray]
    val aTree = trees(0)
  }

  test("reading and parsing of a model") {
    val model = Utils.readModel("testmodels/model.json")
    expectResult(100) { model.ntrees }
    expectResult("model") { model.name }
  }

  test("compare model with test cases") {
    Utils.testModel("testmodels/model")
  }

  test("compute variables contribution") {
    val cond = """{
                  "cond": {
                     "var": "variable1",
                     "op": "<",
                     "val": 33044.5
                  },
                  "if_true": {
                     "prediction": -0.010983234
                  },
                  "if_false": {
                     "prediction": -1.1031008E-4
                  },
                  "if_missing": {
                     "prediction": -0.004943939
                  }
                 }"""

    val condJson = Json.parse(cond)
    val node = Model.parseNode(condJson)

    val model = Model(trees = List(Tree(node)), usedVariables = List("variable1"))
    val x1 = Map("variable1" -> 0.0)
    val x2 = Map("variable1" -> 33045.0)
    val contribution = Map("variable1" -> (-1.1031008E-4 + 0.010983234))
    expectResult((contribution, contribution)) { model.contribution(x1, x2) }
  }
}

object Utils {
  def readModel(filename: String) = {
    val input = Source.fromFile(filename).mkString
    Model.fromJSON(Json.parse(input))
  }

  def readTestFile(filename: String): Seq[Map[String, Double]] = {
    val lines = Source.fromFile(filename).getLines
    lines map {
      _.split("\\s+") map {
        pair =>
          val arr = pair.split(":")
          (arr(0), arr(1) match { case "NA" => Double.NaN; case _ => arr(1).toDouble })
      } toMap
    } toSeq
  }
  def testModel(name: String) {
    val input = Source.fromFile(name + ".json").mkString
    val model = Model.fromJSON(Json.parse(input))
    val testSet = Utils.readTestFile(name + ".tsv")
    for (testCase <- testSet) {
      val error = scala.math.abs(testCase("model") / 1e6 - model.eval(testCase))
      // assume that we get pretty close to the test case
      assert(error < 1e-12, error)
    }
  }
}

