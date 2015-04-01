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
package bonsai

import play.api.libs.json._
import scala.io.Source

/**
 * Main app to manage json encoded decision tree model.
 */
object Bonsai extends App {
  args.toList match {
    // pretty-print a json file
    case "-p" :: inputFile :: Nil => {
      processModel(inputFile, prettyPrint = true)
    }
    // evaluate a model on a test-input file
    case "-e" :: inputFile :: dataFile :: Nil => {
      processModel(inputFile, modelDataFilename = Some(dataFile))
    }
    // generate C++ code for the model
    case "-c" :: modelFiles => {
      modelFiles foreach { jsonFilename => processModel(jsonFilename, Some(".")) }
      println()
    }
    // do all: generate C++ code for all json models in given folder, validate models on tsv sample file.
    case "-a" :: modelsFolder :: Nil => {
      new java.io.File(modelsFolder).listFiles.filter(_.getName.toLowerCase.endsWith(".json"))
        .map({ jsonFile =>
          processModel(jsonFile.getCanonicalPath(), outCppFolder = Some(modelsFolder), modelDataFilename = Some(jsonFile.getCanonicalPath().replaceAll("\\.json$", ".tsv")))
          println()
        })
    }
    case _ => {
      System.err.println("Usage: \n" +
        " to pretty-print a json moodel: Bonsai -p modelFile\n" +
        " to evaluate a model over a test file: Bonsai -e modelFile dataFile\n" +
        " to generate C++ code: Bonsai -c (modelFile)*\n" +
        " to do all (generate *.cpp for all *.json models and evaluate them on *.tsv files): Bonsai -a folderWithFiles")
    }
  }

  /**
   * All model processing is here. Leave out operations where corresponding input/output parameter is None.
   */
  def processModel(modelJsonFilename: String, outCppFolder: Option[String] = None, modelDataFilename: Option[String] = None, prettyPrint: Boolean = false) = {
    println(s"Processing model: ${modelJsonFilename}")

    // parse model json file
    val json = Json.parse(fixAllScientificNotations(Source.fromFile(modelJsonFilename).mkString))
    val m = Model.fromJSON(json)

    // write out CPP code into a file
    if (outCppFolder.isDefined) {
      val resultFilename = new java.io.File(outCppFolder.get, s"model_${m.name}.cpp").getCanonicalPath()
      val p = new java.io.PrintWriter(resultFilename)
      p.print(generateCModel(m) + "\n")
      p.close()
      println(s"Converted to C++ file: ${resultFilename}")
    }

    // evaluate model and validate it against specified output values
    if (modelDataFilename.isDefined) {
      if (!new java.io.File(modelDataFilename.get).exists)
        println("Validation not performed. Missing validation file: " + modelDataFilename.get)
      else {
        println(s"Validating on file: ${modelDataFilename.get}")
        val tsv = TestFile.readFile(modelDataFilename.get)
        val stat = tsv.map({ value: Map[String, Double] =>
          val calc = m.eval(value)
          val orig = value("model") / 1e6
          val diff = orig - calc
          if (Math.abs(diff) > 1e-13) {
            println("!Large Diff:" + diff, "Eval:" + m.eval(value), "Orig:" + value("model") / 1e6)
            1
          } else
            0
        }).toList.map(x => (x, 1)).foldLeft((0, 0))((a, b) => (a._1 + b._1, a._2 + b._2))
        if (stat._1 == 0)
          println(s"Validation on ${stat._2} test cases SUCCEEDED with 0 errors.")
        else
          println(f"Validation FAILED with ${100 * stat._1.toDouble / stat._2}%2.2f%% error rate, i.e. ${stat._1} of ${stat._2} test cases failed.")
      }
    }

    // pretty-print a json file
    if (prettyPrint)
      println(Json.prettyPrint(json))
  }

  def fixAllScientificNotations(x: String) = x.replaceAll("(\\d+)(e|E)\\+(\\d+)", "$1E$3") // 1e+2 is not accepted by the parser ("+" must be removed)
  
  final val ind = "    "

  /**
   * Starting from a boosted tree model generates C++ code.
   * PLEASE NOTE: MLR models are 3-way decision trees (true branch, false branch and indefinite branch),
   * but this function only generates code for the true and false branches, thus turning the model into
   * 2-ways tree.
   */
  def generateCModel(m: Model): String = {
      m.usedVariables.map({
        variable =>
          if (m.usedCategoricalVariables.contains(variable))
            ind + "uint64_t " + variable + "_catbit = <code to retrieve your values>;"
          else
            ind + "float " + variable + " = <code to retrieve your values>;"
      }).mkString("\n") +
      "\n\n" +
      ind + "/* Return value. */\n" + ind + "double net_response = 0.0;\n\n" +
      (m.trees zip (1 to m.trees.length) map { case (tree, i) => ind + "/* Tree " + i + " of " + m.trees.length + ". */\n" + generateC(tree.top) }).mkString("\n") +
      "\n" + ind + "return net_response;\n" +
      "}\n"
  }

  /**
   * recursive function to generate C++ code from a decision tree node.
   */
  def generateC(node: Node, indent: String = ind): String = {
    node match {
      case LessThan(variable, threshold, trueBranch, falseBranch, missingBranch) =>
        indent + "if (" + variable + " < " + threshold + "F) {\n" +
          generateC(trueBranch, indent + ind) +
          indent + "} else {\n" +
          generateC(falseBranch, indent + ind) +
          indent + "}\n"
      case IsIn(variable, catSet, trueBranch, falseBranch, missingBranch) =>
        if (!catSet.forall(level => level >= 0 && level <= 63))
          throw new Error("Invalid categorical variable level in condition. " + catSet.mkString("[", ",", "]"))
        val catBitMask = catSet.toList.sorted.distinct.grouped(12).map(_.mkString("CAT_BMASK(", ", ", ")")).toList
        val catBitMaskStr = if (catBitMask.size > 1) catBitMask.mkString("(", " | ", ")") else catBitMask.mkString("")
        indent + "if ((" + variable + "_catbit & " + catBitMaskStr + ") != 0) {\n" +
          generateC(trueBranch, indent + ind) +
          indent + "} else {\n" +
          generateC(falseBranch, indent + ind) +
          indent + "}\n"
      case Leaf(value) => indent + "net_response += " + value + ";\n"
    }
  }
}

/**
 * Small utility to read test files for regression testing.
 */
object TestFile {
  def readFile(filename: String): Seq[Map[String, Double]] = {
    val lines = Source.fromFile(filename).getLines
    lines map {
      _.split("\\s+") map {
        pair =>
          val arr = pair.split(":")
          (arr(0), arr(1).toDouble)
      } toMap
    } toSeq
  }
}
