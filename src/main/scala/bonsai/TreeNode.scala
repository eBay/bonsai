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
import java.io.InputStream

/**
 * Simple implementation of a decision tree.
 * All the nodes are either: 1) Leafs, that simply returns a value, and
 * 2) Conditional nodes, that compare a variable against a condtion and execute one of the two paths accordingly.
 */
sealed abstract class Node

/**
 * Variable is identified by string. Is probably not the most efficient way to do it.
 * We should experiment with the alternative to use the index to a vector instead.
 */
case class LessThan(variable: String = "", threshold: Double = Double.NaN, trueBranch: Node, falseBranch: Node, invalidBranch: Node) extends Node

case class IsIn(variable: String = "", catSet: Set[String], trueBranch: Node, falseBranch: Node, invalidBranch: Node) extends Node

case class Leaf(prediction: Double = Double.NaN) extends Node

/**
 * Trees evaluate a set of nodes by running the conditionals on a set of features and producing
 * a score.
 */
class Tree(val top: Node) {
  def eval(features: JsObject, node: Node = top): Double = {
    node match {
      case LessThan(variable, threshold, trueBranch, falseBranch, missingBranch) =>
        (features \ variable).asOpt[Double] match {
          case None => eval(features, missingBranch)
          case Some(feature) =>
            if (feature < threshold) eval(features, trueBranch)
            else eval(features, falseBranch)
        }
      case IsIn(variable, catSet, trueBranch, falseBranch, missingBranch) =>
        (features \ variable).asOpt[String] match {
          case None => eval(features, missingBranch)
          case Some(feature) =>
            if (catSet.contains(feature)) eval(features, trueBranch)
            else eval(features, falseBranch)
        }
      case Leaf(value) => value
    }
  }
  override def toString: String = top.toString
}

object Tree {
  def apply(top: Node) = new Tree(top)
}

/** A gradient boosted tree model. It is represented as a  */
case class Model(name: String = "", distribution: String = "gaussian", classes: Seq[String] = Nil, trees: Seq[Tree], val usedVariables: Seq[String] = Nil) {
  /**
   *
   * @return number of trees in the forest.
   */
  def ntrees = trees.length

  lazy val usedCategoricalVariables: Seq[String] = {
    trees.flatMap(x => collectCatVars(x.top))
  }

  def collectCatVars(node: Node): Set[String] = {
    node match {
      case LessThan(variable, threshold, trueBranch, falseBranch, missingBranch) =>
        collectCatVars(trueBranch) ++ collectCatVars(falseBranch) ++ collectCatVars(missingBranch)
      case IsIn(variable, catSet, trueBranch, falseBranch, missingBranch) =>
        Set(variable) ++ collectCatVars(trueBranch) ++ collectCatVars(falseBranch) ++ collectCatVars(missingBranch)
      case Leaf(_) => Set()
    }
  }

  /**
   * predict function for multinomial distribution
   *
   * for multinomial in gbm, it will build n.trees * num.classes trees
   * actual trees for predicting ith class is on the index treeIndex % numClasses
   * e.x. in case of three classes.
   * 1st class trees is on tree index: 0, 3, 6, 9, 12...
   * 2nd class trees is on tree index: 1, 4, 7, 10, 13...
   * 3rd class trees is on tree index: 2, 5, 8, 11, 14...
   *
   * @param features vector of variables and values.
   * @return absolute probability for each class, Note: this is not
   * the relative probability which is returned by gbm predict when
   * specify type="response", to get the relative probability, one
   * needs to do exp(rawScore)/sum(exp(rawScore)) which is what gbm
   * returns.
   *
   */
  def evalMulti(features: JsObject): Map[String, Double] = {
    distribution match {
      case "multinomial" => {
        val scores = (classes map (c => 0.0)).toArray
        for ((tree, treeIndex) <- trees.zipWithIndex) {
          val score = tree.eval(features)
          val classIndex = treeIndex % classes.size
          scores(classIndex) += score
        }
        (for ((score, index) <- scores.zipWithIndex) yield {
          (classes(index), 1.0 / (1.0 + scala.math.exp(-score)))
        }).toMap
      }
      case _ => Map.empty
    }
  }

  /**
   *
   * @param features vector of variables and values.
   * @return score of the trees on the feature vector.
   */
  def eval(features: JsObject): Double = {
    val rawScore = trees.map(_.eval(features)).reduceLeft(_ + _)
    distribution match {
      case "gaussian"  => rawScore
      // conversion from logit to probability
      case "bernoulli" => 1.0 / (1.0 + scala.math.exp(-rawScore))
      case _           => Double.NaN
    }
  }

  /**
   * Collects all the cutpoints for the variables in the model
   * @todo implement
   */
  def cutPoints(): Map[String, List[Double]] = {
    throw new UnsupportedOperationException()
  }

  /**
   * Compute the contribution of the variables to explain the final score
   * Returns a vector containing the amount of score contributed by each variable.
   */
/*
  def contribution(x1: Map[String, JsValue], x2: Map[String, JsValue]): (Map[String, Double], Map[String, Double]) = {
    // println("contribution", x1, x2)
    if (x1 == x2) {
      val zero = x1 map { p => (p._1, 0.0) }
      (zero, zero)
    } else {
      (delta(x1, x2, usedVariables), delta(x1, x2, usedVariables, criterium = "max"))
    }
  }
*/
  /**
   * Estimate the contribution of each factor to the current score using the following heuristic:
   * - take the starting vector (x1)
   * - perturbate it by replacing one variable (v) by the value of the target vector (x2), call this x1'
   * - score it
   * - among all the candidate variables pick the score closest (farther) in absolute value from f(x1)
   * - call f(x1') - f(x) the contribution of variable v
   * - from this vector repeat the procedure for all the other variables until we reach (x2)
   */
  /*
  protected def delta(x1: Map[String, JsValue], x2: Map[String, JsValue], variables: Seq[String], criterium: String = "min"): Map[String, Double] = {
    // println("delta: " + x1 + "\n" + x2 + "\n" + variables)
    if (variables.isEmpty) {
      Map.empty
    } else {
      // println(variables)
      val score = eval(x1)
      // look over all the variables and compute the delta for each one of them
      val deltas = variables map { variable => variable -> (perturb(x1, x2, variable) - score) }
      // println("deltas: " + deltas)
      // pick the one closest to zero in absolute value
      val absDelta =
        if (criterium == "min") {
          deltas minBy { p => scala.math.abs(p._2) }
        } else {
          deltas maxBy { p => scala.math.abs(p._2) }
        }
      //println(absDelta)
      // replace the value in x1 with the corresponding value in x2 for the variable that minimize the score difference
      val minVariable = absDelta._1
      // repeat with the modified x1 until all variables are replaced
      delta(x1 + (minVariable -> x2.getOrElse(minVariable, Double.NaN)), x2, variables filterNot (_ == minVariable)) + absDelta
    }
  }
  protected def perturb(x1: JsValue, x2: JsValue, variable: String): Double = eval(x1 + (variable -> x2.getOrElse(variable, Double.NaN)))
  * 
  */
}

object Model {
  def fromFile(filename: String) = {
    val input = Source.fromFile(filename).mkString
    Model.fromJSON(Json.parse(input))
  }

  def fromInputStream(stream: InputStream) = {
    val input = Source.fromInputStream(stream).mkString
    Model.fromJSON(Json.parse(input))
  }

  def fromJSON(json: JsValue) = {
    val JsString(name) = json \ "name"
    val JsString(distribution) = json \ "distribution"
    val JsArray(variables) = json \ "factors"
    val JsArray(trees) = json \ "trees"
    //this is for multinomial, we need the classes
    val classes = json \ "classes" match {
      case JsArray(classList) => classList
      case _                  => Nil
    }
    Model(name, distribution, classes.map { case JsString(aClass) => aClass; case _ => "" }, trees.map(JsValue => new Tree(parseNode(JsValue))), variables map { case JsString(vv) => vv; case _ => "" })
  }

  def parseNode(json: JsValue): Node = {
    json match {
      case JsObject(Seq(Tuple2("prediction", JsNumber(prediction)))) => Leaf(prediction.toDouble)
      case JsObject(Seq(Tuple2("cond", JsObject(Seq(Tuple2("var", JsString(factor)), Tuple2("op", JsString("in")), Tuple2("val", JsArray(catSet))))),
        Tuple2("if_true", _@ if_true),
        Tuple2("if_false", _@ if_false),
        Tuple2("if_missing", _@ if_missing)
        )) =>
        IsIn(factor, catSet.map({
          case JsNumber(e) => e.toString
          case JsString(e) => e
          case e           => throw new Error("Unable to convert to String " + e)
        }).toSet, parseNode(if_true), parseNode(if_false), parseNode(if_missing))
      case JsObject(Seq(Tuple2("cond", JsObject(Seq(Tuple2("var", JsString(factor)), Tuple2("op", JsString("<")), Tuple2("val", JsNumber(threshold))))),
        Tuple2("if_true", _@ if_true),
        Tuple2("if_false", _@ if_false),
        Tuple2("if_missing", _@ if_missing)
        )) =>
        LessThan(factor, threshold.toDouble, parseNode(if_true), parseNode(if_false), parseNode(if_missing))

      case _ => {
        throw new Error("unmatched json: " + json)
      }
    }

  }
}