package com.micinfotech.generic

import scala.collection.mutable

/**
  * Created by sovar on 7/8/16.
  */
object Generic {
  /**
    * You can execute a series of function one by one.
    * @param functions the functions you want to execute.
    * @param data the original data
    * @tparam A input data type and output data type
    * @return result
    */
  def executeFunctions[A](functions:Seq[(A => A)])(data: A): A = {
    if (functions.isEmpty) data else executeFunctions(functions.tail)(functions.head(data))
  }

  /**
    * Count each element occur number in a collection.
    * @param iter the collelection you want to coun
    * @param asc output sequence order: true means form small to big, false means form big to small
    * @tparam A element type
    * @return
    */
  def countElement[A](iter: Iterable[A], asc: Int = 0) = {
    val temp = mutable.Map.empty[A, Int]
    for (x <- iter) temp += x -> (temp.getOrElse(x, 0) + 1)
    val y = temp.toSeq
    if (asc > 0) y.sortBy(_._2) else if (asc < 0) y.sortBy(-_._2) else y
  }

  def parseOrElse[A](parser: String => A)(default: => A)(str: String) =
    if (str.isEmpty) default else parser(str)

  def getOrElse[A](dict: Map[String, String])(parser: String => A)(default: => A)(key: String) =
    if (dict.contains(key)) parser(dict(key)) else default

  lazy val parseIntOrElse = parseOrElse(Integer.parseInt)_
  lazy val parseHexIntOrElse = parseOrElse(x => Integer.parseInt(x, 16))_
  lazy val parseBooleanOrElse = parseOrElse(x => x.toBoolean)_
  def getStringWithDefault(default: => String)(str: String) = if (str.isEmpty) default else str
  def getStringFromArrayWithDefault(default: => String)(array: Array[String])(idx: Int) =
    if (idx < array.length) array(idx) else default
}
