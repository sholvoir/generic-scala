package com.micinfotech.generic

import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.io.Source

object TableFile {
  /**
    * Table File Read from file
    * @param file intput file
    * @param parser header => (line => T) T construct function
    * @tparam T row type
    * @return Iterator[T]
    */
  def read[T](file: File)(parser: String => (String => T)): Iterator[T] = {
    val lines = Source.fromFile(file).getLines.map(_.trim)
    val tbuilder = parser(lines.next())
    for {
      line <- lines
      if line.nonEmpty && !line.startsWith("#")
    } yield tbuilder(line)
  }

  /**
    * Table File Write to file
    * @param file output file
    * @param header table header
    * @param rows Iterator[T]
    * @param toString row => string function
    * @tparam T row type
    */
  def write[T](file: File)(header: String)(rows: Iterator[T])(toString: T => String): Unit = {
    val out = new PrintWriter(file)
    try {
      out.println(header)
      for (row <- rows)
        out.println(toString(row))
    } finally out.close()
  }

  /**
    * Table file row split
    * @param infile input file
    * @param outdir output file
    * @param identify line => id function
    */
  def rowSplit(infile: File)(outdir: File)(identify: String => String): Unit = {
    val clmfiles: mutable.Map[String, PrintWriter] = mutable.Map.empty
    try {
      val lines = Source.fromFile(infile).getLines.map(_.trim)
      val header = lines.next()
      for (line <- lines) {
        val clmfile = {
          val id = identify(line)
          val clmfileo = clmfiles.get(id)
          if (clmfileo.isDefined) clmfileo.get
          else {
            val pw = new PrintWriter(new File(outdir, id))
            pw.println(header)
            clmfiles(id) = pw
            pw
          }
        }
        clmfile.println(line)
      }
    } finally for ((_, chrfile) <- clmfiles) chrfile.close()
  }

  /**
    * Table File Column Split
    * @param readDelimiter input Table File Column Delimiter
    * @param writeDelimiter output Table File Column Delimiter
    * @param infile intput file
    * @param outfiles filename -> columnNameSeq
    */
  def columnSplit(readDelimiter: String)(writeDelimiter: String)
                 (infile: File)(outfiles: Map[String, Seq[String]]): Unit = {
    val lines = Source.fromFile(infile).getLines.map(_.trim)
    val cols = lines.next().split(readDelimiter)
    val colm = cols.zipWithIndex.toMap
    val ofiles = outfiles.map(f => (new PrintWriter(f._1), f._2.map(colm(_))))
    try {
      ofiles.foreach(f => f._1.println(f._2.map(cols(_)).mkString(writeDelimiter)))
      for (line <- lines) {
        val row = line.split(readDelimiter)
        ofiles.foreach(k => k._1.println(k._2.map(row(_)).mkString(writeDelimiter)))
      }
    } finally ofiles.foreach(_._1.close())
  }

  /**
    * Table File Condition Filter
    * @param infile input file
    * @param outfile output file
    * @param filter line => index(start with 0, exclude firstline) => boolean(remain it?)
    */
  def conditionFilter(infile: File)(outfile: File)(filter: String => Int => Boolean): Unit = {
    val out = new PrintWriter(outfile)
    try {
      val lines = Source.fromFile(infile).getLines
      if (lines.hasNext) out.println(lines.next())
      for ((line, index) <- lines.zipWithIndex)
        if (filter(line)(index))
          out.println(line)
    } finally out.close()
  }

  /**
    * Table file Changing Filter
    * @param infile intput file
    * @param outfile output file
    * @param filter line => index => id
    */
  def changeFilter(infile: File)(outfile: File)(filter: String => Int => Any): Unit = {
    val out = new PrintWriter(outfile)
    try {
      val lines = Source.fromFile(infile).getLines
      if (lines.hasNext) out.println(lines.next())
      var value: Any = null
      for ((line, index) <- lines.zipWithIndex) {
        val x = filter(line)(index)
        if (x != value) {
          out.println(line)
          value = x
        }
      }
    } finally out.close()
  }
}