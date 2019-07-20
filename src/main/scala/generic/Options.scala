package com.micinfotech.generic

object Options {
  /**
    * command line arguments parser. the grammar is:
    * command -x -y optionY -z optionZ ... Argument1 Argument2 ...
    * the result is a map: {"-x" -> "", "-y" -> "optionY", "-z" -> "optionZ", "" -> "Argument1\tArgument2"
    * @param args command line arguments
    * @return option map
    */
  def parse(args: Array[String]): Map[String, String] = {
    if (args.length == 0) Map.empty
    else if (!args.head.startsWith("-")) { //Argument
      val options = parse(args.tail)
      val arg = options.get("")
      if (arg.isDefined) options.updated("", "%s\t%s".format(args.head, arg.get))
      else options.updated("", args.head)
    } else if (args.tail.length == 0 || args.tail.head.startsWith("-")) { //Option
      parse(args.tail) + (args.head -> "")
    } else {
      parse(args.tail.tail) + (args.head -> args.tail.head)
    }
  }

  /**
    * command line Options parser, the rammar is:
    * name,id=id1,sex=sex,nobar
    * the first noequal is a value the other noequal is a key
    * the result is a map {""->"name","id"->"id1","sex"->"sex","nobar"->""}
    * @param args arguments
    * @return option map
    */
  def parse(args: Array[Array[String]]): Map[String, String] = {
    if (args.isEmpty) Map.empty[String, String]
    else args.zipWithIndex.map { case (x, i) =>
      if (x.length > 1) (x(0), x(1)) else if (i == 0) ("", x(0)) else (x(0), "")
    }.toMap
  }
}