/*
 * This file was generated by the Gradle 'init' task.
 *
 * This generated file contains a sample Scala library project to get you started.
 * For more details take a look at the Scala plugin chapter in the Gradle
 * user guide available at https://docs.gradle.org/4.10/userguide/scala_plugin.html
 */

plugins {
    // Apply the scala plugin to add support for Scala
    scala
}

dependencies {
    // Use Scala 2.13 in our library project
    compile("org.scala-lang:scala-library:2.13.0")

    // Use Scalatest for testing our library
    testCompile("junit:junit:4.12")
    testCompile("org.scalatest:scalatest_2.13:3.0.8")

    // Need scala-xml at test runtime
    testRuntime("org.scala-lang.modules:scala-xml_2.13:1.2.0")
}

// In this section you declare where to find the dependencies of your project
repositories {
    // Use jcenter for resolving your dependencies.
    // You can declare any Maven/Ivy/file repository here.
    jcenter()
}

group = "cn.micit"
version = "2.13_1.4.0"
