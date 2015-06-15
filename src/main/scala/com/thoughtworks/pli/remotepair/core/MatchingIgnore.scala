package com.thoughtworks.pli.remotepair.core

import scala.util.control.Breaks._

class MatchingIgnore {

  def generateRule(ignoreFile: String): Array[String] = {
    var ignoreInfo = ignoreFile.split("\n")
    ignoreInfo = diminishBlankLine(ignoreInfo)
    ignoreInfo = diminishAnnotation(ignoreInfo)
    val ignoreRule = new Array[String](ignoreInfo.length)
    for (i <- 0 until ignoreInfo.length) {
      if(ignoreInfo(i).contains("*.")) {
        ignoreRule(i) = ignoreInfo(i).substring(1, ignoreInfo(i).length)
      } else {
        ignoreRule(i) = ignoreInfo(i)
      }
    }
    return ignoreRule
  }

  def isIgnore(fileName: String): Boolean = {
    var isIgnore = false
    val ignoreRule = this.generateRule("*.zip\n\n#bababa\nhello.scala")
    breakable {
      for (i <- 0 until ignoreRule.length) {
        println(ignoreRule(i))
        if(fileName.contains(ignoreRule(i))) {
          isIgnore = true
          break
        }
      }
    }
    isIgnore
  }

  def diminishBlankLine(array: Array[String]): Array[String] = {
    array.filter(!_.isEmpty)
  }

  def diminishAnnotation(array: Array[String]): Array[String] = {
    array.filter(!_.charAt(0).equals('#'))
  }

}

object MatchingIgnore {

  def main(args: Array[String]): Unit = {
    val matchingIgnore = new MatchingIgnore()
    val isIgnore = matchingIgnore.isIgnore("hello.scala")
    println("isIgnore:" + isIgnore)
  }
}
