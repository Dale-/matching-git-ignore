package com.thoughtworks.pli.remotepair.core

import scala.util.control.Breaks._

class matching_ignore {

  def generateRule(ignoreFile: String): Array[String] = {
    val ignoreArray = ignoreFile.split("\n")
    val ignoreInfo = diminishBlankLine(ignoreArray)
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
    val ignoreRule = this.generateRule("*.zip\n\nhello.scala")
    breakable {
      for (i <- 0 until ignoreRule.length) {
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

}
