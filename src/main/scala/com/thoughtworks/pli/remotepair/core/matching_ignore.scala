package com.thoughtworks.pli.remotepair.core

class matching_ignore {

  def generateRule(ignoreFile: String): List[String] = {
    val ignoreArray = ignoreFile.split("\n")
    val ignoreInfo = diminishBlankLine(ignoreArray)
    val ignoreRule = new Array[String](ignoreInfo.length)
    for (i <- 0 until ignoreInfo.length) {
      if(ignoreInfo(i).contains("*.")) {
        ignoreRule(i) = ignoreInfo(i).substring(1, ignoreInfo(i).length)
      }
    }

  }

  def diminishBlankLine(array: Array[String]): Array[String] = {
    array.filter(!_.isEmpty)
  }

}
