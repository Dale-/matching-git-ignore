package com.thoughtworks.pli.remotepair.core

class matching_ignore {

//  def generateRule(ignoreFile: String): List[String] = {
//    val ignoreList = ignoreFile.split("\n").toList
//
//  }

  def diminishBlankLine(list: List[String]): List[String] = {
    list.filter(!_.isEmpty)
  }

}
