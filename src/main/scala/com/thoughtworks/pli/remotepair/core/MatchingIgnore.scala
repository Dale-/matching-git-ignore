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
        if(ignoreInfo(i).contains('/')) {
          if(ignoreInfo(i).charAt(0).equals('/')) {
            ignoreRule(i) = '/' + ignoreInfo(i).substring(ignoreInfo(i).indexOf('*') + 1, ignoreInfo(i).length)
          } else {
            ignoreRule(i) = ignoreInfo(i).substring(0, ignoreInfo(i).indexOf('/') + 1) + " " +
                            ignoreInfo(i).substring(ignoreInfo(i).indexOf('*') + 1, ignoreInfo(i).length);
          }
        } else {
          ignoreRule(i) = ignoreInfo(i).substring(1, ignoreInfo(i).length)
        }
      } else {
        ignoreRule(i) = ignoreInfo(i)
      }
    }
    return ignoreRule
  }

  def isIgnore(fileName: String, ignoreFile: String): Boolean = {
    var isIgnore = false
    val ignoreRule = this.generateRule(ignoreFile)
    breakable {
      for (i <- 0 until ignoreRule.length) {
        if(ignoreRule(i).charAt(0).equals('/')) {
          if(fileName.contains(ignoreRule(i).substring(1, ignoreRule(i).length)) && !fileName.contains('/')) {
            isIgnore = true
            break
          }
        } else if (ignoreRule(i).contains(" ")) {
          val rule = ignoreRule(i).split(" ")
          if(fileName.contains(rule(0)) && fileName.contains(rule(1))) {
            val matchingRule = fileName.replace(rule(0), "")
            println(matchingRule)
            if(!matchingRule.replace(rule(1), "").contains('/')) {
              isIgnore = true
              break
            }
          }
        } else if (fileName.contains(ignoreRule(i))) {
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
    val isIgnore = matchingIgnore.isIgnore("Document/p.java", "/*.c\nDocument/*.java\n*.zip\n\n#bababa\nhello.scala")
    println("isIgnore:" + isIgnore)
  }
}
