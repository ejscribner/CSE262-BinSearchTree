/*
    Elliot J. Scribner
    CSE262 Fall 2018
    ejs320
    Prog2: creates a binary search tree of word pairs and returns the value of a Node given a key
 */

import scala.annotation.tailrec
import scala.io.Source


object BinSearchTree extends App {

    case class Node(key: String, value: String, var left: Node, var right: Node)

    val tree = Node("abc", "1", Node("aaa", "2", null, null),
        Node("bbb", "3", null, null))
    if (args.length == 0) {
        println("No Args Passed")
    }
    if (args.length == 1) {
        println("Only One Arg Passed, Requires 2")
    }
    if (args.length == 2) {
        val file1 = args(0)
        var line1 = Source.fromFile(file1).mkString.toLowerCase
        val words = line1.split("\\s").toList
        for (i <- 0 to words.length - 2 by 2) {
            add(words(i), words(i + 1), tree)
        }
        val file2 = args(1)
        var line2 = Source.fromFile(file2).mkString.toLowerCase
        val wordsToFind = line2.split("\\n")
        for (i <- 0 until wordsToFind.length by 1) {
            find(wordsToFind(i), tree) match {
                case Some(s) => println(wordsToFind(i) + ", " + s)
                case None => println(wordsToFind(i) + ", NONE")
            }

        }
    }
    if (args.length > 2) {
        println("Too Many Args Passed")
    }

    def find(key: String, tree: Node): Option[String] = {
        @tailrec
        def findUtil(key: String, list: List[Node]): Option[String] = {
            list match {
                case Nil => None
                case Node(k, value, left, right) :: remainder =>
                    if (k == key) {
                        Some(value)
                    } else {
                        findUtil(key, List(left, right).filter(_ != null) ++ remainder)
                    }

            }
        }

        findUtil(key, List(tree))
    }
    
    def add(key: String, value: String, tree: Node): Option[String] = {
        if (tree.key.compareTo(key) > 0) {
            if (tree.left != null) {
                add(key, value, tree.left)
            } else {
                tree.left = Node(key, value, null, null)
            }
            return Some(key)
        }
        if (tree.key.compareTo(key) < 0) {
            if (tree.right != null) {
                add(key, value, tree.right)
            } else {
                tree.right = Node(key, value, null, null)
            }
            return Some(key)
        }
        return None
    }
}
