import scala.annotation.tailrec
import scala.io.Source
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map => MutableMap}


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
        //code to add args(0) and split it
        val file1 = args(0)
        var line1 = Source.fromFile(file1).mkString.toLowerCase
        //splits on newline char and creates list
        val words = line1.split("\\s").toList
        //should add the pair to the tree
        for(i <- 0 to words.length - 2 by 2) {
            add(words(i), words(i+1), tree)
        }

        //code to add args(1)
        val file2 = args(1)
        var line2 = Source.fromFile(file2).mkString.toLowerCase
        val wordsToFind = line2.split("\\n")
        for(i <- 0 until wordsToFind.length by 1) {
            find(wordsToFind(i), tree) match {
                case Some(_) => println(wordsToFind(i) + " : " + find(wordsToFind(i), tree))
                case None => println(wordsToFind(i) + " : NONE")
            }

        }

        //call the find method to see if word exists as a key in the tree
        //if it exists, print out the correspoinding value
        //if the it does not, print NONE
    }
    if (args.length > 2) {
        println("Too Many Args Passed")
    }

 //   @tailrec
    def find(key:String, tree:Node): Option[String] = {
//        key match {
//            case tree.key => Some(tree.value)
//            case _ => find(key, tree.left)
//        }
        if(tree == null) {
            return None
        } else if (tree.key == key) {
            return Some(tree.value)
        }
        val checkLeft = find(key, tree.left)
        val checkRight = find(key, tree.right)
        if(checkLeft != None) {
            return checkLeft
        } else if(checkRight != None) {
            return checkRight
        }
        return None

    }

    def add(key:String, value:String, tree:Node):Option[String] = {
        //check which side it needs to go to
        if(tree.key.compareTo(key) > 0) {
            //go to left
            //if it exists, call resursively
            if(tree.left != null) {
                add(key, value, tree.left)
            } else {
                tree.left = Node(key, value, null, null)
            }
            return Some(key)
        }
        if(tree.key.compareTo(key) < 0) {
            //go to right
            if(tree.right != null) {
                add(key, value, tree.right)
            } else {
                tree.right = Node(key, value, null, null)
            }
            return Some(key)
        }
        return None
    }


}
