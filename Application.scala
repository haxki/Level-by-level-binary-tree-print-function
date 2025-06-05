package main.scala

import scala.util.Random

object Application {
  def main(args: Array[String]): Unit = {
    val treeItemCount = 12
    println(s"Tree that contains $treeItemCount random numbers:")
    val tree = generateTree(treeItemCount)
    printTree(tree)
    println()

    println("Sum of numbers: " + sum(tree))
    println()

    val insertedItem1 = Random.between(10,100)
    val insertedItem2 = Random.between(10,100)
    val tree2 = insertTree(insertedItem2, insertTree(insertedItem1, tree))
    println(s"New tree elements: $insertedItem1, $insertedItem2.")
    println("Tree with new elements:")
    printTree(tree2)
    println()

    println("What numbers does this tree contain?")
    (10 until 100).foreach(number =>
      if contains(number, tree2) then
        print(Console.GREEN)
      else print(Console.RED)
      print(number + " ")
      if number % 10 == 9 then println()
    )
    println(Console.RESET)
  }
}
