package com.bebelici.game15

object Game15 extends App {

  val problems = Map(
		  "easy" -> new Game(Vector(Vector(1, 2, 3 ), Vector(4, 0, 5 ), Vector(7, 8, 6 ))),
		  "medium" -> new Game(Vector(Vector(0, 1, 3 ), Vector(4, 2, 5 ), Vector(7, 8, 6 ))),
		  "hard" -> new Game(Vector(Vector(2, 4, 8), Vector(0, 3, 1), Vector(7, 6, 5))))
  
  val game = problems("medium")
  println(game.solution.take(1) mkString " ")
}