package com.bebelici.game15

object Game15 extends App {

  val problems = Map(
    "easy" -> Game(Vector(
      Vector( 1, 2, 3 ),
      Vector( 4, 0, 5 ),
      Vector( 7, 8, 6 )
    )),
    "medium" -> Game(Vector(
      Vector( 0, 5, 2 ),
      Vector( 1, 4, 3 ),
      Vector( 7, 8, 6 )
    )),
    "hard" -> Game(Vector(
      Vector( 2, 4, 8 ),
      Vector( 0, 3, 1 ),
      Vector( 7, 6, 5 )
    )),
    "large" -> Game(Vector(
      Vector( 0,  1,  3,  4 ),
      Vector( 6,  2,  7,  8 ),
      Vector( 5, 10, 15, 11),
      Vector( 9, 13, 14, 12 )
    ))
  )
  
  problems("large").solution.head foreach println
}