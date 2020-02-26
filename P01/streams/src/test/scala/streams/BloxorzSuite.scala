package streams

import org.junit.Assert.assertEquals
import org.junit._

class BloxorzSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """-----oo-ooo----
        |-----ooooooo---
        |ooo---oo-ooooo-
        |ooooooooo--oo--
        |-----oo-oo-ooo-
        |-----oooooo-oo-
        |ooo--oooooo-ooo
        |oToooo-o--ooo-S
        |ooo--ooo---oooo
        |ooo---------ooo""".stripMargin
  }

  trait Level3 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """-----oooooo---T
        |-----o--ooo----
        |-----o--ooooo--
        |Sooooo-----oooo
        |----ooo----oooo
        |----ooo-----ooo
        |------o--oo----
        |------ooooo----
        |------ooooo----
        |-------ooo-----""".stripMargin
  }


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }

  @Test def `find char level 3`: Unit =
    new Level3 {
      val startp = startPos
      assertEquals(Pos(1, 1), startPos)
    }


  @Test def `optimal solution for level 3`: Unit =
    new Level3 {
      assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `optimal solution for level 2`: Unit =
    new Level2 {
      assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }

  @Test def `neighborsWithHistory`: Unit =
    new Level1 {

      assertEquals(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down))
        ),
        neighborsWithHistory(
          Block(Pos(1, 1), Pos(1, 1)), List()).toSet)


      assertEquals(
        Set(
          (Block(Pos(1, 0), Pos(1, 0)), List(Left, Down, Right, Up)),
          (Block(Pos(1, 3), Pos(1, 3)), List(Right, Down, Right, Up)),
          (Block(Pos(2, 1), Pos(2, 2)), List(Down, Down, Right, Up)),
          (Block(Pos(0, 1), Pos(0, 2)), List(Up, Down, Right, Up))
        ),
        neighborsWithHistory(
          Block(Pos(1, 1), Pos(1, 2)), List(Down, Right, Up)).toSet)

    }



  @Test def `newNeighborsOnly`: Unit =
    new Level1 {

      assertEquals(
        Set(
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ),
        newNeighborsOnly(
          Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).to(LazyList),

          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
        ).toSet)

    }

  @Test def `pathsFromStart`: Unit =
    new Level1 {
      val start: Block = pathsFromStart.head._1
      assertEquals(startBlock, start)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
