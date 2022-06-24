package com.techreturners


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppTest extends AnyFlatSpec with Matchers{
  "A grid initialized with * values" should " be created for a grid size of 10 ,10" in {
     val robotWarehouse= new RobotWarehouse(10,10)
    assert(robotWarehouse.grid.head.head=="*")
    assert(robotWarehouse.gridSize==(10,10))
  }

  "The initial position of Robot" should "be at (0,0)" in{
    val robotWarehouse= new RobotWarehouse(10,10,(0,0))
    assert(robotWarehouse.grid.head.head=="*")
    assert(robotWarehouse.initialGrid(0)(0)=="R")
    assert(robotWarehouse.initialGrid(1)(1)=="*")
    assert(robotWarehouse.gridSize==(10,10))
  }


  "The initial position of Robot" should "be at (1,1)" in{
    val robotWarehouse= new RobotWarehouse(10,10,(1,1))
    assert(robotWarehouse.initialGrid(0)(0)=="*")
    assert(robotWarehouse.initialGrid(1)(1)=="R")
    assert(robotWarehouse.robotPosition(robotWarehouse.initialGrid)==(1,1))
  }
  "The position of Robot" should "change one position up when provided with N" in{
    val robotWarehouse= new RobotWarehouse(10,10,(0,0),"N")
    assert(robotWarehouse.initialGrid(0)(0)=="R")
    assert(robotWarehouse.initialGrid(1)(0)=="*")
    assert(robotWarehouse.currentGrid(0)(0)=="*")
    assert(robotWarehouse.currentGrid(1)(0)=="R")
    assert(robotWarehouse.finalGrid(0)(0)=="*")
    assert(robotWarehouse.finalGrid(1)(0)=="R")
    assert(robotWarehouse.robotPosition(robotWarehouse.finalGrid)==(1,0))
  }
  "The position of Robot" should "change one position up and one position right when provided with N E" in{
    val robotWarehouse= new RobotWarehouse(10,10,(0,0),"N E")
    assert(robotWarehouse.initialGrid(0)(0)=="R")
    assert(robotWarehouse.initialGrid(1)(1)=="*")
    assert(robotWarehouse.currentGrid(0)(0)=="*")
    assert(robotWarehouse.currentGrid(1)(1)=="R")
    assert(robotWarehouse.finalGrid(0)(0)=="*")
    assert(robotWarehouse.finalGrid(1)(1)=="R")
    assert(robotWarehouse.robotPosition(robotWarehouse.finalGrid)==(1,1))
  }

  "The position of Robot" should "change five position up and five position right when provided with N E N E N E N E" in{
    val robotWarehouse= new RobotWarehouse(10,10,(0,0),"N E N E N E N E ")
    assert(robotWarehouse.initialGrid(0)(0)=="R")
    assert(robotWarehouse.initialGrid(4)(4)=="*")
    assert(robotWarehouse.currentGrid(0)(0)=="*")
    assert(robotWarehouse.currentGrid(4)(4)=="R")
    assert(robotWarehouse.finalGrid(0)(0)=="*")
    assert(robotWarehouse.finalGrid(4)(4)=="R")
    assert(robotWarehouse.robotPosition(robotWarehouse.finalGrid)==(4,4))
  }
  "The position of Robot" should "not change  when provided with N E S W" in{
    val robotWarehouse= new RobotWarehouse(10,10,(0,0),"N E S W ")
    assert(robotWarehouse.initialGrid(0)(0)=="R")
    assert(robotWarehouse.currentGrid(0)(0)=="R")
    assert(robotWarehouse.finalGrid(0)(0)=="R")
    assert(robotWarehouse.robotPosition(robotWarehouse.finalGrid)==(0,0))
  }
  "The position of Robot" should "not allow  when provided with initial position 10,10 and throw an exception" in{
    //val robotWarehouse= new RobotWarehouse(10,10,(10,10))
  }

  "The position of Robot" should "change  to (10,10)when provided with 0,0 and N E N E N E N E N E N E N E N E N E N E N E " in{
    val robotWarehouse= new RobotWarehouse(10,10,(0,0), "N E N E N E N E N E N E N E N E N E N E N E")
    assert(robotWarehouse.initialGrid(0)(0)=="R")
    assert(robotWarehouse.currentGrid(9)(9)=="R")
    assert(robotWarehouse.finalGrid(9)(9)=="R")
    assert(robotWarehouse.finalGrid(0)(0)=="*")
    assert(robotWarehouse.robotPosition(robotWarehouse.finalGrid)==(9,9))
  }
}
