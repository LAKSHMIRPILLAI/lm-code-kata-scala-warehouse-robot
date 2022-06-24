package com.techreturners
class RobotWarehouse (gridRow:Int,gridCol:Int,robotStartPosition:(Int,Int)=(0,0),robotCommands:String=""){
   val gridSize:(Int,Int)=(gridRow,gridCol)// Grid size
   val robotStart:(Int,Int)=(robotStartPosition._1,robotStartPosition._2)//Starting position of Robot
   val commands:String=robotCommands


   val grid:Array[Array[String]]= createGrid(gridSize)
   val initialGrid: Array[Array[String]] = initiateGrid(grid, robotStart)
   var currentGrid:Array[Array[String]]=initialGrid.map(_.clone())
   var finalGrid:Array[Array[String]]=moveRobot(currentGrid,commands)

   def isValidPosition(position:(Int,Int)):Unit={
     if (position._1 >=gridSize._1 || position._2>=gridSize._2||position._1 <0||position._2<0) {
       println("This is not a valid start position")
       throw new Exception("This is not a valid start position")
     }
   }
    def robotPosition(currentGrid:Array[Array[String]]):(Int, Int)={
      val z=for {
            i <- 0 until gridSize._1
            j <- 0 until gridSize._2
            if currentGrid(i)(j) == "R"
      }yield(i,j)
      z.head
    }

     def moveRobot(currentGrid:Array[Array[String]],commands:String):Array[Array[String]]= {
       val instructions = commands.replaceAll("\\s","")

       var (rowPositionRobot,colPositionRobot): (Int, Int) = robotPosition(currentGrid)
       currentGrid(rowPositionRobot)(colPositionRobot)="*"
       for (i <- instructions.indices) {
         instructions(i).toString match {
           case "N" =>  rowPositionRobot = rowPositionRobot + 1
           case "S" =>  rowPositionRobot = rowPositionRobot - 1
           case "E" =>  colPositionRobot = colPositionRobot + 1
           case "W" =>  colPositionRobot = colPositionRobot - 1
         }
       }
       if (rowPositionRobot>=gridSize._1) rowPositionRobot=gridSize._1-1
       if (colPositionRobot>=gridSize._2) colPositionRobot=gridSize._2-1
       if (rowPositionRobot<0) rowPositionRobot=0
       if (colPositionRobot<0) colPositionRobot=0
       currentGrid(rowPositionRobot)(colPositionRobot)="R"
       currentGrid
    }

   def initiateGrid(grid:Array[Array[String]],start:(Int,Int)):Array[Array[String]]={
     val startGrid:Array[Array[String]]=grid.map(_.clone())
     isValidPosition(start)
     if (start._1<gridSize._1 && start._2<gridSize._2)
        startGrid(start._1)(start._2)="R"
     startGrid
   }



  def createGrid(gridSize:(Int,Int)):Array[Array[String]]={
     val grid =Array.ofDim[String](gridSize._1,gridSize._2)
     for (i <-0 until  gridSize._1; j<-0 until gridSize._2){
       grid(i)(j) ="*"
    }
    grid
  }

}
