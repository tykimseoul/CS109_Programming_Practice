import scala.io.StdIn.readLine
case class Pos(row: Int, col: Int)

def makeField(rows: Int, cols: Int): Array[Array[Int]] = {
  var field = new Array[Array[Int]](rows)
  for (row <- 0 until rows){
    field(row) = new Array[Int](cols)
  }
  for (i <- 0 until rows){
    for (j<-0 until cols){
      field(i)(j)=j+1+(i)*cols
    }
  }
  field(rows-1)(cols-1)=0
  return field
}

def displayField(f:Array[Array[Int]]){
  var result=Array.ofDim[Array[Array[String]]](f.length,f(0).length)
  //make entry
  var entry=Array.ofDim[String](5,6)
  for (i<-0 until 5){
	for (j<-0 until 6){
		entry(i)(j)=" "
	}
  }
  for (i <- 0 until 5){
    entry(i)(0)="|"
    entry(i)(5)="|"
  }
  for (j<-0 until 6){
    entry(0)(j)="-"
    entry(4)(j)="-"
  }
  entry(0)(0)="o"
  entry(0)(5)="o"
  entry(4)(0)="o"
  entry(4)(5)="o"
  //replace with entry
  //for (i <- 0 until result.length){
  //  for (j<-0 until result(0).length){
  //    result(i)(j)=entry
  //  }
  //}
  var empty=Array.ofDim[String](5,6)
  for (i<-0 until 5){
	for (j<-0 until 6){
		empty(i)(j)=" "
	}
  }
  for (i <- 0 until result.length){
    for (j<-0 until result(0).length){
		var c=Array.ofDim[String](5,6)
		for (m<-0 until c.length){
			for (n<-0 until c(0).length){
				c(m)(n)=entry(m)(n)
			}
		}
		result(i)(j)=c
    }
  }
  //enter field numbers
  for (i <- 0 until result.length){
    for (j<-0 until result(0).length){
	  if (f(i)(j)/100!=0){
		result(i)(j)(2)(1)=(f(i)(j)/100).toString
	  }
	  if (f(i)(j)/10!=0){
		result(i)(j)(2)(2)=(f(i)(j)/10).toString
	  }
	  result(i)(j)(2)(3)=(f(i)(j)%10).toString
	  //System.out.println(i + ", " + j + ")" + f(i)(j))
    }
  }
  for (i<-0 until result.length){
	for (j<-0 until result(0).length){
		if (result(i)(j)(2)(3)=="0"&&result(i)(j)(2)(2)==" "){
			result(i)(j)=empty
		}
	}
  }
  //display
  for (i <- 0 until result.length){
    for (k<-0 until result(0)(0).length){
		for (j<-0 until result(0).length){
			for (l<-0 until 6){
				print(result(i)(j)(k)(l))
			}
			print(" ")
			if (j==result(0).length-1){
				println("")
			}
			
		}
    }
  }
}

def findEmpty(field: Array[Array[Int]]): Pos = {
	var ret = new Pos(0,0)
	for (i<-0 until field.length){
		for (j<-0 until field(0).length){
			if(field(i)(j)==0){
				return new Pos(i,j)
			}
		}
	}
	ret
}

def makeMove(field: Array[Array[Int]], delta: Pos) {
	try{
		val now = findEmpty(field)
		val upd = Pos(now.row + delta.row, now.col + delta.col)
		field(now.row)(now.col) = field(upd.row)(upd.col)
		field(upd.row)(upd.col) = 0
	}
	catch{
		case e:ArrayIndexOutOfBoundsException=>
			//println("Impossible move!")
		
	}
	//displayField(field)
}

def shuffle(field: Array[Array[Int]], iter: Int) {
  val moves = Array(Pos(1,0), Pos(-1,0), Pos(0,1), Pos(0,-1))
  for (i <- 1 to iter) {
    val mov = moves((math.random * 4).toInt)
    makeMove(field, mov)
	
  }
  //displayField(field)
}

def strToMove(s: String): Pos = {
	val k=s.toLowerCase.trim
	if (k=="left"){
		return Pos(0,1)
	}
	else if (k=="right"){
		return Pos(0,-1)
	}
	else if (k=="up"){
		return Pos(1,0)
	}
	else if (k=="down"){
		return Pos(-1,0)
	}
	else{
		return Pos(0,0)
	}
}
/*	
def testFinish(f:Array[Array[Int]]):Boolean={
	var a=makeField(f.length, f(0).length)
	var test=true
	for (i<-0 until f.length){
		for (j<-0 until f(0).length){
			if (f(i)(j)(2)(3)==a(i)(j)%10 && f(i)(j)(2)(2)==a(i)(j)/10){
			test=true
			}
			else{
			test=false
			}
		}
	}
	return test
}
*/

def playGame(rows: Int, cols: Int) {
  val f = makeField(rows, cols)
  shuffle(f, 2000)
  while (true) {
    displayField(f)
    val s = readLine("What is your move> ")
    println()
    if (s == "quit")
      return
    val move = strToMove(s)
    makeMove(f, move)
	/*
	if (testFinish(f)){
		print("Congratulations! You have solved the puzzle!")
	}
	*/
  }
}
val d=args(0).toInt
val e=args(1).toInt

playGame(d,e) 