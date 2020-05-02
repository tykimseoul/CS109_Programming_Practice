import scala.io.StdIn.readLine
import scala.util.Random
var rows = 8
var cols = 8
val r = scala.util.Random
var end=0

case class Pos(row: Int, col: Int)
val bombs = scala.collection.mutable.Set[Pos]()
var field: Array[Array[Char]] = null

def makeField(rows: Int, cols: Int): Array[Array[String]] = {
  var field = new Array[Array[String]](rows+2)
  for (row <- 0 until rows+2){
    field(row) = new Array[String](cols+1)
  }
  for (j<-1 until cols+1){
    field(0)(j)=(j/10).toString
    field(1)(j)=(j%10).toString
  }
  for (i<-2 until rows+2){
    field(i)(0)=(63+i).toChar.toString
  }
  field(0)(0)=" "
  field(1)(0)=" "
  //for (j<-1 until 9){
   // field(0)(j)=" "
  //}
  for (i <- 2 until rows+2){
    for (j<-1 until cols+1){
      field(i)(j)="."
    }
  }
  return field
}
def makeBombs(rows: Int, cols: Int, bomb:Int):scala.collection.mutable.Set[Pos]={
  bombs.clear
  for(i<-0 until bomb){
    var np=new Pos((r.nextInt(rows)+65),r.nextInt(cols)+1)
    if (bombs contains np){
      var np2=new Pos((r.nextInt(rows)+65),r.nextInt(cols)+1)
      bombs+=np2
    }
    else{
      bombs+=np
    }
    //print(i,np,bombs)
    //println("")
  }
  return bombs
}
def displayField(f:Array[Array[String]],b:Int){
  //display
  if(b==0){
    for (i <- 0 until f.length){
  		for (j<-0 until f(0).length){
  			print (f(i)(j))
  		}
  		println("")
    }
  }
  else{
    /*for (i <- 2 until f.length){
  		for (j<-1 until f(0).length){
  		  f(i)(j)="."
  		}
    }*/
    for (i <- 0 until f.length){
  		for (j<-0 until f(0).length){
  		  if(bombs contains Pos(i+63,j)){
  		    f(i)(j)="*"
  		  }
  			print(f(i)(j))
  		}
  		println("")
    }
  }
}

def checkGuess(field: Array[Array[String]],guess:String):Int= {
  try{
    var count=0
    var r=guess.toUpperCase.charAt(0).toInt-63
    var c=guess.substring(1).toInt
	  if(c!=field(0).length && r!=field.length){
  	  for (i<- r-1 until r+2){
  	    for (j<- c-1 until c+2){
  	      //around=new Pos(i+63,j)
  	      if (i==r && j==c){
  	        
  	      }
  	      else if (bombs contains Pos(i+63,j)){
  	        count=count+1
  	      }
  	      //println(i+63,j,count)
  	    }
  	  }
	  }
	  else if (c==field(0).length){
	    for (i<- r-1 until r+2){
  	    for (j<- c-1 until c+1){
  	      //around=new Pos(i+63,j)
  	      if (i==r && j==c){
  	        
  	      }
  	      else if (bombs contains Pos(i+63,j)){
  	        count=count+1
  	      }
  	      //println(i+63,j,count)
  	    }
  	  }
	  }
    else if (r==field.length){
	    for (i<- r-1 until r+1){
  	    for (j<- c-1 until c+2){
  	      //around=new Pos(i+63,j)
  	      if (i==r && j==c){
  	        
  	      }
  	      else if (bombs contains Pos(i+63,j)){
  	        count=count+1
  	      }
  	      //println(i+63,j,count)
  	    }
  	  }
	  }
	  return count
	}
	catch{
		case e:ArrayIndexOutOfBoundsException=>
			println("Not in the field!")
			return -1
	}
}

def makeGuess(field: Array[Array[String]],guess:String){
	try{
	  if(guess.substring(0,1)!="#"){
  	  val guessPosition=Pos(guess.toUpperCase.charAt(0).toInt,guess.substring(1).toInt)
  	  if(bombs contains guessPosition){
  	    println("BOOM BOOM BOOM")
  	    //displayField(field,1)
  	    end=1
  	  }
  	  else{
  	    //println(guess.toUpperCase.charAt(0).toInt-63,guess.substring(1).toInt,checkGuess(field,guess).toString)
  	    field(guess.toUpperCase.charAt(0).toInt-63)(guess.substring(1).toInt)=checkGuess(field,guess).toString
  	    //displayField(field,1)
  	  }
	  }
	  else{
	    field(guess.toUpperCase.charAt(1).toInt-63)(guess.substring(2).toInt)="#"
	  }
	}
	catch{
		case e:NumberFormatException=>
			println("Wrong input!")
		case f:ArrayIndexOutOfBoundsException=>
			println("Not in the field!")
	}
	///displayField(field)
}
	
def testFinish(f:Array[Array[String]]):Int={//if 1, ends
	var a=makeField(f.length, f(0).length)
	var count=0
	var flagList=List[Pos]()
	for (i<-2 until f.length){
		for (j<-1 until f(0).length){
		  //println("f("+i+")("+j+"): "+f(i)(j))
			if (f(i)(j)=="#"){
			  var p=new Pos(i+63,j)
			  //println(p)
			  flagList = flagList:+p
			  //println(flagList.length)
			}
		}
	}
	println(bombs)
	for (k<-0 until bombs.size){
	  var b=bombs.toArray
	  if (flagList contains b(k)){
	    
	    //println("contained")
	    //println(b(k))
	    count=count+1
	  }
	  else{
	    //println(flagList)
	    //println(b(k))
	    //println("not contained")
	  }
	}
	if(count==bombs.size){
	  return 1
	}
	else{
	  return 0
	}
}


def playGame(rows: Int, cols: Int, bomb:Int) {
  val f = makeField(rows, cols)
  makeBombs(rows,cols,bomb)
  //shuffle(f, 2000)
  while (true) {
    displayField(f,0)
    println(bombs)
    val s = readLine("Choose a cell> ")
    println()
    if (s == "quit")
      return
    makeGuess(f, s)
    if (end==1){
      displayField(f,1)
      return
    }
    if (testFinish(f)==1){
  		println("Congratulations! You have solved the puzzle!")
  		displayField(f,1)
  		return
	  }
  }//almost done, check finish and display bombs when lost
}

if(args(0).isEmpty && args(1).isEmpty && args(2).isEmpty){
  playGame(8,8,6)
}
else{
  val d=args(0).toInt
  val e=args(1).toInt
  val f=args(2).toInt
  playGame(d,e,f)

}