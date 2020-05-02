import scala.io.StdIn.{readLine,readInt}


val first=19010101
val monthLength = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
val leapMonthLength = Array(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
val fourYears = (365 + 365 + 365 + 366)

def string2date(s: String): (Int, Int, Int) = {
  var year=s.substring(0,4)
  var month=s.substring(5,7)
  var day=s.substring(8,10)
  return (year.toInt, month.toInt, day.toInt)
}

// returns the number of days since 1901/01/01 (day 0)
// returns -1 if illegal, or out of range 1901~2099
def date2num(year: Int, month: Int, day: Int): Int = {
  if (year < 1901 || year > 2099 || month < 1 || month > 12)
    return -1
  val is_leap = (year % 4 == 0)
  val ml = if (is_leap) leapMonthLength else monthLength
  if (day < 1 || day > ml(month-1))
    return -1
  val yearn = year - 1901
  val monthn = month - 1
  val dayn = day - 1
  var total = 0
  total += fourYears * (yearn / 4)
  total += 365 * (yearn % 4)
  for (m <- 0 until monthn)
    total += ml(m)
  total += dayn
  total
}

def find(x:String):String={
  val (year, month, day) = string2date(x)
  //println(x, year, month, day)
  var y=date2num(year, month, day)
  //println(y)
  if(y==(-1)){
    "Illegal Date"
  }
  else if(y%7==0){
    "Tuesday"
  }
  else if(y%7==1){
    "Wednesday"
  }
  else if(y%7==2){
    "Thursday"
  }
  else if(y%7==3){
    "Friday"
  }
  else if(y%7==4){
    "Saturday"
  }
  else if(y%7==5){
    "Sunday"
  }
  else{
    "Monday"
  }
}

def date2string(a: Int, b:Int, c:Int):(String)={
  return a+"/"+b+"/"+c
}

def num2date(n: Int): (Int, Int, Int) = {
  var g=n+1
  var yearn=1901+g/fourYears*4
  g=g%fourYears
  yearn+=g/365
  g=g%365
  var m=0
  var ml=monthLength
  if ((yearn-2016)%4==0){
    ml=leapMonthLength
  }
  while(g>ml(m)){
    g-=ml(m)
    m+=1
  }
  var monthn=m+1
  var dayn=g
  return (yearn, monthn, dayn)
}

def DayCalc(a:String, b:String, c:String){
  if(c==""&&b==""&&a.length==10){
    print(a+" is a "+find(a))
  }
  else if(a.length<10){
    print("Wrong format!")
  }
  else if(c==""){
    var fr=date2num(a.substring(0,4).toInt,a.substring(5,7).toInt,a.substring(8).toInt)
    var se=date2num(b.substring(0,4).toInt,b.substring(5,7).toInt,b.substring(8).toInt)
    if(a.length<10 || b.length<10){
      print("Wrong format!")
    }
    else if(fr==(-1) || se==(-1)){
      print("Wrong usage!")
    }
    else{
      printf("There are %d days between "+a+" and "+b, se-fr)
    }
  }
  else if(c.toInt>=40000){
    print("Out of range!")
  }
  else if(b=="+"){
    var adate=string2date(a)
    var result=date2num(adate._1, adate._2, adate._3)+c.toInt
    var date=num2date(result)
    if(a.length<10){
      print("Wrong format!")
    }
    else{
      printf(a+" + "+c+" days = "+date2string(date._1, date._2, date._3))
    }
  }
  else if(b=="-"){
    var adate=string2date(a)
    var result=date2num(adate._1, adate._2, adate._3)-c.toInt
    var date=num2date(result)
    if(a.length<10){
      print("Wrong format!")
    }
    else{
      printf(a+" - "+c+" days = "+date2string(date._1, date._2, date._3))
    }
  }
  else{
    print("Wrong usage!")
  }
}



def testAll() {
  for (year <- 1901 to 1909) {
    for (month <- 1 to 12) {
      for (day <- 1 to 28) {
        val n = date2num(year, month, day)
	val (y,m,d) = num2date(n)
	print(n, y,m,d)
	println(year, month, day)
	//if (year != y || month != m || day != d)
	  //println(n, y, m, d)
      }
    }
  }
}
val d=args(0).toString
val e=if (args.length >1) args(1).toString else ""
val f=if (args.length >2) args(2).toString else ""
DayCalc(d, e, f)
