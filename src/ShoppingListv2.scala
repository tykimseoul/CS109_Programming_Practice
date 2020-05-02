import scala.io.StdIn.{readLine,readInt}
import scala.util.control.Breaks._

  var grandtotal=0
  var items=Array.empty[String]
  var quantities=Array.empty[Int]
  var prices=Array.empty[Int]
  var totals=Array.empty[Int]
  def input(){
      var item=readLine("What did you buy? ")
      while(item!=""){
        items=items:+item
        print("How many did you buy? ")
        var quantity=readInt()
        quantities=quantities:+quantity
        print("What is the price of each? ")
        var price=readInt()
        prices=prices:+price
        var total=quantity*price
        totals=totals:+total
        grandtotal=grandtotal+total
        println("You bought "+quantity.toString+" "+item.toString+" for "+price.toString+" KRW")
        item=readLine("What did you buy? ")
      }
    println(items.length)
    println("Your purchases:")
    println("-----------------------------------------")
    for (i<-0 to (items.length-1)){
      print(items(i)+"\t\t")
      print(quantities(i))
      print(" * ")
      print(prices(i))
      print(" KRW")
      print("\t")
      print(totals(i))
      println(" KRW")
    }
    println("-----------------------------------------")
  }
    
  
  input()