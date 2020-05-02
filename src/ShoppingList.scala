import scala.io.StdIn.{readLine,readInt}

  var grandtotal=0
  def input(){
    var item=readLine("What did you buy? ")
    while(item!=""){
      print("How many did you buy? ")
      var quantity=readInt()
      print("What is the price of each? ")
      var price=readInt()
      var total=quantity*price
      grandtotal=grandtotal+total
      println("You bought "+quantity.toString+" "+item.toString+" for "+price.toString+" KRW")
      item=readLine("What did you buy? ")
    }
      println("In total, you spent "+grandtotal+" KRW")
  }
input()

