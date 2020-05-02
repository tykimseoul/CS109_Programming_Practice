import scala.io.StdIn.readLine

val MaxNumGuesses = 10
val CodeLength=3
// Create secret: four distinct letters from A-F.
def createSecret(): String = {
  var c = "ABCDEF"
  var secret = ""
  for (i <- 1 to CodeLength) {
    val index = (math.random * c.length).toInt
    val letter = c(index)
    //c = c.substring(0,index) + c.substring(index+1)
    secret = secret + letter
  }
  secret
}

// Check if guess is legal: Four distinct letters from A-F.
// Returns pair (ok: Boolean, message : String) 
def checkGuess(guess: String): (Boolean, String) = {
  if (guess.length != CodeLength)
    return (false, "Your guess must have "+CodeLength+" letters")
  for (i <- 0 until CodeLength) {
    val letter = guess(i)
    if (!"ABCDEF".contains(letter))
      return (false, "You can only use letters A, B, C, D, E, and F.")
    //for (j <- 0 until i) {
     // if (letter == guess(j))
	//return (false, "All letters must be distinct.")
   // }
  }
  return (true, "")
}

// read a guess from the terminal
def getGuess(): String = {
  while (true) {
    var guess = readLine("Enter your guess> ")
    guess = guess.trim.toUpperCase.replace(" ", "")
    val (ok, msg) = checkGuess(guess)
    if (ok)
      return guess
    println(msg)
  }
  "" // only necessary because compiler complains otherwise
}

// Compute (pos, let) where pos is the number of correct letters in
// the correct position, and let is the number of correct letters in
// the wrong position.
def evaluateGuess(secret: String, guess: String): (Int, Int) = {
  var pos = 0
  var let = 0
  var scr=secret
  var ges=guess
  var l=CodeLength
  var scrLeft=""
  var gesLeft=""
  var common=""
  var inScr=0
  var inGes=0
  for (i <- 0 until l) {
    if (ges(i) == scr(i)){
      pos += 1
      common+=scr(i)
      scr=scr.substring(0,i)+"O"+scr.substring(i+1)
      ges=ges.substring(0,i)+"O"+ges.substring(i+1)
    }
  }
  scr=scr.replace("O","")//otherwise the index changes
  ges=ges.replace("O","")
  var together=""
  for (i <- 0 until scr.length){
    if (together.count(_==scr(i))==0){
      together+=scr(i)
    }
  }
  for (i <- 0 until ges.length){
    if (together.count(_==ges(i))==0){
      together+=ges(i)
    }
  }
  //print(scr,ges, common,together)
  for (i <- 0 until together.length) {
    inScr=scr.count(_ ==together(i))
    inGes=ges.count(_ ==together(i))
    if (inGes>=inScr){
      let+=inScr
    }
    else{
      let+=inGes
    }
    //print(i,inScr, inGes)
  }
  //println("")
  (pos, let)
}

// Show history of guessing  
def showHistory(h: Array[String], current: Int, secret: String) {
  for (count <- 0 until current) {
    val guess = h(count)
    val (pos, let) = evaluateGuess(secret, guess)
    printf("%2d: %s : %d positions, %d letters\n", count+1, guess, pos, let)
  }
}

// main game  
def main() {    
  val secret = createSecret()

  val history = new Array[String](MaxNumGuesses)
  var current = 0
  println("Welcome to Mastermind!")
  println("I have created a secret combination:")
  println(CodeLength+" letters from A - F.")
  printf("You have %d guesses to find it.\n", MaxNumGuesses)
  println(secret)
  while (true) {
    showHistory(history, current, secret)
    println("")
    if (current == MaxNumGuesses) {
      printf("My secret was %s, you failed to find it in %d guesses!\n",
             secret, current)
      return
    }
    val guess = getGuess()
    history(current) = guess
    current += 1    
    val (pos, let) = evaluateGuess(secret, guess)
    if (pos == CodeLength) {
      printf("My secret was %s, you guessed correctly in %d guesses!\n",
             secret, current)
      return
    }
  }
}

main()

