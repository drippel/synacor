import scala.collection.mutable.Stack

object testsheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  println( 1.toBinaryString )                     //> 1
  println( 2.toBinaryString )                     //> 10
  println( 256.toBinaryString )                   //> 100000000
  println( (65536 - 1).toBinaryString )           //> 1111111111111111
  
  var s = 2.toBinaryString                        //> s  : String = 10
  s = s.reverse
  s = s.padTo(16,'0')
  println(s)                                      //> 0100000000000000
  
  val t = (s.substring(0,8),s.substring(8))       //> t  : (String, String) = (01000000,00000000)
  println(t)                                      //> (01000000,00000000)
  
  val st = Stack[Int]()                           //> st  : scala.collection.mutable.Stack[Int] = Stack()
  
  st.push(10)                                     //> res0: testsheet.st.type = Stack(10)
  st.push(15)                                     //> res1: testsheet.st.type = Stack(15, 10)
  
  println(st.pop())                               //> 15
  println(st.pop())                               //> 10
  
  val i = 0x7FFF                                  //> i  : Int = 32767
  
  println( 22494 & 32443 )                        //> 22170
  //  and:22170

  println( (32).toBinaryString )                  //> 100000
  println( (~32).toBinaryString )                 //> 11111111111111111111111111011111
  println( ((~32) | 32768).toBinaryString )       //> 11111111111111111111111111011111

  val c = 'A'                                     //> c  : Char = A
  println(c)                                      //> A
  println(c.toInt)                                //> 65
  

}