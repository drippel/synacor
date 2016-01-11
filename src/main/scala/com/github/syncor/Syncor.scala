
package com.github.syncor

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import org.apache.logging.log4j.LogManager
import scala.collection.mutable.HashMap

object Syncor {

  val log = LogManager.getLogger("com.github.syncor")
  
  val memory = HashMap[Int,Int]()
  val registers = Array.fill[Int](8)( { 0 }  )
  val stack = Stack[Int]()

  def main( args : Array[String] ) : Unit = {

    log.info("syncor 0.1")

    log.info("reading...")
    val raw = Files.readAllBytes(Paths.get("./src/main/resource/challenge.bin"))
    
    val pairs = ListBuffer[(String,String)]()
    
    for( i <- 0 until raw.size -1 by 2 ){
      
      val p = (formatByte(raw(i).toBinaryString),formatByte(raw(i+1).toBinaryString))
      
      pairs += p
      
    }
    
    val program = pairs.map((p) => {byteToInt(p)} )

    // program.foreach( (c) => { println(c) } )
    
    load(program.toList)
    
    log.debug("vm:program begin")
    for( i <- 0 until memory.size ){
      log.debug( i +" "+ memory(i) ) 
    }
    log.debug("vm:program end")
    
    try {
      execute()
      debugRegisters()
    }
    catch {
      case e : Exception => {
        log.debug(e)
        e.printStackTrace()
      }
    }
    
  }
  
  def load( program : List[Int] ) : Unit = {
    
    for( i <- 0 until program.size ){
      memory += ( i -> program(i) )
    }
    
  }
  
  
  def formatByte( src : String ) : String = {
    if( src.size > 8 ){
      src.substring( src.size - 8 ).reverse
    }
    else {
      src.reverse.padTo(8, '0')
    }
  }
  
  def byteToInt( byte : (String,String) ) : Int = {
    byteToInt(byte._1,byte._2)
  }
  
  def byteToInt( lowByte : String, hiByte : String ) : Int = {
    
    val lo = lowByte.map( toBit(_ ) )
    val hi = hiByte.map( toBit(_ ) )
    
    val i = (lo(0) * 1) + (lo(1) * 2) +(lo(2) * 4) + (lo(3) * 8) + 
      (lo(4) * 16) + (lo(5) * 32) + (lo(6) * 64) + (lo(7) * 128) + 
      (hi(0) * 256) + (hi(1) * 512) + (hi(2) * 1024) + (hi(3) * 2048) +
      (hi(4) * 4096) +(hi(5) * 8192) +(hi(6) * 16384) + (hi(7) * 32768) 

    i
  }
  
  def toBit( c : Char ) : Int = {
    if( c == '0' ){
      0
    }
    else {
      1
    }
  }
  
  def value( i : Int ) : Int = {
    if( i <= 32767 ) { i }
    else if( i >= 32768 && i <= 32775 ){ registers(i-32768) }
    else { throw new IllegalArgumentException("invalid location") }
  }
  
  def set( r : Int, v : Int ) = {
    registers(r) = v
  }
  
  def execute() = {
    
    log.info( "vm:start" )
    
    var running = true
    var pos = 0
    
    while( running ){
      
      val instruction = memory(pos)

      log.debug("vm:pos:" + pos)
      log.debug("vm:instruction:" + instruction)
      
      // move pos forward - instructions below might move back
      instruction match {
        
        case 0 => { 
          log.trace("halt")
          running = false 
        }
        case 1 => { 
          // set: 1 a b
          // set register <a> to the value of <b>
          pos = pos + 1
          val a = (memory(pos) - 32768)

          pos = pos + 1
          val b = memory(pos)
          
          log.debug( "vm:set a:"+ a +" to "+ b )
          
          set( a, value(b) )

          pos = pos + 1
        }
        case 2 => { 
          // push: 2 a
          // push <a> onto the stack
          pos = pos + 1
          val a = value(memory(pos))
          
          stack.push(a)
          
          log.debug( "vm:push a:"+ a )
              
          pos = pos + 1 
        }
        case 3 => { 
          // pop: 3 a
          // remove the top element from the stack and write it into <a>; empty stack = error
          pos = pos + 1
          val a = memory(pos) - 32768
          
          val i = stack.pop()
          
          set( a, i )
              
          log.debug( "vm:pop a:"+ a+ " i:"+ i )
          pos = pos + 1 
        }
        case 4 => { 
          // eq: 4 a b c
          // set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          pos = pos + 1
          val c = value(memory(pos))
          
          val v = if( b == c ){ 1 } else { 0 } 
          set( a, v )
          
          log.debug( "vm:eq a:"+ a +" b:"+ b +" c:" + c + " v:"+ v )
          pos = pos + 1
        }
        case 5 => { 
          // gt: 5 a b c
          // set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          pos = pos + 1
          val c = value(memory(pos))
          
          set( a, if( b > c ) { 1 } else { 0 } )
          
          log.debug( "vm:gt a:"+ a + " b:"+ b + " c:" + c )
          pos = pos + 1

        }
        case 6 => { 
          // jump to <a>
          pos = pos + 1
          val j = value(memory(pos))

          log.debug("jump a:" + j)
          pos = j 
        }
        case 7 => { 
          // if <a> is nonzero, jump to <b>
          pos = pos + 1
          val a = value(memory(pos))

          pos = pos + 1
          val b = value(memory(pos))

          log.debug("jnz:a:"+ a +" b:"+ b)
          pos = if( a != 0 ){ b }
          else { pos + 1 }
        }
        case 8 => {
          // if <a> is zero, jump to <b>
          pos = pos + 1
          val a = value(memory(pos))

          pos = pos + 1
          val b = value(memory(pos))

          log.debug("jz:a:"+ a +" b:"+ b)
          pos = if( a == 0 ){ b }
          else { pos + 1 }
        }
        case 9 => { 
          // add: 9 a b c
          // assign into <a> the sum of <b> and <c> (modulo 32768)
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          pos = pos + 1
          val c = value(memory(pos))
          
          val sum = ( b + c ) % 32768
          
          set( a, sum )
          
          log.debug("vm:add a:"+ a +" b:"+ b +" c:"+ c + " sum:" + sum ) 
          pos = pos + 1
        }
        case 10 => {
          // mult: 10 a b c
          // store into <a> the product of <b> and <c> (modulo 32768)
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          pos = pos + 1
          val c = value(memory(pos)) 
          
          val prod = ( b * c ) % 32768
          
          set( a, prod )
          
          pos = pos + 1
        }
        case 11 => { 
          // mod: 11 a b c
          // store into <a> the remainder of <b> divided by <c>
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos)) 

          pos = pos + 1
          val c = value(memory(pos)) 
          
          val prod = ( b % c ) % 32768 
          
          set( a, prod )
          
          pos = pos + 1
        }
        case 12 => {
          // and: 12 a b c
          // stores into <a> the bitwise and of <b> and <c>
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          pos = pos + 1
          val c = value(memory(pos))
          
          val i = (b & c) % 32768
          
          log.debug( "vm:and a:"+ a +" b:"+ b +" c:" + c +" and:"+ i )
          
          set( a, i )

          pos = pos + 1
        }
        case 13 => { 
          // or: 13 a b c
          // stores into <a> the bitwise or of <b> and <c>
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          pos = pos + 1
          val c = value(memory(pos))
          
          val i = ( b | c ) % 32768
          
          log.debug( "vm:or a:"+ a +" b:"+ b +" c:" + c +" or:"+ i )
          
          set( a, i )
          
          pos = pos + 1
        }
        case 14 => { 
          // not: 14 a b
          // stores 15-bit bitwise inverse of <b> in <a>
          pos = pos + 1
          val a = memory(pos) - 32768

          pos = pos + 1
          val b = value(memory(pos))

          val i = ( ~(b) & 32767)
          
          log.debug( "vm:not a:"+ a +" b:"+ b +" not:"+ i )
          
          set( a, i )
          pos = pos + 1
        }
        case 15 => { 
          // rmem: 15 a b
          // read memory at address <b> and write it to <a>
          pos = pos + 1
          val a = memory(pos)

          pos = pos + 1
          val b = memory(pos) 

          debugRegisters()
          
          val v = memory(value(b))

          log.debug( "vm:rmem a:"+ a + " b:" + b + " v:"+ v )
          
          if( a >= 32768 ){
            set( (a-32768), v)
          }
          else {
            memory(a) = v
          }

          debugRegisters()

          pos = pos + 1
        }
        case 16 => {
          // wmem: 16 a b
          // write the value from <b> into memory at address <a>
          pos = pos + 1
          val a = memory(pos)
          
          val m = value(a)

          pos = pos + 1
          val b = memory(pos) 
          val v = value(memory(pos)) 
          
          debugRegisters()
          log.debug( "vm:wmem a:"+ a +" m:" + m + " b:" + b +" v:"+ v )
          
          memory(m) = v 

          debugRegisters()
          
          pos = pos + 1
        }
        case 17 => { 
          // call: 17 a
          // write the address of the next instruction to the stack and jump to <a>
          pos = pos + 1
          val a = value(memory(pos)) 
          
          stack.push(pos + 1)
          
          log.debug("vm:call a:"+ a )
          pos = a
        }
        case 18 => {
          // remove the top element from the stack and jump to it; empty stack = halt
          log.debug("vm:return")
          if( stack.isEmpty ){
            running = false
          }
          else {
            pos = stack.pop()
          }
        }
        case 19 => {
         log.trace("out") 
         pos = pos + 1
         val c = value(memory(pos)).toChar

         print(c)
         pos = pos + 1
        }
        case 20 => { 
          // read a character from the terminal and write its ascii code to <a>; 
          // it can be assumed that once input starts, it will continue until a newline is encountered; 
          // this means that you can safely read whole lines from the keyboard and trust that they will be fully read
          pos = pos + 1
          val a = memory(pos) 
          val r = a - 32768 

          val c = currentLine match {
            
            case s : Some[String] => {
              
              // write the next char to a
              
              val c = s.get.head
              
              val t = s.get.tail
              
              if( t.isEmpty() ){
                currentLine = None
              }
              else {
                currentLine = Some(t)
              }
              
              c
              
            }
            case None => {
              
              // read in a new line
              var line = scala.io.StdIn.readLine()
              line = line + "\n"
              
              val c = line.head
              currentLine = Some(line.tail)

              c
              
            }
          }
          
          set(r,c.toInt)
          
          pos = pos + 1
				}
        case 21 => {
          log.trace("noop")
          pos = pos + 1
        }
        case _ => {
          log.info("unknown:" + pos +" "+ memory(pos) )
          running = false
          pos = pos + 1
        }
      
      }
    }
    
    log.info( "halting on:" + pos +" instruction" + memory(pos) ) 
    printRegisters()

    log.info( "vm:end" )
  }
  
  var currentLine : Option[String] = None
  
  def debugRegisters() = {
    
    val l = ListBuffer[Int]() 
    for( i <- 0 until registers.size ) {
      l += registers(i)
    }
    log.debug( "vm:registers: " + l )
    
  }
  
  def printRegisters() = {
    
    val l = ListBuffer[Int]() 
    for( i <- 0 until registers.size ) {
      l += registers(i)
    }
    log.info( "vm:registers: " + l )
    
  }
}
