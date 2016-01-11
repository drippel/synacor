import java.nio.file.{Files, Paths}

import scala.collection.mutable
import scala.io.StdIn

/*
== architecture ==
- three storage regions
  - memory with 15-bit address space storing 16-bit values
  - eight registers
  - an unbounded stack which holds individual 16-bit values
- all numbers are unsigned integers 0..32767 (15-bit)
- all math is modulo 32768; 32758 + 15 => 5

== binary format ==
- each number is stored as a 16-bit little-endian pair (low byte, high byte)
- numbers 0..32767 mean a literal value
- numbers 32768..32775 instead mean registers 0..7
- numbers 32776..65535 are invalid
- programs are loaded into memory starting at address 0
- address 0 is the first 16-bit value, address 1 is the second 16-bit value, etc

== execution ==
- After an operation is executed, the next instruction to read is immediately after the last argument of the current operation.  If a jump was performed, the next operation is instead the exact destination of the jump.
- Encountering a register as an operation argument should be taken as reading from the register or setting into the register as appropriate.

== hints ==
- Start with operations 0, 19, and 21.
- Here's a code for the challenge website: dzcWIZXhisKH
- The program "9,32768,32769,4,19,32768" occupies six memory addresses and should:
  - Store into register 0 the sum of 4 and the value contained in register 1.
  - Output to the terminal the character with the ascii code contained in register 0.

== opcode listing ==
halt: 0
  stop execution and terminate the program
set: 1 a b
  set register <a> to the value of <b>
push: 2 a
  push <a> onto the stack
pop: 3 a
  remove the top element from the stack and write it into <a>; empty stack = error
eq: 4 a b c
  set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
gt: 5 a b c
  set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
jmp: 6 a
  jump to <a>
jt: 7 a b
  if <a> is nonzero, jump to <b>
jf: 8 a b
  if <a> is zero, jump to <b>
add: 9 a b c
  assign into <a> the sum of <b> and <c> (modulo 32768)
mult: 10 a b c
  store into <a> the product of <b> and <c> (modulo 32768)
mod: 11 a b c
  store into <a> the remainder of <b> divided by <c>
and: 12 a b c
  stores into <a> the bitwise and of <b> and <c>
or: 13 a b c
  stores into <a> the bitwise or of <b> and <c>
not: 14 a b
  stores 15-bit bitwise inverse of <b> in <a>
rmem: 15 a b
  read memory at address <b> and write it to <a>
wmem: 16 a b
  write the value from <b> into memory at address <a>
call: 17 a
  write the address of the next instruction to the stack and jump to <a>
ret: 18
  remove the top element from the stack and jump to it; empty stack = halt
out: 19 a
  write the character represented by ascii code <a> to the terminal
in: 20 a
  read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
noop: 21
  no operation
*/

class Opcode(code: Int)

case class Halt() extends Opcode(0)

case class Set(register: Int, value: Int) extends Opcode(1)

case class Push(value: Int) extends Opcode(2)

case class Pop(address: Int) extends Opcode(3)

case class Eq(address: Int, ref: Int, ref2: Int) extends Opcode(4)

case class Gt(address: Int) extends Opcode(3)

case class Out(value: Int) extends Opcode(19)

case class Noop() extends Opcode(21)

object VM extends App {
  val mem = scala.collection.mutable.Map[Int, Int]()
  val stack = mutable.Stack[Int]()

  def getValue(ref: Int, regs: Array[Int]): Int = {
    if (ref <= 32767) ref
    else if (ref <= 32775) regs(ref - 32768)
    else throw new IllegalArgumentException(s"Invalid ref: $ref")
  }

  def setValue(ref: Int, value: Int, regs: Array[Int], mem: mutable.Map[Int, Int]): Unit = {
    //    if (ref <= 0x7FFF) mem(ref) = value else
    if (ref >= 32768 && ref <= 32775) regs(ref - 32768) = value
    else throw new IllegalArgumentException(s"Invalid ref: $ref")
  }

  def evaluate(ops: Seq[Int], regs: Array[Int], mem: mutable.Map[Int, Int], stack: mutable.Stack[Int]) = {
    var ip = 0
    while (ip >= 0 && ip < ops.length) {
      ops(ip) match {
        case 0 => // stop execution and terminate the program
          ip = ops.length
        case 1 => // set: 1 a b : set register <a> to the value of <b>
          regs(ops(ip + 1) - 32768) = getValue(ops(ip + 2), regs)
          ip = ip + 3
        case 2 => // push: 2 a : push <a> onto the stack
          stack.push(getValue(ops(ip + 1), regs))
          ip = ip + 2
        case 3 => // pop: 3 a : remove the top element from the stack and write it into <a>; empty stack = error
          setValue(ops(ip + 1), stack.pop, regs, mem)
          ip = ip + 2
        case 4 => // eq: 4 a b c : set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
          setValue(ops(ip + 1), if (getValue(ops(ip + 2), regs) == getValue(ops(ip + 3), regs)) 1 else 0, regs, mem)
          ip = ip + 4
        case 5 => // gt: 5 a b c : set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
          setValue(ops(ip + 1), if (getValue(ops(ip + 2), regs) > getValue(ops(ip + 3), regs)) 1 else 0, regs, mem)
          ip = ip + 4
        case 6 => // jmp: 6 a : jump to <a>
          ip = getValue(ops(ip + 1), regs)
        case 7 => // jt: 7 a b : if <a> is nonzero, jump to <b>
          ip = if (getValue(ops(ip + 1), regs) != 0) getValue(ops(ip + 2), regs) else ip + 3
        case 8 => // jf: 8 a b : if <a> is zero, jump to <b>
          ip = if (getValue(ops(ip + 1), regs) == 0) getValue(ops(ip + 2), regs) else ip + 3
        case 9 => // add: 9 a b c : assign into <a> the sum of <b> and <c> (modulo 32768)
          setValue(ops(ip + 1), (getValue(ops(ip + 2), regs) + getValue(ops(ip + 3), regs)) % 32768, regs, mem)
          ip = ip + 4
        case 10 => // mult: 10 a b c : store into <a> the product of <b> and <c> (modulo 32768)
          setValue(ops(ip + 1), (getValue(ops(ip + 2), regs) * getValue(ops(ip + 3), regs)) % 32768, regs, mem)
          ip = ip + 4
        case 11 => // mod: 11 a b c : store into <a> the remainder of <b> divided by <c>
          setValue(ops(ip + 1), (getValue(ops(ip + 2), regs) % getValue(ops(ip + 3), regs)) % 32768, regs, mem)
          ip = ip + 4
        case 12 => // and: 12 a b c : stores into <a> the bitwise and of <b> and <c>
          setValue(ops(ip + 1), (getValue(ops(ip + 2), regs) & getValue(ops(ip + 3), regs)) % 32768, regs, mem)
          ip = ip + 4
        case 13 => // or: 13 a b c : stores into <a> the bitwise or of <b> and <c>
          setValue(ops(ip + 1), (getValue(ops(ip + 2), regs) | getValue(ops(ip + 3), regs)) % 32768, regs, mem)
          ip = ip + 4
        case 14 => // not: 14 a b : stores 15-bit bitwise inverse of <b> in <a>
          setValue(ops(ip + 1), (~getValue(ops(ip + 2), regs)) & 0x7FFF, regs, mem)
          ip = ip + 3
        case 15 => // rmem: 15 a b : read memory at address <b> and write it to <a>
          setValue(ops(ip + 1), mem(ops(ip + 2)), regs, mem)
          ip = ip + 3
        case 16 => // wmem: 16 a b : write the value from <b> into memory at address <a>
          mem(ops(ip + 1)) = getValue(ops(ip + 2), regs)
          ip = ip + 3
        case 17 => // call: 17 a : write the address of the next instruction to the stack and jump to <a>
          stack.push(ip + 1)
          ip = getValue(ops(ip + 1), regs)
        case 18 => // ret: 18 : remove the top element from the stack and jump to it; empty stack = halt
          if (stack.nonEmpty) {
            ip = getValue(stack.pop(), regs)
          } else ip = ops.length
          mem(ops(ip + 1)) = getValue(ops(ip + 2), regs)

        case 19 => // out: 19 a : write the character represented by ascii code <a> to the terminal
          print(ops(ip + 1).toChar)
          ip = ip + 2
        case 20 => // in: 20 a :
          // read a character from the terminal and write its ascii code to <a>; it can be assumed that once
          // input starts, it will continue until a newline is encountered; this means that you can safely read
          // whole lines from the keyboard and trust that they will be fully read
          val x = StdIn.readLine()
          setValue(ops(ip + 1), x(0), regs, mem)
          ip = ip + 2
        case 21 => // noop: 21 : no operation
          ip = ip + 1
        case x => throw new IllegalArgumentException(s"Unsupported opcode: $x at ip=$ip")
      }
    }
  }


  val code = load("./src/main/resource/challenge.bin")
  evaluate(code, new Array[Int](8), mutable.Map(), mutable.Stack())


  def load(file: String): Seq[Int] = {
    val byteArray = Files.readAllBytes(Paths.get(file))
    var i = -1
    byteArray.grouped(2).map { x =>
      i = i + 1
      val u = (x(1) << 8) & 0xFFFF
      val l = x(0) & 0xFF
      val t = u | l
      val z = t
      if (z < 0) throw new IllegalStateException(s"$t $i")
      z
    }.toIndexedSeq
  }
}

