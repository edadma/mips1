package xyz.hyperreal.mips1


class CPU( val mem: Memory, val endianness: Endianness ) {

  import LoadInstructions._
  import StoreInstructions._
  import ArithmeticInstructions._

  var pc: Int = 0
  val regs = new Array[Int]( 32 )
  val opcodes =
    Array[Instruction](
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      ADDI,//8
      null,
      null,
      null,
      null,
      null,
      null,
      LUI,
      null,//10
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,//18
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      LB,//20
      null,
      null,
      null,
      LBU,
      LW,
      null,
      null,
      SB,//28
      SH,
      null,
      SW,
      null,
      null,
      null,
      null,
      null,//30
    )

  def exception( ex: String ) = {
    println( s"$ex at $pc" )
  }

  def put( reg: Int, v: Int ) =
    if (reg != 0)
      regs(reg) = v

  def execute: Boolean = {
    val inst = mem.readInt( pc )

    if (opcodes(inst) eq null) {
      println( s"unimplemented instruction at $pc" )
      false
    } else {
      pc += 4
      opcodes(inst >>> 26).execute( this, inst )
    }
  }

}

sealed abstract class Endianness
case object BigEndian extends Endianness
case object LittleEndian extends Endianness