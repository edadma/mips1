package xyz.hyperreal.mips1

import scala.collection.mutable


class CPU( val mem: Memory, val endianness: Endianness ) {

  import LoadInstructions._
  import StoreInstructions._
  import ArithmeticInstructions._
  import JumpInstructions._

  var pc: Int = 0
  val regs = new Array[Int]( 32 )
  val special =
    new RTypeInstruction {
      override def execute( cpu: CPU, inst: Int ) = functions(func(inst)).execute( cpu, inst )

      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {}
    }
  val opcodes =
    Array[Instruction](
      special,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      ADDI,//8
      ADDIU,
      null,
      null,
      null,
      null,
      null,
      delay( LUI ),
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
      delay( LB ),//20
      null,
      null,
      null,
      delay( LBU ),
      delay( LW ),
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
  val functions =
    Array[Instruction](
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      delay( JR ),//8
      null,
      null,
      null,
      null,
      null,
      null,
      null,
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
      ADD
    )
  val delayQueue = new mutable.Queue[(Instruction, Int)]

  def delay( delayed: Instruction ) = new DelayedInstruction( delayed )

  def exception( ex: String ) = {
    println( s"$ex at $pc" )
  }

  def put( reg: Int, v: Int ) =
    if (reg != 0)
      regs(reg) = v

  def execute: Boolean = {
    val inst = mem.readInt( pc )
    val cont =
      if (opcodes(inst) eq null) {
        println( s"unimplemented instruction at $pc" )
        false
      } else {
        pc += 4
        opcodes(inst >>> 26).execute( this, inst )
      }
    val (delayed, inst1) = delayQueue.dequeue

    delayed.execute( this, inst1 )
    cont
  }

}

sealed abstract class Endianness
case object BigEndian extends Endianness
case object LittleEndian extends Endianness