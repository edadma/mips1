package xyz.hyperreal.mips1

import scala.collection.mutable


class CPU( val mem: Memory, val endianness: Endianness ) {

  import LoadInstructions._
  import StoreInstructions._
  import ArithmeticInstructions._
  import JumpInstructions._
  import LogicInstructions._

  var pc: Int = 0
  val regs = new Array[Int]( 32 )
  val special =
    new RTypeInstruction {
      override def execute( cpu: CPU, inst: Int ) = functions(func(inst)).execute( cpu, inst )

      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {}
    }
  val branch =
    new ITypeInstruction {
      override def execute( cpu: CPU, inst: Int ) = branches(rt(inst)).execute( cpu, inst )

      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = {}
    }
  val opcodes =
    Array[Instruction](
      special,
      branch,
      null,
      null,
      null,
      BEQ,
      null,
      null,
      BGTZ,
      ADDI,//8
      ADDIU,
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
      JR,//8
      null,
      null,
      null,
      ANDI,
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
      ADD,//20
      ADDU,
      null,
      null,
      null,
      null,
      AND,
      null,
      null,//28
    )
  val branches =
    Array[Instruction](
      null,
      BGEZ,
      null,
      null,
      null,
      null,
      null,
      null,
      null,//8
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      BGEZAL
    )
  val delayQueue = new mutable.Queue[DelayedInstruction]

  def delay0( action: CPU => Unit ): Unit = delayQueue.enqueue( action(_) )

  def delay1( r: Int, action: (CPU, Int) => Unit ): Unit =
    delayQueue.enqueue(
      new DelayedInstruction {
        private val rv = regs( r )

        def execute( cpu: CPU ) = action( cpu, rv )
      } )

  def delay2( r1: Int, r2: Int, action: (CPU, Int, Int) => Unit ): Unit =
    delayQueue.enqueue(
      new DelayedInstruction {
        private val r1v = regs( r1 )
        private val r2v = regs( r2 )

        def execute( cpu: CPU ) = action( cpu, r1v, r2v )
      } )

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
    val delayed = delayQueue.dequeue

    delayed.execute( this )
    cont
  }

}

abstract class DelayedInstruction {
  def execute( cpu: CPU ): Unit
}

sealed abstract class Endianness
case object BigEndian extends Endianness
case object LittleEndian extends Endianness