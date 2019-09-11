package xyz.hyperreal.mips1


class CPU( val mem: Memory, val endianness: Endianness ) {

  var pc: Int = 0
  val regs = new Array[Int]( 32 )
  val opcodes =
    Array(

    )

  def put( reg: Int, v: Int ) =
    if (reg != 0)
      regs(reg) = v

  def execute: Boolean = {
    val inst = mem.readInt( pc )


    true
  }

}

sealed abstract class Endianness
case object BigEndian extends Endianness
case object LittleEndian extends Endianness