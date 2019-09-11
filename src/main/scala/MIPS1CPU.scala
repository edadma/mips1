package xyz.hyperreal.mips1


class MIPS1CPU( val memory: Memory, val endianness: Endianness ) {

  var pc: Int = 0
  val regs = new Array[Int]( 32 )
  val opcodes =
    Array(

    )
  def execute: Boolean = {
    val inst = memory.readInt( pc )


    true
  }

}

sealed abstract class Endianness
case object BigEndian extends Endianness
case object LittleEndian extends Endianness