package xyz.hyperreal.mips1

import collection.mutable.ArrayBuffer


class MMU( mem: Memory ) extends Addressable {

  val name = "MMU"

  override def start: Long = ???

  override def size: Long = ???

  override def readByte(addr: Long): Int = ???

  override def writeByte(addr: Long, value: Int): Unit = ???

}