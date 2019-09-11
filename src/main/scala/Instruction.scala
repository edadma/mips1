package xyz.hyperreal.mips1


abstract class Instruction {

  def execute( cpu: MIPS1CPU, inst: Int ): Boolean

}

abstract class ITypeInstruction extends Instruction {

  def rs( inst: Int ) = (inst >> 21) & 0x1F

  def rt( inst: Int ) = (inst >> 16) & 0x1F

  def imm( inst: Int ) = inst & 0xFF

}

abstract class JTypeInstruction extends Instruction {

  def targ( inst: Int ) = inst & 0x3FFFFFF

}

abstract class RTypeInstruction extends Instruction {

  def rs( inst: Int ) = (inst >> 21) & 0x1F

  def rt( inst: Int ) = (inst >> 16) & 0x1F

  def rd( inst: Int ) = (inst >> 11) & 0x1F

  def shamt( inst: Int ) = (inst >> 6) & 0x1F

  def func( inst: Int ) = inst & 0x3F


}