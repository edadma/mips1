package xyz.hyperreal.mips1


abstract class Instruction {

  def execute( cpu: CPU, inst: Int ): Boolean

}

abstract class ITypeInstruction extends Instruction {

  def rs( inst: Int ) = (inst >> 21) & 0x1F

  def rt( inst: Int ) = (inst >> 16) & 0x1F

  def imm( inst: Int ) = inst & 0xFFFF

  def execute( cpu: CPU, inst: Int ) = {
    perform( cpu, rs(inst), rt(inst), imm(inst) )
    true
  }

  def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ): Unit

}

abstract class JTypeInstruction extends Instruction {

  def targ( inst: Int ) = inst & 0x3FFFFFF

  def execute( cpu: CPU, inst: Int ) = {
    perform( cpu, targ(inst)<<2 )
    true
  }

  def perform( cpu: CPU, targ: Int ): Unit

}

abstract class RTypeInstruction extends Instruction {

  def rs( inst: Int ) = (inst >> 21) & 0x1F

  def rt( inst: Int ) = (inst >> 16) & 0x1F

  def rd( inst: Int ) = (inst >> 11) & 0x1F

  def shamt( inst: Int ) = (inst >> 6) & 0x1F

  def func( inst: Int ) = inst & 0x3F

  def execute( cpu: CPU, inst: Int ) = {
    perform( cpu, rs(inst), rt(inst), rd(inst), shamt(inst), func(inst) )
    true
  }

  def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ): Unit

}

abstract class ExceptionInstruction extends Instruction {

  def code( inst: Int ) = (inst >> 6) & 0xFFFFF

}