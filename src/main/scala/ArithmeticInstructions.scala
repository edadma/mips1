package xyz.hyperreal.mips1


object ArithmeticInstructions {

  def add( a: Int, b: Int, c: Int, cpu: CPU ) = {
    val sum64 = cpu.regs(a) + b.toLong
    val sum32 = sum64.toInt

    if ((((sum64 >> 32)&1) ^ (sum32 >>> 31)) == 1)
      cpu.exception( "overflow" )
    else
      cpu.put( c, sum32 )
  }

  val ADD =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = add( rs, cpu.regs(rt), rd, cpu )
    }
  val ADDI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = add( rs, imm, cpu.regs(rt), cpu )
    }
  val ADDIU =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, cpu.regs(rs) + imm )
    }
  val ADDU =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rs) + cpu.regs(rt) )
    }
  val DIV =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        cpu.lo = cpu.regs(rs) / cpu.regs(rt)
        cpu.hi = cpu.regs(rs) % cpu.regs(rt)
      }
    }
  val DIVU =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        val rsu = cpu.regs(rs).toLong&0xFFFFFFFF
        val rtu = cpu.regs(rt).toLong&0xFFFFFFFF

        cpu.lo = (rsu / rtu).toInt
        cpu.hi = (rsu % rtu).toInt
      }
    }
  val MULT =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        val prod = cpu.regs(rs).toLong * cpu.regs(rt)

        cpu.lo = prod.toInt
        cpu.hi = (prod >>> 32).toInt
      }
    }
  val MULTU =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        val rsu = cpu.regs(rs).toLong&0xFFFFFFFF
        val rtu = cpu.regs(rt).toLong&0xFFFFFFFF
        val prod = rsu * rtu

        cpu.lo = prod.toInt
        cpu.hi = (prod >>> 32).toInt
      }
    }
  val SUB =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        val diff64 = cpu.regs(rs).toLong - cpu.regs(rt)
        val diff32 = diff64.toInt

        if ((((diff64 >> 32)&1) ^ (diff32 >>> 31)) == 1)
          cpu.exception( "overflow" )
        else
          cpu.put( rd, diff32 )
      }
    }
  val SUBU =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rs) - cpu.regs(rt) )
    }

}