package xyz.hyperreal.mips1


object LoadInstructions {

  val LB =
    new ITypeInstruction {
      override def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.put( rt, cpu.mem.readByte(cpu.regs(rs + imm)) )
    }
  val LBU =
    new ITypeInstruction {
      override def perform(cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.put( rt, cpu.mem.readByte(cpu.regs(rs) + imm) & 0xFF )
    }
  val LH =
    new ITypeInstruction {
      override def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.put( rt, cpu.mem.readShort(cpu.regs(rs) + imm) )
    }
  val LHU =
    new ITypeInstruction {
      override def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.put( rt, cpu.mem.readShort(cpu.regs(rs) + imm) & 0xFFFF )
    }
  val LUI =
    new ITypeInstruction {
      override def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.put( rt, imm<<16 )
    }
  val LW =
    new ITypeInstruction {
      override def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.put( rt, cpu.mem.readInt(cpu.regs(rs) + imm) )
    }

}