package xyz.hyperreal.mips1


object LoadInstructions {

  val LB =
    new ITypeInstruction {
      override def perform( cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), cpu.mem.readByte(cpu.regs(rs(inst)) + imm(inst)) )
    }
  val LBU =
    new ITypeInstruction {
      override def perform(cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), cpu.mem.readByte(cpu.regs(rs(inst)) + imm(inst)) & 0xFF )
    }
  val LH =
    new ITypeInstruction {
      override def perform( cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), cpu.mem.readShort(cpu.regs(rs(inst)) + imm(inst)) )
    }
  val LHU =
    new ITypeInstruction {
      override def perform( cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), cpu.mem.readShort(cpu.regs(rs(inst)) + imm(inst)) & 0xFFFF )
    }
  val LUI =
    new ITypeInstruction {
      override def perform( cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), imm(inst)<<16 )
    }
  val LW =
    new ITypeInstruction {
      override def perform( cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), cpu.mem.readInt(cpu.regs(rs(inst)) + imm(inst)) )
    }

}