package xyz.hyperreal.mips1


object LoadInstructions {

  val LB =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => cpu.put(rt, cpu.mem.readByte(rsv + imm)) )
    }
  val LBU =
    new ITypeInstruction {
      def perform(cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => cpu.put(rt, cpu.mem.readByte(rsv + imm) & 0xFF) )
    }
  val LH =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => cpu.put(rt, cpu.mem.readShort(rsv + imm)) )
    }
  val LHU =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => cpu.put(rt, cpu.mem.readShort(rsv + imm) & 0xFFFF) )
    }
  val LUI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay0( cpu => cpu.put( rt, imm<<16) )
    }
  val LW =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => cpu.put(rt, cpu.mem.readInt(rsv + imm)) )
    }

}