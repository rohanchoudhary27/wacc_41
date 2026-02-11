package wacc.backend

/* Operands */
sealed trait Operand
case class Immediate(value: Int) extends Operand
case class VirtualReg(regNo: Int) extends Operand
case class Label(name: String) extends Operand

/* Addresses */
sealed trait Address
case class AddrRegOffset(baseAddr: VirtualReg, offset: VirtualReg) extends  Address
case class AddrImmOffset(baseAddr: VirtualReg, offset: Immediate) extends Address
case class AddrLabel(name: String) extends Address
case class AddrStack(offset: Int) extends Address

/* Conditions */
enum Condition {
    case EQ, NE, LT, LE, GT, GE
}

/* Positions in a pair */
enum PairPos {
  case Fst
  case Snd
}

/* ---------------------------------------------------------------------------------------------- */

sealed trait Instruction

/* Arithmetic instructions */
case class Add(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Sub(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Mul(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Div(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Mod(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Lsl(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Lsr(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Asr(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction

/* Boolean instructions */
case class Or(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class And(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction
case class Not(reg: VirtualReg, src: VirtualReg) extends Instruction

/* Branching instructions */
case class Cmp(src1: VirtualReg, src2: Operand) extends Instruction
case class BranchUncond(label: Label) extends Instruction
case class BranchCond(cond: Condition, label: Label) extends Instruction
case class LabelDef(label: Label) extends Instruction

/* Data movement instructions */
case class Mov(destReg: VirtualReg, source: Operand) extends Instruction

/* Memory instructions */
case class Load(destReg: VirtualReg, source: Address) extends Instruction
case class Store(destReg: VirtualReg, source: Address) extends Instruction

/* Function instructions */
case class Call(label: Label, args: List[VirtualReg], dest: Option[VirtualReg]) extends Instruction
case object Ret extends Instruction

/* Stack instructions */
case class Push(sourceReg: VirtualReg) extends Instruction
case class Pop(destReg: VirtualReg) extends Instruction

/* Array put/get instructions */
case class ArrayLoad(dest: VirtualReg, array: VirtualReg, index: Operand) extends Instruction
case class ArrayStore(array: VirtualReg, index: Operand, src: Operand) extends Instruction

/* Pair put/get instructions */
case class PairLoad(dest: VirtualReg, pair: VirtualReg, index: PairPos) extends Instruction
case class PairStore(pair: VirtualReg, index: PairPos, src: Operand) extends Instruction

/* Heap allocation instruction for arrays/pairs */
case class HeapAlloc(dest: VirtualReg, size: Operand) extends Instruction

/* Load the address of a label into a register (no memory access) */
case class Lea(destReg: VirtualReg, label: Label) extends Instruction

/* ---------------------------------------------------------------------------------------------- */

/* Top-level of the IR. Data directives and function definitions come first before the list of
   instructions because directives come first in the assembly file and functions come first in the
   AST (and wacc structure) + they have separate register allocations. Also functions have their
   own stack management so we need to know when it's a func definition and when it's just a normal
   label. */
case class DataDirective(label: Label, storedValue: String)
case class FunctionDef(name: String, body: List[Instruction])
case class Program(funcs: List[FunctionDef], data: List[DataDirective], body: List[Instruction])