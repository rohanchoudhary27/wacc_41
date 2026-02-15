package wacc.backend

/* Operands */
sealed trait Operand
case class Immediate(value: Int) extends Operand
case class VirtualReg(regNo: Int, size: Size) extends Operand
case class Label(name: String) extends Operand

/* Addresses */
sealed trait Address
case class AddrRegOffset(baseAddr: VirtualReg, offset: VirtualReg) extends  Address
case class AddrImmOffset(baseAddr: VirtualReg, offset: Immediate) extends Address
case class AddrLabel(name: String) extends Address
case class AddrStack(offset: Int) extends Address

/* ---------------------------------------------------------------------------------------------- */

/* Instruction trait and abstract classes to assert correct register sizes */

sealed trait Instruction

abstract class BinOp(dest: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction {
    require(dest.size == src1.size, 
        s"BinOp Size Mismatch: Dest ${dest.size} != Src1 ${src1.size}")
    
    src2 match {
        case v: VirtualReg => require(dest.size == v.size, 
            s"BinOp Size Mismatch: Dest ${dest.size} != Src2 ${v.size}")
        case _ =>
    }
}

abstract class UnOp(dest: VirtualReg, src: Operand) extends Instruction {
    src match {
        case v: VirtualReg => require(dest.size == v.size, 
            s"UnOp Size Mismatch: Dest ${dest.size} != Src ${v.size}")
        case _ => 
    }
}

abstract class CmpOp(src1: VirtualReg, src2: Operand) extends Instruction {
    src2 match {
        case v: VirtualReg => require(src1.size == v.size, 
            s"Cmp Size Mismatch: Src1 ${src1.size} != Src2 ${v.size}")
        case _ =>
    }
}

abstract class PtrInstr(ptr: VirtualReg) extends Instruction {
    require(ptr.size == Size.X, s"Pointer Source must be 64-bit, got ${ptr.size}")
}

/* ---------------------------------------------------------------------------------------------- */

/* Instruction case classes */

/* Binary operations */
case class Add(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Sub(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Mul(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Div(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Mod(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Lsl(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Lsr(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Asr(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class Or(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
case class And(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)

/* Unary operations */
case class Neg(reg: VirtualReg, src: VirtualReg) extends UnOp(reg, src)
case class Not(reg: VirtualReg, src: VirtualReg) extends UnOp(reg, src)
case class Mov(reg: VirtualReg, src: Operand) extends UnOp(reg, src)

/* Branching instructions */
case class Cmp(src1: VirtualReg, src2: Operand) extends CmpOp(src1, src2)

case class BranchUncond(label: Label) extends Instruction
case class BranchCond(cond: Condition, label: Label) extends Instruction
case class SetCond(dest: VirtualReg, cond: Condition) extends Instruction
case class LabelDef(label: Label) extends Instruction

/* Load/Store instructions */
case class Load(destReg: VirtualReg, source: Address) extends Instruction
case class Store(destReg: VirtualReg, source: Address) extends Instruction

/* Function instructions */
case class Call(label: Label, args: List[VirtualReg], dest: Option[VirtualReg]) extends Instruction
case object Ret extends Instruction

/* Stack instructions */
case class Push(sourceReg: VirtualReg) extends Instruction
case class Pop(destReg: VirtualReg) extends Instruction

/* Array put/get instructions */
case class ArrayLoad(array: VirtualReg, index: Operand, dest: VirtualReg) extends PtrInstr(array)
case class ArrayStore(array: VirtualReg, index: Operand, src: Operand) extends PtrInstr(array)

/* Pair put/get instructions */
case class PairLoad(pair: VirtualReg, index: PairPos, dest: VirtualReg) extends PtrInstr(pair)
case class PairStore(pair: VirtualReg, index: PairPos, src: Operand) extends PtrInstr(pair)

/* Heap allocation instruction for arrays/pairs */
case class HeapAlloc(dest: VirtualReg, size: Operand) extends PtrInstr(dest)

/* Load the address of a label into a register (no memory access) */
case class Lea(dest: VirtualReg, label: Label) extends PtrInstr(dest)

/* Sign extend a register to be a bigger size */
case class SignExtend(dest: VirtualReg, src: VirtualReg) extends Instruction

/* ---------------------------------------------------------------------------------------------- */

/* Top-level of the IR. Data directives and function definitions come first before the list of
   instructions because directives come first in the assembly file and functions come first in the
   AST (and wacc structure) + they have separate register allocations. Also functions have their
   own stack management so we need to know when it's a func definition and when it's just a normal
   label. */
case class DataDirective(label: Label, storedValue: String)
case class FunctionDef(name: String, args: List[VirtualReg], body: List[Instruction])
case class RegProgram(funcs: List[FunctionDef], data: List[DataDirective], body: List[Instruction])