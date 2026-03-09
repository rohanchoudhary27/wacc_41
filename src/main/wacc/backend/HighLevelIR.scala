package wacc.backend

import wacc.InternalCompilerError

object TAC {

    /* Operands */
    sealed trait Operand
    case class Immediate(value: Int) extends Operand
    case class VirtualReg(regNo: Int, size: Size) extends Operand
    case class Label(name: String) extends Operand

    /* Addresses */
    sealed trait Address
    case class AddrRegOffset(baseAddr: VirtualReg, offset: VirtualReg) extends  Address {
        require(baseAddr.size == Size.X,
                InternalCompilerError.sizeMismatch("AddrRegOffset", Size.X, baseAddr.size))
    }

    case class AddrImmOffset(baseAddr: VirtualReg, offset: Immediate) extends Address {
        require(baseAddr.size == Size.X,
                InternalCompilerError.sizeMismatch("AddrRegOffset", Size.X, baseAddr.size))
    }

    case class AddrLabel(name: String) extends Address
    case class AddrStack(offset: Int) extends Address

    /* ---------------------------------------------------------------------------------------------- */

    /* Instruction trait and abstract classes to assert correct register sizes */

    sealed trait Instruction

    /* Enforcing all the same register sizes */
    abstract class BinOp(dest: VirtualReg, src1: VirtualReg, src2: Operand) extends Instruction {
        require(dest.size == src1.size,
                InternalCompilerError.sizeMismatch("BinOp", dest.size, src1.size))
        
        src2 match {
            case v: VirtualReg =>
                require(dest.size == v.size,
                        InternalCompilerError.sizeMismatch("BinOp Src2", dest.size, v.size))
            case _ =>
        }
    }

    /* Enforcing both the same register sizes */
    abstract class UnOp(dest: VirtualReg, src: Operand) extends Instruction {
        src match {
            case v: VirtualReg =>
                require(dest.size == v.size,
                        InternalCompilerError.sizeMismatch("UnOp", dest.size, v.size))
            case _ => 
        }
    }

    /* Enforcing both the same register sizes */
    abstract class CmpOp(src1: VirtualReg, src2: Operand) extends Instruction {
        src2 match {
            case v: VirtualReg => require(src1.size == v.size, 
                InternalCompilerError.sizeMismatch("Cmp", src1.size, v.size))
            case _ =>
        }
    }

    /* Enforcing 64 bit pointer size */
    abstract class PtrInstr(ptr: VirtualReg) extends Instruction {
        require(ptr.size == Size.X, InternalCompilerError.sizeMismatch("Pointer",  Size.X, ptr.size))
    }

    /* Enforcing 32-bit integer index and 64 bit pointer size */
    abstract class ArrayInstr(array: VirtualReg, index: Operand) extends PtrInstr(array) {
        index match {
            case v: VirtualReg if v.size != Size.W => require(v.size == Size.W, 
                InternalCompilerError.sizeMismatch("ArrayInstr", Size.W, v.size))
            case _ => 
        }
    }

    /* ---------------------------------------------------------------------------------------------- */

    /* Instruction case classes */
    /* Binary operations */
    case class Add(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Sub(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Mul(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Div(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Mod(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class DivUnchecked(
        reg: VirtualReg, src1: VirtualReg, src2: Operand
    ) extends BinOp(reg, src1, src2)
    case class ModUnchecked(
        reg: VirtualReg, src1: VirtualReg, src2: Operand
    ) extends BinOp(reg, src1, src2)
    case class Lsl(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Lsr(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Asr(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class Or(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)
    case class And(reg: VirtualReg, src1: VirtualReg, src2: Operand) extends BinOp(reg, src1, src2)

    /* Unary operations */
    case class Neg(reg: VirtualReg, src: VirtualReg) extends UnOp(reg, src)
    case class Not(reg: VirtualReg, src: VirtualReg) extends UnOp(reg, src)
    case class Mov(reg: VirtualReg, src: Operand) extends UnOp(reg, src)
    case class Chr(reg: VirtualReg, src: VirtualReg, widget: Option[Widget]) extends UnOp(reg, src)

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
    case class Call(label: Label, args: List[Operand], dest: Option[VirtualReg]) extends Instruction
    case class TailCall(label: Label, args: List[Operand]) extends Instruction
    case object Ret extends Instruction

    /* I/O instructions */
    case class Print(src: Operand, widget: Widget, newline: Boolean) extends Instruction
    case class Read(dest: VirtualReg, widget: Widget) extends Instruction

    /* Array instructions */
    case class ArrayLoad(array: VirtualReg,
                        index: Operand,
                        dest: VirtualReg) extends ArrayInstr(array, index)
    case class ArrayLoadUnchecked(array: VirtualReg,
                        index: Operand,
                        dest: VirtualReg) extends ArrayInstr(array, index)
    case class ArrayStore(array: VirtualReg,
                        index: Operand,
                        src: Operand,
                        elemSize: Size) extends ArrayInstr(array, index)
    case class ArrayStoreUnchecked(array: VirtualReg,
                        index: Operand,
                        src: Operand,
                        elemSize: Size) extends ArrayInstr(array, index)
    case class ArrayLen(array: VirtualReg, dest: VirtualReg) extends PtrInstr(array)

    /* Pair put/get instructions */
    case class PairLoad(pair: VirtualReg, index: PairPos, dest: VirtualReg) extends PtrInstr(pair)
    case class PairStore(pair: VirtualReg, index: PairPos, src: Operand) extends PtrInstr(pair)

    /* Check if a pointer is null, throwing a runtime error if so */
    case class NullCheck(ptr: VirtualReg) extends PtrInstr(ptr)

    /* Heap allocation instruction for arrays/pairs */
    case class HeapAlloc(dest: VirtualReg, size: Operand) extends PtrInstr(dest)
    /* Heap deallocation (freeing) of the pointer, for arrays and pairs */
    case class Free(ptr: VirtualReg, freeType: FreeType) extends PtrInstr(ptr)

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
}