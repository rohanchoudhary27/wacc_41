package wacc.backend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.backend.Size.*
import wacc.backend.PhysicalReg
import wacc.backend.Arm.*
import wacc.backend.PeepholeOptimisation

class PeepholeOptimisationTest extends AnyFlatSpec with Matchers {

    private val r1 = PhysicalReg.argRegisters(1)
    private val r2 = PhysicalReg.argRegisters(2)

    private val baseReg = PhysicalReg.argRegisters(7)
    private val memAddr = AddrImmOffset(baseReg, Immediate(0))

    private def optimise(instructions: List[Instruction]): List[Instruction] = {
        val prog = Program(data = Nil, body = instructions)
        PeepholeOptimisation.allOptimisations(prog).body
    }

    "The PeepholeOptimiser" should "remove self moves" in {
        val input = List(
            MovReg(X, r1, r1)
        )
        val expected = List()

        optimise(input) shouldBe expected
    }

    it should "remove unconditional branches to the immediately following label" in {
        val input = List(
            BranchUncond(AddrLabel("main")),
            LabelDef(AddrLabel("main"))
        )
        val expected = List(
            LabelDef(AddrLabel("main"))
        )
        
        optimise(input) shouldBe expected
    }

    it should "remove mathematical operations with zero on the same register" in {
        val input = List(
            AddImm(X, r1, r1, Immediate(0)),
            SubImm(X, r2, r2, Immediate(0))
        )
        val expected = List()
        
        optimise(input) shouldBe expected
    }

    it should "replace mathematical operations with zero on different registers with a MovReg" in {
        val input = List(
            AddImm(X, r1, r2, Immediate(0))
        )
        val expected = List(
            MovReg(X, r1, r2)
        )
        
        optimise(input) shouldBe expected
    }

    it should "convert an AND with zero into a direct move of zero" in {
        val input = List(
            AndImm(X, r1, r2, Immediate(0))
        )
        val expected = List(
            Movz(X, r1, Immediate(0))
        )
        
        optimise(input) shouldBe expected
    }

    it should "eliminate redundant loads to the same register (Store-to-Load Forwarding)" in {
        val input = List(
            Stur(X, r1, memAddr),
            Ldur(X, r1, memAddr)
        )
        val expected = List(
            Stur(X, r1, memAddr)
        )
        
        optimise(input) shouldBe expected
    }

    it should "replace redundant loads to a different register with a MovReg" in {
        val input = List(
            Stur(X, r1, memAddr),
            Ldur(X, r2, memAddr)
        )
        val expected = List(
            Stur(X, r1, memAddr),
            MovReg(X, r2, r1)
        )
        
        optimise(input) shouldBe expected
    }

    it should "coalesce immediate moves through a scratch register" in {
        val input = List(
            Movz(X, r1, Immediate(42)),
            MovReg(X, r2, r1)
        )
        val expected = List(
            Movz(X, r2, Immediate(42))
        )
        
        optimise(input) shouldBe expected
    }

    it should "remove alternate (ping-pong) moves" in {
        val input = List(
            MovReg(X, r1, r2),
            MovReg(X, r2, r1)
        )
        val expected = List(
            MovReg(X, r1, r2)
        )
        
        optimise(input) shouldBe expected
    }

    it should "cascade multiple optimisations successfully through the convergence loop" in {
        val input = List(
            Stur(X, r1, memAddr),            
            Ldur(X, r2, memAddr),
            MovReg(X, r1, r2)
        )
        
        val expected = List(
            Stur(X, r1, memAddr),
            MovReg(X, r2, r1)
        )
        
        optimise(input) shouldBe expected
    }
}