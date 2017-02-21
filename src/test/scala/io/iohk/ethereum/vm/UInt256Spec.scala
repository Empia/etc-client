package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.vm.Int256Like._

class UInt256Spec extends FunSuite with PropertyChecks {

  val Modulus: BigInt = BigInt(2).pow(Size * 8)

  val Zero: UInt256 = UInt256(BigInt(0))

  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), Modulus - 1, 1 - Modulus, 2 - Modulus)

  val pairs: Array[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map {case Array(n1, n2) => n1 -> n2}
    .toArray

  val specialCases = Table(("n1", "n2"), pairs: _*)

  //  TODO conversions to and back from unsigned

  test("&") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) & UInt256(n2)) == UInt256(n1 & n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) & UInt256(n2)) == UInt256(n1 & n2))
    }
  }

  test("|") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) | UInt256(n2)) == UInt256(n1 | n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) | UInt256(n2)) == UInt256(n1 | n2))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) ^ UInt256(n2)) == UInt256(n1 ^ n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) ^ UInt256(n2)) == UInt256(n1 ^ n2))
    }
  }

  test("~") {
    forAll(bigIntGen) { n: BigInt =>
      assert(~UInt256(n) == UInt256(~n))
    }
    forAll(Table("n", specialNumbers: _*)) { n: BigInt =>
      assert(~UInt256(n) == UInt256(~n))
    }
  }

  test("negation") {
    forAll(bigIntGen) {(n: BigInt) =>
      assert(-UInt256(n) == UInt256(-n))
    }
    forAll(Table(("n"), specialNumbers: _*)) {(n: BigInt) =>
      assert(-UInt256(n) == UInt256(-n))
    }
    assert(-UInt256(BigInt(1)) == UInt256(BigInt(-1)))
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) + UInt256(n2) == UInt256(n1 + n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) + UInt256(n2) == UInt256(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) - UInt256(n2) == UInt256(n1 - n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) - UInt256(n2) == UInt256(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
  }

  test("div") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) div UInt256(n2)) == UInt256(n1 / n2))
      }
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      whenever(n1 > 0 && n2 > 0) {
        assert((UInt256(n1) div UInt256(n2)) == UInt256(n1 / n2))
      }
    }
    assert((UInt256(1) div Zero) == Zero)
  }

  test("%") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) % UInt256(n2)) == UInt256(n1 % n2))
      }
    }
    assert((UInt256(-1) % UInt256(Modulus - 1)) == Zero)
    assert((UInt256(1) % Zero) == Zero)
  }

  test("addmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(UInt256(n1).addmod(UInt256(n2), UInt256(n3)) == UInt256((n1 + n2) mod n3))
      }
    }
    assert(UInt256(42).addmod(UInt256(42), Zero) == Zero)
  }

  test("mulmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(UInt256(n1).mulmod(UInt256(n2), UInt256(n3)) == UInt256((n1 * n2) mod n3))
      }
    }
    assert(UInt256(42).mulmod(UInt256(42), Zero) == Zero)
  }

  test("**") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert(UInt256(n1) ** UInt256(n2) == UInt256(n1.modPow(n2, Modulus)))
      }
    }
  }

  test("getByte") {
    val dw1 = UInt256(ByteString((100 to 131).map(_.toByte).toArray))

    val testData = Table[UInt256, UInt256, UInt256](
      ("word", "idx", "result"),
      (UInt256(42), UInt256(-1), Zero),
      (UInt256(42), UInt256(Size), Zero),
      (UInt256(42), UInt256(Size + 1), Zero),
      (UInt256(1), UInt256(287), Zero),
      (dw1, UInt256(0), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 100.toByte)),
      (dw1, UInt256(1), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 101.toByte)),
      (dw1, UInt256(30), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -126.toByte)),
      (dw1, UInt256(31), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -125.toByte)),
      (UInt256(ByteString(Array.fill[Byte](32)(-50))), UInt256(8), UInt256(ByteString(-50))))

    forAll(testData) { (a, b, result) =>
      assert(a.getByte(b) == result)
    }
  }

  test("intValue") {
    assert(specialNumbers.map(UInt256(_).intValue).toSeq == Seq(Int.MaxValue, 0, 1, Int.MaxValue, 1, 2))
  }

  test("comparison") {
    type CFUINT = (UInt256, UInt256) => Boolean
    type CFBI = (BigInt, BigInt) => Boolean
    case class Cmp(uint: CFUINT, bi: CFBI)

    val cmpFuncUInt256 = Seq[CFUINT](_ > _, _ >= _, _ < _, _ <= _)
    val cmpFuncBigInt = Seq[CFBI](_ > _, _ >= _, _ < _, _ <= _)
    val comparators = cmpFuncUInt256.zip(cmpFuncBigInt).map(Cmp.tupled)

    val uin256Gen = bigIntGen.map(UInt256(_))

    forAll(Table("comparators", comparators: _*)) { cmp =>
      forAll(bigIntGen, bigIntGen) { (a, b) =>
        val (x, y) = (UInt256(a), UInt256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
      forAll(specialCases) { (a, b) =>
        val (x, y) = (UInt256(a), UInt256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      UInt256(ByteString(Array.fill(Size + 1)(1.toByte)))
    }
  }

  test("UInt256 converted to a byte array should always have length 32 bytes") {
    forAll(bigIntGen) { n =>
      assert(UInt256(n).bytes.size == 32)
    }
    // regression
    assert(UInt256(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935")).bytes.size == 32)
  }

  test("2-way bytes conversion") {
    forAll(getUInt256Gen()) { x =>
      val y = UInt256(x.bytes)
      assert(x === y)
    }

    forAll(getByteStringGen(0, 32)) { xs =>
      val ys = UInt256(xs).bytes
      assert(xs.dropWhile(_ == 0) === ys.dropWhile(_ == 0))
    }
  }

  test("byteSize") {
    val table = Table[BigInt, Int](("x", "expected"), (0, 0), (1, 1), (255, 1), (256, 2), (65535, 2), (65536, 3),
      (BigInt(2).pow(256) - 1, 32), (BigInt(2).pow(256), 0))
    forAll(table) { (x, expected) =>
      assert(UInt256(x).byteSize === expected)
    }

    forAll(getUInt256Gen(min = 1)) { x =>
      import math._
      val byteSize = 1 + floor(log(x.n.doubleValue) / log(256)).toInt
      assert(x.byteSize === byteSize)
    }
  }
}
