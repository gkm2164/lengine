package lengine

import lengine.PreludeImpl._
import lengine.types.collections._
import lengine.types.{ComplexNumber, LengineLazyValue, LengineString, RatioNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.Arrays

/** Runtime library behavior. Each case started as a bug from docs/known-bugs.md. */
class RuntimeTest extends AnyFlatSpec with Matchers {
  private def seq(xs: Any*): LengineSequence =
    LengineSequence.create(Arrays.asList(xs.map(_.asInstanceOf[AnyRef]): _*))
  private def key(k: String): LengineMapKey = LengineMapKey.create(LengineString.create(k))
  private def ratio(a: Long, b: Long) = RatioNumber.create(a, b)
  private def complex(a: Long, b: Long) = ComplexNumber.create(a, b)

  "ratios" should "be kept in lowest terms with the sign on top" in {
    ratio(2, 4) shouldBe ratio(1, 2)
    ratio(-2, 4).toString shouldBe "-1/2"
    ratio(2, -4).toString shouldBe "-1/2"
    ratio(1, 2).hashCode shouldBe ratio(2, 4).hashCode
  }

  they should "become integers when the denominator reaches 1" in {
    _ADD.invoke(ratio(1, 2), ratio(1, 2)) shouldBe 1L
    _MULT.invoke(ratio(2, 3), 3L) shouldBe 2L
    _ADD.invoke(ratio(1, 3), ratio(1, 6)) shouldBe ratio(1, 2)
  }

  they should "compare by value" in {
    _EQUALS.invoke(ratio(1, 2), ratio(1, 2)) shouldBe true
    _LESS_THAN.invoke(ratio(1, 3), ratio(1, 2)) shouldBe true
    _GREATER_THAN.invoke(ratio(-1, 3), ratio(-1, 2)) shouldBe true
    _LESS_THAN.invoke(ratio(1, 2), 1L) shouldBe true
  }

  "=" should "compare numbers across types" in {
    _EQUALS.invoke(1L, 1.0) shouldBe true
    _EQUALS.invoke(ratio(1, 2), 0.5) shouldBe true
    _EQUALS.invoke(1L, 2L) shouldBe false
    _NOT_EQUALS.invoke(1L, 1.0) shouldBe false
    _EQUALS.invoke(complex(1, 0), 1L) shouldBe true
    _EQUALS.invoke('a', 97L) shouldBe false // characters only equal characters
    _EQUALS.invoke(LengineString.create("a"), LengineString.create("a")) shouldBe true
  }

  "complex numbers" should "promote reals with a zero imaginary part" in {
    _ADD.invoke(1L, complex(1, 1)) shouldBe complex(2, 1)
    _ADD.invoke(1.5, complex(1, 2)) shouldBe ComplexNumber.create(2.5, 2L)
    _ADD.invoke(ratio(1, 2), complex(1, 0)) shouldBe ComplexNumber.create(ratio(3, 2), 0L)
  }

  they should "invert with a^2 + b^2" in {
    _DIV.invoke(complex(1, 0), complex(0, 1)) shouldBe complex(0, -1) // 1/i = -i
    _DIV.invoke(complex(1, 0), complex(1, 1)) shouldBe ComplexNumber.create(ratio(1, 2), ratio(-1, 2))
    complex(3, 4).inverse() shouldBe ComplexNumber.create(ratio(3, 25), ratio(-4, 25))
    ComplexNumber.create(1.0, 1.0).inverse() shouldBe ComplexNumber.create(0.5, -0.5)
  }

  "character arithmetic" should "produce Longs that keep working" in {
    _ADD.invoke('a', 'b') shouldBe 195L
    _ADD.invoke(_ADD.invoke('a', 'b'), 1L) shouldBe 196L
    _ADD.invoke('a', 1L) shouldBe 98L
  }

  "range" should "yield Longs from head too" in {
    _RANGE.invoke(1L, 5L).head() shouldBe 1L
    _ADD.invoke(_RANGE.invoke(1L, 5L).head(), 1L) shouldBe 2L
  }

  it should "treat a backward range as empty" in {
    val r = _RANGE.invoke(5L, 1L)
    r.len() shouldBe 0L
    r.IS_NIL() shouldBe true
    r.iterator().hasNext shouldBe false
    _INCLUSIVE_RANGE.invoke(3L, 3L).len() shouldBe 1L
  }

  "boolean & and |" should "work" in {
    _BYTE_AND.invoke(true, false) shouldBe false
    _BYTE_OR.invoke(true, false) shouldBe true
    _BYTE_AND.invoke(6L, 3L) shouldBe 2L
  }

  "lists" should "not be changed by appending" in {
    val l = LengineList.create(Array[AnyRef](Long.box(1L), Long.box(2L)))
    val l2 = _APPEND_ITEM.invoke(l, 3L)
    l.toString shouldBe "(cons 1 (cons 2 nil))"
    l2.toString shouldBe "(cons 1 (cons 2 (cons 3 nil)))"
  }

  "nil ++ seq" should "append each element once" in {
    LengineList.nil().append(seq(1L, 2L, 3L)).toString shouldBe "(cons 1 (cons 2 (cons 3 nil)))"
  }

  "maps" should "replace the value of an existing key" in {
    val m = LengineMap.builder().put(key("a"), 1L).build()
    val m2 = m.putEntry(LengineMapEntry.create(key("a"), 2L))
    m2.get(key("a")) shouldBe 2L
    m2.len() shouldBe 1L
    m.get(key("a")) shouldBe 1L // the original is untouched
  }

  they should "return nil for a missing key" in {
    LengineMap.builder().put(key("a"), 1L).build().get(key("b")) shouldBe LengineList.nil()
  }

  they should "agree on equals and hashCode however they were built" in {
    val m1 = LengineMap.builder().put(key("a"), 1L).put(key("b"), 2L).build()
    val m2 = LengineMap.builder().put(key("a"), 1L).build().putEntry(LengineMapEntry.create(key("b"), 2L))
    m1 shouldBe m2
    m1.hashCode shouldBe m2.hashCode
  }

  they should "be built from a sequence of entries" in {
    val m = LengineMap.create(seq(LengineMapEntry.create(key("a"), 1L), LengineMapEntry.create(key("b"), 2L)))
    m.len() shouldBe 2L
  }

  they should "be loopable, collecting into a sequence" in {
    val m = LengineMap.builder().put(key("a"), 1L).build()
    val b = m.asInstanceOf[traits.Buildable[LengineSequence, LengineSequenceBuilder]].BUILDER()
    b.ADD(1L)
    b.BUILD() shouldBe seq(1L)
  }

  "map entries" should "have value equality" in {
    LengineMapEntry.create(key("a"), 1L) shouldBe LengineMapEntry.create(key("a"), 1L)
  }

  "sequences" should "be equal and hash alike regardless of shape" in {
    val leaf = seq(1L, 2L)
    val nonLeaf = seq(1L).add(2L)
    leaf shouldBe nonLeaf
    nonLeaf shouldBe leaf
    leaf.hashCode shouldBe nonLeaf.hashCode
  }

  "sets" should "drop the head in tail" in {
    val s = LengineSet.create(seq(1L, 2L, 3L))
    val t = s.tail().asInstanceOf[LengineSet]
    t.len() shouldBe 2L
    t.contains(s.head()) shouldBe false
  }

  they should "hash alike when equal" in {
    val s1 = LengineSet.create(seq(1L, 2L))
    val s2 = LengineSet.create(seq(2L)).add(1L)
    s1 shouldBe s2
    s1.hashCode shouldBe s2.hashCode
    val hs = new java.util.HashSet[AnyRef]()
    hs.add(s1)
    hs.contains(s2) shouldBe true
  }

  "take" should "not force more elements than it takes" in {
    var forced = 0
    def lazyTail(n: Long, limit: Long): LengineStream =
      UnresolvedStream.create(LengineLazyValue.create(() => {
        forced += 1
        if (n > limit) throw new RuntimeException(s"element $n forced")
        LengineStream.cons(n, lazyTail(n + 1, limit))
      }))
    val s = LengineStream.cons(1L, lazyTail(2, 2))
    _TAKE.invoke(2L, s).toString should include("2")
    forced shouldBe 1
  }
}
