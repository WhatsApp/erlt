package com.whatsapp.sterlang.test

import com.whatsapp.sterlang.UnionFind

class UnionFindSpec extends org.scalatest.flatspec.AnyFlatSpec {

  "UnionFind" should "should create unique vars" in {
    val uf = new UnionFind[Int]
    val var1 = uf.make(1)
    val var2 = uf.make(1)

    assert(var1 !== var2)
  }

  it should "generate monotonically increasing ids" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("foo")
    val var2 = uf.make("bar")

    assert(uf.id(var1) === 1)
    assert(uf.id(var2) === 2)
  }

  it should "consider the same var equal to itself" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("foo")
    val var2 = uf.make("bar")

    assert(uf.eq(var1, var1) === true)
    assert(uf.eq(var2, var2) === true)
  }

  it should "consider vars different until they are linked" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("foo")
    val var2 = uf.make("bar")

    assert(uf.eq(var1, var2) === false)
  }

  it should "not try to change anything when linking a var to itself" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("foo")

    assert(uf.link(var1, var1) === false)
  }

  it should "do something when linking different vars" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("foo")
    val var2 = uf.make("bar")

    assert(uf.link(var1, var2) === true)
  }

  it should "report linked vars as equal" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("foo")
    val var2 = uf.make("bar")

    uf.link(var1, var2)

    assert(uf.eq(var1, var2) === true)
    assert(uf.eq(var2, var1) === true)
  }

  it should "should link transitively" in {
    val uf = new UnionFind[String]

    val var1 = uf.make("1")
    val var2 = uf.make("2")
    val var3 = uf.make("1")
    val var4 = uf.make("2")

    uf.link(var1, var2)
    uf.link(var3, var4)

    assert(uf.eq(var1, var2) === true)
    assert(uf.eq(var3, var4) === true)
    assert(uf.eq(var1, var4) === false)

    uf.link(var2, var3)

    assert(uf.eq(var1, var4) === true)
  }
}
