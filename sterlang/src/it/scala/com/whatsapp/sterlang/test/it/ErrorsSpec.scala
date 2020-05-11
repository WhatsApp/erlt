package com.whatsapp.sterlang.test.it

class ErrorsSpec extends DirSpec {
  testDir("examples/neg")
  testDir("examples/err")

  override def testFile(f: String): Unit = {
    if (f.endsWith("core.erl")) {
      super.testFile(f)
    } else {
      assert(SterlangTestUtil.processIllTyped(f))
    }
  }

  override def testFileVerbose(f: String): Unit = {
    // doing nothing
  }
}
