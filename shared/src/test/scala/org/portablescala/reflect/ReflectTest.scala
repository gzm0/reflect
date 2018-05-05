package org.portablescala.reflect

import utest._

object ReflectTest extends TestSuite{
  import ReflectTest.{Accessors, VC}

  private final val Prefix = "org.portablescala.reflect.ReflectTest$"

  private final val NameClassEnableDirect =
    Prefix + "ClassEnableDirect"
  private final val NameClassEnableDirectNoZeroArgCtor =
    Prefix + "ClassEnableDirectNoZeroArgCtor"
  private final val NameObjectEnableDirect =
    Prefix + "ObjectEnableDirect$"
  private final val NameTraitEnableDirect =
    Prefix + "TraitEnableDirect"
  private final val NameAbstractClassEnableDirect =
    Prefix + "AbstractClassEnableDirect"
  private final val NameClassNoPublicConstructorEnableDirect =
    Prefix + "ClassNoPublicConstructorEnableDirect"

  private final val NameInnerClass = {
    Prefix + "ClassWithInnerClassWithEnableReflectiveInstantiation$" +
    "InnerClassWithEnableReflectiveInstantiation"
  }

  private final val NameClassEnableIndirect =
    Prefix + "ClassEnableIndirect"
  private final val NameClassEnableIndirectNoZeroArgCtor =
    Prefix + "ClassEnableIndirectNoZeroArgCtor"
  private final val NameObjectEnableIndirect =
    Prefix + "ObjectEnableIndirect$"
  private final val NameTraitEnableIndirect =
    Prefix + "TraitEnableIndirect"
  private final val NameAbstractClassEnableIndirect =
    Prefix + "AbstractClassEnableIndirect"
  private final val NameClassNoPublicConstructorEnableIndirect =
    Prefix + "ClassNoPublicConstructorEnableIndirect"

  private final val NameClassDisable =
    Prefix + "ClassDisable"
  private final val NameObjectDisable =
    Prefix + "ObjectDisable$"
  private final val NameTraitDisable =
    Prefix + "TraitDisable"

  private final val NameInnerObject = {
    Prefix + "ClassWithInnerObjectWithEnableReflectiveInstantiation$" +
    "InnerObjectWithEnableReflectiveInstantiation"
  }

  val tests = Tests{
    'testClassRuntimeClass - {
      def test(name: String) = {
        val optClassData = Reflect.lookupInstantiatableClass(name)
        assert(optClassData.isDefined)
        val classData = optClassData.get

        val runtimeClass = optClassData.get.runtimeClass
        assert(name == runtimeClass.getName)
      }

      'ClassEnableDirect - test(NameClassEnableDirect)
      'ClassEnableDirectNoZeroArgCtor - test(NameClassEnableDirectNoZeroArgCtor)
      'ClassEnableIndirect - test(NameClassEnableIndirect)
      'ClassEnableIndirectNoZeroArgCtor - test(NameClassEnableIndirectNoZeroArgCtor)
    }

    'testObjectRuntimeClass - {
      def test(name: String) = {
        val optClassData = Reflect.lookupLoadableModuleClass(name)
        assert(optClassData.isDefined)
        val classData = optClassData.get

        val runtimeClass = optClassData.get.runtimeClass
        assert(name == runtimeClass.getName)
      }

      'ObjectEnableDirect - test(NameObjectEnableDirect)
      'ObjectEnableIndirect - test(NameObjectEnableIndirect)
    }

    'testClassCannotBeFound - {
      def test(name: String) = assert(Reflect.lookupInstantiatableClass(name).isEmpty)

      'ObjectEnableDirect - test(NameObjectEnableDirect)
      'TraitEnableDirect - test(NameTraitEnableDirect)
      'AbstractClassEnableDirect - test(NameAbstractClassEnableDirect)
      'ClassNoPublicConstructorEnableDirect - test(NameClassNoPublicConstructorEnableDirect)
      'ObjectEnableIndirect - test(NameObjectEnableIndirect)
      'TraitEnableIndirect - test(NameTraitEnableIndirect)
      'AbstractClassEnableIndirect - test(NameAbstractClassEnableIndirect)
      'ClassNoPublicConstructorEnableIndirect - test(NameClassNoPublicConstructorEnableIndirect)
      'ClassDisable - test(NameClassDisable)
      'ObjectDisable - test(NameObjectDisable)
      'TraitDisable - test(NameTraitDisable)                           
    }
  
    'testObjectCannotBeFound - {
      def test(name: String) = assert(Reflect.lookupLoadableModuleClass(name).isEmpty)
  
      'ClassEnableDirect - test(NameClassEnableDirect)
      'ClassEnableDirectNoZeroArgCtor - test(NameClassEnableDirectNoZeroArgCtor)
      'TraitEnableDirect - test(NameTraitEnableDirect)
      'AbstractClassEnableDirect - test(NameAbstractClassEnableDirect)
      'ClassNoPublicConstructorEnableDirect - test(NameClassNoPublicConstructorEnableDirect)
      'ClassEnableIndirect - test(NameClassEnableIndirect)
      'TraitEnableIndirect - test(NameTraitEnableIndirect)
      'AbstractClassEnableIndirect - test(NameAbstractClassEnableIndirect)
      'ClassNoPublicConstructorEnableIndirect - test(NameClassNoPublicConstructorEnableIndirect)
      'ClassDisable - test(NameClassDisable)
      'ObjectDisable - test(NameObjectDisable)
      'TraitDisable - test(NameTraitDisable)
    }
  
    'testClassNoArgCtor - {
      for (name <- Seq(NameClassEnableDirect, NameClassEnableIndirect)) {
        val optClassData = Reflect.lookupInstantiatableClass(name)
        assert(optClassData.isDefined)
        val classData = optClassData.get
  
        val instance = classData.newInstance().asInstanceOf[Accessors]
        assert(-1 == instance.x)
        assert(name.stripPrefix(Prefix) == instance.y)
      }
    }
  
    'testClassNoArgCtorErrorCase - {
      for (name <- Seq(NameClassEnableDirectNoZeroArgCtor,
          NameClassEnableIndirectNoZeroArgCtor)) {
        val optClassData = Reflect.lookupInstantiatableClass(name)
        assert(optClassData.isDefined)
        val classData = optClassData.get
  
        intercept[InstantiationException](classData.newInstance())
      }
    }
  
    'testClassCtorWithArgs - {
      for (name <- Seq(NameClassEnableDirect, NameClassEnableDirectNoZeroArgCtor,
          NameClassEnableIndirect, NameClassEnableIndirectNoZeroArgCtor)) {
        val optClassData = Reflect.lookupInstantiatableClass(name)
        assert(optClassData.isDefined)
        val classData = optClassData.get
  
        val optCtorIntString =
          classData.getConstructor(classOf[Int], classOf[String])
        assert(optCtorIntString.isDefined)
        val instanceIntString =
          optCtorIntString.get.newInstance(543, "foobar").asInstanceOf[Accessors]
        assert(543 == instanceIntString.x)
        assert("foobar" == instanceIntString.y)
  
        val optCtorInt = classData.getConstructor(classOf[Int])
        assert(optCtorInt.isDefined)
        val instanceInt =
          optCtorInt.get.newInstance(123).asInstanceOf[Accessors]
        assert(123 == instanceInt.x)
        assert(name.stripPrefix(Prefix) == instanceInt.y)
  
        // Value class is seen as its underlying
        val optCtorShort = classData.getConstructor(classOf[Short])
        assert(optCtorShort.isDefined)
        val instanceShort =
          optCtorShort.get.newInstance(21).asInstanceOf[Accessors]
        assert(42 == instanceShort.x)
        assert(name.stripPrefix(Prefix) == instanceShort.y)
  
        // Non-existent
        assert(!classData.getConstructor(classOf[Boolean]).isDefined)
        assert(!classData.getConstructor(classOf[VC]).isDefined)
  
        // Non-public
        assert(!classData.getConstructor(classOf[String]).isDefined)
        assert(!classData.getConstructor(classOf[Double]).isDefined)
      }
    }
  
    'testInnerClass - {
      val outer = new ClassWithInnerClassWithEnableReflectiveInstantiation(15)
  
      val optClassData = Reflect.lookupInstantiatableClass(NameInnerClass)
      assert(optClassData.isDefined)
      val classData = optClassData.get
  
      val optCtorOuterString =
        classData.getConstructor(outer.getClass, classOf[String])
      assert(optCtorOuterString.isDefined)
      val instanceOuterString =
        optCtorOuterString.get.newInstance(outer, "babar").asInstanceOf[Accessors]
      assert(15 == instanceOuterString.x)
      assert("babar" == instanceOuterString.y)
    }
  
    'testLocalClass - {
      @EnableReflectiveInstantiation
      class LocalClassWithEnableReflectiveInstantiation
  
      val fqcn = classOf[LocalClassWithEnableReflectiveInstantiation].getName
      assert(!Reflect.lookupInstantiatableClass(fqcn).isDefined)
    }
  
    'testObjectLoad - {
      for (name <- Seq(NameObjectEnableDirect, NameObjectEnableIndirect)) {
        val optClassData = Reflect.lookupLoadableModuleClass(name)
        assert(optClassData.isDefined)
        val classData = optClassData.get
  
        val instance = classData.loadModule().asInstanceOf[Accessors]
        assert(101 == instance.x)
        assert(name.stripPrefix(Prefix) == instance.y)
      }
    }
  
    'testInnerObjectWithEnableReflectiveInstantiation_issue_3228 - {
      assert(!Reflect.lookupLoadableModuleClass(NameInnerObject).isDefined)
      assert(!Reflect.lookupInstantiatableClass(NameInnerObject).isDefined)
    }
  
    'testLocalClassWithReflectiveInstantiationInLambda_issue_3227 - {
      // Test that the presence of the following code does not prevent linking
      { () =>
        @EnableReflectiveInstantiation
        class Foo
      }
    }

  }

  trait Accessors {
    val x: Int
    val y: String
  }

  final class VC(val self: Short) extends AnyVal

  // Entities with directly enabled reflection

  @EnableReflectiveInstantiation
  class ClassEnableDirect(val x: Int, val y: String) extends Accessors {
    def this(x: Int) = this(x, "ClassEnableDirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  @EnableReflectiveInstantiation
  class ClassEnableDirectNoZeroArgCtor(val x: Int, val y: String)
      extends Accessors {
    def this(x: Int) = this(x, "ClassEnableDirectNoZeroArgCtor")
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  @EnableReflectiveInstantiation
  object ObjectEnableDirect extends Accessors {
    val x = 101
    val y = "ObjectEnableDirect$"
  }

  @EnableReflectiveInstantiation
  trait TraitEnableDirect extends Accessors

  @EnableReflectiveInstantiation
  abstract class AbstractClassEnableDirect(val x: Int, val y: String)
      extends Accessors {

    def this(x: Int) = this(x, "AbstractClassEnableDirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  @EnableReflectiveInstantiation
  class ClassNoPublicConstructorEnableDirect private (val x: Int, val y: String)
      extends Accessors {

    protected def this(y: String) = this(-5, y)
  }

  class ClassWithInnerClassWithEnableReflectiveInstantiation(_x: Int) {
    @EnableReflectiveInstantiation
    class InnerClassWithEnableReflectiveInstantiation(_y: String)
        extends Accessors {
      val x = _x
      val y = _y
    }
  }

  // Entities with reflection enabled by inheritance

  @EnableReflectiveInstantiation
  trait EnablingTrait

  class ClassEnableIndirect(val x: Int, val y: String)
      extends EnablingTrait with Accessors {

    def this(x: Int) = this(x, "ClassEnableIndirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  class ClassEnableIndirectNoZeroArgCtor(val x: Int, val y: String)
      extends EnablingTrait with Accessors {
    def this(x: Int) = this(x, "ClassEnableIndirectNoZeroArgCtor")
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  object ObjectEnableIndirect extends EnablingTrait with Accessors {
    val x = 101
    val y = "ObjectEnableIndirect$"
  }

  trait TraitEnableIndirect extends EnablingTrait with Accessors

  abstract class AbstractClassEnableIndirect(val x: Int, val y: String)
      extends EnablingTrait with Accessors {

    def this(x: Int) = this(x, "AbstractClassEnableIndirect")
    def this() = this(-1)
    def this(vc: VC) = this(vc.self.toInt * 2)

    protected def this(y: String) = this(-5, y)
    private def this(d: Double) = this(d.toInt)
  }

  class ClassNoPublicConstructorEnableIndirect private (
      val x: Int, val y: String)
      extends EnablingTrait with Accessors {

    protected def this(y: String) = this(-5, y)
  }

  // Entities with reflection disabled

  class ClassDisable(val x: Int, val y: String) extends Accessors

  object ObjectDisable extends Accessors {
    val x = 101
    val y = "ObjectDisable$"
  }

  trait TraitDisable extends Accessors

  // Regression cases

  class ClassWithInnerObjectWithEnableReflectiveInstantiation {
    @EnableReflectiveInstantiation
    object InnerObjectWithEnableReflectiveInstantiation
  }
}
