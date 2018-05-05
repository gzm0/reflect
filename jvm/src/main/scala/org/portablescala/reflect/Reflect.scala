package org.portablescala.reflect

import scala.collection.mutable

import java.lang.reflect._

object Reflect {
  def lookupLoadableModuleClass(fqcn: String): Option[LoadableModuleClass] =
    load(fqcn).filter(isModuleClass).map(new LoadableModuleClass(_))

  def lookupInstantiatableClass(fqcn: String): Option[InstantiatableClass] =
    load(fqcn).filter(isInstantiatableClass).map(new InstantiatableClass(_))

  private def isModuleClass(clazz: Class[_]): Boolean = {
    try {
      val fld = clazz.getField("MODULE$")
      clazz.getName.endsWith("$") && (fld.getModifiers & Modifier.STATIC) != 0
    } catch {
      case _: NoSuchFieldException => false
    }
  }

  private def isInstantiatableClass(clazz: Class[_]): Boolean = {
    (clazz.getModifiers() & Modifier.ABSTRACT) == 0 &&
    clazz.getConstructors().size > 0 &&
    !isModuleClass(clazz)
  }

  private def load(fqcn: String): Option[Class[_]] = {
    try {
      val clazz = Class.forName(fqcn)
      if (inheritsAnnotation(clazz)) Some(clazz)
      else None
    } catch {
      case _: ClassNotFoundException => None
    }
  }

  private def inheritsAnnotation(clazz: Class[_]): Boolean = {
    val cache = mutable.Map.empty[Class[_], Boolean]

    def c(clazz: Class[_]): Boolean =
      cache.getOrElseUpdate(clazz, l(clazz))

    def l(clazz: Class[_]): Boolean = {
      if (clazz.getAnnotation(classOf[EnableReflectiveInstantiation]) != null) {
        true
      } else {
        (Iterator(clazz.getSuperclass) ++ Iterator(clazz.getInterfaces: _*))
          .filter(_ != null)
          .exists(c)
      }
    }

    c(clazz)
  }
}
