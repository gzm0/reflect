package org.portablescala.reflect

final class LoadableModuleClass private[reflect] (clazz: Class[_]) {
  def runtimeClass: Class[_] = clazz
  def loadModule(): Any = clazz.getField("MODULE$").get(null)
}
