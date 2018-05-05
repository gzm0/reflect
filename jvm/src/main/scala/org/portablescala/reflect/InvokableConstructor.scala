package org.portablescala.reflect

import java.lang.reflect.Constructor

final class InvokableConstructor private[reflect] (ctor: Constructor[_]) {
  def parameterTypes: List[Class[_]] = ctor.getParameterTypes().toList
  def newInstance(args: Any*): Any = newInstance(args: _*)
}
