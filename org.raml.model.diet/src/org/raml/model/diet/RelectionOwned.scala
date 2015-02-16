package org.raml.model.diet

import java.lang.reflect.Modifier
import java.lang.reflect.Field
import scala.annotation.StaticAnnotation
import scala.annotation.ClassfileAnnotation

class Feature[VType](val name: String, val getF: Function0[VType], val setF: Function1[VType, Unit], val validator: Function1[VType, Unit] = null) {
  def get() = getF();

  def set(v: VType) = setF(v);

  def readOnly(): Boolean = setF != null;
}
object Feature {

  def apply[VType](name: String, getF: Function0[VType], setF: Function1[VType, Unit], validator: Function1[VType, Unit] = null): Feature[VType] =
    new Feature(name, getF, setF, validator);
}

trait Featured {

  type Features = List[Feature[_]]
  
  type StringFeature=Feature[String]

  def features(): Features
}
abstract class Describable extends Featured {

}
