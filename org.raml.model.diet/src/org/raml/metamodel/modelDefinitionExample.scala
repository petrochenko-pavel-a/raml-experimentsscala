package org.raml.metamodel;
object Protocol extends EnumType(Str) {
  values("HTTP", "HTTPS")
}


object ParameterType extends EnumType(Str) {
  values("date", "string", "number", "integer", "file", "boolean");
}
object ActionType extends EnumType(Str) {
  values("GET", "POST", "PUT", "DELETE", "PATCH", "HEAD","OPTIONS");
}

object ExampleOrDefaultValType extends Child("Represents example or default", Parameter, Str) {
  override def validation(): Condition = parent.ConstraintDetails.selectActual.tValidator();
}

object Parameter extends MetaType("Abstract parameter in RAML model") {

  val paramerterName = req(key(str));

  val parameterType = prop(ParameterType);
  
  val repeat = bool();

  val required = bool();

  val example = prop(ExampleOrDefaultValType);

  val default = prop(ExampleOrDefaultValType);
  

  object ConstraintDetails extends OneOfTypes[TypeAttrs](StringAttrs, NumberAttrs, OtherAttrs) with Embedded {
    descriminator(
      (parameterType.$ === "string") -> StringAttrs,
      (parameterType.$ === "number") -> NumberAttrs,
      (parameterType.$ === "integer") -> NumberAttrs,
      (parameterType.$ === "file") -> FileAttrs,
      otherwise -> OtherAttrs);
  }
}
object FormParameter extends MetaType("form parameters", Parameter) {

}
object QueryParameter extends MetaType("query parameters", Parameter) {

}
object UriParameter extends MetaType("uri parameters", Parameter) {

}
object HeaderParameter extends MetaType("query parameters", Parameter) {

}

object RAMLAction extends MetaType("Raml action model", Parameter){
  val queryParameters=multivalue(prop(QueryParameter));
  val actionType=prop(ActionType);
}

trait TypeAttrs extends Type {
  def tValidator(): Condition;
}

object OtherAttrs extends Child("date, or boolean", Parameter) with TypeAttrs {
  def tValidator(): Condition = null;
}
object FileAttrs extends Child("file", Parameter) with TypeAttrs {
  require(parent.actualType() instanceOf FormParameter).description("form type is allowed only in forms");

  def tValidator(): Condition = null;
}

object StringAttrs extends Child("String typed parameter", Parameter) with TypeAttrs {

  val pattern = prop(RegExp);

  val minLength = prop(PositiveInt);

  val maxLength = prop(PositiveInt);

  val enum = prop(Enum);

  mutuallyExclusive(pattern, enum, unionOf(minLength, maxLength));

  def tValidator(): Condition = null;

}

object NumberAttrs extends Child("Number typed parameter", Parameter) with TypeAttrs {
  val min = prop(NumberType).description("This is minimum value");
  val max = prop(NumberType).description("This is maximum value");

  require(min.$ < max.$).description("min should be less then max");

  def tValidator() = ($ < max.$ && $ > min.$).description("value should be less then max and more then min")

}