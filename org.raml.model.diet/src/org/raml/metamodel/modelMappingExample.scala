package org.raml.metamodel;

object JavaMethodParameter extends MetaType("Extremly simple parameter", JavaObject) {
  val sname = str;
  val pType = str;
  
}
object JavaAnnotation extends MetaType("Java Annotation", JavaObject) {
  val annotationName = key(str());
  val value = str(); //EXTREME SIMPLIFICATION
}
object JavaObject extends MetaType("Java Model Object") {
  val annotations = multivalue (prop(JavaAnnotation))
}

object JavaMethod extends MetaType("Simple resource type", JavaObject) {
  val parameters = multivalue(prop(JavaMethodParameter))

}

//
/**
 * Think more about it!!!//TODO
 */
object Mapping extends ModelMapping(JavaMethod, RAMLAction) {
  mappingAssertion(to.actionType.$ <=> from.superType().annotations.range.annotationName.$);
  mappingAssertion(to.queryParameters.$ <=> from.parameters.$.where(
    JavaObject.annotations.range.annotationName.$ === "QueryParameter"));
}

object PMapping extends ModelMapping(JavaMethodParameter, Parameter) {
  mappingAssertion(from.sname.$<=>to.paramerterName.$);
  mappingAssertion((from.pType.$ === "java.util.Date") <=> (to.parameterType.$ === "date"));
  mappingAssertion((from.pType.$ === "boolean") <=> ((to.parameterType.$ === "boolean") && (to.required.$ === true)));
  mappingAssertion((from.pType.$ === "int") <=> ((to.parameterType.$ === "integer") && to.required.$ === true));
  mappingAssertion((from.pType.$ === "double") <=> ((to.parameterType.$ === "double") && to.required.$ === true));
  
}

/*
 * Why not use scala for defining transformations and constraints, 
 * Scala AST is to rich?, probably yes. This stuff may be represented in a simple form easily;
 * Model object solution is simpler to implement, simpler to get something workable
 * we may mix it with scala AST later
 */