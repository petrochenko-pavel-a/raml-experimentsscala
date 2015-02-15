package org.raml.model.diet;

case class Project(override val name: String,folders:Package*) extends BasicElement[Package](name,folders) {
  def parent(): Null = null;
}
case class Package(override val name: String,apis:Api*) extends ParentedElement[Api](name,apis){
   override def parent():Project = super.parent().asInstanceOf[Project];
}
trait ApiMember[C<:IModelElement[_]] extends IModelElement[C];
trait ResourceTypeMember[C<:IModelElement[_]] extends IModelElement[C];
trait ResourceMember[C<:IModelElement[_]] extends IModelElement[C];
trait MethodMember[C<:IModelElement[_]] extends IModelElement[C];
trait MimeMember extends IModelElement[Nothing];

trait FormChildren extends IModelElement[Nothing];
trait BodyMember[C<:IModelElement[_]] extends IModelElement[C];
trait ResponseMember[C<:IModelElement[_]] extends IModelElement[C];


abstract class NoChildren(override val name:String) extends ParentedElement[Nothing](name);
abstract class Parameter(override val name:String) extends NoChildren(name);
abstract class NormalParameter(override val name:String) extends Parameter(name) 


case class QueryParameter(override val name:String) extends NormalParameter(name) with ResourceTypeMember[Nothing] with MethodMember[Nothing] with ResourceMember[Nothing];
case class UriParameter(override val name:String) extends NormalParameter(name) with ResourceTypeMember[Nothing] with ResourceMember[Nothing];
case class HeaderParameter(override val name:String) extends NormalParameter(name) with MimeMember with FormChildren with MethodMember[Nothing];
case class FormParameter(override val name:String) extends Parameter(name) with FormChildren ;

abstract class MimeTypeDescription(override val name:String, val example:String="",members:Seq[MimeMember]=List()) extends ParentedElement[MimeMember](name,members)
 with BodyMember[MimeMember] with ResponseMember[MimeMember];

case class Form(members:FormChildren*) extends ParentedElement[FormChildren]("multipart/formdata",members)
with BodyMember[FormChildren];

case class Json(override val example:String="",var schema:String="",members:Seq[HeaderParameter]=List()) extends MimeTypeDescription("application/json",example) ;
case class XML(override val example:String="",var schema:String="",members:Seq[HeaderParameter]=List()) extends MimeTypeDescription("application/xml",example);
case class Generic(override val name:String, override val example:String="",members:Seq[HeaderParameter]=List()) extends MimeTypeDescription(name,example,members);


case class Api(override val name: String,members:ApiMember[_]*) extends ParentedElement[ApiMember[_]](name,members){
  override def parent():Package = super.parent().asInstanceOf[Package];
}

abstract class HasTraits[C<:IModelElement[_]](override val name: String,members:Seq[C]) extends ParentedElement[C](name,members){
  var is: Seq[String]=List(); 
}
abstract class HasType[C<:IModelElement[_]](override val name: String,members:Seq[C]) extends HasTraits[C](name,members){
  var resourceType:String=null;
}

case class Resource(override val name: String,members:ResourceMember[_]*) extends HasType[ResourceMember[_]](name,members)
 with ApiMember[ResourceMember[_]] with ResourceMember[ResourceMember[_]] 

case class Method(override val name: String,members:MethodMember[_]*) extends HasTraits[MethodMember[_]](name,members)
with ResourceTypeMember[MethodMember[_]] with ResourceMember[MethodMember[_]] 

case class Body(members:BodyMember[_]*) extends ParentedElement[BodyMember[_]]("",members)
with MethodMember[BodyMember[_]]

case class Response(override val name: String,members:ResponseMember[_]*) extends ParentedElement[ResponseMember[_]](name,members)
with MethodMember[ResponseMember[_]]

case class SecuritySchemaDeclaration(override val name: String,var value:String) extends NoChildren(name) with ApiMember[Nothing]
case class SchemaDeclaration(override val name: String,var value:String) extends NoChildren(name) with ApiMember[Nothing]
case class TraitDeclaration(override val name: String,value:MethodMember[_]*) extends HasTraits[MethodMember[_]](name,value) with ApiMember[MethodMember[_]]
case class ResourceTypeDeclaration(override val name: String,value:ResourceTypeMember[_]*) extends HasType[ResourceTypeMember[_]](name,value) with ApiMember[ResourceTypeMember[_]]
