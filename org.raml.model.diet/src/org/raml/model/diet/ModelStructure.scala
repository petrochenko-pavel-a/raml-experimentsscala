package org.raml.model.diet;

case class Project(var name: String,var members:Seq[Package]=List()) extends BasicElement[Package](members) {
  def parent(): Null = null;
}
case class Package(var name: String,var members:Seq[Api]=List()) extends ParentedElement[Api](members){
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


abstract class NoChildren(name:String) extends ParentedElement[Nothing](Nil){var members:Seq[Nothing]=Nil};
abstract class Parameter(name:String) extends NoChildren(name);
abstract class NormalParameter(name:String) extends Parameter(name) 


case class QueryParameter(var name:String) extends NormalParameter(name) with ResourceTypeMember[Nothing] with MethodMember[Nothing] with ResourceMember[Nothing];
case class UriParameter(var name:String) extends NormalParameter(name) with ResourceTypeMember[Nothing] with ResourceMember[Nothing];
case class HeaderParameter(var name:String) extends NormalParameter(name) with MimeMember with FormChildren with MethodMember[Nothing];
case class FormParameter( var name:String) extends Parameter(name) with FormChildren ;

abstract class MimeTypeDescription(  ch:Seq[MimeMember]=List()) extends ParentedElement[MimeMember](ch)
 with BodyMember[MimeMember] with ResponseMember[MimeMember]{var example:String};

case class Form(var members:Seq[FormChildren]=List()) extends ParentedElement[FormChildren](members)with BodyMember[FormChildren]
{var name:String="multipart/formdata";}

case class Json(var example:String="",var schema:String="",var name:String="application/json",var members:Seq[MimeMember]=List()) 
extends MimeTypeDescription(members);
case class XML(var example:String="",var schema:String="",var name:String="application/xml",var members:Seq[MimeMember]=List()) extends MimeTypeDescription();
case class Generic(var name:String,var example:String="",var members:Seq[MimeMember]=List()) extends MimeTypeDescription(members);


case class Api(var name: String,var members:Seq[ApiMember[_]]=List()) extends ParentedElement[ApiMember[_]](members){
  override def parent():Package = super.parent().asInstanceOf[Package];
}

abstract class HasTraits[C<:IModelElement[_]](members:Seq[C]) extends ParentedElement[C](members){
  var is: Seq[String]=List(); 
}
abstract class HasType[C<:IModelElement[_]](members:Seq[C]) extends HasTraits[C](members){
  var resourceType:String=null;
}

case class Resource(var name: String, var members:Seq[ResourceMember[_]]=List()) extends HasType[ResourceMember[_]](members)
 with ApiMember[ResourceMember[_]] with ResourceMember[ResourceMember[_]]{
}

case class Method(var name: String, var members:Seq[MethodMember[_]]=List()) extends HasTraits[MethodMember[_]](members)
with ResourceTypeMember[MethodMember[_]] with ResourceMember[MethodMember[_]] {
  }

case class Body(var members:Seq[BodyMember[_]]) extends ParentedElement[BodyMember[_]](members)with MethodMember[BodyMember[_]]{var name:String=""}

case class Response(var name: String,var members:Seq[ResponseMember[_]]) extends ParentedElement[ResponseMember[_]](members)
with MethodMember[ResponseMember[_]]

case class SecuritySchemaDeclaration(var name: String,var value:String) extends NoChildren(name) with ApiMember[Nothing]
case class SchemaDeclaration(var name: String,var value:String) extends NoChildren(name) with ApiMember[Nothing]
case class TraitDeclaration(var name: String,var members:Seq[MethodMember[_]]=List()) extends HasTraits[MethodMember[_]](members) with ApiMember[MethodMember[_]]
case class ResourceTypeDeclaration( var name: String,var members:Seq[ResourceTypeMember[_]]=List()) extends HasType[ResourceTypeMember[_]](members) with ApiMember[ResourceTypeMember[_]]
