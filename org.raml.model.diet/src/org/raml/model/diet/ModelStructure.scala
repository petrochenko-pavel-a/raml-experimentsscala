package org.raml.model.diet;


/*
 * traits and abstract classes
 */
trait ApiMember[C<:IModelElement[_]] extends IModelElement[C];
trait ResourceTypeMember[C<:IModelElement[_]] extends IModelElement[C];
trait ResourceMember[C<:IModelElement[_]] extends IModelElement[C];
trait MethodMember[C<:IModelElement[_]] extends IModelElement[C];
trait MimeMember extends IModelElement[Nothing];

trait FormChildren extends IModelElement[Nothing];
trait BodyMember[C<:IModelElement[_]] extends IModelElement[C];
trait ResponseMember[C<:IModelElement[_]] extends IModelElement[C];


abstract class NoChildren(name:String) extends ParentedElement[Nothing](Nil){var members:Seq[Nothing]=Nil; var diet:Boolean=false;};
abstract class Parameter(name:String) extends NoChildren(name);
abstract class NormalParameter(name:String) extends Parameter(name) 

/*
 * Model classes itself
 * 
 */

case class QueryParameter(var name:String) extends NormalParameter(name) with ResourceTypeMember[Nothing] with MethodMember[Nothing] with ResourceMember[Nothing];
case class UriParameter(var name:String) extends NormalParameter(name) with ResourceTypeMember[Nothing] with ResourceMember[Nothing];
case class HeaderParameter(var name:String) extends NormalParameter(name) with MimeMember with FormChildren with MethodMember[Nothing];
case class FormParameter( var name:String) extends Parameter(name) with FormChildren ;

abstract class MimeTypeDescription(  ch:Seq[MimeMember]=List()) extends ParentedElement[MimeMember](ch)
 with BodyMember[MimeMember] with ResponseMember[MimeMember]{var example:String};

case class Form(var members:Seq[FormChildren]=List(),var diet:Boolean=false) extends ParentedElement[FormChildren](members)with BodyMember[FormChildren]
{var name:String="multipart/formdata";}

case class Json(var example:String="",var schema:String="",var name:String="application/json",var members:Seq[MimeMember]=List(),var diet:Boolean=false) 
extends MimeTypeDescription(members);
case class XML(var example:String="",var schema:String="",var name:String="application/xml",var members:Seq[MimeMember]=List(),var diet:Boolean=false) extends MimeTypeDescription();
case class Generic(var name:String,var example:String="",var members:Seq[MimeMember]=List(),var diet:Boolean=false) extends MimeTypeDescription(members);

case class Project(var name: String,var members:Seq[Package]=List(),var diet:Boolean=false) extends BasicElement[Package](members) {
  def parent(): Null = null;
}

case class Package(var name: String,var members:Seq[Api]=List(),var diet:Boolean=false) extends ParentedElement[Api](members){
   override def parent():Project = super.parent().asInstanceOf[Project];
}

object Protocol extends Enumeration {
   type Protocol = Value
   val HTTP,HTTPS = Value
}

case class Api(var name: String,var members:Seq[ApiMember[_]]=List(),var diet:Boolean=false) extends ParentedElement[ApiMember[_]](members){
  override def parent():Package = super.parent().asInstanceOf[Package];
  import Protocol._
  var protocols:Set[Protocol]=Set();
  var version:String="";
  var baseUri:String="";
  var mediaType:String="";
  var title:String="";
  //TODO securedBy  
  case class TitleFeature() extends StringFeature("title",()=>title,x=>title=x);
  
  case class VersionFeature() extends StringFeature("version",()=>version,x=>version=x);
  
  case class BaseUriFeature() extends StringFeature("baseUri",()=>baseUri,x=>baseUri=x);
  
  case class MediaTypeFeature() extends StringFeature("mediaType",()=>mediaType,x=>mediaType=x);
  
  case class ProtocolFeature() extends Feature[Set[Protocol]]("protocols",()=>protocols,x=>protocols=x);
  
  override def valueFeatures()=List(ProtocolFeature(),VersionFeature(),BaseUriFeature(),TitleFeature(),MediaTypeFeature());
}

abstract class HasTraits[C<:IModelElement[_]](members:Seq[C]) extends ParentedElement[C](members){
  var is: Seq[String]=List(); 
}
abstract class HasType[C<:IModelElement[_]](members:Seq[C]) extends HasTraits[C](members){
  var resourceType:String=null;
}

case class Resource(var name: String, var members:Seq[ResourceMember[_]]=List(),var diet:Boolean=false) extends HasType[ResourceMember[_]](members)
 with ApiMember[ResourceMember[_]] with ResourceMember[ResourceMember[_]];

case class Method(var name: String, var members:Seq[MethodMember[_]]=List(),var diet:Boolean=false) extends HasTraits[MethodMember[_]](members)
with ResourceTypeMember[MethodMember[_]] with ResourceMember[MethodMember[_]]

case class Body(var members:Seq[BodyMember[_]],var diet:Boolean=false) extends ParentedElement[BodyMember[_]](members)with MethodMember[BodyMember[_]]{var name:String=""}

case class Response(var name: String,var members:Seq[ResponseMember[_]],var diet:Boolean=false) extends ParentedElement[ResponseMember[_]](members)
with MethodMember[ResponseMember[_]]

case class SecuritySchema(var name: String,var value:String) extends NoChildren(name) with ApiMember[Nothing]

case class GlobalSchema(var name: String,var value:String) extends NoChildren(name) with ApiMember[Nothing]

case class Trait(var name: String,var members:Seq[MethodMember[_]]=List(),var diet:Boolean=false) extends HasTraits[MethodMember[_]](members) with ApiMember[MethodMember[_]]

case class ResourceType( var name: String,var members:Seq[ResourceTypeMember[_]]=List(),var diet:Boolean=false) extends HasType[ResourceTypeMember[_]](members) with ApiMember[ResourceTypeMember[_]]

/*
 * Companion objects
 */

object Project{
  def apply(nm:String,p:Package*)={new Project(nm,p)}
}
object Body{
  def apply(p:BodyMember[_]*)={new Body(p)} 
}
object Package{
 def apply(nm:String,p:Api*)={new Package(nm,p)} 
}
object Api{
  
  def apply(nm:String,p:ApiMember[_]*)={new Api(nm,p)} 
}
object Resource{
  def apply(nm:String,p:ResourceMember[_]*)={new Resource(nm,p)} 
}
object Method{
  def apply(nm:String,p:MethodMember[_]*)={new Method(nm,p)} 
}
object Response{
  def apply(nm:String,p:ResponseMember[_]*)={new Response(nm,p)} 
}