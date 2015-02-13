package org.raml.model.diet;

/**
 * Tag interfaces
 */
trait ApiMember[ParentType<:CanHaveChild, ChildType <: IModelElement[_, _]] extends IModelElement[ParentType, ChildType]
trait ResourceMember[ParentType <: CanHaveResources[_], ChildType <: IModelElement[_, _]] extends IModelElement[ParentType, ChildType]
trait CanHaveResources[A <: IModelElement[_, _]] extends IModelElement[CanHaveResources[_], A]


case class Project(override val name: String,folders:Package*) extends BasicElement[Null, Package](name,folders) {
  def parent(): Null = null;
}

case class Package(override val name: String,apis:Api*) extends ParentedElement[Project, Api](name,apis);
case class Api(override val name: String,members:ApiMember[_,_]*) extends ParentedElement[Package, ApiMember[_, _]](name,members);

case class Resource(override val name: String,members:ResourceMember[_,_]*) extends ParentedElement[CanHaveResources[_], ResourceMember[_, _]](name,members)
  with ResourceMember[CanHaveResources[_], ResourceMember[_, _]]
  with CanHaveResources[ResourceMember[_, _]] with ApiMember[CanHaveResources[_], ResourceMember[_, _]]

case class Method(override val name: String) extends ParentedElement[Resource, IModelElement[_, _]](name) with ResourceMember[Resource, IModelElement[_, _]]


