package org.raml.model.diet

import org.raml.model.diet.IModelElement

object RamlExpriments extends App {
  var project = Project("Test project", 
        Package("testApis", 
            Api("my api", 
                Resource("/", 
                    Resource("hello", 
                          List(Method("GET"))))))
        )
  var v1:IModelElement[Package]=project.deepCopy();
  var app=v1->"testApis"->"my api";
  app+=Resource("h2",List(Method("GET",List(Body(List(Json(""))),Response("200",List(Json()))))));
  println(project.allChildren(classOf[Method]));
  println(v1.allChildren(classOf[Resource]));
}