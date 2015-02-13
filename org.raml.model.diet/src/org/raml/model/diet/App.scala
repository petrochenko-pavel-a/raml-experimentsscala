package org.raml.model.diet

object RamlExpriments extends App {

  
  var project = Project("Test project", 
        Package("testApis", 
            Api("my api", 
                Resource("/", 
                    Resource("hello", 
                        Method("GET")))))
        )
  println(project.allChildren(classOf[Method]));
}