package org.raml.model.diet

object RamlExpriments extends App {
  
  var project = Project("Test project", 
        Package("testApis",Api("my api")),Package("XX"))
  var v1:IModelElement[Package]=project.deepCopy();
  var d=v1.features();
  for (f<-d){
    println(f.name+":"+f.get())
  }
  var app=v1->"testApis"->"my api";
  
  app+=Resource("h2",Method("GET",Body(Json()),Response("200",Json())));  
}