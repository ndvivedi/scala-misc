object Shortest {
  class Vertex(val name:String) {
    override def toString = name
  }
  class Edge(val v1:Vertex, val v2:Vertex) {
    def contains(v:Vertex): Boolean = v == v1 || v == v2
    override def toString = v1 + "-" + v2
  }

  def shortestPath(g: List[Edge], start: Vertex, end: Vertex): List[Edge] = {
    def edges(g: List[Edge], v: Vertex): List[Edge] = g.filter(e => e.contains(v))
    def out(g: List[Edge], e: Edge): List[Edge] = edges(g, e.v1) ++ edges(g, e.v2)
    def trav0(paths: List[List[Edge]], visited: Set[Vertex]): List[Edge] = {
      println("paths " + paths)
      println("visited " + visited)
      val newPaths = (
        for (path <- paths;
             nextEdge <- out(g, path.last).filter(e => ! (visited.contains(e.v1) && visited.contains(e.v2)))) yield path :+ nextEdge);
      val newVisited = visited ++ newPaths.map(p => List(p.last.v1, p.last.v2)).flatten
      if (newVisited.size == visited.size)
        List()
      else
        newPaths.filter((p: Path) => p.last.contains(end)) match {
          case List() => trav0(newPaths, newVisited);
          case x::_ => x;
        }
    }
    trav0(edges(g, start).map(x=>List(x)), Set[Vertex]())
  }

  type Graph = List[Edge]
  type Path = List[Edge]
  //try to not use the :+ operator
  //a little cleaner? maybe?
  def shortestPath2(g: Graph, start: Vertex, end: Vertex): Path = {
    def edges(v: Vertex): List[Edge] = g.filter(e => e.contains(v))
    def out(e: Edge): List[Edge] = edges(e.v1) ++ edges(e.v2)
    def trav0(paths: List[Path], visited: Set[Vertex]): Path = {
      def visitedByEdge(e:Edge): Boolean = !(visited.contains(e.v1) && visited.contains(e.v2))
      val newPaths = (
        for (path <- paths;
             nextEdge <- out(path.first) filter visitedByEdge) 
           yield nextEdge :: path);
      val newVisited = visited ++ newPaths.map(p => List(p.first.v1, p.first.v2)).flatten

      if (newVisited.size == visited.size)
        //If we haven't visted any new nodes we are done without finding a path
        List()
      else
        newPaths.filter((p: Path) => p.first.contains(end)) match {
          case List() => trav0(newPaths, newVisited);
          case x::_ => x;
        }
    }
    trav0(edges(start).map(x => List(x)), Set[Vertex]())
  }
}
