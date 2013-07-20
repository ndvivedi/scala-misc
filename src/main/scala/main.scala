import Shortest._

object Main extends App {

  def small(): (List[Edge], List[Vertex]) = {
    val verts = (for (i <- 0 until 5) yield new Vertex(i.toString)).toList
    val graph = List(
        new Edge(verts(0), verts(1)),
        new Edge(verts(0), verts(2)),
        new Edge(verts(0), verts(3)),
        new Edge(verts(2), verts(4)),
        new Edge(verts(3), verts(4))
      )
    (graph, verts)
  }

  val (g, v) = small()
  println(shortestPath2(g, v(0), v(4)))
  println(shortestPath2(g, v(0), new Vertex("not in graph")))

}
