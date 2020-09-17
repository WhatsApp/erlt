/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.whatsapp.analyzer.util

object SCC {

  type Component = List[Vertex]

  case class Vertex(label: String)
  case class Edge(from: Vertex, to: Vertex)
  case class G(vertices: List[Vertex], edges: List[Edge])

  case class State(
      graph: G,
      count: Int,
      visited: Map[Vertex, Boolean],
      dfNumber: Map[Vertex, Int],
      lowlinks: Map[Vertex, Int],
      stack: List[Vertex],
      components: List[Component],
  )

  private def initial(g: G): State =
    State(
      graph = g,
      count = 1,
      visited = g.vertices.map((_, false)).toMap,
      dfNumber = Map(),
      lowlinks = Map(),
      stack = Nil,
      components = Nil,
    )

  def components(graph: G): List[Component] = {
    var state = search(graph.vertices.head, initial(graph))
    while (state.visited.exists(_._2 == false)) {
      state.visited.find(_._2 == false).foreach { tuple =>
        val (vertex, _) = tuple
        state = search(vertex, state)
      }
    }
    state.components
  }

  private def search(vertex: Vertex, state: State): State = {

    val newState =
      state.copy(
        visited = state.visited.updated(vertex, true),
        dfNumber = state.dfNumber.updated(vertex, state.count),
        count = state.count + 1,
        lowlinks = state.lowlinks.updated(vertex, state.count),
        stack = vertex :: state.stack,
      )

    def processVertex(st: State, w: Vertex): State = {
      if (!st.visited(w)) {
        val st1 = search(w, st)
        val min = smallest(st1.lowlinks(w), st1.lowlinks(vertex))
        st1.copy(lowlinks = st1.lowlinks.updated(vertex, min))
      } else {
        if ((st.dfNumber(w) < st.dfNumber(vertex)) && st.stack.contains(w)) {
          val min = smallest(st.dfNumber(w), st.lowlinks(vertex))
          st.copy(lowlinks = st.lowlinks.updated(vertex, min))
        } else st
      }
    }

    val strslt =
      adjacent(vertex, newState).foldLeft(newState)(processVertex)

    if (strslt.lowlinks(vertex) == strslt.dfNumber(vertex)) {
      val index = strslt.stack.indexOf(vertex)
      val (comp, rest) = strslt.stack.splitAt(index + 1)
      strslt.copy(
        stack = rest,
        components = strslt.components :+ comp,
      )
    } else strslt
  }

  private def smallest(x: Int, y: Int): Int =
    if (x < y) x else y

  private def adjacent(vertex: Vertex, state: State): List[Vertex] =
    for { edge <- state.graph.edges if edge.from == vertex } yield edge.to

}
