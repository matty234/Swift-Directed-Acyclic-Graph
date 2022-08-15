//
//  Assembly.swift
//  SeqDac
//
//  Created by Matthew Brown on 12/08/2022.
//

import Foundation
import SwiftUI




var g: Graph = Graph<String>()
g.addVertex(vertex: "Root")
g.addVertex(vertex: "Root2")
g.addVertex(vertex: "Root3")

g.addVertex(parent: "Root", vertex: "Child1")
g.addVertex(parent: "Root", vertex: "Child3")
g.addVertex(parent: "Root3", vertex: "Child1")


g.addVertex(parent: "Root2", vertex: "Child1")
g.addVertex(parent: "Child3", vertex: "Child6")
g.addVertex(parent: "Child1", vertex: "Child4")
g.addVertex(parent: "Child1", vertex: "Child2")
g.addVertex(parent: "Child1", vertex: "Child5")

var layers = g.assignLayers();
print(layers)
print(g.getCrossovers(layers: layers))

var crossoverCount = 0;
(layers, crossoverCount) = g.getCrossoversWithFix(layers: layers);
print(g.assignWeights(layers: layers))
print("layers: \(layers)")



public class GraphEdge: Equatable, CustomStringConvertible {
    public var from: String;
    public var to: String;
    
    init(from: String, to: String) {
        self.from = from;
        self.to = to;
    }
    
    
    public static func == (lhs: GraphEdge, rhs: GraphEdge) -> Bool {
        return lhs.to == rhs.to && lhs.from == rhs.from;
    }
    
    public var description: String {
        return "\(self.from) --> \(self.to)"
    }
    
    
}

public class Graph<E>: NSCopying {
   
    public var edges: [GraphEdge];
    
    // - TODO: make this generic at some point
    public var vertices: [String];
    
    
    
    private init(vertices: [String], edges: [GraphEdge]) {
        self.edges = edges;
        self.vertices = vertices
    }
    
    public init(edges: [GraphEdge]) {
        self.edges = edges;
        self.vertices = [];
        for edge in edges {
            self.vertices.append(edge.to);
            self.vertices.append(edge.from);
        }
    }
    
    public init() {
        self.edges = []
        self.vertices = [];
    }
    
    
    public func addVertex(parent: String, vertex: String) {
        self.edges.append(GraphEdge(from: parent, to: vertex))
        if !self.vertices.contains(vertex) {
            self.vertices.append(vertex)
        }
    }
    
    public func addVertex(vertex: String) {
        self.vertices.append(vertex);
    }
    
    
    public func assignLayers() -> [[String]] {
        let layerAssignment = LayerAssignment<E>(graph: self);
        return layerAssignment.slice();
    }
    
    public func getCrossovers(layers: [[String]]) -> Int {
        let crossovers = VertexOrdering<E>(graph: self, layers: layers)
        let (heuristic, toResolve) = crossovers.calculateCrossOvers()
        return heuristic
    }
    
    
    public func getCrossoversWithFix(layers: [[String]]) -> ([[String]], Int) {
        let crossovers = VertexOrdering<E>(graph: self, layers: layers)
        let newLayers = crossovers.attemptFix()
        let (heuristic, toResolve) = crossovers.calculateCrossOvers()
        return (newLayers, heuristic)
    }
    
    
    
    public func assignWeights(layers: [[String]]) -> (offsets: [String: Int], weights: [String: Int]) {
        let weights =  WeightCalculator<E>(graph: self).calculateWeights();
        /*let maxWeight = layers[0].reduce(into: 0.0) {
            $0 = $0 + (CGFloat(weights[$1] ?? 0))
        }*/
        
        let offsets = layers.reduce(into: [String: Int]()) {
            let layer = $1;
            var accumulate = 0;
            for layerItem in layer {
                $0[layerItem] = accumulate;
                accumulate = accumulate + (weights[layerItem] ?? 0);
            }
        }
        
    
        return  (offsets: offsets, weights: weights);
    }
    
    public func copy(with zone: NSZone? = nil) -> Any {
        let copy = Graph(vertices: vertices, edges: edges);
        return copy
    }
    
}


public class LayerAssignment<E> {
    private var _graph: Graph<E>
     
    public init(graph: Graph<E>) {
        self._graph = graph.copy() as! Graph<E>
    }

    /**
        Slices the vertices to find layers
        
        - Returns: the slices
     */
    
    public func slice() -> [[String]] {
        var slices: [[String]] = [];
        var verticesWithoutIncomingEdges = self.getVerticesWithoutIncomingEdges(edges: _graph.edges, vertices: Array(_graph.vertices))

        while verticesWithoutIncomingEdges.count > 0 {
            slices.append(verticesWithoutIncomingEdges);
            
            let newEdges = _graph.edges.reduce(into: [GraphEdge]()) {
                if verticesWithoutIncomingEdges.contains($1.from) {
                    return
                }
                
                $0.append($1);
            }
            self._graph.edges = newEdges;
            
            
            
            let newVertices = _graph.vertices.reduce(into: [String]()) {
                if verticesWithoutIncomingEdges.contains($1) {
                    return
                }
                
                $0.append($1);
            }
            self._graph.vertices = newVertices;
            
            verticesWithoutIncomingEdges = self.getVerticesWithoutIncomingEdges(edges: self._graph.edges, vertices: Array(self._graph.vertices))
            
        }
        
        return slices;
    }
    
    /**
        Finds all vertices that do not have incoming edges.
        
        - Parameters:
            - edges: the edges to check for incomingness
            - vertices: the vertices to reduce
        - Returns: a list of vertices that do not have incoming edges
     */
    private func getVerticesWithoutIncomingEdges(edges: [GraphEdge], vertices: [String]) -> [String] {
        let vertices_count_incoming_edge = edges.reduce(into: [String]()) {
            $0.append($1.to)
        }
        let vertices_that_do_not_have_incoming_edge = vertices.reduce(into: [String]()) {
            if !vertices_count_incoming_edge.contains($1) {
                $0.append($1)
            }
        }
        
        return vertices_that_do_not_have_incoming_edge;
    }
}


public class VertexOrdering<E> {
    private var MAX_ATTEMPTS = 25;

    
    private var _graph: Graph<E>
    private var layers: [[String]]
    
    
    public init(graph: Graph<E>, layers: [[String]]) {
        self._graph = graph.copy() as! Graph<E>
        self.layers = layers
    }
    
    
    public func attemptFix() -> [[String]] {
        var (currentCrossovers, toFixLocations) = calculateCrossOvers();
        var attempts = 0;
        
        while attempts < MAX_ATTEMPTS && currentCrossovers > 0 {
            attempts += 1;
            
            print("---- Attempt \(attempts) ----")
            
            let (topFixLayer, topFixPosition) = toFixLocations[0]
            if topFixPosition + 1 >= self.layers[topFixLayer].count {
                print("Switching: \(self.layers[topFixLayer][topFixPosition]) (out of \(toFixLocations)) -wrap ")
                self.layers[topFixLayer].swapAt(topFixPosition, 0)

            } else {
                
                print("Switching: \(self.layers[topFixLayer][topFixPosition]) (out of \(toFixLocations))  ")

                self.layers[topFixLayer].swapAt(topFixPosition, topFixPosition + 1)
            }
            
            print("\n")
            
            
            
            (currentCrossovers, toFixLocations) = calculateCrossOvers();

        }
        
        return self.layers
    }
    
    
    public func calculateCrossOvers() -> (Int, [(layerIndex: Int, valueIndex: Int)]) {
        var crossoverCount: Int = 0;
        
        var toResolve: [(layerIndex: Int, valueIndex: Int)] = [];
        
        for (idx, layer) in layers.enumerated() {
            if idx + 1 >= layers.count {
                return (crossoverCount, toResolve)
            }
            let from = layer;
            let to = layers[idx + 1]
            var fromLimit = 0;
            var previousRoot = ""
            
            for fromVal in from {
                
                let edgesFrom = self._graph.edges.filter {
                    $0.from == fromVal
                }
                
            
                
                for edgeFinal in edgesFrom {
                    let indexOfToInLayer = to.firstIndex(of: edgeFinal.to)!
                    print("\(edgeFinal) : \(indexOfToInLayer)")
                    
                    
                        if indexOfToInLayer >= fromLimit {
                            fromLimit = indexOfToInLayer
                        } else {
                            
                            if previousRoot != fromVal && previousRoot != "" {
                                print("Needs fix at \(edgeFinal) (ifxOfToInLayer: \(indexOfToInLayer) fromLimit \(fromLimit))")
                                toResolve.append((layerIndex: idx+1, valueIndex: indexOfToInLayer))
                                crossoverCount = crossoverCount + (fromLimit-indexOfToInLayer)
                            }
                        }
                    }
                    previousRoot = fromVal

                }
            print("-")
            
        }
        return (crossoverCount, toResolve);
    }
    
}

public class WeightCalculator<E> {
    
    private var _graph: Graph<E>
    
    private var vertexWeights: [String: Int];
     
    private var termini: [String];
    public init(graph: Graph<E>) {
        self._graph = graph
        self.vertexWeights = [:];
        self.termini = [];
    }
    
    
    
    
    

    /**
     This function calculates the display weights of each subgraph. The startAt should be all the termini of the graph.
     */
    public func calculateWeights() -> [String: Int] {
        
        var graphTermini = self._graph.vertices
        
        
        for edge in self._graph.edges {
            graphTermini = graphTermini.filter({ vertex in
                vertex != edge.from
            })
        }

 
        for terminus in graphTermini {
            ascend(terminus, 1)
        }
        
        return vertexWeights
        
        
    }
    
    private func ascend(_ root: String, _ value: Int) -> Void {
        
        vertexWeights[root] = (vertexWeights[root] ?? 0) + value;
        
        let upwardsVertexes = self._graph.edges.reduce(into: [String]()) {
            let edge = $1;
        
            if edge.to != root {
                return
            }
            
            $0.append(edge.from)
        }
    
        
        for vertex in upwardsVertexes {
            ascend(vertex, value)
        }
        
    }
    
    
    
}
