﻿// Copyright Chris Collazo 2015
open System
open System.Collections.Generic

type Key = {
    num  : int;
    prob : double;
}

type BST = {
    ele    : Key;
    height : int;
    cost   : double;
    left   : BST option;
    right  : BST option;
}

let KeyCost key height =
    key.prob * ( (double)height + 1.0 )

let rec PrintTree tree =
    printfn "Key %d with prob: %f, cost: %f has children:" tree.ele.num tree.ele.prob tree.cost
    match tree.left with
    | Some t -> printfn "Left: %d" t.ele.num
    | None -> ()
    match tree.right with
    | Some t -> printfn "Right: %d" t.ele.num
    | None -> ()
    printfn "\n"
    match tree.left with
    | Some t -> PrintTree t
    | None -> ()
    match tree.right with
    | Some t -> PrintTree t
    | None -> ()

let OptimalTree (keys : List<Key>) =
    let lookup = new Dictionary<int*int*int, BST>()

    let rec MinCostTree i j h =
        match lookup.ContainsKey (i, j, h) with
        | true ->  printfn "In dict: %d, %d, %d" i j h
                   lookup.Item (i, j, h)
        | false -> match i with
                   | i when i = j -> let leaf = { ele = keys.[i]; 
                                                  height = h; 
                                                  cost = KeyCost keys.[i] h; 
                                                  left = None; 
                                                  right = None }
                                     printfn "Not in dict: %d, %d, %d - adding" i j h
                                     lookup.Add( (i, j, h), leaf )
                                     leaf
                   | _ -> let treeList = new List<BST>()
                          for k in i .. j do
                              let leftChild =  match k with
                                               | k when k = i -> None
                                               | _ -> Some( MinCostTree i (k-1) (h+1) )
                              let rightChild = match k with
                                               | k when k = j -> None
                                               | _ -> Some( MinCostTree (k+1) j (h+1) )
                              let leftCost =   match leftChild with
                                               | Some t -> t.cost
                                               | None -> 0.0
                              let rightCost =  match rightChild with
                                               | Some t -> t.cost
                                               | None -> 0.0
                              treeList.Add( { ele = keys.[k]; 
                                              height = h; 
                                              left = leftChild; 
                                              right = rightChild; 
                                              cost = leftCost + rightCost + KeyCost keys.[k] h } )
                          let mintree = List.ofSeq treeList
                                        |> List.minBy (fun t -> t.cost)
                          printfn "Not in dict: %d, %d, %d - adding" i j h
                          lookup.Add( (i, j, h), mintree )
                          mintree
    MinCostTree 0 (keys.Count - 1) 0


[<EntryPoint>]
let main argv = 
    let keys = new List<Key>()
    // Add your keys, and their probabilities. OptimalTree will arrange them optimally,
    // creating least cost tree structure based on the likelihood any leaf will be needed
    keys.Add({num = 1; prob = 0.25})
    keys.Add({num = 2; prob = 0.2})
    keys.Add({num = 3; prob = 0.05})
    keys.Add({num = 4; prob = 0.2})
    keys.Add({num = 5; prob = 0.3})
//    keys.Add({num = 6; prob = 0.09})
//    keys.Add({num = 7; prob = 0.13})
//    keys.Add({num = 8; prob = 0.09})
    let otree = OptimalTree keys
    printfn "\n"
    PrintTree otree
    0
