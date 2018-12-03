module Seq = let rec cycle xs = seq {yield! xs; yield! cycle xs}

open System
open System.Collections.Generic

let part1 : int seq -> int =
  Seq.sum

let part2 : int seq -> int = 
  let set = HashSet()
  Seq.cycle
  >> Seq.scan (+) 0
  >> Seq.skipWhile set.Add
  >> Seq.head

[<EntryPoint>]
let main argv =
  let input =
    argv.[0]
    |> System.IO.File.ReadAllLines 
    |> Seq.map int

  input |> part1 |> printfn "Part 1: %i"
  input |> part2 |> printfn "Part 2: %i"

  Console.ReadKey() |> ignore
  0