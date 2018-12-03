open System
open System.Text.RegularExpressions

let internal matchesToArray (matches:MatchCollection) = 
  let array = Array.zeroCreate matches.Count
  matches.CopyTo(array, 0)
  array
  
let internal getMatches pattern = 
  Regex(pattern).Matches
  >> matchesToArray
  >> Array.map(fun x -> x.Value)

let getInts = 
  getMatches "-?\d+"
  >> Array.map int

type SquareClaim = 
  { id : int
    dx : int array
    dy : int array }
  
let toClaim (array : int array) =
  { id = array.[0]
    dx = [|array.[1]..array.[1]+array.[3]-1|]
    dy = [|array.[2]..array.[2]+array.[4]-1|]  }

let mapArea claim = 
  claim.dx |> Seq.collect (fun x -> claim.dy |> Seq.map (fun y -> (x, y)))

let part1 : string seq -> int =
  Seq.map (getInts >> toClaim)
  >> Seq.collect mapArea
  >> Seq.groupBy id
  >> Seq.filter (fun (_, value) -> Seq.length value > 1)
  >> Seq.length

let part2 input =
  "TODO"

[<EntryPoint>]
let main argv =
  let input =
    argv.[0]
    |> System.IO.File.ReadAllLines
    |> Array.toSeq

  input |> part1 |> printfn "Part 1: %i"
  
  Console.ReadKey() |> ignore
  0
