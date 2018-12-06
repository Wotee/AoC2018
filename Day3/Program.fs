open System
open Utils
open System.Diagnostics

type Claim = {
  id : int
  coords : (int*int) list
}

let claim (array : int array) =
  let createList from size = [ from .. size+from-1 ]
  { id = array.[0]
    coords = List.allPairs (createList array.[1] array.[3]) (createList array.[2] array.[4])}
  
let part2 singleClaimList =
  let singleClaims = singleClaimList |> List.map fst |> Set.ofList
  Seq.filter (fun claim -> Set.isSubset (claim.coords |> Set.ofList) singleClaims)
  >> Seq.map (fun claim -> claim.id)
  >> Seq.head

let splitByClaimCount =
  List.collect (fun claim -> claim.coords)
  >> List.groupBy id
  >> List.partition(fun (_, value) -> Seq.length value > 1)

[<EntryPoint>]
let main argv =
  let claims =
    "input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.toList
    |> List.map (Parser.getInts >> claim)

  let multiClaims, singleClaims = splitByClaimCount claims
  multiClaims.Length |> printfn "Part 1: %i"
  claims |> (part2 singleClaims) |> printfn "Part 2: %i"
  Console.ReadKey() |> ignore
  0
