open System
open Utils

type Claim = {
  id : int
  coords : (int*int) seq
}

let claim (array : int array) =
  let createSeq from size = seq { from .. size+from-1}
  { id = array.[0]
    coords = Seq.allPairs (createSeq array.[1] array.[3]) (createSeq array.[2] array.[4]) }

let part1 : Claim seq -> int =
  Seq.collect (fun claim -> claim.coords)
  >> Seq.groupBy id
  >> Seq.filter (fun (_, value) -> Seq.length value > 1)
  >> Seq.length
  
let part2 singleClaimList =
  let singleClaims = singleClaimList |> List.map fst |> Set.ofList
  Seq.filter (fun claim -> Set.isSubset (claim.coords |> Set.ofSeq) singleClaims)
  >> Seq.map (fun claim -> claim.id)
  >> Seq.head

let splitByClaimCount =
  Seq.collect (fun claim -> claim.coords)
  >> Seq.groupBy id
  >> Seq.toList
  >> List.partition(fun (_, value) -> Seq.length value > 1)

[<EntryPoint>]
let main argv =
  let claims =
    "input.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.map (Parser.getInts >> claim)

  let multiClaims, singleClaims = splitByClaimCount claims
  multiClaims.Length |> printfn "Part 1: %i"
  claims |> (part2 singleClaims) |> printfn "Part 2: %i"

  Console.ReadKey() |> ignore
  0
