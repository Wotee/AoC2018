open System

let part1 : string seq -> int = 
  let multiply f1 f2 x = f1 x * f2 x
  let amountOfNTuples n =
    Seq.map (Seq.countBy id >> Seq.map snd)
    >> Seq.filter (Seq.contains n)
    >> Seq.length
  multiply (amountOfNTuples 2) (amountOfNTuples 3)

let part2 : string seq -> string = 
  let createPairSeq sequence = 
    Seq.allPairs sequence sequence
    |> Seq.filter (fun (x,y) -> x <> y)
  
  let zip (a, b) = Seq.zip a b
  
  let diffCount = 
    zip
    >> Seq.countBy (fun (first, second) -> first <> second)
    >> Seq.choose (fun (match', amount) -> if match' then Some amount else None)
    >> Seq.head

  createPairSeq
  >> Seq.filter (fun x -> diffCount x = 1)
  >> Seq.head
  >> zip
  >> Seq.choose(fun (x,y) -> if x = y then Some x else None)
  >> String.Concat
  
[<EntryPoint>]
let main argv =
  let input =
    argv.[0]
    |> System.IO.File.ReadAllLines 
    |> Array.toSeq
  
  input |> part1 |> printfn "Part 1: %i"
  input |> part2 |> printfn "Part 2: %s"

  Console.ReadKey() |> ignore
  0