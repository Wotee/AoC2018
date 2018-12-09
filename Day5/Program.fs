open System

let isOppositePolarity str char =
  String.length str >= 1
  && Char.ToLower str.[str.Length-1] = Char.ToLower char
  && str.[str.Length-1] <> char

let reactElements acc elem = 
  if isOppositePolarity acc elem
  then acc.[0..(acc.Length)-2]
  else acc + (string elem)

let part1 = 
  Seq.fold reactElements ""
  >> Seq.length

let getLengthWithoutChar delChar = 
  Seq.fold (fun acc elem ->
    if (Char.ToLower elem = delChar) then acc
    else reactElements acc elem
    ) ""
  >> Seq.length

let part2 (input:string) = 
  {'a'..'z'}
  |> Seq.map (fun c -> getLengthWithoutChar c input)
  |> Seq.min

[<EntryPoint>]
let main argv =
  let input = System.IO.File.ReadAllText("input.txt")

  input |> part1 |> printfn "%A"
  input |> part2 |> printfn "%A"

  Console.ReadKey() |> ignore
  0
