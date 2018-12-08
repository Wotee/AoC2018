open System
open System.IO
open System.Text.RegularExpressions
open Utils

let getGuardSleepRhytms (actionLog:string seq) =
  let idOrMinute = Parser.getCustom "[#:]\d+"
  let validInts = Array.map (fun (str:string) -> str |> Seq.tail |> String.Concat |> int)
  let toTuple array = Array.head array, Array.tail array
  let toEvenLengthSecond (x, array) =
    let length = Array.length array
    if length % 2 = 0 then x, array else x, array.[0..length - 2]
  
  (actionLog
  |> String.Concat).Split("Guard ")
  |> Seq.tail // Skip the start that is rubbish
  |> Seq.map (idOrMinute >> validInts >> toTuple >> toEvenLengthSecond)
  |> Seq.groupBy fst
  |> Seq.map (fun (first, second) -> first, second |> Seq.collect snd)
  |> Seq.filter (fun (x, y) -> y |> Seq.length > 0)
 
let countMinutesSlept = 
  Seq.chunkBySize 2
  >> Seq.collect (fun array -> [array.[0]..array.[1]-1])
  >> Seq.countBy id
  >> Seq.maxBy snd

let part1 (sleepRhytms: seq<int*seq<int>>) =
  let countTotalSleep = 
    Seq.mapi (fun i x -> if i % 2 = 0 then -x else x)
    >> Seq.reduce (+)
  
  let countMostSleptMinute = 
    countMinutesSlept >> fst
  
  let mostSleepingGuard = 
    sleepRhytms
    |> Seq.maxBy (fun (_, sleepRhythm) -> sleepRhythm |> countTotalSleep)
    |> fst
  
  let getSleepRhythm guardId = 
    sleepRhytms
    |> Seq.find (fun (id, rhythm) -> id = mostSleepingGuard)
    |> snd
  
  mostSleepingGuard * (mostSleepingGuard |> getSleepRhythm |> countMostSleptMinute)

let part2 = 
  Seq.map (fun (id, rhythm) -> id, countMinutesSlept rhythm)
  >> Seq.maxBy (fun (id, (min,times)) -> times)
  >> (fun (id, (min,times)) -> id * min)

[<EntryPoint>]
let main argv =
  let sleepRhythms =
    "input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.sort
    |> getGuardSleepRhytms

  sleepRhythms |> part1 |> printfn "Part1: %A"
  sleepRhythms |> part2 |> printfn "Part2: %A"

  Console.ReadKey() |> ignore
  0