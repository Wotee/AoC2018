namespace Utils

module Parser =
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
  