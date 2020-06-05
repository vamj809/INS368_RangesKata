// Learn more about F# at http://fsharp.org
//new Range(string rangeExpression)	Listo		
//bool Contains (int[] input)			Listo
//int[] GetAllPoints()			    Listo		
//int[] GetEndPoints()		
//bool ContainsRange (Range input)		    
//bool OverlapsRange (Range input)			
//bool Equals (Range input)			

open System

type Range(rangeExpression:string) = 
    class
        //Validate Expression
        let mutable Expression = rangeExpression.Trim().Split(',');
        let mutable startVal = Int32.Parse(Expression.[0].Substring(1));
        let mutable endVal   = Int32.Parse(Expression.[1].Substring(0,Expression.[1].Length - 1));
        
        do
            if(Expression.[0].StartsWith('(') = true)
                then startVal <- startVal + 1
            elif(Expression.[0].StartsWith('[') = false)
                then eprintfn "Rango Invalido"
            
            if(Expression.[1].EndsWith(')') = true)
                then endVal <- endVal - 1
            elif(Expression.[1].EndsWith(']') = false)
                then eprintfn "Rango Invalido"
            
            if(startVal > endVal) then printfn "Rango Invalido"
            
            printf "%s " rangeExpression
            
        member this.Contains(input:int[]):bool = 
            let mutable isContained = true
            for value in input do
                if (value < startVal || value > endVal)
                then isContained <- false
            isContained
            
        member this.GetAllPoints():int[] =
            [| for i in startVal .. endVal -> i |]
          
        member this.GetEndPoints():int[] =
            [| startVal; endVal |]
            
        member this.ContainsRange(input:Range):bool =
            let inputValues = input.GetEndPoints()
            if(startVal <= inputValues.[0] && inputValues.[1] <= endVal)
            then true
            else false
            
        member this.OverlapsRange(input:Range):bool =
            let inputValues = input.GetEndPoints()
            if((startVal <= inputValues.[0] && inputValues.[0] <= endVal) || 
               (startVal <= inputValues.[1] && inputValues.[1] <= endVal))
            then true
            else false
            
        member this.Equals(input:Range):bool =
            let inputValues = input.GetEndPoints()
            if(startVal = inputValues.[0] && inputValues.[1] = endVal)
            then true
            else false
    end

let PrintIntArray(array:int[]) = 
    printf "{"
    for i in 0 .. array.Length - 1 do
        printf "%d" array.[i]
        if(i < array.Length - 1) then printf ","
    printfn "}"

[<EntryPoint>]
let main argv =
    if argv.Length = 2
    then
        if argv.[1] = "GetAllPoints" then PrintIntArray(Range(argv.[0]).GetAllPoints())
        elif argv.[1] = "GetEndPoints" then PrintIntArray(Range(argv.[0]).GetEndPoints())
    elif argv.Length = 3
    then
        if argv.[1] = "ContainsRange" then
            Console.WriteLine(Range(argv.[0]).ContainsRange(Range(argv.[2])))
        elif argv.[1] = "OverlapsRange" then
            Console.WriteLine(Range(argv.[0]).OverlapsRange(Range(argv.[2])))
        elif argv.[1] = "Equals" then
            Console.WriteLine(Range(argv.[0]).Equals(Range(argv.[2])))
    elif argv.Length = 1 && argv.[0] = "Contains" then
        Console.WriteLine(Range("(1,10)").Contains([|3|]))
        Console.WriteLine(Range("(1,10)").Contains([|7;4;3|]))
        Console.WriteLine(Range("(1,10)").Contains([|2;6;12|]))
        Console.WriteLine(Range("(1,10)").Contains([|-3|]))
    0// return an integer exit code
