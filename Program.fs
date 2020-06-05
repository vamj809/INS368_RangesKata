// Learn more about F# at http://fsharp.org

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
    else
        Console.WriteLine(Range("(1,5)").Contains([|2;3|]))
        Console.WriteLine(Range("(2,6]").Contains([|3;6|]))
        Console.WriteLine(Range("[3,7)").Contains([|3;7|]))
        Console.WriteLine(Range("[4,8]").Contains([|3|]))
        PrintIntArray(Range("(1,3)").GetAllPoints())
        PrintIntArray(Range("[2,3]").GetAllPoints())
        PrintIntArray(Range("(7,10]").GetAllPoints())
        PrintIntArray(Range("[13,14]").GetAllPoints())
        Console.WriteLine(Range("[15,17]").ContainsRange(Range("[15,18)")))
        Console.WriteLine(Range("[10,11)").ContainsRange(Range("(9,11)")))
        Console.WriteLine(Range("(29,35)").ContainsRange(Range("[29,35]")))
        Console.WriteLine(Range("(32,43]").ContainsRange(Range("(3,5]")))
        PrintIntArray(Range("[3,8)").GetEndPoints())
        PrintIntArray(Range("(10,18]").GetEndPoints())
        PrintIntArray(Range("(5,50)").GetEndPoints())
        PrintIntArray(Range("[22,25]").GetEndPoints())
        Console.WriteLine(Range("(1,5)").OverlapsRange(Range("(3,18)")))
        Console.WriteLine(Range("[4,9]").OverlapsRange(Range("[5,10]")))
        Console.WriteLine(Range("(7,19]").OverlapsRange(Range("(1,7]")))
        Console.WriteLine(Range("[13,19)").OverlapsRange(Range("[19,25)")))
        Console.WriteLine(Range("(3,5)").Equals(Range("(3,5)")))
        Console.WriteLine(Range("(25,28)").Equals(Range("(35,38)")))
        Console.WriteLine(Range("[7,11]").Equals(Range("(6,11]")))
        Console.WriteLine(Range("[14,24]").Equals(Range("[14,24)")))
    0// return an integer exit code
