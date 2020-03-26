namespace Bored

module Boring =
    open System.Text

    let spaces count =
        let builder = StringBuilder()
        for _ in 1 .. count do
            builder.Append(' ') |> ignore
        builder.ToString()

    let leftPadding line width = spaces ((String.length line) * width)

    let shiftLeft line =
        let len = String.length line
        [ for i in (len - 1) .. -1 .. 1 -> line.[i..] ]

    let moveRight line width =
        let width' = (String.length line) * width
        [ for i in 0 .. width' -> (spaces i) + line ]

    let shiftRight line width =
        let len = String.length line
        [ for i in 1 .. len -> 
            (leftPadding line width) 
            + (spaces i) 
            + line.[..(len - i)] ]

    let moveLeft line width =
        let width' = (String.length line) * width
        [ for i in width' .. -1 .. 1 -> (spaces i) + line ]

    let swiggy line window count =
        let rec repeat count =
            match count with 
            | 0 -> []
            | _ ->
                moveRight line window
                @ moveLeft line window
                @ repeat (count - 1)
        repeat count 

    let rna line window count =
        let opening =
            moveRight line window
            @ shiftRight line window
            @ ((shiftRight line window) |> List.rev).[1..]
            @ moveLeft line window 
            @ ((shiftLeft line) |> List.rev).[..(String.length line - 2)]

        let body =
            (shiftLeft line).[1..]
            @ moveRight line window
            @ shiftRight line window
            @ ((shiftRight line window) |> List.rev).[1..]
            @ moveLeft line window 
            @ ((shiftLeft line) |> List.rev).[..(String.length line - 2)]

        let rec repeat count =
            match count with
            | 0 -> []
            | _ -> body @ repeat (count - 1)

        opening @ repeat count

    let slide line window =
        let len = String.length line
        let width = window * len
        [ for i in (len - 1) .. -1 .. 0 do
            for j in 0 .. (width - len) ->
                (if i > 0 then line.[..(i - 1)] else "") 
                + spaces (j) 
                + line.[i..i] 
                + spaces (width - len - j)
                + line.[(i + 1)..] ]

    let hourGlass line window count =
        let rec repeat count =
            match count with
            | 0 -> []
            | _ -> 
            (slide line window).[1..] 
              @ ((slide line window) |> List.rev).[1..] 
                @ repeat (count - 1)

        repeat count

    let borify line window =
        let count = 1
        swiggy line 3 1
        @ swiggy line 5 1
        @ swiggy line window 1
        @rna line 3 1 
        @ shiftLeft line 
        @ rna line 5 1 
        @ shiftLeft line 
        @ rna line window 1 
        @ shiftLeft line 
        @ [ line ] 
        @ hourGlass line window count


module Driver =
    open System
    open Boring

    [<EntryPoint>]
    let main argv =
        let line =
            match argv.Length with
            | 0 -> "I'm bored"
            | _ -> String.Join(' ', argv)

        let sw = Diagnostics.Stopwatch()
        sw.Start()
        let output = String.Join("\n", (borify line 7))
        sw.Stop()
        System.IO.File.WriteAllText("text.txt", output)
        printfn "%s" output
        printfn "Took %i ms" sw.ElapsedMilliseconds
        0 // return an integer exit code
