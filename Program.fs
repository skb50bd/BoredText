﻿namespace Bored

module Boring =
    open System.Text
    let spaces count =
        let builder = StringBuilder()
        for _ in 1 .. count do
            builder.Append(' ') |> ignore
        builder.ToString()

    let shiftLeft line =
            let len = String.length line
            [ for i in (len - 1) .. -1 .. 1 -> line.[i..] ]

    let dnaHalf line window count =
        let leftPadding line width = spaces ((String.length line) * width)

        let moveRight line width =
            let width' = (String.length line) * width
            [ for i in 0 .. width' -> (spaces i) + line ]

        let shiftRight line width =
            let len = String.length line
            [ for i in 1 .. len -> (leftPadding line width) + (spaces i) + line.[..(len - i)] ]

        let moveLeft line width =
            let width' = (String.length line) * width
            [ for i in width' .. -1 .. 1 -> (spaces i) + line ]

        let opening =
            moveRight line window
            @ shiftRight line window
              @ ((shiftRight line window) |> List.rev).[1..]
                @ moveLeft line window @ ((shiftLeft line) |> List.rev).[..(String.length line - 2)]

        let body =
            (shiftLeft line).[1..]
            @ moveRight line window
              @ shiftRight line window
                @ ((shiftRight line window) |> List.rev).[1..]
                  @ moveLeft line window @ ((shiftLeft line) |> List.rev).[..(String.length line - 2)]

        let rec repeat count =
            match count with
            | 0 -> []
            | _ -> body @ repeat (count - 1)

        opening @ repeat count

    let hourGlass line window count =
        let len = String.length line
        let width = window * len

        let slide =
            [ for i in (len - 1) .. -1 .. 0 do
                for j in 0 .. (width - len) ->
                    (if i > 0 then line.[.. (i - 1)] else "") 
                    + spaces (j)
                    + line.[i..i]
                    + spaces (width - len - j)
                    + line.[(i + 1)..] ]

        let rec repeat count =
            match count with
            | 0 -> []
            | _ -> 
                slide.[1..]
                @ (slide |> List.rev).[1..]
                @ repeat (count - 1)

        repeat count
    
    let borify line window =
        let count = 1
        dnaHalf line 1 1
        @ dnaHalf line 3 1
        @ dnaHalf line 5 1
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
        let output = 
            String.Join("\n", (borify line 5))
        sw.Stop()
        System.IO.File.WriteAllText("text.txt", output)
        printfn "%s" output
        printfn "Took %i ms" sw.ElapsedMilliseconds
        0 // return an integer exit code