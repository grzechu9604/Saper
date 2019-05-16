// Learn more about F# at http://fsharp.org

open System

let rec get_int_in_range_from_input communicate min max =
    let get_int_from_input comunicate default_eror_value =
        try 
            printfn comunicate
            let x = System.Console.ReadLine(); 
            int(x)
        with 
            | :? System.FormatException -> 
                printfn "Invalid number!" 
                default_eror_value 
                in
    let value = get_int_from_input communicate (min - 1)
    if (value < min || value > max) then
        printfn "Wprowadź poprawną wartość z przedziału <%A, %A>" min max
        get_int_in_range_from_input communicate min max
    else
        value
    
let validate_mines_amount x y mines_amount =
    x * y > mines_amount

let random_number_generator min max =
    (new System.Random()).Next(min, max)

let generate_board x y =
    let _board : int[,] = Array2D.zeroCreate x y
    _board

let assign_mines mines_amount board =
        for r = 0 to Array2D.length1 board - 1 do
            for c = 0 to Array2D.length2 board - 1 do
                board.[r, c] := 1

let print_table table =
    for r = 0 to Array2D.length1 table - 1 do
        for c = 0 to Array2D.length2 table - 1 do
            printf "%A " table.[r, c]
        printfn ""

[<EntryPoint>]
let main argv =
    //let board = generate_board 5 6
    //print_table board
    //assign_mines 1 board
    //print_table board
    let x = get_int_in_range_from_input "wprowadź x" 0 10
    let y = get_int_in_range_from_input "wprowadź y" 0 10

    printf "%A %A" x y

    0 // return an integer exit code
