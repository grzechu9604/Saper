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

let generate_board x y mines_amount =
    let rec assign_mines mines_amount board =
        if (mines_amount = 0) then 
            board
        else 
            let rand_x = random_number_generator 0 (Array2D.length1 board - 1)
            let rand_y = random_number_generator 0 (Array2D.length2 board - 1)
    
            if (board.[rand_x, rand_y] = 1) then
                assign_mines mines_amount board
            else
                board.SetValue(1, rand_x, rand_y)
                assign_mines (mines_amount - 1) board
     in
        let _board : int[,] = Array2D.zeroCreate x y
        assign_mines mines_amount _board


let print_table table =
    for r = 0 to Array2D.length1 table - 1 do
        for c = 0 to Array2D.length2 table - 1 do
            printf "%A " table.[r, c]
        printfn ""

[<EntryPoint>]
let main argv =
    let game_variant = get_int_in_range_from_input "Wybierz wariant gry od 1 do 3" 1 3
    let board = generate_board 8 8 16

    print_table board

    0 // return an integer exit code
