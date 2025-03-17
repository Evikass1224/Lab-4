//Дерево содержит целые числа. Получить дерево из вещественных значений, 
//разделив каждый элемент исходного на заданное число. 

type Tree1 = {
    D: float
    Left: Tree1 option
    Right: Tree1 option
}

//добавление элемента в дерево
let rec Add1 value tree : Tree1 option =
    match tree with
    | Some n when value < n.D ->
        Some {n with Left = Add1 value n.Left}
    | Some n when value > n.D ->
        Some {n with Right = Add1 value n.Right}
    | Some n -> Some n
    | None ->  Some {
        D = value
        Left = None
        Right = None
    }

//вывод дерева с помощью отступа
let rec Print1 tree indent =
    match tree with
    | None -> ()
    | Some n ->
        let format = n.D.ToString()
        printfn "%s%s" indent format
        Print1 n.Left (indent + "_ ")
        Print1 n.Right (indent + "_ ")

//создание дерева из строки чисел
let Create (input: string) =
    input.Split(' ')
    |> Seq.fold (fun (tree, F) str ->
        match System.Int32.TryParse(str) with
        | (true, value) when value <= System.Int32.MaxValue -> 
            (Add1 (float value) tree, F)                        //вставка элемента в дерево
        | (true, _) -> 
            printfn "Число %s превышает максимально допустимое значение. Побробуйте снова." str
            (tree, false)
        | _ -> 
            printfn "Некорректный ввод числа: %s Побробуйте снова." str
            (tree, false)
    ) (None, true)                                                //начальное состояние - пустое дерево

//случайное заполнение дерева
let RandomTree1 count =
    let r = System.Random()
    let rec Randomadd tree n =
        if n > 0 then
            let value = float (r.Next(-100000, 100000))  //случ. число от -100000 до 99999
            let plusTree = Add1 value tree
            Randomadd plusTree (n - 1)
        else
            tree
    Randomadd None count

//имитирование map для преобраз. целых чисел в вещест-ые с помощью деления
let rec Treemap1 tree index =
    let floatvalue value index : float =
        float value / index

    match tree with
    | None -> None
    | Some n ->
        Some {
            D = floatvalue n.D index
            Left = Treemap1 n.Left index
            Right = Treemap1 n.Right index
        }

//начало
let rec input1 () =
    printfn ""
    printfn "Выберите способ ввода и нажмите Enter:"
    printfn "1) Ввод чисел с клавиатуры"
    printfn "2) Генерация случайных чисел"

    let choice = System.Console.ReadLine()
    match choice with
    | "1" ->
        printfn "Введите целые числа через пробел и нажмите Enter: "
        let input = System.Console.ReadLine() 
        let (tree, F) = Create input
        if not F then
            printfn ""
            input1 ()
        else
            printfn "Дерево: "
            Print1 tree ""
            printfn ""
            IndexInput tree
    | "2" ->
        printfn "Введите количество элементов в дереве: "
        let countInput = System.Console.ReadLine()
        match System.Int32.TryParse(countInput) with
        | (true, count) when count > 0 ->
            let tree = RandomTree1 count
            printfn "Дерево: "
            Print1 tree ""
            printfn ""
            IndexInput tree
        | _ ->
            printfn "Введите корректное положительное целое число."
            input1 ()
    | _ ->
        printfn "Некорректный выбор. Попробуйте еще раз."
        input1 () 

//ввод делителя
and IndexInput tree =
    printfn "Введите делитель элементов дерева:"
    let indexinput = System.Console.ReadLine()

    match System.Int32.TryParse(indexinput) with
    | (true, index) -> 
        let treerezult = Treemap1 tree index
        printfn "Дерево: "
        Print1 treerezult ""
    | _ -> 
        printfn "Длеитель должен быть целым числом."
        IndexInput tree

//--------------------------------------------------------------------        

//Дерево содержит строки. Найти суммарную длину этих строк.

type Tree2 = {
    D: string
    Left: Tree2 option
    Right: Tree2 option
}

//добавление элемента в дерево
let rec Add2 value tree : Tree2 option =
    match tree with
    | Some n when value < n.D ->
        Some {n with Left = Add2 value n.Left}
    | Some n when value > n.D ->
        Some {n with Right = Add2 value n.Right}
    | Some n -> Some n
    | None -> Some {
        D = value
        Left = None
        Right = None
    }

//создание дерева из строки
let Create2 (input: string) =
    if System.String.IsNullOrWhiteSpace(input) then
        None
    else
        input.Split(' ')
        |> Seq.fold (fun tree str -> Add2 str tree) None

//имитирование fold для подсчёта суммы длин строк
let rec Treefoldsum tree =
    match tree with
    | None -> 0
    | Some n ->
        n.D.Length + Treefoldsum n.Left + Treefoldsum n.Right   //сумма длин строк

//вывод дерева с помощью отступа
let rec Print2 tree indent =
    match tree with
    | None -> ()
    | Some n ->
        printfn "%s%s" indent n.D
        Print2 n.Left (indent + "_ ")
        Print2 n.Right (indent + "_ ")

//генерирование списка строк разной длины
let Randomstring n minv maxv : string list =
    let r = System.Random()
    let chars = ['A'..'Z'] @ ['a'..'z'] @ ['0'..'9'] @ ['!'..'/']
    List.init n (fun _ ->
        let length = r.Next(minv, maxv + 1)
        String.init length (fun _ -> string chars.[r.Next(chars.Length)]))

//создание дерева из строк в списке
let RandomTree2 n minv maxv =
    let randomStrings = Randomstring n minv maxv
    randomStrings
    |> List.fold (fun tree value -> Add2 value tree) None

//начало
let rec input2 () =
    printfn ""
    printfn "Выберите способ ввода и нажмите Enter:"
    printfn "1) Ввод чисел с клавиатуры"
    printfn "2) Генерация случайных чисел"

    let choice = System.Console.ReadLine()
    match choice with
    | "1" ->
        printfn "Введите строки через пробел и нажмите Enter: "
        let input = System.Console.ReadLine()

        match Create2 input with
        | Some tree -> 
            printfn "Дерево:"
            Print2 (Some tree) ""
            let sumstring = Treefoldsum (Some tree)
            printfn "Сумма длин всех строк в дереве: %d" sumstring
            printfn ""
        | None -> 
            printfn "Ошибка! Пустая строка. Пожалуйста, попробуйте еще раз."
            input2 ()

    | "2" -> 
        printfn "Введите количество строк: "
        let n = System.Console.ReadLine()

        match System.Int32.TryParse(n) with
        | (true, n) when n > 0 -> 
            let minv = 2
            let maxv = 15
            let tree = RandomTree2 n minv maxv
            printfn "Дерево:"
            Print2 tree ""
            let sumstring = Treefoldsum tree
            printfn ""
            printfn "Сумма длин всех строк в дереве: %d" sumstring
        | _ -> 
            printfn "Ошибка! Введено некорректное количество строк. Пожалуйста, попробуйте снова."
            input2 ()

    | _ -> 
        printfn "Некорректный выбор. Попробуйте еще раз."
        input2 ()



//--------------------------------------------------------------------

//старт
let rec Start () =
    printfn ""
    printfn "Выберите задание и нажмите Enter: "
    printfn "1 задание"
    printfn "2 задание"
    printfn "Для выхода введите 'q' и Enter"

    let number = System.Console.ReadLine()

    match number with
    | "1" -> 
        printfn ""
        printfn "----:: Дерево целых чисел -> дерево вещественных чисел ::----"
        input1()
        Start() 
    | "2" -> 
        printfn ""
        printfn "----:: Сумарная длина строк в дереве ::----"
        input2()
        Start() 
    | "q" -> 
        printfn "Выход из программы." 
    | _ -> 
        printfn "Ошибка! Входные данные не являются натуральным числом."  
        printfn ""  
        Start() 

Start() 