module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    History: MathExpression list
    Input: string
}

type Msg =
    | SetInput of string
    | CalculateExpression
    | ExpRes of MathExpression

let calculatorApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IExpressionCalculatorApi>

let init () =
    let model = { History = []; Input = "" }
    let cmd = Cmd.none
    model, cmd

let update msg model =
    match msg with
    | SetInput value -> { model with Input = value.Trim() }, Cmd.none
    | CalculateExpression ->

        let cmd = Cmd.OfAsync.perform calculatorApi.getResult model.Input ExpRes

        { model with Input = "" }, cmd
    | ExpRes exp ->
        {
            model with
                History = model.History @ [ exp ]
        },
        Cmd.none

open Feliz

let private todoAction model dispatch =
    Html.div [
        prop.className "flex flex-col sm:flex-row mt-4 gap-4"
        prop.children [
            Html.input [
                prop.className
                    "shadow appearance-none border rounded w-full py-2 px-3 outline-none focus:ring-2 ring-teal-300 text-grey-darker"
                prop.value model.Input
                prop.placeholder "2+2*2="
                prop.autoFocus true
                prop.onChange (SetInput >> dispatch)
                prop.onKeyPress (fun ev ->
                    if ev.key = "Enter" then
                        dispatch CalculateExpression)
            ]
            Html.button [
                prop.className
                    "flex-no-shrink p-2 px-12 rounded bg-teal-600 outline-none focus:ring-2 ring-teal-300 font-bold text-white hover:bg-teal disabled:opacity-30 disabled:cursor-not-allowed"
                prop.disabled (MathExpression.isValid model.Input |> not)
                prop.onClick (fun _ -> dispatch CalculateExpression)
                prop.text "Calculate"
            ]

            Html.p [
                prop.text "Введенное математическое выражение не может быть распознано"
                prop.hidden (MathExpression.isValid model.Input |> not)
            ]
        ]
    ]

let private todoList model dispatch =
    Html.div [
        prop.className "bg-white/80 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
        prop.children [
            Html.ol [
                prop.className "list-decimal ml-6"
                prop.children [
                    for exp in model.History do
                        Html.li [ prop.className "my-1"; prop.text exp.Value ]
                ]
            ]

            todoAction model dispatch
        ]
    ]

let view model dispatch =
    Html.section [
        prop.className "h-screen w-screen"
        prop.style [ style.backgroundColor "gray" ]

        prop.children [
            Html.div [
                prop.className "flex flex-col items-center justify-center h-full"
                prop.children [
                    Html.h1 [
                        prop.className "text-center text-5xl font-bold text-white mb-3 rounded-md p-4"
                        prop.text "F# Calculator"
                    ]
                    todoList model dispatch
                ]
            ]
        ]
    ]
