namespace Shared

open System

type GetExpressionResultResponse = { Value: string; Result: int }

module GetExpressionResultResponse =
    let isValid (exp: string) =
        exp |> Seq.forall (fun c -> Char.IsLetter(c) || c = ' ')

type ExprTree =
    | Op of Operation: string * Left: ExprTree * Right: ExprTree
    | Value of int

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IExpressionCalculatorApi = {
    getResult: string -> Async<GetExpressionResultResponse>
    getExpressionTree: string -> Async<ExprTree>
}
