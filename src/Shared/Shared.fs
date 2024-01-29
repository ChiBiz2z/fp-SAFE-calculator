namespace Shared

open System

type MathExpression = { Value: string; Result: int }

module MathExpression =
    let isValid (exp: string) =
        exp |> Seq.forall (fun c -> Char.IsLetter(c) || c = ' ')

type ExprTree =
    | Op of Operation: char * Left: ExprTree * Right: ExprTree
    | Value of int

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IExpressionCalculatorApi = {
    getResult: string -> Async<int>
    getExpressionTree: string -> Async<ExprTree>
}
