namespace Shared

open System

type GetExpressionResultResponse = { Value: string; Result: int }

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
