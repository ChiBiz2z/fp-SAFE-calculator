module Server

open System.Text.RegularExpressions
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open System

open Shared

//todo remove to another module
module ExpressionTree =
    /// check if symbol is math operator
    /// </summary>
    /// <param name="c"></param>
    let isOperator c =
        c = "+" || c = "-" || c = "*" || c = "/" || c = "%"

    /// <summary>
    /// Get priority of the operation
    /// </summary>
    /// <param name="op">operation symbol. Error if can't find operation</param>
    let getPriority op =
        match op with
        | "(" -> 1
        | "+"
        | "-" -> 2
        | "*"
        | "/"
        | "%" -> 3
        | _ -> failwith "Can't find operation"

    /// <summary>
    /// Parse infix expression to ExprTree type in functional style
    /// Algorithm - https://leetcode.ca/2020-04-14-1597-Build-Binary-Expression-Tree-From-Infix-Expression/
    /// </summary>
    /// <param name="expr">char list of symbols</param>
    /// <param name="ops">operation stack</param>
    /// <param name="stack">values stack</param>
    let rec parse expr ops stack =
        match expr with
        | [] ->
            let _, stack = buildWhile (fun _ -> true) ops stack
            List.head stack

        // if element is '(' then we put bracket in operation stack and start recursion from rest part of list
        | "(" :: rest -> parse rest ("(" :: ops) stack

        // if element is ')' then build tree while char won't be '('
        | ch :: rest when isOperator ch ->
            let ops, stack = buildWhile (fun op -> getPriority op >= getPriority ch) ops stack
            parse rest (ch :: ops) stack

        // if element is operator build tree while stack priority is higher than current
        | ch :: rest when isOperator ch ->
            let ops, stack = buildWhile (fun op -> getPriority op >= getPriority ch) ops stack
            parse rest (ch :: ops) stack

        // recursion with new value element in stack
        | ch :: rest -> parse rest ops (Value(int ch) :: stack) // push

    /// <summary>
    /// function to build tree roots from values in the stack while predicate is true
    /// </summary>
    /// <param name="pred">predicate (func that return bool)</param>
    /// <param name="ops">operation stack</param>
    /// <param name="stack">values stack</param>
    and private buildWhile pred ops stack =
        match ops with
        | op :: rest when pred op ->
            let right = stack |> List.head // get top
            let left = List.head (List.tail stack) //get pred top
            let stack = Op(op, left, right) :: List.tail (List.tail stack) // make list without top 2 elements
            buildWhile pred rest stack
        | _ -> ops, stack

    /// <summary>
    /// Calculate result of expression tree
    /// </summary>
    let rec compute =
        function
        | Value(x) -> x
        | Op(op, L, R) ->
            let l = compute L
            let r = compute R

            match op with
            | "+" -> l + r
            | "-" -> l - r
            | "*" -> l * r
            | "/" -> l / r
            | "%" -> l % r
            | _ -> failwith "Неизвестная операция"

let calculatorApi = {
    getResult =
        fun exp -> async {
            let l =
                List.ofArray (Regex.Split(exp, "(\\d+|\\D)") |> Array.filter (fun x -> x <> ""))

            let tree = ExpressionTree.parse l [] []

            return {
                Value = exp
                Result = ExpressionTree.compute tree
            }
        }

    getExpressionTree =
        fun exp -> async {
            let l =
                List.ofArray (Regex.Split(exp, "(\\d+|\\D)") |> Array.filter (fun x -> x <> ""))

            let tree = ExpressionTree.parse l [] []

            return tree
        }
}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue calculatorApi
    |> Remoting.buildHttpHandler

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0
