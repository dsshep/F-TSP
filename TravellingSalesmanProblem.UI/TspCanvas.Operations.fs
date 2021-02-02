namespace TravellingSalesmanProblem.UI.TspCanvas

open System
open System.Collections.Generic
open Avalonia.Threading
open Elmish
open TravellingSalesmanProblem
open TravellingSalesmanProblem.UI.TspCanvas.Types

[<RequireQualifiedAccess>]
module Operations =
    let performCalculation  cities lookup =
        let queue = Queue<_>()
        
        let sub() =
            async {
                let greedySortedCities = Greedy.calculate lookup cities |> snd
                let result =
                    Genetic.calculate
                        5
                        queue.Enqueue
                        (ProblemFunctions.calculateRouteDistance lookup)
                        lookup
                        greedySortedCities
                        
                return result
            }
         
        Cmd.OfAsync.perform sub () Msg.Complete, queue

    let pulse =
        let sub dispatch =
            DispatcherTimer.RunOnce(
                (fun () -> Msg.Pulse |> dispatch),
                TimeSpan.FromMilliseconds 100.0)
            |> ignore
        
        Cmd.ofSub sub
