namespace TravellingSalesmanProblem.UI.TspCanvas

open System.Collections.Generic
open System.Diagnostics
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Controls.Shapes
open Avalonia.Media
open Elmish
open TravellingSalesmanProblem.Types
open TravellingSalesmanProblem.UI.TspCanvas
open TravellingSalesmanProblem.UI.TspCanvas.Types

module Canvas =    
    let ellipseWidthHeight = 10.0
    let sw = Stopwatch()
    
    let start =
        sw.Start()
        
        let sub dispatch =
            dispatch Start
            
        Cmd.batch [ Cmd.ofSub sub; Operations.pulse ]
    
    let calcRelativePoint state (loc: Point) offset =
        let x = ((loc.X - state.XMin) / state.XScale) + offset
        let y = ((loc.Y - state.YMin) / state.YScale) + offset
        (x, y)
    
    let calculateLines state (cities: City list) (color: ISolidColorBrush) =
        cities
        |> List.pairwise
        |> List.indexed
        |> List.map (fun (i, (c1, c2)) ->
            let x1, y1 = calcRelativePoint state c1.Location ellipseWidthHeight
            let x2, y2 = calcRelativePoint state c2.Location ellipseWidthHeight
            Line.create [
                Line.startPoint (Point(x1, y1))
                Line.endPoint (Point(x2, y2))
                Line.strokeThickness 1.0
                Line.stroke
                    (if i = 0 then
                        Brushes.Green
                    elif i = cities.Length - 2 then
                        Brushes.Blue
                    else color)
            ])
    
    let init x y cities lookup =
        let halfEllipseWidthHeight = ellipseWidthHeight / 2.0
        
        let xMax = cities |> List.map (fun c -> c.Location.X) |> List.max
        let yMax = cities |> List.map (fun c -> c.Location.Y) |> List.max
        let xMin = cities |> List.map (fun c -> c.Location.X) |> List.min
        let yMin = cities |> List.map (fun c -> c.Location.Y) |> List.min
        let deltaX = (xMax - xMin) |> abs
        let deltaY = (yMax - yMin) |> abs
        
        let xScale = deltaX / x
        let yScale = deltaY / y
        
        let initialState = {
            Id = 1
            X = x
            Y = y
            Cities = cities
            CityViews = []
            LineViews = []
            Lookup = lookup
            XMin = xMin
            YMin = yMin
            XScale = xScale
            YScale = yScale
            Completed = false
            Generation = 0
            GenerationAcc = []
            Result = None
            Updates = Queue()
        }
        
        let cityViews =
            cities
            |> List.map (fun c ->
                let x', y' = calcRelativePoint initialState c.Location halfEllipseWidthHeight
                
                DockPanel.create [
                    DockPanel.left x'
                    DockPanel.top y'
                    DockPanel.children [
                        Ellipse.create [
                            Ellipse.width ellipseWidthHeight
                            Ellipse.height ellipseWidthHeight
                            Ellipse.left x'
                            Ellipse.top y'
                            Ellipse.fill Brushes.Blue ]
                        TextBlock.create [
                            TextBlock.text c.Name
                            TextBlock.zIndex 10
                            TextBlock.left x'
                            TextBlock.top y' 
                        ] ]
                    ])
        
        let lineViews = calculateLines initialState cities Brushes.Red
        
        { initialState with
            LineViews = lineViews
            CityViews = cityViews }, start
    
    let update (msg: Msg) (state: State) =
        match msg with
        | Start ->
            let asyncWork =
                Operations.performCalculation state.Cities state.Lookup
                
            let cmd, queue = asyncWork
            
            { state with Updates = queue }, cmd
            
        | Pulse ->
            match state.Updates.TryDequeue() with
            | true, update ->
                let cities = update.Best
                let newLines = calculateLines state cities Brushes.Orange
                { state with
                    LineViews = newLines
                    Cities = update.Best
                    GenerationAcc = state.GenerationAcc @ [ update ]
                    Generation = state.Generation + 1 }, Operations.pulse
                
            | false, _ when state.Completed ->
                printfn "%A" state.Cities
                state, Cmd.none
                
            | false, _ ->
                state, Operations.pulse
            
        | GeneticUpdate update ->
            state.Updates.Enqueue update
            state, Cmd.none
            
        | Complete g ->
            sw.Stop()
            { state with
                Completed = true
                Result = Some g }, Cmd.none
        
    let view (state: State) (_) =
        let message =
            match state.Result with
            | Some g ->
                sprintf "Generation: %i Best: %f. In %A. Genetic Score: %i."
                    state.Generation (fst g.Record) sw.Elapsed g.DiversityPercentage 
            | None -> sprintf "Generation: %i" state.Generation

        DockPanel.create [
            DockPanel.background Brushes.Black
            DockPanel.children [
                Canvas.create [
                    Canvas.background Brushes.Black
                    Canvas.width state.X
                    Canvas.height state.Y
                    Canvas.zIndex 2
                    Canvas.children [
                        for l in state.LineViews do
                            l
                        for c in state.CityViews do
                            c    
                        TextBlock.create [
                            TextBlock.text message
                            TextBlock.zIndex 1
                        ]
                    ]
                ]
            ]
        ]
        