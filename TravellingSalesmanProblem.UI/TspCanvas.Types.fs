module TravellingSalesmanProblem.UI.TspCanvas.Types

open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.Types
open TravellingSalesmanProblem
open TravellingSalesmanProblem.Types

type State = {
    Id: int
    X: float
    Y: float
    CityViews: IView<DockPanel> list
    LineViews: IView<Line> list
    Cities: City list
    Lookup: IDictionary<(City * City), float>
    XMin: float
    YMin: float
    XScale: float
    YScale: float
    Completed: bool
    Generation: int
    GenerationAcc: Genetic.Progress list
    Result:Genetic.CalculationResult option
    Updates: Queue<Genetic.Progress>
}

type Msg =
    | Start
    | Complete of Genetic.CalculationResult
    | Pulse
    | GeneticUpdate of Genetic.Progress
