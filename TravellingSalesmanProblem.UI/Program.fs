namespace TravellingSalesmanProblem.UI

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts
open TravellingSalesmanProblem
open TravellingSalesmanProblem.Types
open TravellingSalesmanProblem.UI.TspCanvas

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "Travelling Salesman Problem"
        base.Width <- 800.0
        base.Height <- 800.0
        
        let cities =
            System.IO.File.ReadAllLines("Cities.csv")
            |> Array.map (fun c ->
                let segments = c.Split(',')
                { Name = segments.[0]
                  Location = { X = segments.[1] |> float; Y = segments.[2] |> float } })
        
        let lookup = Lookup.buildLookup cities
        
        try 
            Elmish.Program.mkProgram
                (fun () -> Canvas.init 720.0 720.0 cities lookup)
                Canvas.update
                Canvas.view
            //|> Program.withConsoleTrace
            |> Program.withHost this
            |> Program.run
        with e ->
            ()
        
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)