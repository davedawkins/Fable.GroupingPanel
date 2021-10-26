module Main

open Fable.Core.JsInterop
open Browser.Dom
open Sutil
open Sutil.Html

//importAll "../styles/main.scss"
//importAll "../styles/bootstrap.min.css"

// App
App.app() |> Sutil.Program.mountElement "feliz-app"
//ReactDOM.render(App.app, document.getElementById "feliz-app")