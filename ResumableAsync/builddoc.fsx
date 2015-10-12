#load @"../packages/FSharp.Formatting.2.11.0/FSharp.Formatting.fsx"
open FSharp.Literate
open System.IO

let source = __SOURCE_DIRECTORY__
let template = Path.Combine(source, @"..\packages\FSharp.Formatting.2.11.0\literate\templates\template-project.html")


//let script = Path.Combine(source, "MonadicReplayPairHistory.fs")
//Literate.ProcessScriptFile(script, template)

//let doc = Path.Combine(source, "../docs/document.md")
//Literate.ProcessMarkdown(doc, template)

let doc() =
    // Load the template & specify project information
    let projTemplate = template //source + "template-project.html"
    let projInfo =
      [ "page-description", "Resumable monad with F#"
        "page-author", "William Blum"
        "project-name", "Resumable Monad" ]

    // Process all files and save results to 'output' directory
    Literate.ProcessDirectory
      (source, projTemplate, source + "\\..\\output", replacements = projInfo)


doc()