module App

open System

open Fable.Core
open Fable.Core.JsInterop

open Browser.Dom
open Browser.Types

open Types.Algorithm

let algorithms = Static.algorithmDescriptions

let mutable activeTimers: float list = []

let mutable globalModelRef : Model option = None

let initialModel = {
    ArrayData = [||]
    AnimationQueue = []
    OperationCount = 0
    CurrentAnimation = None
    AnimationSpeed = 1 
    MaxValue = 100
    IsRunning = false
    IsPaused = false
    AnimationTimer = None
    History = []
    CurrentHistoryIndex = -1
}

let renderArray (arr: int array) (markIndices: int array) (container: HTMLElement) (maxValue: int) =
    container.innerHTML <- ""
    let barWidth = max 2 ((int container.clientWidth / arr.Length) - 2)
    
    arr |> Array.iteri (fun i value ->
        let bar = document.createElement("div")
        bar.className <- "bar"
        bar?style?width <- sprintf "%dpx" barWidth
        bar?style?height <- sprintf "%d%%" ((value * 100) / maxValue)
        
        if Array.contains i markIndices then
            bar?style?backgroundColor <- "#ff5722"
            
        container.appendChild(bar) |> ignore
    )

let updateAlgorithmInfo (algorithmId: string) =
    let algorithmDescription = document.getElementById("algorithmDescription")
    let averageTimeComplexity = document.getElementById("averageTimeComplexity")
    let worstTimeComplexity = document.getElementById("worstTimeComplexity")
    let spaceComplexity = document.getElementById("spaceComplexity")
    let algorithmName = document.getElementById("algorithmName")
    let algorithmType = document.getElementById("algorithmType")
    
    match Map.tryFind algorithmId algorithms with
    | Some algo ->
        algorithmDescription.innerHTML <- sprintf "<p>%s</p>" algo.Description
        averageTimeComplexity.innerHTML <- sprintf "%s" algo.TimeComplexity.Average
        worstTimeComplexity.innerHTML <- sprintf "%s" algo.TimeComplexity.Worst
        spaceComplexity.innerHTML <- algo.SpaceComplexity
        algorithmName.innerHTML <- sprintf "%s" algo.Name
        algorithmType.innerHTML <- sprintf "%s" algo.Type
    | None -> ()

let updateOperationCounter (count: int) =
    let counter = document.getElementById("operationCounter")
    if not (isNull counter) then
        counter.textContent <- string count

let updateHistoryControls (model: Model) =
    let prevStateBtn = document.getElementById("prevStateBtn") :?> HTMLButtonElement
    let nextStateBtn = document.getElementById("nextStateBtn") :?> HTMLButtonElement
    let historySlider = document.getElementById("historySlider") :?> HTMLInputElement
    let historyCounter = document.getElementById("historyCounter")
    
    if model.History.Length > 0 then
        historySlider.min <- "0"
        historySlider.max <- string (model.History.Length - 1)
        historySlider.value <- string model.CurrentHistoryIndex
        historySlider.disabled <- model.IsRunning && not model.IsPaused
        
        historyCounter.textContent <- sprintf "Step %d of %d" (model.CurrentHistoryIndex + 1) model.History.Length
    else
        historySlider.disabled <- true
        historyCounter.textContent <- "No history recorded"

    let isPrevStateBtnDisabled = model.CurrentHistoryIndex <= 0 || (model.IsRunning && not model.IsPaused)
    let isNextStateBtnDisabled = model.CurrentHistoryIndex >= model.History.Length - 1 || (model.IsRunning && not model.IsPaused)

    if isPrevStateBtnDisabled then prevStateBtn.classList.add("disabled") else prevStateBtn.classList.remove("disabled")
    if isNextStateBtnDisabled then nextStateBtn.classList.add("disabled") else nextStateBtn.classList.remove("disabled")
    
    prevStateBtn.disabled <- isPrevStateBtnDisabled
    nextStateBtn.disabled <- isNextStateBtnDisabled

let bubbleSort (arr: int array) =
    let animations = ResizeArray<Animation>()
    let mutable arr = Array.copy arr
    
    for i in 0..(arr.Length - 1) do
        for j in 0..(arr.Length - i - 2) do
            
            animations.Add(
                { Array = Array.copy arr
                  MarkIndices = [|j; j+1|]
                  CompareIndices = Some(j, j+1)
                  SwapIndices = None
                  PivotIndex = None
                  MergeIndices = None })
            
            if arr.[j] > arr.[j+1] then
                
                let temp = arr.[j]
                arr.[j] <- arr.[j+1]
                arr.[j+1] <- temp
                
                animations.Add(
                    { Array = Array.copy arr
                      MarkIndices = [|j; j+1|]
                      CompareIndices = None
                      SwapIndices = Some(j, j+1)
                      PivotIndex = None
                      MergeIndices = None })
    
    animations |> List.ofSeq

let selectionSort (arr: int array) =
    let animations = ResizeArray<Animation>()
    let mutable arr = Array.copy arr
    
    for i in 0..(arr.Length - 1) do
        let mutable minIndex = i
        
        for j in (i+1)..(arr.Length - 1) do
            
            animations.Add(
                { Array = Array.copy arr
                  MarkIndices = [|minIndex; j|]
                  CompareIndices = Some(minIndex, j)
                  SwapIndices = None
                  PivotIndex = None
                  MergeIndices = None })
            
            if arr.[j] < arr.[minIndex] then
                minIndex <- j
        
        if minIndex <> i then
            
            let temp = arr.[i]
            arr.[i] <- arr.[minIndex]
            arr.[minIndex] <- temp
            
            animations.Add(
                { Array = Array.copy arr
                  MarkIndices = [|i; minIndex|]
                  CompareIndices = None 
                  SwapIndices = Some(i, minIndex)
                  PivotIndex = None
                  MergeIndices = None })
    
    animations |> List.ofSeq

let insertionSort (arr: int array) =
    let animations = ResizeArray<Animation>()
    let mutable arr = Array.copy arr
    
    for i in 1..(arr.Length - 1) do
        let key = arr.[i]
        let mutable j = i - 1
        
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|i|]
              CompareIndices = None
              SwapIndices = None
              PivotIndex = None
              MergeIndices = None })
        
        while j >= 0 && arr.[j] > key do
            animations.Add(
                { Array = Array.copy arr
                  MarkIndices = [|j; j+1|]
                  CompareIndices = Some(j, j+1)
                  SwapIndices = None
                  PivotIndex = None
                  MergeIndices = None })
            
            arr.[j+1] <- arr.[j]
            j <- j - 1
            
            animations.Add(
                { Array = Array.copy arr
                  MarkIndices = [|j+1|]
                  CompareIndices = None
                  SwapIndices = None
                  PivotIndex = None
                  MergeIndices = None })
        
        arr.[j+1] <- key
        
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|j+1|]
              CompareIndices = None
              SwapIndices = None
              PivotIndex = None
              MergeIndices = None })
    
    animations |> List.ofSeq

let rec quickSortHelper (arr: int array) (low: int) (high: int) (animations: ResizeArray<Animation>) =
    if low < high then
        let pivotIndex = partitionArray arr low high animations
        quickSortHelper arr low (pivotIndex - 1) animations
        quickSortHelper arr (pivotIndex + 1) high animations

and partitionArray (arr: int array) (low: int) (high: int) (animations: ResizeArray<Animation>) =
    let pivot = arr.[high]
    let mutable i = low - 1
    
    animations.Add(
        { Array = Array.copy arr
          MarkIndices = [|high|]
          CompareIndices = None
          SwapIndices = None
          PivotIndex = Some(high)
          MergeIndices = None })
    
    for j in low..(high-1) do
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|j; high|]
              CompareIndices = Some(j, high)
              SwapIndices = None
              PivotIndex = Some(high)
              MergeIndices = None })
        
        if arr.[j] <= pivot then
            i <- i + 1
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
            
            if i <> j then
                animations.Add(
                    { Array = Array.copy arr
                      MarkIndices = [|i; j|]
                      CompareIndices = None
                      SwapIndices = Some(i, j)
                      PivotIndex = Some(high)
                      MergeIndices = None })
    
    let temp = arr.[i+1]
    arr.[i+1] <- arr.[high]
    arr.[high] <- temp
    
    animations.Add(
        { Array = Array.copy arr
          MarkIndices = [|i+1; high|]
          CompareIndices = None
          SwapIndices = Some(i+1, high)
          PivotIndex = None
          MergeIndices = None })
    
    i + 1

let quickSort (arr: int array) =
    let animations = ResizeArray<Animation>()
    let mutable arr = Array.copy arr
    
    quickSortHelper arr 0 (arr.Length - 1) animations
    
    animations |> List.ofSeq

let rec mergeSortHelper (arr: int array) (left: int) (right: int) (animations: ResizeArray<Animation>) =
    if left < right then
        let mid = (left + right) / 2
        
        
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [| for i in left..mid -> i |]
              CompareIndices = None
              SwapIndices = None
              PivotIndex = None
              MergeIndices = None })
        
        mergeSortHelper arr left mid animations
        mergeSortHelper arr (mid + 1) right animations
        mergeArrays arr left mid right animations

and mergeArrays (arr: int array) (left: int) (mid: int) (right: int) (animations: ResizeArray<Animation>) =
    let n1 = mid - left + 1
    let n2 = right - mid
    
    
    let L = Array.zeroCreate n1
    let R = Array.zeroCreate n2
    
    
    for i in 0..(n1-1) do
        L.[i] <- arr.[left + i]
    
    for j in 0..(n2-1) do
        R.[j] <- arr.[mid + 1 + j]
    
    
    let mutable i = 0
    let mutable j = 0
    let mutable k = left
    
    while i < n1 && j < n2 do
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|left + i; mid + 1 + j|]
              CompareIndices = Some(left + i, mid + 1 + j)
              SwapIndices = None
              PivotIndex = None
              MergeIndices = None })
        
        if L.[i] <= R.[j] then
            arr.[k] <- L.[i]
            i <- i + 1
        else
            arr.[k] <- R.[j]
            j <- j + 1
        
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|k|]
              CompareIndices = None
              SwapIndices = None
              PivotIndex = None
              MergeIndices = Some([|k|]) })
        
        k <- k + 1
    
    
    while i < n1 do
        arr.[k] <- L.[i]
        
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|k|]
              CompareIndices = None
              SwapIndices = None
              PivotIndex = None
              MergeIndices = Some([|k|]) })
        
        i <- i + 1
        k <- k + 1

    while j < n2 do
        arr.[k] <- R.[j]
        
        animations.Add(
            { Array = Array.copy arr
              MarkIndices = [|k|]
              CompareIndices = None
              SwapIndices = None
              PivotIndex = None
              MergeIndices = Some([|k|]) })

        j <- j + 1
        k <- k + 1

let mergeSort (arr: int array) =
    let animations = ResizeArray<Animation>()
    let mutable arr = Array.copy arr
    
    mergeSortHelper arr 0 (arr.Length - 1) animations
    
    animations |> List.ofSeq

let generateNewArray (size: int) (maxValue: int) =
    let random = Random()
    Array.init size (fun _ -> random.Next(1, maxValue + 1))

let rec processAnimations (model: Model) =
    globalModelRef <- Some model
    
    match model.AnimationTimer with
    | Some timerId -> 
        window.clearTimeout(timerId)
        
        activeTimers <- activeTimers |> List.filter (fun id -> id <> timerId)
    | None -> ()

    match model.AnimationQueue, model.IsPaused with
    | [], _ -> 
        let container = document.getElementById("visualizationContainer") :?> HTMLElement
        let finalModel = { 
            model with 
            IsRunning = false 
            IsPaused = false
            AnimationTimer = None
        }
        
        let updateUIFunc = document.getElementById("startBtn") :?> HTMLButtonElement
        updateUIFunc.textContent <- "Start"

        let algorithmSelect = document.getElementById("algorithmSelect") :?> HTMLSelectElement
        let generateBtn = document.getElementById("generateBtn") :?> HTMLButtonElement
        let speedSlider = document.getElementById("speedSlider") :?> HTMLInputElement
        let resetBtn = document.getElementById("resetBtn") :?> HTMLButtonElement
        let prevStateBtn = document.getElementById("prevStateBtn") :?> HTMLButtonElement
        let nextStateBtn = document.getElementById("nextStateBtn") :?> HTMLButtonElement
        
        algorithmSelect.disabled <- false
        generateBtn.disabled <- false
        speedSlider.disabled <- false
        prevStateBtn.disabled <- false
        nextStateBtn.disabled <- false
        resetBtn.textContent <- "Reset"

        prevStateBtn.classList.remove("disabled")
        nextStateBtn.classList.remove("disabled")
        generateBtn.classList.remove("disabled")
        
        updateHistoryControls finalModel
        
        finalModel
    | _, true ->
        model
        
    | animation :: rest, false ->
        let container = document.getElementById("visualizationContainer") :?> HTMLElement
        renderArray animation.Array animation.MarkIndices container model.MaxValue
        updateOperationCounter (model.OperationCount + 1)
        
        let historyState = {
            Array = animation.Array
            MarkIndices = animation.MarkIndices
            OperationCount = model.OperationCount + 1
            Animation = Some animation
        }
        
        let newHistory =
            if model.CurrentHistoryIndex < model.History.Length - 1 then
                model.History |> List.take (model.CurrentHistoryIndex + 1) |> List.append [historyState]
            else
                model.History @ [historyState]
        
        let updatedModel = { 
            model with 
            AnimationQueue = rest 
            OperationCount = model.OperationCount + 1 
            CurrentAnimation = Some animation
            History = newHistory
            CurrentHistoryIndex = newHistory.Length - 1
        }
        
        updateHistoryControls updatedModel

        let timerId = window.setTimeout((fun _ -> 
            match globalModelRef with
            | Some latestModel -> 
                let newModel = processAnimations { 
                    latestModel with 
                    AnimationQueue = rest
                    OperationCount = model.OperationCount + 1
                    CurrentAnimation = Some animation
                    AnimationTimer = None
                }
                
                globalModelRef <- Some newModel
            | None ->
                
                let newModel = processAnimations updatedModel
                globalModelRef <- Some newModel
        ), model.AnimationSpeed)
        
        activeTimers <- timerId :: activeTimers
        
        { updatedModel with AnimationTimer = Some timerId }

let restoreHistoryState (model: Model) (index: int) =
    if index >= 0 && index < model.History.Length then
        let state = model.History.[index]
        let container = document.getElementById("visualizationContainer") :?> HTMLElement
        
        renderArray state.Array state.MarkIndices container model.MaxValue
        updateOperationCounter state.OperationCount
        
        { model with 
            CurrentHistoryIndex = index
            CurrentAnimation = state.Animation }
    else
        model

let main() =
    let container = document.getElementById("visualizationContainer") :?> HTMLElement
    let algorithmSelect = document.getElementById("algorithmSelect") :?> HTMLSelectElement
    let startBtn = document.getElementById("startBtn") :?> HTMLButtonElement
    let resetBtn = document.getElementById("resetBtn") :?> HTMLButtonElement
    let generateBtn = document.getElementById("generateBtn") :?> HTMLButtonElement
    let speedSlider = document.getElementById("speedSlider") :?> HTMLInputElement
    let speedValue = document.getElementById("speedValue") :?> HTMLElement
    
    let prevStateBtn = document.getElementById("prevStateBtn") :?> HTMLButtonElement
    let nextStateBtn = document.getElementById("nextStateBtn") :?> HTMLButtonElement
    let historySlider = document.getElementById("historySlider") :?> HTMLInputElement
    
    let arraySize = 50
    let maxValue = 100
    let initialArray = generateNewArray arraySize maxValue
    let mutable model = { 
        initialModel with 
        ArrayData = initialArray 
        MaxValue = maxValue
    }
    
    globalModelRef <- Some model
    
    speedSlider.value <- string model.AnimationSpeed
    let initialSpeedLabel = "Extreme"
    speedValue.textContent <- sprintf "%s (%d ms)" initialSpeedLabel model.AnimationSpeed
    
    renderArray model.ArrayData [||] container model.MaxValue
    updateAlgorithmInfo algorithmSelect.value
    updateHistoryControls model
    
    let updateUIState (isRunning: bool) (isPaused: bool) =
        let state = isRunning && not isPaused

        algorithmSelect.disabled <- isRunning
        generateBtn.disabled <- isRunning
        speedSlider.disabled <- state
        
        historySlider.disabled <- state
        prevStateBtn.disabled <- state
        nextStateBtn.disabled <- state
        
        startBtn.textContent <- 
            if state then "Pause" 
            elif isPaused then "Resume"
            else "Start"

        generateBtn.classList.remove("disabled")
                    
        if isRunning then
            generateBtn.classList.add("disabled")
        
        resetBtn.textContent <- if isRunning then "Stop" else "Reset"
    
    updateUIState false false
    
    historySlider.oninput <- fun _ ->
        model <- 
            match globalModelRef with
            | Some latestModel -> latestModel
            | None -> model

        if not model.IsRunning || model.IsPaused then
            let index = int historySlider.value
            model <- restoreHistoryState model index
            globalModelRef <- Some model
            updateHistoryControls model
        false
    
    prevStateBtn.onclick <- fun _ ->
        model <- 
            match globalModelRef with
            | Some latestModel -> latestModel
            | None -> model

        if not model.IsRunning || model.IsPaused then
            if model.CurrentHistoryIndex > 0 then
                let newIndex = model.CurrentHistoryIndex - 1
                model <- restoreHistoryState model newIndex
                globalModelRef <- Some model
                updateHistoryControls model
        false
    
    nextStateBtn.onclick <- fun _ ->
        model <- 
                match globalModelRef with
                | Some latestModel -> latestModel
                | None -> model

        if not model.IsRunning || model.IsPaused then
            if model.CurrentHistoryIndex < model.History.Length - 1 then
                let newIndex = model.CurrentHistoryIndex + 1
                model <- restoreHistoryState model newIndex
                globalModelRef <- Some model
                updateHistoryControls model
        false
    
    generateBtn.onclick <- fun _ ->
        model <- 
            match globalModelRef with
            | Some latestModel -> latestModel
            | None -> model

        if not model.IsRunning then
            let newArray = generateNewArray arraySize maxValue
            model <- { 
                model with 
                ArrayData = newArray 
                AnimationQueue = [] 
                OperationCount = 0 
                IsRunning = false
                IsPaused = false
                History = []
                CurrentHistoryIndex = -1
            }
            
            globalModelRef <- Some model

            renderArray model.ArrayData [||] container model.MaxValue
            updateOperationCounter 0
            updateHistoryControls model
        false
    
    algorithmSelect.onchange <- fun _ ->
        if not model.IsRunning then
            updateAlgorithmInfo algorithmSelect.value
            updateOperationCounter 0
        false
    
    speedSlider.oninput <- fun _ ->
        let speed = int speedSlider.value
        model <- { model with AnimationSpeed = speed }
        
        match globalModelRef with
        | Some currentModel -> 
            globalModelRef <- Some { currentModel with AnimationSpeed = speed }
        | None -> 
            globalModelRef <- Some model
            
        let speedLabel = 
            match speed with
            | s when s <= 10 -> "Extreme"
            | s when s <= 30 -> "Fast"
            | s when s <= 70 -> "Medium"
            | s when s <= 120 -> "Slow"
            | _ -> "Lame"

        speedValue.textContent <- sprintf "%s (%d ms)" speedLabel speed
        false

    resetBtn.onclick <- fun _ ->
        let prevStateBtn = document.getElementById("prevStateBtn") :?> HTMLButtonElement
        let nextStateBtn = document.getElementById("nextStateBtn") :?> HTMLButtonElement

        if model.IsRunning then
            for timerId in activeTimers do
                window.clearTimeout(timerId)
        
            activeTimers <- []
            
            model <- { 
                model with 
                AnimationQueue = [] 
                OperationCount = 0 
                IsRunning = false
                IsPaused = false
                AnimationTimer = None
                History = []
                CurrentHistoryIndex = -1
            }
                        
            globalModelRef <- Some model
            
            updateUIState false false
            
            renderArray model.ArrayData [||] container model.MaxValue
            updateOperationCounter 0
            updateHistoryControls model
        else
            model <- { 
                model with 
                History = []
                CurrentHistoryIndex = -1
            }
            
            globalModelRef <- Some model
            
            renderArray model.ArrayData [||] container model.MaxValue
            updateOperationCounter 0
            updateHistoryControls model
        false

        prevStateBtn.classList.add("disabled")
        nextStateBtn.classList.add("disabled")

    startBtn.onclick <- fun _ ->    
        model <- 
            match globalModelRef with
            | Some latestModel -> latestModel
            | None -> model
        
        if not model.IsRunning then
            let animations = 
                match algorithmSelect.value with
                | "1" -> bubbleSort model.ArrayData
                | "2" -> selectionSort model.ArrayData
                | "3" -> insertionSort model.ArrayData
                | "4" -> quickSort model.ArrayData
                | "5" -> mergeSort model.ArrayData
                | _ -> []
            
            updateUIState true false
            
            let initialState = {
                Array = model.ArrayData
                MarkIndices = [||]
                OperationCount = 0
                Animation = None
            }
            
            model <- { model with 
                        AnimationQueue = animations 
                        OperationCount = 0 
                        IsRunning = true
                        IsPaused = false
                        History = [initialState]
                        CurrentHistoryIndex = 0 }
            
            globalModelRef <- Some model
            
            model <- processAnimations model
        elif model.IsPaused then
            updateUIState true false
                
            model <- { model with IsPaused = false }
            
            globalModelRef <- Some model
            
            model <- processAnimations model
        else
            updateUIState true true
            
            for timerId in activeTimers do
                window.clearTimeout(timerId)
            
            activeTimers <- []
            model <- { 
                model with 
                IsPaused = true
                AnimationTimer = None
            }

            globalModelRef <- Some model
            updateHistoryControls model
        false

document.addEventListener("DOMContentLoaded", (fun _ -> main()), false)
