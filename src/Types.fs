module Types

module Algorithm = 
    type Model = {
        ArrayData: int array
        AnimationQueue: Animation list
        OperationCount: int
        CurrentAnimation: Animation option
        AnimationSpeed: int
        MaxValue: int
        IsRunning: bool
        IsPaused: bool
        AnimationTimer: float option
        History: HistoryState list
        CurrentHistoryIndex: int
    }

    and Animation = {
        Array: int array
        MarkIndices: int array
        CompareIndices: (int * int) option
        SwapIndices: (int * int) option
        PivotIndex: int option
        MergeIndices: int array option
    }

    and HistoryState = {
        Array: int array
        MarkIndices: int array
        OperationCount: int
        Animation: Animation option
    }

    type TimeComplexity = {
        Average: string
        Worst: string
    }

    type AlgorithmDescription = {
        Name: string
        Description: string
        TimeComplexity: TimeComplexity
        SpaceComplexity: string
        Type: string
    }