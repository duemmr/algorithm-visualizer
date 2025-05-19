module Static

open Types
open Types.Algorithm

let algorithmDescriptions = Map.ofList [
        "1", { 
            Name = "Bubble sort"
            Type = "Repeated comparison and swap"
            Description = "Bubble Sort repeatedly steps through the list, compares adjacent elements and swaps them if they are in the wrong order."
            SpaceComplexity = "O(1)"
            TimeComplexity = {
                Average = "O(n²)"
                Worst = "O(n²)"
            }
        }
        "2", {       
            Name = "Selection sort"
            Type = "Minimum selection"
            Description = "Selection Sort divides the input list into a sorted and an unsorted region, and iteratively shrinks the unsorted region by extracting the smallest element."
            SpaceComplexity = "O(1)" 
            TimeComplexity = {
                Average = "O(n²)"
                Worst = "O(n²)"
            }
        }
        "3", { 
            Name = "Insertion sort"
            Type = "Incremental insertion"
            Description = "Insertion Sort builds the final sorted array one item at a time. It's much less efficient on large lists than quicksort or mergesort."
            SpaceComplexity = "O(1)" 
            TimeComplexity = {
                Average = "O(n²)"
                Worst = "O(n²)"
            }
        }
        "4", { 
            Name = "Quick sort"
            Type = "Divide-and-Conquer"
            Description = "Quick Sort is a divide-and-conquer algorithm that picks an element as a pivot and partitions the array around the pivot."
            SpaceComplexity = "O(log n)" 
            TimeComplexity = {
                Average = "O(n log n)"
                Worst = "O(n²)"
            }
        }
        "5", { 
            Name = "Merge sort"
            Type = "Divide-and-Conquer with merge"
            Description = "Merge Sort is a divide-and-conquer algorithm that divides the input array into two halves, recursively sorts them, and then merges the sorted halves."
            SpaceComplexity = "O(n)" 
            TimeComplexity = {
                Average = "O(n log n)"
                Worst = "O(n log n)"
            }
        }
    ]