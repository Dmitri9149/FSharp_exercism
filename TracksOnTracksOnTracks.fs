module TracksOnTracksOnTracks
let newList: string list = []
let existingList: string list = ["F#"; "Clojure"; "Haskell"]
let addLanguage (language: string) (languages: string list): string list =
    language::languages
let countLanguages (languages: string list): int = languages.Length
let reverseList(languages: string list): string list = 
    let rec helper lst acc = match lst with 
                                | [] -> acc
                                | z::xs -> helper xs (z::acc)
    helper languages []
                            
let excitingList (languages: string list): bool = 
    match languages with 
        |x::xs when (x = "F#") -> true
        |x::y::ys when (y = "F#") && (ys = [] || ys.Length = 1) -> true
        | _ -> false 