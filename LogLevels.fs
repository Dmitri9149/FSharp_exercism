module LogLevels

let message (logLine: string): string = 
    match logLine[1] with 
    |'I' -> logLine[8..].Trim()
    |'W' -> logLine[11..].Trim()
    |'E' -> logLine[9..].Trim()
    |_ -> "Something strange !! But it is passed the test !"
    

let logLevel(logLine: string): string = 
    match logLine[1] with 
    |'I' -> logLine[1..4].ToLower()
    |'W' -> logLine[1..7].ToLower()
    |'E' -> logLine[1..5].ToLower()
    |_ -> "Something strange !! But it is passed the test !"

let reformat(logLine: string): string = 
    message logLine + " (" + logLevel logLine + ")"