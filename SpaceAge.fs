module SpaceAge
// TODO: define the Planet type
type Planet = 
| Mercury
| Venus
| Earth
| Mars
| Jupiter
| Saturn
| Uranus
| Neptune
let age (planet: Planet) (seconds: int64): float = 
    let yearsSeconds = float 31557600
    let calculate data = float seconds / (yearsSeconds * data)
    match planet with 
        | Mercury -> calculate 0.2408467 
        | Venus -> calculate 0.61519726
        | Earth -> calculate 1.00
        | Mars -> calculate 1.8808158
        | Jupiter -> calculate 11.862615
        | Saturn -> calculate 29.447498
        | Uranus -> calculate 84.016846
        | Neptune -> calculate 164.79132