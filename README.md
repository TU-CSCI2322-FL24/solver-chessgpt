# ChessGPT

Group Members: Morgan Powers, Shaan Desai, Owen Story, Brandon Raimondo, Robert Arsiaga

Our chosen way to represent game state is as a tuple where the first element is a list of pieces belonging to the white team, and the second element is a list of pieces belonging to the black team.
```haskell
type Game = ([Piece], [Piece])
```
An alternate way to represent it could be as a nested list of Maybe pieces to represent the whole chess grid (using Maybe to distinguish between spaces with a piece and spaces without), but we thought this could be inefficient as we don't really need to know about the whole grid including empty spaces, we just need to know about currently active pieces.
```haskell
type Game = [[Maybe Piece]]
```
