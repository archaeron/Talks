
type Action = MoveMouse (Int, Int) | Click (Int, Int)

type alias State =
	{ player : (Int, Int
	, shots : List (Int, Int)
	}

inputs : Signal Action
inputs =
	Singnal.mergeMany
		[ Signal.map MouseMove Mouse.position
		, Singal.map Click Mouse.clicks
		]

update : Action -> State -> State
update action state =
	case action of
		MouseMove pos ->
			{ state
			| player <- pos
			}
		Click pos ->
			{ state
			| shots <- pos :: state.shots
			}

state : Signal State
state = Signal.foldp update defaultState inputs

--- VIEW
viewPlayer : Player -> Form

viewShot : (Int, Int) -> Form

viewShots : List (Int, Int) -> Form

view : State -> Element
view state =
	Graphics.Collage.collage 1200 1200
		viewPlayer state.player :: viewShots state.shots

-- MAIN

main : Signal Element
main = Signal.map view state
