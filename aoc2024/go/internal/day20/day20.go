package day20

import (
	"fmt"
	"strings"

	"github.com/alamillac/aoc/internal/input"
)

const START = 'S'
const END = 'E'
const WALL = '#'
const TRACK = '.'

func readInput() string {
	return input.GetInput(20)
	// return input.GetTestInput(20)
}

func isNewLine(c rune) bool {
	return c == '\n'
}

type RaceMap struct {
	rMap   [][]rune
	width  int
	height int
}

func (rMap *RaceMap) load(mapStr string) {
	rows := strings.FieldsFunc(mapStr, isNewLine)
	rMap.height = len(rows)
	rMap.rMap = make([][]rune, rMap.height)
	for j, rowStr := range rows {
		rMap.width = len(rowStr)
		row := make([]rune, rMap.width)
		for i, c := range rowStr {
			row[i] = c
		}
		rMap.rMap[j] = row
	}
}

func (rMap *RaceMap) findEnd() (*Pos, error) {
	for j, row := range rMap.rMap {
		for i, c := range row {
			if c == END {
				return &Pos{
					i: i,
					j: j,
				}, nil
			}
		}
	}
	return nil, fmt.Errorf("Not found")
}

func (rMap *RaceMap) findStart() (*Pos, error) {
	for j, row := range rMap.rMap {
		for i, c := range row {
			if c == START {
				return &Pos{
					i: i,
					j: j,
				}, nil
			}
		}
	}
	return nil, fmt.Errorf("Not found")
}

func (rMap *RaceMap) isInsideMap(pos *Pos) bool {
	if pos.i < 0 || pos.j < 0 || pos.i >= rMap.width || pos.j >= rMap.height {
		return false
	}

	return true
}

func (rMap *RaceMap) isStart(pos *Pos) bool {
	if !rMap.isInsideMap(pos) {
		return false
	}
	return rMap.rMap[pos.j][pos.i] == START
}

func (rMap *RaceMap) isExit(pos *Pos) bool {
	if !rMap.isInsideMap(pos) {
		return false
	}
	return rMap.rMap[pos.j][pos.i] == END
}

func (rMap *RaceMap) isWall(pos *Pos) bool {
	if !rMap.isInsideMap(pos) {
		return false
	}
	return rMap.rMap[pos.j][pos.i] == WALL
}

func (rMap *RaceMap) isTrack(pos *Pos) bool {
	if !rMap.isInsideMap(pos) {
		return false
	}
	return rMap.rMap[pos.j][pos.i] == TRACK ||
		rMap.rMap[pos.j][pos.i] == START ||
		rMap.rMap[pos.j][pos.i] == END
}

func (rMap *RaceMap) isValid(pos *Pos) bool {
	return rMap.isInsideMap(pos) && !rMap.isWall(pos)
}

func (rMap *RaceMap) nextMove(pos *Pos, visited map[string]int) (*Pos, bool) {
	// Try up
	nextPos := pos.goUp()
	_, wasVisited := visited[nextPos.toStr()]
	if !wasVisited && rMap.isValid(nextPos) {
		return nextPos, true
	}

	// Try left
	nextPos = pos.goLeft()
	_, wasVisited = visited[nextPos.toStr()]
	if !wasVisited && rMap.isValid(nextPos) {
		return nextPos, true
	}

	// Try right
	nextPos = pos.goRight()
	_, wasVisited = visited[nextPos.toStr()]
	if !wasVisited && rMap.isValid(nextPos) {
		return nextPos, true
	}

	// Try down
	nextPos = pos.goDown()
	_, wasVisited = visited[nextPos.toStr()]
	if !wasVisited && rMap.isValid(nextPos) {
		return nextPos, true
	}

	return nil, false
}

func (rMap *RaceMap) findCheats(pos *Pos) ([][2]*Pos, bool) {
	found := false
	cheats := [][2]*Pos{}
	// Check up
	if rMap.isWall(pos.goUp()) && rMap.isTrack(pos.goUp().goUp()) {
		cheats = append(cheats, [2]*Pos{pos.goUp().goUp(), pos})
		found = true
	}

	// Check left
	if rMap.isWall(pos.goLeft()) && rMap.isTrack(pos.goLeft().goLeft()) {
		cheats = append(cheats, [2]*Pos{pos.goLeft().goLeft(), pos})
		found = true
	}

	// Check right
	if rMap.isWall(pos.goRight()) && rMap.isTrack(pos.goRight().goRight()) {
		cheats = append(cheats, [2]*Pos{pos.goRight().goRight(), pos})
		found = true
	}

	// Check down
	if rMap.isWall(pos.goDown()) && rMap.isTrack(pos.goDown().goDown()) {
		cheats = append(cheats, [2]*Pos{pos.goDown().goDown(), pos})
		found = true
	}
	return cheats, found
}

func (rMap *RaceMap) rateTrack(end *Pos) []int {
	movsToEnd := 0
	trackRate := make(map[string]int)
	pos := end
	trackRate[pos.toStr()] = 0
	var found bool
	cheats := [][2]*Pos{}
	for {
		posCheats, foundCheats := rMap.findCheats(pos)
		if foundCheats {
			cheats = append(cheats, posCheats...)
		}
		pos, found = rMap.nextMove(pos, trackRate)
		if !found {
			panic("Solution not found")
		}

		movsToEnd += 1
		trackRate[pos.toStr()] = movsToEnd
		if rMap.isStart(pos) {
			break
		}
	}

	validCheatsSaves := []int{}
	for _, cheat := range cheats {
		posBeforCheat := cheat[0]
		posAfterCheat := cheat[1]
		scoreBeforeCheat := trackRate[posBeforCheat.toStr()]
		scoreAfterCheat := trackRate[posAfterCheat.toStr()]
		saves := scoreBeforeCheat - scoreAfterCheat - 2
		if saves > 0 {
			validCheatsSaves = append(validCheatsSaves, saves)
		}
	}
	return validCheatsSaves
}

type Pos struct {
	i int
	j int
}

func (p *Pos) toStr() string {
	return fmt.Sprintf("%d-%d", p.i, p.j)
}

func (p *Pos) goUp() *Pos {
	return &Pos{
		i: p.i,
		j: p.j - 1,
	}
}

func (p *Pos) goDown() *Pos {
	return &Pos{
		i: p.i,
		j: p.j + 1,
	}
}

func (p *Pos) goLeft() *Pos {
	return &Pos{
		i: p.i - 1,
		j: p.j,
	}
}

func (p *Pos) goRight() *Pos {
	return &Pos{
		i: p.i + 1,
		j: p.j,
	}
}

func countSaves(saves []int, minValue int) int {
	count := 0
	for _, s := range saves {
		if s >= minValue {
			count += 1
		}
	}
	return count
}

func Part1() {
	mapStr := readInput()
	raceMap := RaceMap{}
	raceMap.load(mapStr)

	end, err := raceMap.findEnd()
	if err != nil {
		panic(err)
	}
	cheatSaves := raceMap.rateTrack(end)
	numSavesGreaterThan100 := countSaves(cheatSaves, 100)
	fmt.Printf("Number of cheats that save at least 100 picoseconds %d\n", numSavesGreaterThan100)
}

func Part2() {
	fmt.Println("Hola")
}
