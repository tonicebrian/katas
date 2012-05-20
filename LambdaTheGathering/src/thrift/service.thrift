struct Move {
    1: i32 playerId,
    2: i32 actionType, // 1 or 2, see the rules
    3: i32 slot,
    4: string card
}

struct Slot {
    1: i32 playerId,
    2: i32 vitality,
    3: string value  // value is either an integer in string format or a parenthesized function composition without white spaces
}

service GameState {
    void processMove(1:Move theMove);
    list<Slot> getLastUpdate();
}
