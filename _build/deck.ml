type suit = Clubs | Diamonds | Hearts | Spades

type number = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine
            | Ten | Jack | Queen | King

type card = number * suit

let init_deck = 
  [(Ace, Clubs); (Ace, Diamonds); (Ace, Hearts); (Ace, Spades);
   (Two, Clubs); (Two, Diamonds); (Two, Hearts); (Two, Spades);
   (Three, Clubs); (Three, Diamonds); (Three, Hearts); (Three, Spades);
   (Four, Clubs); (Four, Diamonds); (Four, Hearts); (Four, Spades);
   (Five, Clubs); (Five, Diamonds); (Five, Hearts); (Five, Spades);
   (Six, Clubs); (Six, Diamonds); (Six, Hearts); (Six, Spades);
   (Seven, Clubs); (Seven, Diamonds); (Seven, Hearts); (Seven, Spades);
   (Eight, Clubs); (Eight, Diamonds); (Eight, Hearts); (Eight, Spades);
   (Nine, Clubs); (Nine, Diamonds); (Nine, Hearts); (Nine, Spades);
   (Ten, Clubs); (Ten, Diamonds); (Ten, Hearts); (Ten, Spades);
   (Jack, Clubs); (Jack, Diamonds); (Jack, Hearts); (Jack, Spades);
   (Queen, Clubs); (Queen, Diamonds); (Queen, Hearts); (Queen, Spades);
   (King, Clubs); (King, Diamonds); (King, Hearts); (King, Spades);
  ]