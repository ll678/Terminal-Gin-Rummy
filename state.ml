type t

let discard card prev_st = 
  if prev_st.last_move = Draw "discard" and
  (List.hd prev_st.discard_pile) = card then Illegal else
  Legal ({
    (* todo: insert other parameters of state *)
    discard = card :: prev_st.discard;
    last_move = Discard (card);
    last_state = prev_st;
    players = Players.remove_card prev_st.current_player card
  })