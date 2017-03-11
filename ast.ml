type priority =
    | Free
    | OccupiedHorizontal
    | OccupiedVertical
    | Blocked

  type node = {
      name : string;
      xLoc : int;
      yLoc : int;
      status : priority;
      successors : string option list;
      parent : string;
  }
