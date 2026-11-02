package fp.common.astar

import fp.common.Position

enum Status derives CanEqual:
  case Running
  case Found(path: List[Position])
  case NoPath
