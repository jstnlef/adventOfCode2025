module Common.Vector3d

type Vector3d = int * int * int

let distanceSquared (p1x, p1y, p1z) (p2x, p2y, p2z) : float =
  let dx = float (p1x - p2x)
  let dy = float (p1y - p2y)
  let dz = float (p1z - p2z)
  (dx * dx) + (dy * dy) + (dz * dz)

let distance p1 p2 : float = sqrt (distanceSquared p1 p2)
