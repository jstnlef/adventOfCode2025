module Common.Vector3d

type Vector3d = int * int * int

let distance (p1x, p1y, p1z) (p2x, p2y, p2z) : float =
  let dx = float (p1x - p2x)
  let dy = float (p1y - p2y)
  let dz = float (p1z - p2z)
  sqrt ((dx * dx) + (dy * dy) + (dz * dz))
