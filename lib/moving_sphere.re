open Base;
open Vec3;

type t = {
  center0: Vec3.t,
  center1: Vec3.t,
  time0: float,
  time1: float,
  radius: float,
  material: Material.t,
};

let create = (center0, center1, time0, time1, radius, material) => {
  center0,
  center1,
  time0,
  time1,
  radius,
  material,
};

let calc_center = ({center0, center1, time0, time1, _}: t, time: float) =>
  lerp(center0, center1, (time -. time0) /. (time1 -. time0));

let hit =
    (r: Ray.t, {radius, material, _} as sphere: t)
    : option(Material.hit_record) => {
  let center = calc_center(sphere, r.time);
  Sphere.hit(r, Sphere.create(center, radius, material));
};

let bounding_box = (sphere: t): Aabb.t => {
  open Vec3;
  let r = Vec3.create(sphere.radius, sphere.radius, sphere.radius);
  let box0: Aabb.t = (
    {min: sphere.center0 -| r, max: sphere.center0 +| r}: Aabb.t
  )
  and box1: Aabb.t = (
    {min: sphere.center1 -| r, max: sphere.center1 +| r}: Aabb.t
  );
  Aabb.union(box0, box1);
};
