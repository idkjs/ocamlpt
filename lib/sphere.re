open Base;
open Vec3;

type t = {
  center: Vec3.t,
  radius: float,
  material: Material.t,
};

let create = (center, radius, material) => {center, radius, material};

/*
 void get_sphere_uv(const vec3& p, double& u, double& v) {
     auto phi = atan2(p.z(), p.x());
     auto theta = asin(p.y());
     u = 1-(phi + pi) / (2*pi);
     v = (theta + pi/2) / pi;
 }
 */

let get_sphere_uv = (p: Vec3.t) => {
  open Float;
  let phi = atan2(p.z, p.x)
  and theta = asin(p.y);
  let u = 1. - (phi + pi) / (2. * pi)
  and v = (theta + pi / 2.) / pi;
  (u, v);
};

let hit =
    (r: Ray.t, {center, radius, material}: t): option(Material.hit_record) => {
  open Float;
  let t_min = 0.00001;
  let oc = r.origin -| center;
  let a = Vec3.dot(r.direction, r.direction)
  and half_b = Vec3.dot(oc, r.direction)
  and c = Vec3.dot(oc, oc) -. radius *. radius;

  let hit_record_from_t = (t, face_direction): option(Material.hit_record) =>
    if (t > t_min) {
      let p = Ray.at(t, r);
      let outward_normal = (p -| center) /| radius;
      let normal =
        switch (face_direction) {
        | Material.FrontFace => outward_normal
        | Material.BackFace => negate(outward_normal)
        };

      let (u, v) = get_sphere_uv(p);
      Some({t, p, normal, material, u, v, face_direction});
    } else {
      None;
    };

  let quater_discriminant = half_b *. half_b -. a *. c;
  if (quater_discriminant > 0.) {
    let root = sqrt(quater_discriminant);
    let t1 = (-. half_b -. root) /. a;
    hit_record_from_t(t1, Material.FrontFace)
    |> Option_ext.or_else(~f=() =>
         let t2 = (-. half_b +. root) /. a;
         hit_record_from_t(t2, Material.BackFace);
       );
  } else {
    None;
  };
};

let bounding_box = (sphere: t): Aabb.t => {
  open Vec3;
  let r = Vec3.create(sphere.radius, sphere.radius, sphere.radius);
  {min: sphere.center -| r, max: sphere.center +| r};
};
