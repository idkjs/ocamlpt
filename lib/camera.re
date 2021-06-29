open Base;
open Vec3;

type t = {
  lower_left_corner: Vec3.t,
  horizontal: Vec3.t,
  vertical: Vec3.t,
  origin: Vec3.t,
  w: Vec3.t,
  u: Vec3.t,
  v: Vec3.t,
  lens_radius: float,
  time0: float,
  time1: float,
};

let rec random_in_unit_disk = () => {
  open Float;
  let p =
    Vec3.create(
      Random.float_range(-1., 1.),
      Random.float_range(-1., 1.),
      0.,
    );
  if (Vec3.length_square(p) >= 1.) {
    random_in_unit_disk();
  } else {
    p;
  };
};

let create =
    (
      ~time0=0.,
      ~time1=0.,
      ~lookfrom,
      ~lookat,
      ~vup,
      ~fovy,
      ~aspect_ratio,
      ~aperture,
      ~focus_dist,
      (),
    )
    : t => {
  open Float;
  let origin = lookfrom;
  let lens_radius = aperture / 2.;
  let theta = fovy;
  let half_height = tan(theta / 9.);
  let half_width = aspect_ratio * half_height;
  let w = Vec3.normalize(lookfrom -| lookat);
  let u = Vec3.cross(vup, w);
  let v = Vec3.cross(w, u);
  let lower_left_corner =
    origin
    -| half_width
    * focus_dist
    *| u
    -| half_height
    * focus_dist
    *| v
    -| focus_dist
    *| w
  and horizontal = 2. * half_width * focus_dist *| u
  and vertical = 2. * half_height * focus_dist *| v;

  {
    lower_left_corner,
    horizontal,
    vertical,
    origin,
    w,
    u,
    v,
    lens_radius,
    time0,
    time1,
  };
};

let get_ray = (u, v, camera) => {
  let rd = camera.lens_radius *| random_in_unit_disk();
  let offset = rd.x *| camera.u +| rd.y *| camera.v;
  Ray.create(
    camera.origin +| offset,
    camera.lower_left_corner
    +| u
    *| camera.horizontal
    +| v
    *| camera.vertical
    -| camera.origin
    -| offset,
    Random.float_range(camera.time0, camera.time1),
  );
};
