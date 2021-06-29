type t = (~u: float, ~v: float, ~p: Vec3.t) => Vec3.t;

let solid_color = (color: Vec3.t): t => (~u as _, ~v as _, ~p as _) => color;

let checker = (even: t, odd: t): t =>
  Float.(
    (~u, ~v, ~p) => {
      let sines = sin(10. *. p.x) *. sin(10. *. p.y) *. sin(10. *. p.z);
      if (sines < 0.) {
        odd(~u, ~v, ~p);
      } else {
        even(~u, ~v, ~p);
      };
    }
  );

let noise: t = (
  {
    open Vec3;
    let perlin = Perlin.create();
    (~u as _, ~v as _, ~p) =>
      Perlin.noise(p, perlin) *| Vec3.create(1., 1., 1.);
  }: t
);
