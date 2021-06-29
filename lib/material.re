open Base;
open Vec3;

type t =
  | Lambertian({albedo: Texture.t})
  | Metal({
      albedo: Vec3.t,
      fuzzness: float,
    })
  | Dielectric({ref_index: float});

type face_direction =
  | FrontFace
  | BackFace;

type hit_record = {
  t: float,
  p: Vec3.t,
  normal: Vec3.t,
  material: t,
  u: float,
  v: float,
  face_direction,
};

let random_unit_vector = () => {
  open Float;
  let a = Random.float_range(0., 2. * pi)
  and z = Random.float_range(-1., 1.);
  let r = sqrt(1. - z * z);
  Vec3.create(r * cos(a), r * sin(a), z);
};

type scatter_result = {
  scattered: Ray.t,
  attenuation: Vec3.t,
};

let schlick = (cosine, ref_idx) => {
  open Float;
  let r0 = (1. - ref_idx) / (1. + ref_idx);
  let r0 = r0 * r0;
  r0 + (1. - r0) * int_pow(1. - cosine, 5);
};

let scatter = (r: Ray.t, hit_record: hit_record) =>
  Float.(
    fun
    | Lambertian({albedo}) => {
        let scatter_direction = hit_record.normal +| random_unit_vector();
        let scattered = Ray.create(hit_record.p, scatter_direction, r.time)
        and attenuation =
          albedo(~u=hit_record.u, ~v=hit_record.v, ~p=hit_record.p);
        Some({scattered, attenuation});
      }

    | Metal({albedo, fuzzness}) => {
        let reflected = reflect(normalize(r.direction), hit_record.normal);
        let scattered =
          Ray.create(
            hit_record.p,
            reflected +| fuzzness *| random_unit_vector(),
            r.time,
          )
        and attenuation = albedo;
        Option.some_if(
          Vec3.dot(scattered.direction, hit_record.normal) > 0.,
          {scattered, attenuation},
        );
      }

    | Dielectric({ref_index}) => {
        let attenuation = Vec3.create(1.0, 1.0, 1.0);
        let etai_over_etat =
          switch (hit_record.face_direction) {
          | FrontFace => 1.0 /. ref_index
          | BackFace => ref_index
          };

        let unit_direction = Vec3.normalize(r.direction);
        let cos_theta =
          min(dot(negate(unit_direction), hit_record.normal), 1.0);
        let sin_theta = sqrt(1.0 - cos_theta * cos_theta);
        if (etai_over_etat
            * sin_theta > 1.0  /* Total internal reflection, must reflect */
            || Random.float(1.) < schlick(cos_theta, etai_over_etat)) {
          let reflected = reflect(unit_direction, hit_record.normal);
          let scattered = Ray.create(hit_record.p, reflected, r.time);
          Some({scattered, attenuation});
        } else {
          let refracted =
            refract(unit_direction, hit_record.normal, etai_over_etat);
          let scattered = Ray.create(hit_record.p, refracted, r.time);
          Some({scattered, attenuation});
        };
      }
  );
