open Base;

module type Shape = {
  type t;
  let hit: (Ray.t, t) => option(Material.hit_record);
  let bounding_box: t => Aabb.t;
};

module type Shape_instance = {
  module S: Shape;
  let this: S.t;
};

let build_shape =
    (type a, module S: Shape with type t = a, shape: a)
    : (module Shape_instance) =>
  (module
   {
     module S = S;
     let this = shape;
   });

type t = {root: (module Shape_instance)};

let hit = (r: Ray.t, scene: t): option(Material.hit_record) => {
  module RootInstance = (val scene.root: Shape_instance);
  RootInstance.S.hit(r, RootInstance.this);
};

module rec Bvh_node: {
  include Shape;
  let create: list(module Shape_instance) => t;
} = {
  type t = {
    left: (module Shape_instance),
    right: (module Shape_instance),
    aabb: Aabb.t,
  };

  let bounding_box = (node: t) => node.aabb;

  let hit = (r: Ray.t, node: t): option(Material.hit_record) =>
    Float.(
      if (Aabb.hit(r, 0.00001, Float.max_value, node.aabb)) {
        module LhsInstance = (val node.left: Shape_instance);
        module RhsInstance = (val node.right: Shape_instance);
        let lhs_result = LhsInstance.S.hit(r, LhsInstance.this);
        let rhs_result = RhsInstance.S.hit(r, RhsInstance.this);
        switch (lhs_result, rhs_result) {
        | (Some(lhs), Some(rhs)) =>
          if (lhs.t < rhs.t) {
            lhs_result;
          } else {
            rhs_result;
          }
        | (Some(_), _) => lhs_result
        | (_, Some(_)) => rhs_result
        | _ => None
        };
      } else {
        None;
      }
    );

  let rec create = (shapes: list(module Shape_instance)) => {
    let axis_index = Random.int(3);
    let get_axis =
      switch (axis_index) {
      | 0 => ((pt: Vec3.t) => pt.x)
      | 1 => ((pt: Vec3.t) => pt.y)
      | _ => ((pt: Vec3.t) => pt.z)
      };

    let compare_shape_by_axis = (lhs, rhs) => {
      module LhsInstance = (val lhs: Shape_instance);
      module RhsInstance = (val rhs: Shape_instance);
      let left_box = LhsInstance.S.bounding_box(LhsInstance.this);
      let right_box = RhsInstance.S.bounding_box(RhsInstance.this);
      Float.compare(get_axis(left_box.min), get_axis(right_box.min));
    };

    let get_aabb = shape => {
      module Instance = (val shape: Shape_instance);
      Instance.S.bounding_box(Instance.this);
    };

    switch (shapes) {
    | [] => failwith("Cannot give bvh_node an empty list of shapes")
    | [elm] => {left: elm, right: elm, aabb: get_aabb(elm)}
    | [elm1, elm2] =>
      let aabb = Aabb.union(get_aabb(elm1), get_aabb(elm2));
      if (compare_shape_by_axis(elm1, elm2) < 0) {
        {left: elm1, right: elm2, aabb};
      } else {
        {left: elm2, right: elm1, aabb};
      };
    | _ =>
      let sorted_shapes = shapes |> List.sort(~compare=compare_shape_by_axis);
      let (first_half, second_half) =
        List.split_n(sorted_shapes, List.length(sorted_shapes) / 2);
      let left = create(first_half)
      and right = create(second_half);
      let aabb = Aabb.union(left.aabb, right.aabb);
      {
        left: build_shape((module Bvh_node), left),
        right: build_shape((module Bvh_node), right),
        aabb,
      };
    };
  };
};

module Builder = {
  type t = list(module Shape_instance);

  let create: t = ([]: t);

  let add = (type a, module S: Shape with type t = a, shape: a, scene: t): t => [
    build_shape((module S), shape),
    ...scene,
  ];

  let build = (shapes: t) => {
    root: build_shape((module Bvh_node), Bvh_node.create(shapes)),
  };
};
