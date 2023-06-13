module Input = struct
  type t = Index8.t

  let to_index8 (i : Rgba32.t) =
    let i8 = Index8.create i.width i.height in
    for x = 0 to i.width - 1 do
      for y = 0 to i.height - 1 do
        let c = Rgba32.get i x y in
        let cid = Color.add_color i8.colormap c.color in
        Index8.set i8 x y cid
      done
    done;
    i8

  let load fn =
    match Png.load fn [] with
    | Index8 i -> i
    | Rgb24 i -> to_index8 @@ Rgb24.to_rgba32 i
    | Rgba32 i -> to_index8 i
    | Index16 _ | Cmyk32 _ -> assert false
end

module Tile = struct
  type _t = Index8.t

  let build_tiles (input : Input.t) size =
    let ntiles = input.width * input.height in
    let tiles =
      List.init ntiles (fun i ->
          let x = i mod input.width in
          let y = i / input.width in
          let tile = Index8.create size size in
          tile.colormap <- input.colormap;
          for ix = 0 to size - 1 do
            for iy = 0 to size - 1 do
              let x = (x + ix) mod input.width in
              let y = (y + iy) mod input.height in
              let c = Index8.get input x y in
              Index8.set tile ix iy c
            done
          done;
          tile)
    in
    let tbl = Hashtbl.create 1023 in
    List.iter (fun i ->
        let b = Index8.dump i in
        match Hashtbl.find_opt tbl b with
        | Some (i, w) -> Hashtbl.replace tbl b (i, w+1)
        | None -> Hashtbl.replace tbl b (i, 1)) tiles;
    Hashtbl.fold (fun _ (i, w) acc -> (i, w)::acc) tbl []

  (* Can [target] be placed at [(+dx, +dy)] without any conflict with [base]? *)
  let match_ base target dx dy =
    assert (base.Index8.width = target.Index8.width && dx < base.width);
    assert (base.height = target.height && dy < base.height);
    try
      for ix = max 0 dx to base.width - 1 + (min 0 dx) do
        for iy = max 0 dy to base.height - 1 + (min 0 dy) do
          if not (Index8.get base ix iy = Index8.get target (ix-dx) (iy-dy)) then raise Exit
        done
      done;
      true
    with
    | Exit -> false

  let build_propagator tile tiles =
    let f dir =
      let dx, dy = Model.Dir.dxy dir in
      let itiles = List.mapi (fun i tile -> (i, tile)) tiles in
      List.fold_left (fun acc (i,tile') ->
          if match_ tile tile' dx dy then i::acc else acc)
        [] itiles
    in
    Model.DirArray.create f

  let build (input : Input.t) size =
    let tilesw = build_tiles input size in
    let tiles = List.map fst tilesw in
    tiles,
    List.map (fun (tile, weight) ->
        { Model.weight; propagator= build_propagator tile tiles }) tilesw
end

let () =
  let input = Input.load "Flowers.png" in
  let tile_images, tiles = Tile.build input 3 in
  Format.eprintf "%d tiles@." (List.length tiles);
  List.iteri (fun i tile_info ->
      Format.eprintf "tile%d " i;
      Model.DirArray.iter (fun dir is -> Format.eprintf "%s:%d " (Model.Dir.to_string dir) (List.length is)) tile_info.Model.propagator;
      Format.eprintf "@.") tiles;
  let width = 100 in
  let height = 100 in
  let config = Model.init ~width ~height ~n:0 ~periodic:true ~use_entropy:true ~scanline:false ~ground:false ~tiles:tiles in
  match Model.run (Random.State.make_self_init ()) ~limit:None config with
  | `Error -> prerr_endline "cannot finish"
  | `PropError -> prerr_endline "prop error"
  | `Finished array ->
      prerr_endline "finished";
      let out = Index8.create width height in
      out.colormap <- input.colormap;
      let red = Color.add_color out.colormap Color.{r=255; g=0; b=0} in
      for x = 0 to width - 1 do
        for y = 0 to height - 1 do
          let tile = array.(x + y * width) in
          let c =
            if tile = -1 then red
            else
              let i8 = List.nth tile_images tile in
              Index8.get i8 1 1
          in
          Index8.set out x y c
        done
      done;
      Png.save "out.png" [] (Images.Index8 out)
