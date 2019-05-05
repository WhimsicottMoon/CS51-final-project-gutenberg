open Plplot

let () =
  let xmin = 0.0 in
  let xmax = 15.0 in
  let ymin = 0.0 in
  let ymax = 20.0 in


  (* Initialize plplot *)
  plinit ();

  (* Create a labelled box to hold the plot. *)
  plenv xmin xmax ymin ymax 0 0 ;
  pllab "log of rank" "log of frequency" "Zipf's law for XXXX" ;

  (* Plot the data that was prepared above. *)
  let oldx = ref 0. in
  let oldy = ref 0. in
  let colorer = ref 2 in
  let plotter ((w, f, r) : (string * int * int)) : unit =
    let rlog = log10(float_of_int r) in
    let flog = log10(float_of_int f) in
    plstring( 101, rlog, flog , "#(728)" );
    (if !oldy <> 0 then pljoin(!oldx, !oldy, rlog, flog);
    oldx := rlog;
    oldy := flog; ()) in

  List.iter ((List.iter plotter); colorer := !colorer + 1) nice_lst ;;
  let l = List.length book_files in
    let text_colors = Array.init l (fun x -> x + 2) in
    let text = Array.init l (fun x -> List.nth book_files x) in
    let line_colors = Array.init l (fun x -> x + 2) in
    let line_styles = Array.make l 1.0 in
    let line_widths = Array.make l 1.0 in
    let symbol_colors = Array.init l (fun x -> List.nth book_files x) in
    let opt_array = [| [PL_LEGEND_LINE]; [PL_LEGEND_LINE; PL_LEGEND_SYMBOL] |] in
    let symbol_scales = Array.make l 1.0 in
    let symbol_numbers = Array.make l 2.0 in
    let symbols = Array.make l "#(728)" in

    pllegend [PL_LEGEND_BACKGROUND; PL_LEGEND_BOUNDING_BOX] []
          0.0 0.0 0.1 15
          1 1 0 0
          opt_array
          1.0 1.0 2.0
          1.0 text_colors text
          [||] [||] [||] [||]
          line_colors line_styles line_widths
          symbol_colors symbol_scales symbol_numbers symbols;
;;
