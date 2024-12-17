open Format

type config =
  { mutable country : string
  ; mutable location : string
  ; mutable tp_layer : string
  ; mutable is_manual : bool
  }

let check_and_insert ~config_map config =
  if Base.String.is_substring ~substring:".ovpn" config
  then (
    let country_locations =
      Base.String.split ~on:'.' config |> List.hd |> Base.String.split ~on:'-'
    in
    let country = List.hd country_locations in
    match Base.Hashtbl.find config_map country with
    | None ->
      let set = Base.Hash_set.create (module Base.String) in
      Base.Hash_set.add set (List.nth country_locations 1);
      let _ = Base.Hashtbl.add config_map ~key:country ~data:set in
      ()
    | Some set -> Base.Hash_set.add set (List.nth country_locations 1))
;;

let prepare_config_list () =
  let config_map = Base.Hashtbl.create (module Base.String) in
  Array.iter (check_and_insert ~config_map) (Sys.readdir "/etc/openvpn/");
  config_map
;;

let print_config config =
  printf "Country: %s\n" config.country;
  printf "Location: %s\n" config.location;
  printf "Trasport Layer: %s\n" config.tp_layer;
  print_flush ();
  ()
;;

let remove_dns_from_interface () =
  let _ = Stdlib.Sys.command "sudo systemd-resolve --interface=enp42s0 --set-dns=" in
  ()
;;

let connect config =
  let _ =
    Stdlib.Sys.command
      (Stdlib.Format.sprintf
         "sudo openvpn --config /etc/openvpn/%s-%s.prod.surfshark.com_%s.ovpn \
          --auth-user-pass /home/elgaeraw/.config/.vpn/auth"
         config.country
         config.location
         config.tp_layer)
  in
  ()
;;

let connect_vpn config =
  printf "Connecting to the following config in 5 secs!\n";
  print_config config;
  remove_dns_from_interface ();
  Unix.sleep 5;
  printf "Connecting now...\n";
  print_flush ();
  connect config
;;

let check_modes i arg ~conf =
  match arg with
  | "-m" -> conf.is_manual <- true
  | "-tcp" -> conf.tp_layer <- "tcp"
  | "-c" ->
    conf.country <- Sys.argv.(i + 1);
    conf.is_manual <- true
  | "-l" ->
    conf.location <- Sys.argv.(i + 1);
    conf.is_manual <- true
  | _ -> ()
;;

let take_input lst label =
  printf "%s List: \n" label;
  let srt_lst = List.sort String.compare lst in
  List.iter (printf "%s \t") srt_lst;
  printf "\n";
  let item_def = List.hd srt_lst in
  printf "Select %s [%s]: " label item_def;
  print_flush ();
  match In_channel.input_line In_channel.stdin with
  | Some l when not (Base.String.is_empty l) -> l
  | _ -> item_def
;;

let main () =
  let conf = { country = ""; location = ""; tp_layer = "udp"; is_manual = false } in
  Array.iteri (check_modes ~conf) Sys.argv;
  if not conf.is_manual
  then (
    printf
      "AutoConnect Mode!!! If you want to set manual location pass -m in args: vpn -m\n";
    printf
      "OR you can directly specify the country and location code: vpn -c <country_code> \
       -l <location_code>\n";
    printf "For TCP layer use -tcp in args: vpn -m -tcp\n";
    conf.country <- "in";
    conf.location <- "mum";
    connect_vpn conf)
  else (
    print_flush ();
    printf "Manual Mode!!!\n";
    let config_map = prepare_config_list () in
    let countries = Base.Hashtbl.keys config_map in
    if Base.String.is_empty conf.country && Base.String.is_empty conf.location
    then (
      conf.country <- take_input countries "Country";
      let locations = Base.Hashtbl.find_exn config_map conf.country in
      conf.location <- take_input (Base.Hash_set.to_list locations) "Location")
    else if Base.String.is_empty conf.country
    then (
      conf.country <- take_input countries "Country";
      let locations = Base.Hashtbl.find_exn config_map conf.country in
      if
        List.filter
          (fun x -> String.equal x conf.location)
          (Base.Hash_set.to_list locations)
        |> List.is_empty
      then (
        printf
          "Entered location does not exist in the selected country. Select a location \
           from below:\n";
        conf.location <- take_input (Base.Hash_set.to_list locations) "Location"))
    else if Base.String.is_empty conf.location
    then (
      if List.filter (fun x -> String.equal x conf.country) countries |> List.is_empty
      then (
        printf
          "Entered country does not exist in the options. Select a country from below:\n";
        conf.country <- take_input countries "Country");
      let locations = Base.Hashtbl.find_exn config_map conf.country in
      conf.location <- take_input (Base.Hash_set.to_list locations) "Location")
    else (
      if List.filter (fun x -> String.equal x conf.country) countries |> List.is_empty
      then (
        printf
          "Entered country does not exist in the options. Select a country from below:\n";
        conf.country <- take_input countries "Country");
      let locations = Base.Hashtbl.find_exn config_map conf.country in
      if
        List.filter
          (fun x -> String.equal x conf.location)
          (Base.Hash_set.to_list locations)
        |> List.is_empty
      then (
        printf
          "Entered location does not exist in the selected country. Select a location \
           from below:\n";
        conf.location <- take_input (Base.Hash_set.to_list locations) "Location"));
    connect_vpn conf)
;;

main ()
