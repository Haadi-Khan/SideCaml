# Instructions
As of MS3, the only requirements needed to install and build our system are to ensure the proper libraries are installed. In particular, the following libraries must be installed: `OUnit2`, `Core`, `ANSITerminal`, `Lacaml`, `Cohttp`, `Lwt`, `Yojson`

To do so, run the following command:

`opam install Cohttp cohttp-lwt-unix lwt_ssl bisect_ppx Lwt Yojson ounit2 core ansiterminal lacaml`
