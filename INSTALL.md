# Instructions
The following libraries must be installed: `OUnit2`, `Core`, `ANSITerminal`, `Lacaml`, `Cohttp`, `Lwt`, `Yojson`

To do so, run the following command:

`opam install Cohttp cohttp-lwt-unix lwt_ssl bisect_ppx Lwt Yojson ounit2 core ansiterminal lacaml`

Next, clone the data from our github. The files are large so we can not upload them to CMSX

Assuming you have an SSH key,

```
git clone -n --depth=1 --filter=tree:0 \
  git@github.coecis.cornell.edu:hmk68/3110-final-project.git    
cd 3110-final-project
git sparse-checkout set --no-cone /data
git checkout
rm -rf .git
```

This will download just the data directory. If you don't have an SSH key, substitute the second line for
`https://github.coecis.cornell.edu/hmk68/3110-final-project.git`

You can then move this data folder to the project's base level in the directory structure.

