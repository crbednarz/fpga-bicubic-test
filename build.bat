bash -c "yosys -p \"synth_ice40 -json hardware.json -dsp\" Top.v"
nextpnr-ice40 --up5k --package sg48 --json hardware.json --asc hardware.asc --pcf Top.pcf --freq 25
bash -c "icepack hardware.asc hardware.bin"
copy hardware.bin hardware.dfu
dfu-suffix -v 1d50 -p 6146 -a hardware.bin
dfu-util -a 0 -D hardware.dfu -R