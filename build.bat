bash -c "mkdir -p logs"
bash -c "yosys -p \"synth_ice40 -json hardware.json -dsp\" Top.v | tee logs/yosys.log"
powershell "nextpnr-ice40 --up5k --package sg48 --json hardware.json --asc hardware.asc --pcf Top.pcf --freq 30 2>&1 | tee logs/nextpnr.log"
bash -c "icepack hardware.asc hardware.bin | tee logs/icepack.log"
copy hardware.bin hardware.dfu
dfu-suffix -v 1d50 -p 6146 -a hardware.bin
dfu-util -a 0 -D hardware.dfu -R