bash -c "mkdir -p logs"
bash -c "yosys -p \"synth_ice40 -json hardware.json -dsp\" Top.v | tee logs/yosys.log"
powershell "nextpnr-ice40 --up5k --package sg48 --json hardware.json --asc hardware.asc --pcf Top.icebreaker.pcf --freq 30 2>&1 | tee logs/nextpnr.log"
bash -c "icepack hardware.asc hardware.bin | tee logs/icepack.log"
iceprog hardware.bin