# RAYRAW-StrADC

FPGA firmware for the streaming-readout-type DAQ system.
Sources for RAYRAW-v1 and v2 are located under ./hdl/rayraw-impl/rayraw-v1 (2).

## Vivado version
Vivado 2025.2 is expected.

# INSTALLATION guide
The procedure to generate the Vivado project. You need to complete the following things once after cloning the project.

## Update submodule
You need to update the submodule after cloning.
```
git submodule update --init
```

## Generate .xpr file
Generate the Vivado project from the TCL script. The Vivado project related directories and files are genereted under build directory.
The files in the build_v1(2) directory are not under Git management. You can remove them and re-generate the project anytime.

**For Windows User:**
Launch the for-windows-v1(2).bat in CMD or PowerShell with the argument for the path for vivado.

- for-windows-v1.bat: Generates the Vivado project under the build_v1 dirctory.
- for-windows-v2.bat: Generates the Vivado project under the build_v2 dirctory.

Example
```
[RAYRAW-TRG dir] .\for-windows-v2.bat C:\AMDDesignTools\2025.2\Vivado\bin\vivado.bat
```

**For Linux User:**
Please execute project.tcl by vivado with the batch mode with same arguments written in for-windows.bat.

## Modify gig_ethernet_pcs_pma IP
On AMANEQ, the reference clock frequency for GTX is 156.25 since AMANEQ supports 10GbE.
In order to use the pcs_pma IP core for GbE, the IP setting must be changed.
After cloning the repogitory to your PC, once you need to do the procedure below.
```
[TCL console] set_param project.defaultIPCacheSetting none
[TCL console] set_property IS_MANAGED true [get_files gig_ethernet_pcs_pma.xci]
```
Once reset and generates the output products, and again set IS_MANAGED to false.
```
[TCL console] set_property IS_MANAGED false [get_files gig_ethernet_pcs_pma.xci]
```
Then, change the IP setting. Please CPLL_FBDIV_45 from 5 to 4 in gig_ethernet_pcs_pma_gtwizard_gt.vhd and change gtrefclk period from 8.000 to 6.400 in gig_ethernet_pcs_pma_ooc.xdc.
Finnaly, please update .dcp file from Design Runs. Select gig_ethernet_pcs_pma IP and click the triangle button (Launch Runs).
After updating, please check that the .dcp file date is actually updated.

![image](https://github.com/RyotaroHonda/AMANEQ-Skeleton/assets/41090607/ef80a234-1e09-4c0c-974d-42bb4d387098)
