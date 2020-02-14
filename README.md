# The Noah with Multi-Physics options (NoahMP) land surface model

This README file describes how to build and run the NoahMP land surface model.

Model developers can also find information about the development workflow and technical documents.

## Build

Normally you can build the model in three steps: 1) run `configure`, 2) check and modify `makefile.in` if necessary, and 3) `make`. A successful build will produce an execuable named `run/noahmp.exe`.

### Run `configure`

Run `configure` to select a option that matches your operating system (Linux, Mac OS X Darwin), compiler (GCC/Gfortran, Intel, PGI), and parallel environment (seq for sequential and dm for MPI).

```bash
./configure
```

The script will generate a file named `makefile.in`.

### Check and edit `makefile.in`

The `makefile.in` details the configurations of the compiler and libraries. Check whether they fit you well.

Normally you have to modify the `NETCDFMOD` and `NETCDFLIB` variables to speficy the locations of the NetCDF Fortran 90 headers and libraries, respectively.

NoahMP uses two NetCDF libraries for Fortran and C respectively:
`libnetcdff` and `libnetcdf`. Make sure that the two files are all located under `NETCDFLIB`. If the user's NetCDF library combined them together (only has one), the user will need to manually change this part in order to successfully compile NoahMP.
See the section below on porting about how to change this.

Notes: If you are going to create model output file that is more than 2Gb,
      you should consider using NetCDF large file support function. To activate
      this, one must set the environment variable `WRFIO_NCD_LARGE_FILE_SUPPORT`.
      In a bash environment, do

```bash
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
```

### Make

If `makefile.in` fits you, type `make` at the command prompt:

```bash
make
```

If compiling is successful, an executable named `noahmp.exe` will be created under the `run` directory.

## Run

The NoahMP model use a namelist called `noahmp.namelist` as well as some additional parameter files (.TBL files), which are located under the `run` directory. Users need to copy those files to the directory where the model is going to be executed.

For a NoahMP cold start run (i.e. not from a restart file), the user needs to turn off the flag `from_restart=.false.` in `noahmp.namelist` and provide an initialization file to the `INIT_FILE` option.

For running NoahMP from restart file, the user needs to switch on the flag `from_restart=.true.` in `noahmp.namelist` and provide an existing restart file to the option `RESTART_FILE`. Running from a restart condition is common when the land surface has been
'spun-up' by running NoahMP in an offline or 'uncoupled' capacity.

## Model Development

This repository adopts the Git Flow [<https://nvie.com/posts/a-successful-git-branching-model>] branching convention and workflow.

Normally you should follow the instructions below.

1. `git clone https://github.com/esmwg/hrldas.git -o esmwg -b develop` to clone this repository. The `-o` flag sets the name of the reference that points to this GitHub repository. The `-b` flag create a local branch named `develop` that trackes the `develop` branch on GitHub.
2. `git checkout -b <your_own_branch_name> develop` to create and checkout your own branch based on the `develop` branch.
3. Edit and commit on your own branch.
4. `git fetch esmwg` fetches updates from GitHub. Updates may be available since the clone or last fetch.
5. `git merge develop` merges the updates into your own branch.
6. `git push eswmg <your_own_branch_name>:feature-<your_branch_on_GitHub>` pushes your work to a GitHub branch prefixed by "feature-". The "feature-" prefix is mandatory. It is also much easier to identify yourself in the branch name, such as "feature-zh-correct-a-bug".
7. Open the GitHub website, create a Pull Request to merge your own branch into the `develop` branch.
8. If your local branch is edited after the Pull Request. Push your edits to the same branch on GitHub will automatically update the Pull Request.
9. If the Pull Request is accepted, delete your own branch using `git branch -d <your_own_branch_name>`.

Common Git settings are below for your reference.

- `git config user.name <your_name>`
- `git config user.email <your_email>`
- `git config credential.helper cache`

## Model Debugging

There are two debugging methods shown as follow. You can combine the two methods or use separately. Both methods require recompiling.

### Print diagnostic information at run time

Set the environment variable `HYDRO_D=1` and rebuild the model.

```bash
export HYDRO_D=1
```

A "1" for `HYDRO_D` results in NoahMP producing some run-time diagnostic information.
When `HYDRO_D` is set to "0 "or not defined, the diagnostic information will not be produced
during run-time.

### Compile a debugable executable for use by debuggers

Edit "`makefile.in`", and append the compiler debug options "`-g`" to the fortran compiler "`F90`".

## Example

An example can be found in [<https://github.com/esmwg/NoahMP-Example>].

The example helps you run your first NoahMP simulation. Use the example as an template of your own simulations.

## Porting

The NoahMP model does not presently support OpenMP. The default support platform is Linux
with the Intel compiler and Linux with the GFortran compiler. However, NoahMP is fairly easy to port to other systems.
The basic steps to do so are as follows:

1. Provide your own `makefile.in` under the `arch` directory.
2. Edit `configure`, add your own compling options in the "Compiling Option" section.
3. Edit `configure`, add the path of your own `makefile.in` in the "Compiling Configurations" section.
4. `./configure`
5. `make`

If there is no error, then users can compile  NoahMP on the new platform.

## References

Please cite the following papers in your publications:

- Niu, G.-Y., Yang, Z.-L., Mitchell, K. E., Chen, F., Ek, M. B., Barlage, M., Kumar, A., Manning, K., Niyogi, D., Rosero, E., Tewari, M., & Xia, Y. (2011). The community Noah land surface model with multiparameterization options (Noah-MP): 1. Model description and evaluation with local-scale measurements. _Journal of Geophysical Research: Atmospheres_, _116_(12), D12109. <https://doi.org/10.1029/2010JD015139>
- Yang, Z.-L., Niu, G.-Y., Mitchell, K. E., Chen, F., Ek, M. B., Barlage, M., M., Lounguevergne, L., Manning, K., Niyogi, D., Tewari, M, & Xia, Y. (2011). The community Noah land surface model with multiparameterization options (Noah-MP): 2. Evaluation over global river basins. _Journal of Geophysical Research: Atmospheres_, _116_(D12), D12110. <https://doi.org/10.1029/2010JD015140>
- Zheng, H., Yang, Z.-L., Lin, P., Wei, J., Wu, W.-Y., Li, L., Zhao, L., & Wang, S. (2019). On the sensitivity of the precipitation partitioning into evapotranspiration and runoff in land surface parameterizations. _Water Resources Research_, _55_(1), 95–111. <https://doi.org/10.1029/2017WR022236>
