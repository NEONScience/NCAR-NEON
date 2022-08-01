conda create --name neon -c conda-forge python=3.9 numpy scipy matplotlib pandas netcdf4 jupyter xarray tqdm bokeh jupyterlab fsspec ujson gcsfs dask zarr
conda activate neon
python -m bokeh serve --show main.py --port 5012

