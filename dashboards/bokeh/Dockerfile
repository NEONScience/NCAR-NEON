FROM continuumio/anaconda3:2022.10

WORKDIR /apps

COPY . .

ENV PORT=8080

RUN chown 1000:1000 /opt/conda/ && \
    chmod 777 entrypoint.sh

ARG USER=bokeh
RUN useradd -s /bin/bash -m ${USER}
USER ${USER}

RUN conda install conda=22.9.0
RUN conda create --name neon -c conda-forge python numpy scipy matplotlib pandas netcdf4 jupyter xarray tqdm bokeh=2.4.3 jupyterlab fsspec ujson gcsfs dask zarr pillow

RUN echo "conda activate neon" >> ~/.bashrc
SHELL ["/bin/bash", "--login", "-c"]

ENTRYPOINT ["./entrypoint.sh"]
