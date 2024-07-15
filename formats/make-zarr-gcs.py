from kerchunk.hdf import SingleHdf5ToZarr 
import fsspec
import ujson
from fsspec.implementations.local import LocalFileSystem

import dask
from dask.distributed import Client

import pathlib


def gen_json(u):
    so = dict(
        mode="rb", anon=True, default_fill_cache=False,
        default_cache_type="none"
    )
    with fsspec.open(u, **so) as inf:
        h5chunks = SingleHdf5ToZarr(inf, u, inline_threshold=300)
        with open(f"jsons-gcs/{u.split('/')[-1]}.json", 'wb') as outf:
           outf.write(ujson.dumps(h5chunks.translate()).encode())


if __name__ == "__main__":
    client = Client(n_workers=8)
    fs = fsspec.filesystem('gs', anon=True)
    urls = ['gs://' + f for f in fs.glob("gs://neon-ncar-dev/archive-h5/*/lnd/hist/*.nc")]
#    urls = ['gs://' + f for f in fs.glob("gs://neon-ncar-dev/eval_files-h5/v2/*/*.nc")]

    pathlib.Path('./jsons-gcs/').mkdir(exist_ok=True)

    dask.compute(*[dask.delayed(gen_json)(f) for f in urls])