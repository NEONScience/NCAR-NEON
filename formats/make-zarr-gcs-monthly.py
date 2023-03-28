from glob import glob
from kerchunk.combine import MultiZarrToZarr
import pathlib
import os


if __name__ == "__main__":

    indir = './jsons-gcs/'
    outdir = './jsons-gcs-monthly/'

    pathlib.Path(outdir).mkdir(exist_ok=True)

    years = ['2018', '2019', '2020', '2021', '2022']
    sites = [name for name in os.listdir('archive-h5') if os.path.isdir(os.path.join('archive-h5',name))]

    for site in sites:
        for year in years:
            for month in range (1, 13):
                yearmonth = year + '-' + str(month).zfill(2)
                print(site + ' ' + yearmonth)
                path = indir + site + '.transient.clm2.h1.' + yearmonth + '*'
                json_list = sorted(glob(path))
                if len(json_list) > 0:
                    mzz = MultiZarrToZarr(json_list,concat_dims=["time"])
                    outname = outdir + site + '.transient.clm2.h1.' + yearmonth + '.json' 
                    mzz.translate(outname)



