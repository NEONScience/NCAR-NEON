import argparse
import os
import bald
from google.cloud import storage


def generateMetadata(targetBucket, stagingPrefix, metadataVocab, metadataFormat):
    metadataExt = 'json'
    storage_client = storage.Client()
    bucket = storage_client.bucket(targetBucket)
    for blob in storage_client.list_blobs(targetBucket, prefix=stagingPrefix):
        bytes = blob.download_as_bytes()
        ncdf = bald.load_netcdf(None, memory=bytes)
        context = {'@vocab': metadataVocab}
        metadata = ncdf.rdfgraph().serialize(format=metadataFormat, context=context, indent=4)
        outputKey =  os.path.splitext(blob.name)[0] + '.' + metadataExt
        outputBlob = bucket.blob(outputKey)
        outputBlob.upload_from_string(metadata, content_type='application/json')


def createCombinedDataset(rcloneRemote, sourceBucket, targetBucket, stagingPrefix, distPrefix, metadataVocab, metadataFormat):
    # Move data from source staging to target staging
    sourceStaging = rcloneRemote+':'+sourceBucket + '/' + stagingPrefix + '/'
    targetStaging = rcloneRemote+':'+targetBucket + '/' + stagingPrefix + '/'
    os.system('rclone copy ' + sourceStaging + ' ' + targetStaging)
    # Generate metadata for combined dataset on target
    generateMetadata(targetBucket, stagingPrefix, metadataVocab, metadataFormat)
    # Move combined dataset from target staging to target dist
    targetDist = rcloneRemote+':'+targetBucket + '/' + distPrefix + '/'
    os.system('rclone copy ' + targetStaging + ' ' + targetDist)
    # Clean up staging
    targetStagingDelete = rcloneRemote+':'+targetBucket + '/' + stagingPrefix + '/'
    os.system('rclone delete ' + sourceStaging)
    os.system('rclone delete ' + targetStagingDelete)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--rcloneRemote')
    parser.add_argument('--sourceBucket')
    parser.add_argument('--targetBucket')
    parser.add_argument('--stagingPrefix')
    parser.add_argument('--distPrefix')
    parser.add_argument('--metadataVocab')
    parser.add_argument('--metadataFormat')
    args=parser.parse_args()
    createCombinedDataset(args.rcloneRemote, args.sourceBucket, args.targetBucket, 
                          args.stagingPrefix, args.distPrefix, args.metadataVocab, args.metadataFormat)

