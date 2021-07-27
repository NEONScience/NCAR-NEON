import argparse
import os
import bald
from boto3 import resource
import json

def generateMetadata(targetRemote, targetBucket, stagingPrefix, metadataVocab, metadataFormat):
    metadataExt = 'json'
    s3 = getTargetS3(targetRemote)
    bucket = s3.Bucket(targetBucket)
    s3uri = "s3://" + targetRemote
    for s3object in bucket.objects.filter(Prefix=stagingPrefix):
        if s3object.key.endswith('/'):
            continue
        s3endpoint = s3uri + '/' + targetBucket + '/' + s3object.key
        ncdf = bald.load_netcdf(s3endpoint)
        context = {'@vocab': metadataVocab}
        metadata = ncdf.rdfgraph().serialize(format=metadataFormat, context=context, indent=4).decode('utf-8')
        outputKey =  os.path.splitext(s3object.key)[0] + '.' + metadataExt
        outputObject = s3.Object(targetBucket, outputKey)
        outputObject.put(Body=metadata)


def getTargetS3(targetRemote):
    # Parse target access from config
    s3configfile = os.getenv('S3_NC_CONFIG')
    with open(s3configfile) as f:
        s3config = json.load(f)
    s3uri = "s3://" + targetRemote
    targetUrl = s3config['hosts'][s3uri]['url']
    accessKey = s3config['hosts'][s3uri]['credentials']['accessKey']
    secretKey = s3config['hosts'][s3uri]['credentials']['secretKey']
    s3 = resource('s3', aws_access_key_id=accessKey, aws_secret_access_key=secretKey, endpoint_url=targetUrl)
    return s3


def createCombinedDataset(sourceRemote, sourceBucket, targetRemote, targetRemoteDelete,
                          targetBucket, stagingPrefix, distPrefix, metadataVocab, metadataFormat):
    # Move data from source staging to target staging
    sourceStaging = sourceRemote+':'+sourceBucket + '/' + stagingPrefix + '/'
    targetStaging = targetRemote+':'+targetBucket + '/' + stagingPrefix + '/'
    os.system('rclone copy ' + sourceStaging + ' ' + targetStaging + ' --s3-no-check-bucket')
    # Generate metadata for combined dataset on target
    generateMetadata(targetRemote, targetBucket, stagingPrefix, metadataVocab, metadataFormat)
    # Move combined dataset from target staging to target dist
    targetDist = targetRemote+':'+targetBucket + '/' + distPrefix + '/'
    os.system('rclone copy ' + targetStaging + ' ' + targetDist + ' --s3-no-check-bucket')
    # Clean up staging
    targetStagingDelete = targetRemoteDelete+':'+targetBucket + '/' + stagingPrefix + '/'
    os.system('rclone delete ' + sourceStaging)
    os.system('rclone delete ' + targetStagingDelete)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--sourceRemote')
    parser.add_argument('--sourceBucket')
    parser.add_argument('--targetRemote')
    parser.add_argument('--targetRemoteDelete')
    parser.add_argument('--targetBucket')
    parser.add_argument('--stagingPrefix')
    parser.add_argument('--distPrefix')
    parser.add_argument('--metadataVocab')
    parser.add_argument('--metadataFormat')
    args=parser.parse_args()
    createCombinedDataset(args.sourceRemote, args.sourceBucket, args.targetRemote, args.targetRemoteDelete, 
                          args.targetBucket, args.stagingPrefix, args.distPrefix, args.metadataVocab, args.metadataFormat)

