import os
from boto3 import resource
import json
from google.cloud import storage
import base64
import binascii


def createBucketListing():

    # During migration to GCS (until sync is disabled), list objects from GCS and upload listing.csv to ECS
    gcsPrefix = "https://storage.neonscience.org/"
    gcsBucketName = "neon-ncar"
    storage_client = storage.Client()
    blobs = storage_client.list_blobs(gcsBucketName)
    listing = 'object,last_modified,etag' + '\n'
    for blob in blobs:
        checksum = binascii.hexlify(base64.urlsafe_b64decode(blob.md5_hash)).decode()
        listing += gcsPrefix + gcsBucketName + "/" + blob.name + ',' + blob.updated.strftime("%Y-%m-%d %H:%M:%S") + ',' + checksum + '\n' 

    s3 = getTargetS3("neon-ncar-writer")
    outputObject = s3.Object("neon-ncar", "listing.csv")
    outputObject.put(Body=listing)


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


if __name__ == '__main__':
    createBucketListing()