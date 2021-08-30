import os
from boto3 import resource
import json


def createBucketListing():
    prefix = "https://neon-ncar.s3.data.neonscience.org/"
    s3 = getTargetS3("neon-ncar-writer")
    bucket = s3.Bucket("neon-ncar")
    listing = 'object,last_modified,etag' + '\n'
    for s3object in bucket.objects.all():
        if s3object.key.endswith('/'):
            continue
        listing += prefix + s3object.key + ',' + s3object.last_modified.strftime("%Y-%m-%d %H:%M:%S") + ',' + s3object.e_tag.strip('"') + '\n' 
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