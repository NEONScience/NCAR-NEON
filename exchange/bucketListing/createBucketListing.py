from google.cloud import storage
import base64
import binascii
import argparse


def createBucketListing(project, bucket):

    gcsPrefix = "https://storage.neonscience.org/"
    gcsBucketName = bucket
    storage_client = storage.Client(project)
    blobs = storage_client.list_blobs(gcsBucketName)
    listing = 'object,last_modified,etag' + '\n'
    for blob in blobs:
        checksum = binascii.hexlify(base64.urlsafe_b64decode(blob.md5_hash)).decode()
        listing += gcsPrefix + gcsBucketName + "/" + blob.name + ',' + blob.updated.strftime("%Y-%m-%d %H:%M:%S") + ',' + checksum + '\n' 

    gcsBucket = storage_client.bucket(gcsBucketName)
    listingBlob = gcsBucket.blob("listing.csv")
    listingBlob.upload_from_string(listing)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--project')
    parser.add_argument('--bucket')
    args=parser.parse_args()
    createBucketListing(args.project, args.bucket)