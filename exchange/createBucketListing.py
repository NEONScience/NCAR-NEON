from google.cloud import storage
import base64
import binascii


def createBucketListing():

    gcsPrefix = "https://storage.neonscience.org/"
    gcsBucketName = "neon-ncar"
    storage_client = storage.Client()
    blobs = storage_client.list_blobs(gcsBucketName)
    listing = 'object,last_modified,etag' + '\n'
    for blob in blobs:
        checksum = binascii.hexlify(base64.urlsafe_b64decode(blob.md5_hash)).decode()
        listing += gcsPrefix + gcsBucketName + "/" + blob.name + ',' + blob.updated.strftime("%Y-%m-%d %H:%M:%S") + ',' + checksum + '\n' 

    gcsBucket = storage_client.bucket(gcsBucketName)
    listingBlob = gcsBucket.blob("listing.csv")
    listingBlob.upload_from_string(listing)


if __name__ == '__main__':
    createBucketListing()