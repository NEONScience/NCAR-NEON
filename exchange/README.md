## Dependencies
https://github.com/NEONScience/bald - dev-ddl branch
https://github.com/NEONScience/S3-netcdf-python - issue29 branch
rdflib-jsonld
rclone

## Configuration
Configure rclone remotes for source and target
Configure S3-netcdf-python ~/.s3nc.json for target
Configure keys for target, can pull from above config

## Args
--sourceRemote rclone remote with delete for source
--sourceBucket source bucket containing staged data
--targetRemote rclone remote for target
--targetRemoteDelete rclone remote with delete for target
--targetUrl 
--targetBucket 
--stagingPrefix prefix for staged data
--distPrefix prefix for distributed data
--metadataVocab e.g. http://purl.org/dc/terms/
--metadataFormat e.g. json-ld