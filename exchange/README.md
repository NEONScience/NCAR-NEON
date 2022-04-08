### Dependencies
https://github.com/NEONScience/bald - dev-ddl branch  
rdflib-jsonld  
rclone  
google-cloud-storage

### Configuration
Configure rclone remote  

### Args
```
--rcloneRemote         rclone remote with delete for source and target
--sourceBucket         source bucket containing staged data  
--targetBucket         target bucket
--stagingPrefix        prefix for staged data  
--distPrefix           prefix for distributed data  
--metadataVocab        e.g. http://purl.org/dc/terms/  
--metadataFormat       e.g. json-ld  
```
