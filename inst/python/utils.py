from google.cloud import storage
from google.cloud.storage import Blob
from google.auth.transport.requests import AuthorizedSession
from google.resumable_media import requests, common

## The return value is a blob
def get_input_stream(credentials, bucket_name, file_name):
    if(credentials==""):
        client = storage.Client.create_anonymous_client()
    else:
        client = storage.Client.from_service_account_json(credentials)
    bucket = client.get_bucket(bucket_name)
    return bucket.get_blob(file_name)

def read_stream(stream,start,end):
    return  bytearray(stream.download_as_string(start = int(start), end = int(end)))



## The return value is a upload stream
def get_output_stream(credentials, bucket_name, file_name,content_type, chunk_size):
    client = storage.Client.from_service_account_json(credentials)
    stream=GCSObjectStreamUpload(client = client,
                            bucket_name = bucket_name,
                            blob_name =file_name,
                            content_type = content_type,
                            chunk_size = chunk_size)
    return stream


def open_output_stream(stream):
    stream.start()

def close_output_stream(stream):
    stream.stop()

def write_stream(stream, data):
    stream.write(data)


## The class is from https://dev.to/sethmlarson/python-data-streaming-to-google-cloud-storage-with-resumable-uploads-458h
## with slight changes.
class GCSObjectStreamUpload(object):
    def __init__(
            self, 
            client: storage.Client,
            bucket_name: str,
            blob_name: str,
            content_type: str,
            chunk_size: int
        ):
        self._client = client
        self._bucket = self._client.bucket(bucket_name)
        self._blob = self._bucket.blob(blob_name)

        self._buffer = b''
        self._buffer_size = 0
        self._chunk_size = chunk_size
        self._read = 0
        
        self._transport = AuthorizedSession(
            credentials=self._client._credentials
        )
        self._request = None  # type: requests.ResumableUpload
        self._content_type = content_type

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, *_):
        if exc_type is None:
            self.stop()

    def start(self):
        url = (
            f'https://www.googleapis.com/upload/storage/v1/b/'
            f'{self._bucket.name}/o?uploadType=resumable'
        )
        self._request = requests.ResumableUpload(
            upload_url=url, chunk_size=self._chunk_size
        )
        self._request.initiate(
            transport=self._transport,
            content_type=self._content_type,
            stream=self,
            stream_final=False,
            metadata={'name': self._blob.name},
        )

    def stop(self):
        self._request.transmit_next_chunk(self._transport)
        
    def write(self, data: bytes) -> int:
        data_len = len(data)
        self._buffer_size += data_len
        self._buffer += data
        del data
##        while self._buffer_size >= self._chunk_size:
##            try:
##                self._request.transmit_next_chunk(self._transport)
##            except common.InvalidResponse:
##                self._request.recover(self._transport)

    def read(self, chunk_size: int) -> bytes:
        to_read = min(chunk_size, self._buffer_size)
        memview = memoryview(self._buffer)
        self._buffer = memview[to_read:].tobytes()
        self._read += to_read
        self._buffer_size -= to_read
        return memview[:to_read].tobytes()
    
    def tell(self) -> int:
        return self._read



credentials="D:\\OneDrive\\keys\\google_cloud.json"
bucket_name ="bioconductor_test"
file_name="test1"
content_type="application/octet-stream"
chunk_size=256 *1024
client = storage.Client.from_service_account_json(credentials)
