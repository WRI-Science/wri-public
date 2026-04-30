import os

WRI_PROJECT_ROOT = os.environ.get("WRI_PROJECT_ROOT", "/home/shares/wwri-wildfire")

#### Goal ####
# The goal of this script is to download the raw scanfi data for further processing.
# Once this has been done it does not need to be run again unless there is an update to the data.

#### Packages ####
from ftplib import FTP
import os

#### Setup and File Paths ####
natural_habitats_base_path = os.path.join(WRI_PROJECT_ROOT, "data", "natural_habitats")

raw_data_save_path = natural_habitats_base_path + "raw/scanfi/"

# data url
ftp_url = "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/"

#### Functions ####
def download_ftp_directory(ftp_url, local_download_folder):
    # Extract hostname and path
    ftp_host = ftp_url.split('/')[2]
    ftp_path = '/'.join(ftp_url.split('/')[3:])
    
    # Connect to FTP server
    ftp = FTP(ftp_host)
    ftp.login()
    ftp.cwd(ftp_path)
    
    # Ensure local download folder exists
    os.makedirs(local_download_folder, exist_ok=True)
    
    # List files in the directory
    files = ftp.nlst()
    
    for file in files:
        local_file_path = os.path.join(local_download_folder, file)
        
        with open(local_file_path, 'wb') as f:
            ftp.retrbinary(f'RETR {file}', f.write)
            print(f"Downloaded: {file}")
    
    ftp.quit()
    print("All files downloaded successfully.")

#### Processing ####
print("Downloading...")
download_ftp_directory(ftp_url, raw_data_save_path)
print("Finished downloading!")
