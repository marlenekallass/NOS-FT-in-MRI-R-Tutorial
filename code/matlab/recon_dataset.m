%% Adapted from: https://github.com/ismrmrd/ismrmrd/blob/master/examples/matlab/recon_dataset.m

%% Read datast

addpath('external')

filename = fullfile('..', '..', 'rawData', 'testdata.h5');

if exist(filename, 'file')
    dset = ismrmrd.Dataset(filename, 'dataset');
else
    error(['File ' filename ' does not exist.  Please generate it.'])
end

% Read xml header
hdr = ismrmrd.xml.deserialize(dset.readxml);

% Encoding and reconstruction information
enc_Nx = hdr.encoding.encodedSpace.matrixSize.x;
enc_Ny = hdr.encoding.encodedSpace.matrixSize.y;
enc_Nz = hdr.encoding.encodedSpace.matrixSize.z;

% Number of slices, coils, repetitions, contrasts etc.
try
  nSlices = hdr.encoding.encodingLimits.slice.maximum + 1;
catch
    %Note that the script will not work with non-slice encoded data
    nSlices = 1; 
end

try 
    nCoils = hdr.acquisitionSystemInformation.receiverChannels;
catch
    nCoils = 1;
end

try
    nReps = hdr.encoding.encodingLimits.repetition.maximum + 1;
catch
    nReps = 1;
end

try
    nContrasts = hdr.encoding.encodingLimits.contrast.maximum + 1 + 1;
catch
    nContrasts = 1;
end


D = dset.readAcquisition();

% Ignore noise scans
isNoise = D.head.flagIsSet('ACQ_IS_NOISE_MEASUREMENT');
firstScan = find(isNoise==0,1,'first');
if firstScan > 1
    noise = D.select(1:firstScan-1);
else
    noise = [];
end
meas  = D.select(firstScan:D.getNumber);
clear D;

%% Get k-space matrix
% We only need a single slice matrix for the demo

rep = 1;
contrast = 1;
coil = 1;

slice = 19; % Nice slice for foot dataset


% Initialize the K-space storage array
K = zeros(enc_Nx, enc_Ny, enc_Nz, nCoils);

% Select the appropriate measurements from the data
acqs = find(  (meas.head.idx.contrast==(contrast-1)) ...
            & (meas.head.idx.repetition==(rep-1)) ...
            & (meas.head.idx.slice==(slice-1)));

for p = 1:length(acqs)
    ky = meas.head.idx.kspace_encode_step_1(acqs(p)) + 1;
    kz = meas.head.idx.kspace_encode_step_2(acqs(p)) + 1;
    K(:,ky,kz,:) = meas.data{acqs(p)};
end

k_slice = reshape(K(:, :, 1, coil),enc_Nx, enc_Ny); 

figure
colormap gray
imagesc(abs(k_slice));
axis off;
pbaspect([1 1 1]);
%% Reconstruct and plot image
figure
colormap gray
im_test = abs(fftshift(ifft2(fftshift(k_slice))));
imagesc(im_test); 
axis off;
pbaspect([1 1 1]);
    
%% Export k-space matrix

filename = fullfile('data','k_slice.h5');

if exist(filename,'file')
    delete(filename);
end

h5create(filename, '/K_slice_real', size(k_slice), 'Datatype', 'double');
h5write(filename, '/K_slice_real', real(k_slice));
h5create(filename, '/K_slice_imag', size(k_slice), 'Datatype', 'double');
h5write(filename, '/K_slice_imag', imag(k_slice));


