%% Working with an existing ISMRMRD data set

% This is a simple example of how to reconstruct images from data
% acquired on a fully sampled cartesian grid
%
% Capabilities:
%   2D/3D
%   multiple slices/slabs
%   multiple contrasts, repetitions
%   
% Limitations:
%   only works with a single encoded space
%   fully sampled k-space (no partial fourier or undersampling)
%   multiple repetitions
%   doesn't handle averages, phases, segments and sets
%   ignores noise scans (no pre-whitening)
% 

% We first create a data set using the example program like this:
%   ismrmrd_generate_cartesian_shepp_logan -r 5 -C -o shepp-logan.h5
% This will produce the file shepp-logan.h5 containing an ISMRMRD
% dataset sampled evenly on the k-space grid -128:0.5:127.5 x -128:127
% (i.e. oversampled by a factor of 2 in the readout direction)
% with 8 coils, 5 repetitions and a noise level of 0.5
% with a noise calibration scan at the beginning
%
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Loading an existing file %
%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%filename = 'brain.h5';
filename = 'knee.h5';
if exist(filename, 'file')
    dset = ismrmrd.Dataset(filename, 'dataset');
else
    error(['File ' filename ' does not exist.  Please generate it.'])
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read some fields from the XML header %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We need to check if optional fields exists before trying to read them

hdr = ismrmrd.xml.deserialize(dset.readxml);

% Encoding and reconstruction information
% Matrix size
enc_Nx = hdr.encoding.encodedSpace.matrixSize.x;
enc_Ny = hdr.encoding.encodedSpace.matrixSize.y;
enc_Nz = hdr.encoding.encodedSpace.matrixSize.z;
rec_Nx = hdr.encoding.reconSpace.matrixSize.x;
rec_Ny = hdr.encoding.reconSpace.matrixSize.y;
rec_Nz = hdr.encoding.reconSpace.matrixSize.z;

% Field of View
% ich glaube rec_FOV braucht man nicht unbedingt
enc_FOVx = hdr.encoding.encodedSpace.fieldOfView_mm.x;
enc_FOVy = hdr.encoding.encodedSpace.fieldOfView_mm.y;
enc_FOVz = hdr.encoding.encodedSpace.fieldOfView_mm.z;
rec_FOVx = hdr.encoding.reconSpace.fieldOfView_mm.x;
rec_FOVy = hdr.encoding.reconSpace.fieldOfView_mm.y;
rec_FOVz = hdr.encoding.reconSpace.fieldOfView_mm.z;

% Number of slices, coils, repetitions, contrasts etc.
% We have to wrap the following in a try/catch because a valid xml header may
% not have an entry for some of the parameters

try
  nSlices = hdr.encoding.encodingLimits.slice.maximum + 1;
catch
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

% TODO add the other possibilities

% Read all the data
% Reading can be done one acquisition (or chunk) at a time, 
% but this is much faster for data sets that fit into RAM.
tic
D = dset.readAcquisition();
toc
% Note: can select a single acquisition or header from the block, e.g.
% acq = D.select(5);
% hdr = D.head.select(5);
% or you can work with them all together

% Ignore noise scans
% TODO add a pre-whitening example
% Find the first non-noise scan
% This is how to check if a flag is set in the acquisition header
isNoise = D.head.flagIsSet('ACQ_IS_NOISE_MEASUREMENT');
firstScan = find(isNoise==0,1,'first');
if firstScan > 1
    noise = D.select(1:firstScan-1);
else
    noise = [];
end
meas  = D.select(firstScan:D.getNumber);
clear D;

%% Reconstruct images
% Wir brauchen nicht den ganzen k-Raum für die demo, wählen einfach eine
% slice, eine spule und einen contrast

if nSlices > 1
    slice = 10; %irgendein slice
else
    slice = 1; %für 3D FT encoding, aber klappt bisher nicht
end
rep = 1;
contrast = 1;
coil = 1;


% Initialize the K-space storage array
K = zeros(enc_Nx, enc_Ny, enc_Nz, nCoils);

% Select the appropriate measurements from the data
acqs = find(  (meas.head.idx.contrast==(contrast-1)) ...
            & (meas.head.idx.repetition==(rep-1)) ...
            & (meas.head.idx.slice==(slice-1)));
%etwas komisch das es meas.head.idx.coil nicht gibt

for p = 1:length(acqs)
    ky = meas.head.idx.kspace_encode_step_1(acqs(p)) + 1;
    kz = meas.head.idx.kspace_encode_step_2(acqs(p)) + 1;
    K(:,ky,kz,:) = meas.data{acqs(p)};
    %offenbar ist hier die konvention das der slice selection gradient in x
    %richtung geht
    %komisch, kann mir vorstellen dass da was schiefgehen kann
end

%wir wählen hier schonmal nur eine coil aus,
%wenn nCoil>1 muss man am ende noch das bild über alle coils averagen
K_slice = reshape(K(:, :, :, 1),enc_Nx, enc_Ny, enc_Nz); 

%%
if nSlices>1
  
    figure
    colormap gray
    im_test = abs(fftshift(ifft2(fftshift(K_slice))));
    imagesc(im_test); axis image; axis off; colorbar;
    %sieht evtl nicht schön aus, wenn es nur von einer surface coil kommt
else
  
    %das währe wenn es nicht slices sind sondern 3d FT aber funktioniert
    %nicht
    figure
    colormap gray
    im_test = abs(fftshift(fft3(fftshift(K_slice))));
    imagesc(im_test); axis image; axis off; colorbar;
end

    
%%

k_filename = 'K_slice_test.h5';
if exist(k_filename,'file')
    delete(k_filename);
end
h5create('K_slice_test.h5', '/K_slice_real', size(K_slice), 'Datatype', 'double');
h5write('K_slice_test.h5', '/K_slice_real', real(K_slice));
h5create('K_slice_test.h5', '/K_slice_imag', size(K_slice), 'Datatype', 'double');
h5write('K_slice_test.h5', '/K_slice_imag', imag(K_slice));


