CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:28Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112616  20190522121836  1901_5055_018                   2C  D   APEX                            2140                            040306                          846 @�V$i@1   @�V%W?�@.8���F�ckƧ1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A��A   A@  A^ffA�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B��B(  B0ffB8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C�fC	�fC�fC�fC�fC  C�C  C  C  C�fC�fC   C"�C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C=�fC@  CB�CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`�Cb  Cd  Ce�fCg�fCj  Cl�Cn�Cp�Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C�  C�  C��D   D � D  D� D  Dy�D��D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD��D� D  D� D  D� D  D� D  D� D  D� D  D� DfDy�D��D� D  D� DfD�fD  Dy�D  D� D  D� DfD� D��D� D  Dy�D  D�fD  D� D  D� D��D � D!fD!�fD"  D"� D#  D#�fD$  D$y�D%  D%�fD&fD&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4�fD5fD5�fD6  D6� D6��D7� D8fD8� D8��D9� D:fD:� D;  D;y�D<  D<� D=  D=� D>  D>� D>��D?� D@fD@� DA  DA� DB  DBy�DC  DCy�DC��DD� DE  DE� DE��DF� DGfDG� DH  DH� DI  DI�fDJ  DJ� DK  DKy�DK��DLy�DM  DM�fDN  DN� DO  DOy�DP  DP�fDQ  DQ� DR  DRy�DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D_��D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy� D���D�FfD�ffD��3D��3D�<�D�,�D���D�� D�  D�� D�� D���D��D�l�D���D��fD�&fD�Vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @`  @���@���A��A4��AS33At��A�ffA�ffA�ffA���A�ffA�ffA�ffA�ffB33B33B33B��B%33B-��B533B<��BE33BM33BU33B]33Be33Bm33Bu33B}33B���B���B���B���B���B�ffB���B���B���B���B���B���B���B���B���B���B�Bƙ�Bʙ�BΙ�Bҙ�B�ffBڙ�Bޙ�B♚B晚BꙚB���B�B���B���B���CL�CL�CL�C33C	33C33C33C33CL�CffCL�CL�CL�C33C33CL�C!ffC#L�C%L�C'L�C)L�C+L�C-33C/L�C1L�C3L�C5L�C7L�C9L�C;L�C=33C?L�CAffCCL�CEL�CGL�CIL�CKffCML�COL�CQL�CSL�CUL�CWL�CYL�C[33C]L�C_ffCaL�CcL�Ce33Cg33CiL�CkffCmffCoffCqffCsffCuL�CwL�CyL�C{L�C}L�CL�C��fC��fC���C���C���C��fC��fC��3C��fC���C���C��fC��fC��fC��fC��3C��fC���C���C���C��fC��fC���C���C���C��fC��fC��fC��fC��fC��fC���C���C��fC��3C��fC���C���C��fC��fC��fC���C���C��fC��fC��fC��fC��3C��fC���C��fC��fC��fC��fC��3C��fC��fC��3C��fC��fC��fC���C���C��fC��fC���C¦fCæfCĦfCŦfCƦfCǦfCȦfCɦfCʦfC˦fC̳3Cͳ3CΦfCϙ�CЦfCѳ3CҦfCӦfCԳ3CզfC֦fCצfCؙ�C٦fCڳ3CۦfCܦfCݙ�CަfC߳3C�fCᙚC�fC�3C�fC�fC�3C�3C�3C�fC�fC�fC�fC��fC�fC�fC�3C�fC�fC�fC��fC��3C��fC��fC���C��fC��3C��fC��fC��fC��3C��fD S3D �3DS3D�3DL�D��DS3D�3DL�D�3DS3D�3DS3D�3DS3D�3DS3D�3D	S3D	�3D
S3D
�3DY�D��DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3DٚDL�D��DS3D�3DS3DٚDY�D�3DL�D�3DS3D�3DS3DٚDS3D��DS3D�3DL�D�3DY�D�3DS3D�3DS3D��D S3D ٚD!Y�D!�3D"S3D"�3D#Y�D#�3D$L�D$�3D%Y�D%ٚD&S3D&�3D'S3D'�3D(L�D(�3D)S3D)�3D*S3D*�3D+S3D+�3D,S3D,ٚD-S3D-�3D.S3D.�3D/S3D/�3D0S3D0��D1L�D1�3D2S3D2�3D3S3D3�3D4Y�D4ٚD5Y�D5�3D6S3D6��D7S3D7ٚD8S3D8��D9S3D9ٚD:S3D:�3D;L�D;�3D<S3D<�3D=S3D=�3D>S3D>��D?S3D?ٚD@S3D@�3DAS3DA�3DBL�DB�3DCL�DC��DDS3DD�3DES3DE��DFS3DFٚDGS3DG�3DHS3DH�3DIY�DI�3DJS3DJ�3DKL�DK��DLL�DL�3DMY�DM�3DNS3DN�3DOL�DO�3DPY�DP�3DQS3DQ�3DRL�DR�3DSY�DS�3DTS3DT�3DUS3DU�3DVS3DV�3DWS3DW�3DXS3DX�3DYS3DY�3DZS3DZ�3D[S3D[�3D\S3D\ٚD]S3D]�3D^S3D^�3D_S3D_��D`L�D`�3DaS3Da�3DbS3Db�3DcS3Dc�3DdS3Dd�3DeS3De�3DfS3Df�3DgS3Dg�3DhS3Dh�3DiS3Di�3DjL�Dj�3DkY�Dk�3DlS3Dl�3DmS3Dm�3DnS3Dn�3DoS3Do�3DpS3Dp�3DqS3Dq�3DrS3Dr�3DsS3Ds�3DtS3Dt�3DuS3Du�3DvS3Dv�3DwFfDys3D��3D�0 D�P D���D���D�&fD�fD��fD�ٚD�	�D�i�Dǩ�D��3D�fD�VfD�fD�� D� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�|�A�~�A˃AˁA˃A˃A˃A˃A˃A˅Aˉ7AˋDAˋDAˋDAˍPAˏ\AˋDAˉ7Aˉ7AˋDAˍPAˋDAˋDAˍPAˏ\Aˏ\Aˏ\AˍPA�t�A� �AʓuA��A���A�A�v�A�hsA�=qA��A¶FA��+A�l�A��A�-A���A��\A�\)A�K�A�(�A��wA���A���A�-A�jA��9A�ffA�+A���A���A��uA�5?A�S�A�{A���A��A��A��A�E�A�^5A��A�|�A��#A��yA��A~��A|��Ay�Au��As/Aq�;Aq&�Ao�;AnA�Aj��AgƨAf�DAc�A`�9A]|�A\n�AY��ASK�AQ?}AL�yAIt�AFn�AC��A@�A@JA<M�A8�!A6��A5�A4��A1�FA,{A)�7A'hsA&�A&ZA&�!A'�A(-A)&�A*Q�A*z�A*�+A*E�A)�A*JA)�A)K�A(�DA'�A&n�A$��A#33A!�;A E�A�yA�mA��A$�A`BA��AffAQ�A��AbA��A��A  AA9XAl�A��A�DA-A��AA�A�-A ~�A (�A ��AS�A��AVAbNA I�@�C�A �!AM�A;dA�^Al�A�AI�A/At�A-AȴAbNA5?A|�AbAl�A ��A E�A�^A5?A(�A5?A7LA�A �A�A�;A5?A�mA|�At�A\)A�AM�A/A�-@��;@��@���@�p�@�j@�v�@�Q�@�A�@�S�@�|�@��@��#@�Z@�1'@�\)@旍@�=q@噚@�%@�@�Z@�I�@�9X@�l�@��@⟾@�~�@�ff@�p�@��@�@�$�@�
=@��@�?}@�j@�bN@��@ߕ�@�33@�^@���@��@� �@��@�r�@ߕ�@��@ް!@�v�@���@�ƨ@�1@��@�M�@ݙ�@���@�$�@�-@��T@���@ۮ@���@ڸR@�
=@��@�G�@���@���@�1@ץ�@֏\@֏\@�
=@���@�v�@�E�@��T@ղ-@Չ7@Չ7@Ձ@�hs@�&�@Լj@ԃ@�A�@��@�dZ@� �@�Z@��@�I�@�b@���@�l�@���@�v�@��@љ�@�`B@�?}@���@�r�@�=q@͑h@�&�@���@�z�@��
@ˍP@��@ʗ�@�V@�M�@�~�@ʇ+@�-@�`B@���@��@��@���@�z�@�  @� �@ȋD@�j@�  @�"�@��@���@�V@ź^@�7L@���@�Ĝ@ě�@�Z@öF@�|�@�"�@�ff@�@��h@�O�@��@���@�r�@�  @�dZ@�E�@�@��@�G�@��@��9@�A�@��
@�+@���@���@��y@���@�E�@���@��@���@�r�@�bN@�9X@�1@��P@��@��@���@���@���@�&�@�r�@�A�@��m@�o@��@�/@�Z@�1@��@���@�"�@���@�M�@��@���@�p�@���@�Z@��m@���@�|�@�\)@��@��!@�ff@��@�@��@�@�V@��@�@��H@�@�&�@�%@���@��u@�b@��@�1'@��
@�dZ@�
=@�5?@���@�p�@�O�@�7L@�/@�/@���@�z�@�1'@���@�"�@��R@�^5@�@��@��#@���@�@��-@�X@��@��/@��u@��u@��u@��u@��@�A�@��!@��^@�&�@�Q�@��m@�l�@�
=@�ȴ@��\@�ff@�J@��^@�p�@�&�@��`@���@�Z@�Q�@�I�@�1'@�b@�ƨ@�dZ@�"�@��y@���@�~�@�$�@�@���@��T@�%@��@� �@��@���@�\)@�@��H@��@���@�v�@�=q@�$�@�{@��@��D@���@��!@u�T@l9X@c33@Z^5@S��@K��@C@:��@5O�@.��@)X@"-@�@%@z�@1'@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A�|�A�~�A˃AˁA˃A˃A˃A˃A˃A˅Aˉ7AˋDAˋDAˋDAˍPAˏ\AˋDAˉ7Aˉ7AˋDAˍPAˋDAˋDAˍPAˏ\Aˏ\Aˏ\AˍPA�t�A� �AʓuA��A���A�A�v�A�hsA�=qA��A¶FA��+A�l�A��A�-A���A��\A�\)A�K�A�(�A��wA���A���A�-A�jA��9A�ffA�+A���A���A��uA�5?A�S�A�{A���A��A��A��A�E�A�^5A��A�|�A��#A��yA��A~��A|��Ay�Au��As/Aq�;Aq&�Ao�;AnA�Aj��AgƨAf�DAc�A`�9A]|�A\n�AY��ASK�AQ?}AL�yAIt�AFn�AC��A@�A@JA<M�A8�!A6��A5�A4��A1�FA,{A)�7A'hsA&�A&ZA&�!A'�A(-A)&�A*Q�A*z�A*�+A*E�A)�A*JA)�A)K�A(�DA'�A&n�A$��A#33A!�;A E�A�yA�mA��A$�A`BA��AffAQ�A��AbA��A��A  AA9XAl�A��A�DA-A��AA�A�-A ~�A (�A ��AS�A��AVAbNA I�@�C�A �!AM�A;dA�^Al�A�AI�A/At�A-AȴAbNA5?A|�AbAl�A ��A E�A�^A5?A(�A5?A7LA�A �A�A�;A5?A�mA|�At�A\)A�AM�A/A�-@��;@��@���@�p�@�j@�v�@�Q�@�A�@�S�@�|�@��@��#@�Z@�1'@�\)@旍@�=q@噚@�%@�@�Z@�I�@�9X@�l�@��@⟾@�~�@�ff@�p�@��@�@�$�@�
=@��@�?}@�j@�bN@��@ߕ�@�33@�^@���@��@� �@��@�r�@ߕ�@��@ް!@�v�@���@�ƨ@�1@��@�M�@ݙ�@���@�$�@�-@��T@���@ۮ@���@ڸR@�
=@��@�G�@���@���@�1@ץ�@֏\@֏\@�
=@���@�v�@�E�@��T@ղ-@Չ7@Չ7@Ձ@�hs@�&�@Լj@ԃ@�A�@��@�dZ@� �@�Z@��@�I�@�b@���@�l�@���@�v�@��@љ�@�`B@�?}@���@�r�@�=q@͑h@�&�@���@�z�@��
@ˍP@��@ʗ�@�V@�M�@�~�@ʇ+@�-@�`B@���@��@��@���@�z�@�  @� �@ȋD@�j@�  @�"�@��@���@�V@ź^@�7L@���@�Ĝ@ě�@�Z@öF@�|�@�"�@�ff@�@��h@�O�@��@���@�r�@�  @�dZ@�E�@�@��@�G�@��@��9@�A�@��
@�+@���@���@��y@���@�E�@���@��@���@�r�@�bN@�9X@�1@��P@��@��@���@���@���@�&�@�r�@�A�@��m@�o@��@�/@�Z@�1@��@���@�"�@���@�M�@��@���@�p�@���@�Z@��m@���@�|�@�\)@��@��!@�ff@��@�@��@�@�V@��@�@��H@�@�&�@�%@���@��u@�b@��@�1'@��
@�dZ@�
=@�5?@���@�p�@�O�@�7L@�/@�/@���@�z�@�1'@���@�"�@��R@�^5@�@��@��#@���@�@��-@�X@��@��/@��u@��u@��u@��u@��@�A�@��!@��^@�&�@�Q�@��m@�l�@�
=@�ȴ@��\@�ff@�J@��^@�p�@�&�@��`@���@�Z@�Q�@�I�@�1'@�b@�ƨ@�dZ@�"�@��y@���@�~�@�$�@�@���@��T@�%@��@� �@��@���@�\)@�@��H@��@���@�v�@�=q@�$�@�{@��@��D@���@��!@u�T@l9X@c33@Z^5@S��@K��@C@:��@5O�@.��@)X@"-@�@%@z�@1'@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	{B	�B	�B	�B	2-B	aHB	�{B	�LB	�B
�B
6FB
<jB
H�B
_;B
�B
��B
��B
��B
��B
�3B
B
�)B  B�B�B�B+B/B�BB
��B
�B
�1B
M�B
9XB
)�B
!�B
+B
H�B
l�B
aHB
K�B
&�B
+B	�mB	��B	�dB	��B	�VB	x�B	iyB	ZB	N�B	E�B	<jB	2-B	'�B	�B	1B��B�B�ZB�B��BÖB�RB�!B�B�B�B��BB��B�BɺBƨB�}B�?B��B��B��B��B�-B�RBȴB�B��B	�B	9XB	?}B	C�B	D�B	Q�B	e`B	q�B	z�B	x�B	s�B	l�B	dZB	\)B	R�B	L�B	J�B	G�B	K�B	L�B	T�B	\)B	p�B	t�B	[#B	,B	�B	uB��BɺB�XB�?B�dB�qB�dBÖB�sB��BŢBɺB�B�BB�mB	bB	oB	B��B	bB	&�B	$�B	6FB	VB	[#B	iyB	�B	�%B	�\B	��B	��B	��B	�JB	�B	� B	� B	�B	��B	�jB	�qB	��B	�
B	�NB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�`B	��B	�XB	��B	�VB	�bB	�\B	�bB	�DB	�DB	�JB	�\B	�hB	�\B	�PB	�PB	�PB	�hB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	��B	ɺB	B	ƨB	��B	��B	��B	��B	��B	��B	��B	�/B	�BB	�/B	�)B	�5B	�;B	�NB	�HB	�;B	�5B	�)B	�B	�NB	�mB	�`B	�TB	�NB	�HB	�;B	�/B	�)B	�BB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
+B
+B
+B
%B
B
B
B
B
%B
%B
%B
%B
%B
%B
	7B
DB
DB

=B
DB
DB
DB
DB

=B

=B

=B

=B
	7B
	7B
	7B

=B
DB
JB

=B
	7B
+B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
	7B

=B
DB
JB
PB
VB
\B
\B
\B
VB
DB
	7B
	7B
1B
+B
+B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B
DB
JB
JB
JB
JB
PB
PB
VB
PB
VB
\B
\B
\B
VB
VB
\B
\B
\B
\B
bB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
+B
.B
6FB
>wB
C�B
H�B
O�B
T�B
\)B
aHB
dZB
jB
o�B
s�B
w�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	{B	�B	�B	�B	7LB	e`B	��B	�RB	�B
(�B
A�B
N�B
_;B
{�B
��B
�B
�'B
�-B
�XB
��B
��B
�BVB'�B%�B,BB�BI�B(�B�B
�HB
ƨB
�^B
cTB
Q�B
E�B
49B
=qB
^5B
�DB
|�B
n�B
B�B
/B
B	��B	�BB	�LB	��B	�bB	�B	iyB	W
B	M�B	G�B	@�B	?}B	-B	�B	{B	%B��B�`B�yB�sB��B��BŢBB��B�wB��B�B�B�B��B��B��BƨB�B�B�B�3B�?BĜB��B�B	\B	9XB	@�B	E�B	F�B	R�B	hsB	w�B	�B	�B	�B	z�B	p�B	gmB	_;B	W
B	S�B	P�B	P�B	R�B	ZB	cTB	{�B	�\B	}�B	8RB	)�B	+B	#�B��B��B�^B�jB�wB�XBB��B�fBƨBŢB��B�5B�`B	{B	�B	B��B		7B	+B	�B	,B	R�B	W
B	dZB	~�B	�B	�VB	��B	��B	�B	��B	�7B	�B	~�B	u�B	�7B	�jB	�dB	�dB	��B	�NB	�B	�B	�B
  B
B	��B	��B	��B	��B	��B	��B	�B	��B	�'B	�VB	��B	��B	��B	�JB	�VB	�PB	�oB	��B	�{B	�\B	�hB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�?B	�B	�B	�B	�B	��B	��B	��B	��B	B	ƨB	��B	��B	��B	��B	��B	��B	��B	�5B	�ZB	�BB	�5B	�5B	�;B	�TB	�ZB	�ZB	�TB	�HB	�
B	�NB	�mB	�sB	�`B	�NB	�ZB	�HB	�BB	�)B	�BB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
B
  B
B
  B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
+B
B
%B
B
%B
B
B
B
B
%B
B
B
B
B
%B
+B
1B
1B

=B
	7B
+B

=B

=B
B
B
B
B
1B
%B
%B
1B
+B
+B

=B
JB
DB

=B
DB
PB
JB
DB
DB

=B
JB

=B

=B
	7B
DB
DB
DB
VB
DB
DB

=B
	7B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
+B

=B

=B
DB
PB
PB
VB
\B
bB
\B
VB
VB
DB
DB
	7B
	7B
1B
+B
+B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
JB
PB
JB
PB
PB
VB
\B
VB
\B
bB
bB
\B
\B
bB
bB
bB
bB
bB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
.B
6FB
?}B
C�B
I�B
O�B
VB
]/B
aHB
e`B
jB
o�B
s�B
x�B
|�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�t�<�1<�`B<���<�o<�o<�C�<���<T��<e`B<�9X<e`B<D��<#�
<T��<�9X<���<�t�<ě�<�t�<���=H�9<�1<ě�<�/<�t�<�C�<�1<�<�/=+<�/=�w<�`B=�P=\)<�1<�C�<�j<���<e`B<#�
<#�
<49X<e`B<�9X<�t�<T��<�1<��
<��
<D��<���=\)<�t�<�h<�9X<��
<���<���<#�
<���<�1<e`B<#�
<D��<�/=o<�C�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<49X<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<���=+<D��<D��<�9X=�P<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�C�<���<��
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.7 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250132012011312501320120113125013  AO  ARGQ                                                                        20111205112616  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125013  IP                  G�O�G�O�G�O�                