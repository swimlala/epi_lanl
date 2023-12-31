CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  0   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:41Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  >0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  D    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  EP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  N�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  T�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  _p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  e`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  kP   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    k�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    n�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    q�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  t�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    t�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    t�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    t�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    t�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  t�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    t�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    u   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    u   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         u    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         u$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        u(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    u,Argo profile    3.1 1.2 19500101000000  20181024140841  20181024140841  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�021   @��ey\��@5M�����dx���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP�CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D	y�D
  D
� D  D� D  D� D  D� D��Dy�D  D�fDfD� D��D� D  D� DfD� D  D� D  D� D  D� D  Dy��D�C�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@ƸRA\)A#\)AC\)AaA��A��A��GA��A��AѮA�A�B �
B�
B�
B�
B �
B)=pB0�
B8�
B@�
BH�
BP�
BX�
B`�
Bh�
Bq=pBx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL)CN5�CPO]CR5�CT5�CV5�CX5�CZO]C\5�C^5�C`5�Cb5�Cd5�CfO]Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C�'�C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C�C��C��C��C��C��C��C��C��C��C��C�'�C�'�C��C��C��C��C��C��C��C�C�C��C��C��C��C��C��C��C�C�C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD�D�qD	D	�D
qD
�qDqD�qDqD�qDqD�qDD�DqD��D�D�qDD�qDqD�qD�D�qDqD�qDqD�qDqD�qDqDy�4D�J=D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aχ+AσAω7Aϕ�Aϗ�Aϙ�Aϛ�Aϟ�Aϝ�Aϙ�Aϝ�AϬAϣ�Aϥ�AϮAϲ-Aϴ9Aϴ9AϸRA϶FA϶FAϺ^A�ȴA��A���Aϝ�AζFA���A�ffA�^5A�\)A�(�A���A��A�M�A��A�jA�
=A��TAʃA���A�33Aƥ�AŋDA��A�ZA��DA�x�A�S�A�l�A� �A�bNA�-A���A�x�A�O�A���A�7LA��RA�%A��HA��A�ZA�-A�~�A��\A���A���A�JA�
=A��mA�ĜA��;A��A�A�A�+A��
A��TA�  A�|�A�1'A���A�VA���A��jA�x�A��DA���A�v�A�VA��A�JA���A��hA���A�+A��yA�/A���A�"�A}�#A{Aw��Aw�At�/Ar�ArJAp��Ap-Al-Ai�
Ai�FAg��Ac�
AcS�Abv�A`bNA^~�A\�AX�yAV�!AT�yAS��AS�AR^5AP�yANI�AL  AK�AKO�AG\)AFE�AD��AB�\A@�DA?��A>��A;�A:JA9
=A8�A77LA6jA5�;A5;dA4r�A3��A25?A/"�A-O�A,5?A+XA+A*v�A)`BA(~�A(1'A'��A'G�A&��A%�A$1'A!��A A�A�;A�A+AA��A��A��AffAA�A�
A��A�DAffAI�AA��A?}A�A�AK�A�
A&�A(�A�At�A�Ar�A��A��A/A�A��A
�HA
�uA	�mA	?}A��A�!A^5A��A��At�A��A��At�AG�A 9X@��F@�|�@�C�@�+@��y@�ff@���@���@���@���@�(�@�ƨ@�33@�R@�@�r�@�V@�x�@���@��@�Q�@畁@���@�Ĝ@��;@�R@�V@�A�@�1'@�1@ߥ�@�l�@���@���@�r�@���@��@ّh@١�@���@���@�K�@�K�@��m@�|�@�n�@�J@�V@Դ9@�1'@��y@�hs@�V@Гu@�Z@�ƨ@��y@�@�O�@�Z@���@��;@�ƨ@ɉ7@ȣ�@�ƨ@ǍP@�M�@��@�`B@��@�I�@��m@��D@�7L@�1@�v�@�@���@���@���@�~�@��@� �@��H@�p�@�Z@���@���@�O�@�3�@{|�@ie,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aχ+AσAω7Aϕ�Aϗ�Aϙ�Aϛ�Aϟ�Aϝ�Aϙ�Aϝ�AϬAϣ�Aϥ�AϮAϲ-Aϴ9Aϴ9AϸRA϶FA϶FAϺ^A�ȴA��A���Aϝ�AζFA���A�ffA�^5A�\)A�(�A���A��A�M�A��A�jA�
=A��TAʃA���A�33Aƥ�AŋDA��A�ZA��DA�x�A�S�A�l�A� �A�bNA�-A���A�x�A�O�A���A�7LA��RA�%A��HA��A�ZA�-A�~�A��\A���A���A�JA�
=A��mA�ĜA��;A��A�A�A�+A��
A��TA�  A�|�A�1'A���A�VA���A��jA�x�A��DA���A�v�A�VA��A�JA���A��hA���A�+A��yA�/A���A�"�A}�#A{Aw��Aw�At�/Ar�ArJAp��Ap-Al-Ai�
Ai�FAg��Ac�
AcS�Abv�A`bNA^~�A\�AX�yAV�!AT�yAS��AS�AR^5AP�yANI�AL  AK�AKO�AG\)AFE�AD��AB�\A@�DA?��A>��A;�A:JA9
=A8�A77LA6jA5�;A5;dA4r�A3��A25?A/"�A-O�A,5?A+XA+A*v�A)`BA(~�A(1'A'��A'G�A&��A%�A$1'A!��A A�A�;A�A+AA��A��A��AffAA�A�
A��A�DAffAI�AA��A?}A�A�AK�A�
A&�A(�A�At�A�Ar�A��A��A/A�A��A
�HA
�uA	�mA	?}A��A�!A^5A��A��At�A��A��At�AG�A 9X@��F@�|�@�C�@�+@��y@�ff@���@���@���@���@�(�@�ƨ@�33@�R@�@�r�@�V@�x�@���@��@�Q�@畁@���@�Ĝ@��;@�R@�V@�A�@�1'@�1@ߥ�@�l�@���@���@�r�@���@��@ّh@١�@���@���@�K�@�K�@��m@�|�@�n�@�J@�V@Դ9@�1'@��y@�hs@�V@Гu@�Z@�ƨ@��y@�@�O�@�Z@���@��;@�ƨ@ɉ7@ȣ�@�ƨ@ǍP@�M�@��@�`B@��@�I�@��m@��D@�7L@�1@�v�@�@���@���@���@�~�@��@� �@��H@�p�@�Z@���@���@�O�@�3�@{|�@ie,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ɺB
ɺB
��B
��B
��B
��B
��B
��B
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�#B
�B	7B33Bm�B��B�B�B�B�'B�-B�^B�B�B=qBS�B[#Bk�B{�Bt�Bl�BcTBW
BVBS�BT�BYBYBXBZBdZBl�Bu�Br�Bk�BcTB`BB]/BZBW
BR�BK�B8RB8RB9XB5?B1'B+B�BJB��B�B�fB�BB��BÖB�^BBŢBB�^B�FB��B�PBt�Be`BN�B+B	7B
�/B
ŢB
�}B
�B
��B
|�B
]/B
<jB
+B
�B
B	�B	��B	�HB	�B	�
B	��B	ƨB	�LB	�B	��B	��B	�B	~�B	w�B	iyB	^5B	P�B	?}B	2-B	&�B	"�B	�B	�B	bB	B��B��B�B�fB�NB�5B�)B�#B�B��B��BȴBƨBĜBB��B�wB�jB�^B�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�JB�DB�=B�7B�7B�7B�7B�1B�1B�+B�+B�+B�7B�7B�1B�1B�7B�%B~�Bz�Bw�Bs�Bq�Bm�BjBffBdZBbNBaHB`BB_;B^5B]/B]/B\)B\)B]/B]/B]/B\)B[#B[#BYBYBYBW
B\)B_;B_;B_;B_;B_;B_;B^5B^5B_;BaHBbNBbNBbNBbNBaHB_;B]/B_;B^5B^5BaHBbNB`BB^5B^5B]/B]/B_;BaHBaHBaHBaHB`BBaHBffBgmBjBq�Bt�Bw�B{�B}�B~�B�B�%B�B�%B�+B�DB�JB�PB�hB��B��B��B�{B�uB�hB�bB�VB�PB�JB�DB�=B�%B�B�B}�By�Br�Bm�Bm�Bm�Br�B~�B�DB�uB��B�uB�\B�=B�DB�JB��B�hB�VB�DB�7B�1B�1B�7B	�xB
)B
*01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
ɺB
ɺB
��B
��B
��B
��B
��B
��B
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�#B
�B	7B33Bm�B��B�B�B�B�'B�-B�^B�B�B=qBS�B[#Bk�B{�Bt�Bl�BcTBW
BVBS�BT�BYBYBXBZBdZBl�Bu�Br�Bk�BcTB`BB]/BZBW
BR�BK�B8RB8RB9XB5?B1'B+B�BJB��B�B�fB�BB��BÖB�^BBŢBB�^B�FB��B�PBt�Be`BN�B+B	7B
�/B
ŢB
�}B
�B
��B
|�B
]/B
<jB
+B
�B
B	�B	��B	�HB	�B	�
B	��B	ƨB	�LB	�B	��B	��B	�B	~�B	w�B	iyB	^5B	P�B	?}B	2-B	&�B	"�B	�B	�B	bB	B��B��B�B�fB�NB�5B�)B�#B�B��B��BȴBƨBĜBB��B�wB�jB�^B�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�JB�DB�=B�7B�7B�7B�7B�1B�1B�+B�+B�+B�7B�7B�1B�1B�7B�%B~�Bz�Bw�Bs�Bq�Bm�BjBffBdZBbNBaHB`BB_;B^5B]/B]/B\)B\)B]/B]/B]/B\)B[#B[#BYBYBYBW
B\)B_;B_;B_;B_;B_;B_;B^5B^5B_;BaHBbNBbNBbNBbNBaHB_;B]/B_;B^5B^5BaHBbNB`BB^5B^5B]/B]/B_;BaHBaHBaHBaHB`BBaHBffBgmBjBq�Bt�Bw�B{�B}�B~�B�B�%B�B�%B�+B�DB�JB�PB�hB��B��B��B�{B�uB�hB�bB�VB�PB�JB�DB�=B�%B�B�B}�By�Br�Bm�Bm�Bm�Br�B~�B�DB�uB��B�uB�\B�=B�DB�JB��B�hB�VB�DB�7B�1B�1B�7B	�xB
)B
*01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140841                              AO  ARCAADJP                                                                    20181024140841    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140841  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140841  QCF$                G�O�G�O�G�O�0               