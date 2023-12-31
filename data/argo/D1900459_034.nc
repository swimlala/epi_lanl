CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS   g   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
Argo float     history       14-Jul-2022 03:11:12Zcreation      
references        (http://www.argodatamgt.org/Documentation   comment       bThis netCDF file is generated using BODC's argoReader and netCDF writer software (argo@bodc.ac.uk)     user_manual_version       3.4    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    <H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    <X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    <\   REFERENCE_DATE_TIME                	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    <`   DATE_CREATION                  	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    <p   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    <�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    <�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  <�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  <�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  =   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        =H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    =L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    =P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     =T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    =t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    =x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     =|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     =�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     =�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    =�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
_FillValue        A.�~       
resolution        >�E�vQ�        =�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    =�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        =�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            =�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            =�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    >   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    >   VERTICAL_SAMPLING_SCHEME                   	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    >   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ?   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ?   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        axis      Z      
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  ?    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  @�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  BX   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  C�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  D\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  D�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  E,   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  F�   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  Hd   PRES_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PRES_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                  h  J    PSAL_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PSAL_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                  h  Jh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   standard_name         TEMP_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                  h  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PRES_ADJUSTED_ERROR    units         decibar    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  K8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PSAL_ADJUSTED_ERROR    units         psu    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  L�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         TEMP_ADJUSTED_ERROR    units         degree_Celsius     conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  Np   	PARAMETER               	            	long_name         /List of parameters with calibration information    source_name       	PARAMETER      conventions       Argo reference table 3     
_FillValue                  0  P   SCIENTIFIC_CALIB_EQUATION               	             	long_name         'Calibration equation for this parameter    source_name       SCIENTIFIC_CALIB_EQUATION      
_FillValue                    P<   SCIENTIFIC_CALIB_COEFFICIENT            	             	long_name         *Calibration coefficients for this equation     source_name       SCIENTIFIC_CALIB_COEFFICIENT   
_FillValue                    S<   SCIENTIFIC_CALIB_COMMENT            	             	long_name         .Comment applying to this parameter calibration     source_name       SCIENTIFIC_CALIB_COMMENT   
_FillValue                    V<   SCIENTIFIC_CALIB_DATE               	            	long_name         Date of calibration    source_name       SCIENTIFIC_CALIB_DATE      conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Y<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     source_name       HISTORY_INSTITUTION    conventions       Argo reference table 4     
_FillValue                    Yh   HISTORY_STEP                     	long_name         Step in data processing    source_name       HISTORY_STEP   conventions       Argo reference table 12    
_FillValue                    Yx   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    source_name       HISTORY_SOFTWARE   conventions       Institution dependent      
_FillValue                    Y�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     source_name       HISTORY_SOFTWARE_RELEASE   conventions       Institution dependent      
_FillValue                    Y�   HISTORY_REFERENCE                        	long_name         Reference of database      source_name       HISTORY_REFERENCE      conventions       Institution dependent      
_FillValue                    Y�   HISTORY_DATE                     	long_name         #Date the history record was created    source_name       HISTORY_DATE   conventions       YYYYMMDDHHMISS     
_FillValue                  8  Z�   HISTORY_ACTION                       	long_name         Action performed on data   source_name       HISTORY_ACTION     conventions       Argo reference table 7     
_FillValue                    Z�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   source_name       HISTORY_PARAMETER      conventions       Argo reference table 3     
_FillValue                  @  Z�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   source_name       HISTORY_START_PRES     units         decibar    
_FillValue        G�O�        [0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    source_name       HISTORY_STOP_PRES      units         decibar    
_FillValue        G�O�        [@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    source_name       HISTORY_PREVIOUS_VALUE     
_FillValue        G�O�        [P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   source_name       HISTORY_QCTEST     conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  @  [`Argo profile    3.1 1.2 19500101000000  20220714031113  20220714031113  1900459 Argo UK                                                         Jon Turton                                                      PSAL            TEMP            PRES               "A   BO  9181                            2C  D   PROVOR                          OIN 03-F206                     n/a                             842 @��I�q�1   @��I�q��D[��S��@DK��S��1   ARGOS   Primary sampling: averaged                                                                                                                                                                                                                                      ����A   B   A   @�  Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CR  Cf  Cz  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D� D	� D� D� D� D� D"� D'� D,� D1� D6� D;� D@� DE� DJ� DO� DT� DY� D^� Dc� Dh� Dm� Dr� Dw� D|� D�� D�` D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�@ D�� D�� D�� D�� D�� D�� D�� D�� B�B��B�B��Bz�BuNBl�BsBB��B�B��B��B��B�oB�gB�_BlzBN�B?�B.�B2:BLB`�BD�B'�B�B
�UB
�5B
�B
�gB
��B
JB
g�B
YEB
:�B
$B
�B	�sB	�UB	��B	�B	�B	��B	�B	��B	��B	�qB	�B	��B	�iB	��B	��B	��B	��B	�%B	��B	�eB	�B	��B	�B	�OB	�/B	��B	��B	�B	�6B	�B
�B
B
IB
,%B
9zB
?�B
DB
I�B
T~B
a�B
l�B
xpB
�qB
��B
�>B
�|B
}�B
�{B
��B
��B
��B
³B
ȫB
�]B
��B
��B
��B
�jB
�nB,B
�B�BB#/B)pAc;dAcC�Ac7LAc&�Ab��AbI�Aa�wA_��AP��AG;dAD�jADn�AB��A@��A=XA8�!A6JA2��A0JA-�;A+�wA*�`A*�`A*1A%�
A!�7A�Av�A�A�
Av�A
��Al�A�/@��@�@�  @��@�?}@��@��T@�t�@� �@�+@��@�$�@��7@�x�@��@��!@�|�@�@��^@��+@��w@�7L@�o@�33@�`B@~��@xr�@r�@o+@j^5@g+@e�T@ax�@YX@W+@T�@Q��@Rn�@Q�7@O�@K��@G��@D�/@B�H@AG�@@b@?\)@>�y@>��@=O�@<j@<Z@;t�@9��@8 �@65?@4�@3"�@1�@/�@.5?@-��@,�@+t�@*^5@)X@(1'@&�y@&$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 1111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CR  Cf  Cz  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D� D	� D� D� D� D� D"� D'� D,� D1� D6� D;� D@� DE� DJ� DO� DT� DY� D^� Dc� Dh� Dm� Dr� Dw� D|� D�� D�` D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�@ D�� D�� D�� D�� D�� D�� D�� D�� BYBY�BYBV�BO�BJLBA�BHB�B��B�BԽB��B��B�mBreBa]BAyB#�B�B�B9B!B5�B�B
��B
�B
�UB
�6B
�B
yhB
e�B
TLB
= B
.HB
�B	�	B	��B	�xB	�[B	��B	��B	zB	oB	iB	f�B	a�B	\yB	_B	^�B	fqB	h�B	e�B	k�B	u�B	~,B	��B	�kB	�B	��B	�B	�UB	�4B	��B	˅B	�B	�;B	�B	ۍB	�B	�MB
(B
}B
�B
B
�B
)�B
6�B
A�B
MrB
VsB
_�B
h?G�O�G�O�B
}|B
��B
��B
��B
��B
��B
�]B
��B
��B
��B
�jB
�nB
�,B
ߢB
��B
�B
�.B
�oAc;dAcC�Ac7LAc&�Ab��AbI�Aa�wA_��AP��AG;dAD�jADn�AB��A@��A=XA8�!A6JA2��A0JA-�;A+�wA*�`A*�`A*1A%�
A!�7A�Av�A�A�
Av�A
��Al�A�/@��@�@�  @��@�?}@��@��T@�t�@� �@�+@��@�$�@��7@�x�@��@��!@�|�@�@��^@��+@��w@�7L@�o@�33@�`B@~��@xr�@r�@o+@j^5@g+@e�T@ax�@YX@W+@T�@Q��@Rn�@Q�7@O�@K��@G��@D�/@B�H@AG�@@b@?\)@>�y@>��@=O�@<j@<Z@;t�@9��@8 �@65?@4�@3"�@1�@/�@.5?@-��@,�@+t�@*^5@)X@(1'@&�y@&$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 1111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL - dS                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                             ds=0.042                                                                                                                                                                                                                                                       Float that auto-corrects for pressure drifts. Calibration error is manufacturer specified accuracy.                                                                                                                                                             The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   OWC(2018v01). Mapping scales LON4/2.8 LAT 3/2 MAPPINGSCALE_PHI 0.1/0.02 MAPPINGSCALE_AGE 5/10 MAP_P_DELTA 150                                                                                                                                                   201009171616402022071310204920220713102049  BO  BO  BO  BO  ARGQARGQARSQARSQSCUTSCUTnullOW  2.0 2.0 null0.1                                                                                                                                                                                                                                                                 20051107163057200511071630572010091716164020220713102049QCF$QCF$IP  IP  PSAL            PSAL                                            Ea� Ea� G�O�G�O�Ea� Ea� G�O�G�O�Ft Ft Ft Ft 131072          131072                                          