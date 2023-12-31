CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
Argo float     history       14-Jul-2022 03:10:05Zcreation      
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
resolution        ?PbM���     �  B�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  DT   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  D�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  E4   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  E�   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  G`   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  I   PRES_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PRES_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                  p  J�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PSAL_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                  p  KH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   standard_name         TEMP_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                  p  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PRES_ADJUSTED_ERROR    units         decibar    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  L(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PSAL_ADJUSTED_ERROR    units         psu    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  M�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         TEMP_ADJUSTED_ERROR    units         degree_Celsius     conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  O�   	PARAMETER               	            	long_name         /List of parameters with calibration information    source_name       	PARAMETER      conventions       Argo reference table 3     
_FillValue                  0  Q\   SCIENTIFIC_CALIB_EQUATION               	             	long_name         'Calibration equation for this parameter    source_name       SCIENTIFIC_CALIB_EQUATION      
_FillValue                    Q�   SCIENTIFIC_CALIB_COEFFICIENT            	             	long_name         *Calibration coefficients for this equation     source_name       SCIENTIFIC_CALIB_COEFFICIENT   
_FillValue                    T�   SCIENTIFIC_CALIB_COMMENT            	             	long_name         .Comment applying to this parameter calibration     source_name       SCIENTIFIC_CALIB_COMMENT   
_FillValue                    W�   SCIENTIFIC_CALIB_DATE               	            	long_name         Date of calibration    source_name       SCIENTIFIC_CALIB_DATE      conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Z�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     source_name       HISTORY_INSTITUTION    conventions       Argo reference table 4     
_FillValue                    Z�   HISTORY_STEP                     	long_name         Step in data processing    source_name       HISTORY_STEP   conventions       Argo reference table 12    
_FillValue                    Z�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    source_name       HISTORY_SOFTWARE   conventions       Institution dependent      
_FillValue                    Z�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     source_name       HISTORY_SOFTWARE_RELEASE   conventions       Institution dependent      
_FillValue                    Z�   HISTORY_REFERENCE                        	long_name         Reference of database      source_name       HISTORY_REFERENCE      conventions       Institution dependent      
_FillValue                    Z�   HISTORY_DATE                     	long_name         #Date the history record was created    source_name       HISTORY_DATE   conventions       YYYYMMDDHHMISS     
_FillValue                  8  [�   HISTORY_ACTION                       	long_name         Action performed on data   source_name       HISTORY_ACTION     conventions       Argo reference table 7     
_FillValue                    \0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   source_name       HISTORY_PARAMETER      conventions       Argo reference table 3     
_FillValue                  @  \@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   source_name       HISTORY_START_PRES     units         decibar    
_FillValue        G�O�        \�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    source_name       HISTORY_STOP_PRES      units         decibar    
_FillValue        G�O�        \�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    source_name       HISTORY_PREVIOUS_VALUE     
_FillValue        G�O�        \�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   source_name       HISTORY_QCTEST     conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  @  \�Argo profile    3.1 1.2 19500101000000  20220714031007  20220714031007  1900459 Argo UK                                                         Jon Turton                                                      PSAL            TEMP            PRES                A   BO  8994                            2C  D   PROVOR                          OIN 03-F206                     n/a                             842 @��H�� 1   @��H�� �D]����@DA���o1   ARGOS   Primary sampling: averaged                                                                                                                                                                                                                                      ����A   A   A   @�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CR  Cf  Cz  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  D� D	� D� D� D� D� D"� D'� D,� D1� D6� D;� D@� DE� DJ� DO� DT� DY� D^� Dc� Dh� Dm� Dr� Dw� D|� D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�` D�� D�` D�� D�@ D�� D�@ D�� D�` B>=B<JB;oB<�B:YB?SBQ�BL�B-B!B�B�rB�!B�eB�GB�wB�$B˅B�HB�\B�jBN@B%B�B=NB6�B-�BuB�B
��B
�B
��B
�(B
�MB
{jB
d�B
T�B
A�B
)hB
�B	�B	�#B	�B	�9B	�B	�0B	��B	��B	��B	xB	q�B	�SB	�UB	�vB	��B	�7B	��B	��B	��B	��B	ąB	�WB	�(B	��B	��B	�B	��B
B
�B
;B
B
%3B
*�B
0B
6|B
>RB
H�B
VeB
a
B
iB
rB
z�B
��B
�%B
�{B
�GB
��B
�9B
��B
��B
�_B
үB
؜B
�WB
��B
�B
�8B
��B
�&B�BHB	�B�B�B�B�B#�B%�B(nB+PB.~AXz�AXn�AX�+AX��AXn�AVZAP�AN�jAI�FAE��AE�PAE/AD��AD�!ADffACXAA��A?�A<z�A:{A7+A0�A,$�A)��A)7LA&�A$ĜA"�A��A�AQ�Az�A�A
Q�A$�A-@���@��H@�@�E�@���@ԃ@�@�5?@��@���@��P@��@���@�^5@�j@��@�M�@�dZ@��;@���@�  @~@}�-@{�F@st�@l�/@o|�@lZ@iX@i�7@g
=@d��@a�^@\�/@Xr�@T�j@Qx�@OK�@KdZ@H��@F5?@F�R@F5?@E��@Dj@B�\@@r�@>�@<Z@:-@7�@5�@3S�@2=q@1hs@0��@0 �@/l�@.�+@-�@-?}@,��@+��@+33@*��@)��@)7L@(Q�@'�P@'
=@&��@&ff@&@%`B@%V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CR  Cf  Cz  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  D� D	� D� D� D� D� D"� D'� D,� D1� D6� D;� D@� DE� DJ� DO� DT� DY� D^� Dc� Dh� Dm� Dr� Dw� D|� D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�` D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�@ D�� D�` D�� D�` D�� D�@ D�� D�@ D�� D�` B;BHBmB�BWBQB&�B!�BB�B׳B�pB�B�cB�EB�uB�"B��B�FBsZB[hB#?B
�B
��BMB�B�B
�tB
۔B
��B
�B
��B
x)B
hOB
PlB
9�B
)�B
�B	�lB	��B	�B	�(B	��B	�?B	�#B	�7B	r�B	d�B	U�B	M$B	F�B	V[B	n\B	t}B	l�B	g?B	i�B	��B	��B	��B	��B	�]B	�.B	��B	��B	�B	ӻB	�B	ݲB	�?B	�B	�7B	��B
B
B
UB
�B
+gB
6B
>B
G!B
O�B
V�B
Z&B
a|B
mHB
z�B
�:B
��B
��B
�_B
��B
��B
�WB
��B
��B
�8B
˭B
�&B
��B
�GB
޺B
�B
��B
��B
��B
��B
��B
�mB OB}AXz�AXn�AX�+AX��AXn�AVZAP�AN�jAI�FAE��AE�PAE/AD��AD�!ADffACXAA��A?�A<z�A:{A7+A0�A,$�A)��A)7LA&�A$ĜA"�A��A�AQ�Az�A�A
Q�A$�A-@���@��H@�@�E�@���@ԃ@�@�5?@��@���@��P@��@���@�^5@�j@��@�M�@�dZ@��;@���@�  @~@}�-@{�F@st�@l�/@o|�@lZ@iX@i�7@g
=@d��@a�^@\�/@Xr�@T�j@Qx�@OK�@KdZ@H��@F5?@F�R@F5?@E��@Dj@B�\@@r�@>�@<Z@:-@7�@5�@3S�@2=q@1hs@0��@0 �@/l�@.�+@-�@-?}@,��@+��@+33@*��@)��@)7L@(Q�@'�P@'
=@&��@&ff@&@%`B@%V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL - dS                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                             ds=0.042                                                                                                                                                                                                                                                       Float that auto-corrects for pressure drifts. Calibration error is manufacturer specified accuracy.                                                                                                                                                             The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   OWC(2018v01). Mapping scales LON4/2.8 LAT 3/2 MAPPINGSCALE_PHI 0.1/0.02 MAPPINGSCALE_AGE 5/10 MAP_P_DELTA 150                                                                                                                                                   201009171616402022071310204920220713102049  BO  BO  BO  BO  ARGQARGQARSQARSQRTQCSCUTnullOW  2.0 2.0 null0.1                                                                                                                                                                                                                                                                 20051017040816200510171645302010091716164020220713102049QCP$QCP$IP  IP                                                                  @�  @�  G�O�G�O�D�` D�` G�O�G�O�F� F� F� F� 1.688850e+15    131072                                          