CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   b   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CSIO   source        
Argo float     history       92015-11-08T12:08:01Z creation;2015-11-08T12:08:01Z update      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7    PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8$   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8D   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8H   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8P   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8T   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8\   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8d   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8l   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8p   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8x   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8|   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  ;   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;t   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  <�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  @p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  B\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  DH   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  E�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  G�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  I�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    I�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    L�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    O�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  R�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    S   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    S   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    S   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    S   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  S   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ST   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    Sd   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Sh   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Sx   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         S|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        S�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    S�Argo profile    3.1 1.2 19500101000000  20151108120801  20200707075746  2902598 PROVOR                          CHINA ARGO EQUIVALENT                                           ZENGHONG LIU                                                    PRES            TEMP            PSAL               1A   HZ  0277_135043_049                 2C  D   OIN-13CH-S31-46                 N/A                             841 @�}���1   @�}���@4#�
=p��d�n��1   ARGOS   A   A   A   Primary sampling: averaged []                                                                                                                                                                                                                                      ?�  @@  @�  @�  @�  @�  A  Ap  A�  B  B0  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CD  CT  Cn  C�� C�� C�  C�  C�� C C�  C�  C� C�  D @ D� D  D  D@ D� D%� D,  D2@ D8� D>� DE  DK� DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @@  @�  @�  @�  @�  A  Ap  A�  B  B0  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CD  CT  Cn  C�� C�� C�  C�  C�� C C�  C�  C� C�  D @ D� D  D  D@ D� D%� D,  D2@ D8� D>� DE  DK� DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AދDAށA�|�A�x�A�x�A�|�A�~�A�z�A�ffA�Q�A�$�Aٺ^A�ȴA��TA�C�A�t�A��TA�9XA���A���A�7LA���A��
A�ȴA��hA�5?A}�A_�AH�A9&�A+�FA �jAn�A�!A5?@��@���@�&�@�`B@�p�@���@�E�@��@��u@�Ĝ@�r�@�G�@��@���@��D@���@��u@�ȴ@���@�X@�ƨ@��D@��@���@�Q�@�@}/@v�R@p  @j��@eV@`1'@\�j@W
=@Q�#@Mp�@I7L@Gl�@D�/@B^5@?K�@<�@:�@7|�@5�@2��@0  @+��@'|�@#�
@!&�@ȴ@j@-@K�@�
@��@  @p�@�@	�#@�u@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AދDAށA�|�A�x�A�x�A�|�A�~�A�z�A�ffA�Q�A�$�Aٺ^A�ȴA��TA�C�A�t�A��TA�9XA���A���A�7LA���A��
A�ȴA��hA�5?A}�A_�AH�A9&�A+�FA �jAn�A�!A5?@��@���@�&�@�`B@�p�@���@�E�@��@��u@�Ĝ@�r�@�G�@��@���@��D@���@��u@�ȴ@���@�X@�ƨ@��D@��@���@�Q�@�@}/@v�R@p  @j��@eV@`1'@\�j@W
=@Q�#@Mp�@I7L@Gl�@D�/@B^5@?K�@<�@:�@7|�@5�@2��@0  @+��@'|�@#�
@!&�@ȴ@j@-@K�@�
@��@  @p�@�@	�#@�u@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB� B�B�B�B�B�B�B�B�%B�7BÖBffBiyB��BȴB�B��B�B�/B��BA�B��B�BW
B
��B
�!B
�B	e`B�B�XB�VB{�Bw�Bm�BffBgmBt�B�\B��B�qB�NB��B	JB	)�B	B�B	^5B	u�B	�DB	��B	�RB	��B	��B	�#B	�fB	�B	��B
B
%B

=B
\B
oB
�B
�B
�B
"�B
%�B
+B
.B
.B
2-B
33B
6FB
8RB
;dB
>wB
@�B
B�B
D�B
F�B
I�B
K�B
L�B
Q�B
T�B
XB
[#B
\)B
_;B
aHB
cTB
ffB
hsB
jB
l�B
n�B
p�B
r�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�-B�4B�4B�B�B�B�B�+B�7B�dB��Bq?Br�B�,B�NB�B�SB�B�UB�MBGB��B�+B\ZB�B
�bB
�B	jbB�AB�TB��B}�By�Bp2Bh[Bh�Bv<B��B��B�lB�&B�iB	�B	*sB	CB	^�B	vB	��B	�uB	��B	��B	�B	ۂB	�B	��B	�6B
mB
{B

�B
�B
�B
�B
B
B
#*B
&2B
+:B
.pB
.hB
2xB
3uB
6cB
8|B
;�B
>�B
@�B
B�B
D�B
F�B
I�B
K�B
MB
R0B
U:B
X:B
[MB
\NB
_`B
awB
c�B
f�B
h�B
j�B
l�B
n�B
p�B
r�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?��<( �<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES = PRES - [PRES_SurfaceOffsetCorrectedNotResetNegative_1dbarResolution_dbar]; PRES_ADJUSTED = PRES                                                                                                                                                          no change                                                                                                                                                                                                                                                       PSAL_ADJUSTED = sal(CNDC,TEMP,PRES_ADJUSTED); PSAL_ADJ corrects conductivity cell thermal mass (CTM), Johnson et al, 2007, JAOT                                                                                                                                 PRES_SurfaceOffsetCorrectedNotResetNegative_1dbarResolution_dbar in TECH file for N-1 profile                                                                                                                                                                   no change                                                                                                                                                                                                                                                       same as for PRES_ADJUSTED; CTL: alpha=0.1410, tau=6.68;                                                                                                                                                                                                         Pressures adjusted using PRES_SurfaceOffsetCorrectedNotResetNegative_1dbarResolution_dbar; Pressure drift corrected onboard the float; Manufacturers sensor accuracy                                                                                            No significant temperature drift detected; SBE sensor accuracy                                                                                                                                                                                                  No significant salinity drift detected; Manufacturers sensor accuracy;                                                                                                                                                                                          202007070757462020070707574620200707075746  HZ  ARGQ                                                                        20151108120801  QCP$                G�O�G�O�G�O�D7BFE           HZ  ARGQ                                                                        20151108120801  QCF$                G�O�G�O�G�O�0               HZ  ARGQ                                                                        20151108120801                      G�O�G�O�G�O�                HZ  ARGQ                                                                        20151108120801                      G�O�G�O�G�O�                HZ  ARSQCTL v1.0                                                                20200707075216  QC  PSAL            ?�  D�� G�O�                HZ  ARSQSIQCV2.0WOD2001 & Argo                                                  20200707075737  IP                  ?�  D�� G�O�                