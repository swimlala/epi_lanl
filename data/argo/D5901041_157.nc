CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   history       d2011-12-20T10:04:06Z creation; 2018-01-03T17:11:46Z updated; 2018-01-03T22:18:53Z converted from 2.2   title         Argo float vertical profile    institution       AOML   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   axis      T      
resolution        >�E�vQ�        8   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        8   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8$   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8,   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    80   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    88   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8<   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8@   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           8D   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9d   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ;   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        <4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  =T   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  AD   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    DD   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    GD   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    JD   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  MD   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    O$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    O4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         OH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         OL   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        OP   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OT   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    Mp   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        Np   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     Nt   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     N�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     N�Argo profile    3.1 1.2 19500101000000  5901041 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  20111220100406  20180103222818  2146_68526_157                  2C  D   846 @�)З��1   @�-5�@ @3}p��
=�dj~��#1   ARGOS   A   B   B   @�33A+33A���A�ffA�33BffB4��BPffBlffB�ffB�33B���B�33B���B���B���B���C��C� CL�C�C#�C+L�C4L�C>33CH� CQ�fC]L�Cg�3CtL�C��C��fC�ٚC�ٚC�@ C��fC���C��3C��fCɌ�Cӳ3C�s3C�&fC��D@ D�3D3DFfD�D&Y�D.ٚD7�fDA3DK  DU��D`��Dl�Dx�D�l�D�)�D�  D���D�C3D���D�  D�)�D©�D̆fD� D��D홚D�L�111111111111111111111111111111111111111111111111111111111111111111111111@���A(  A�33A���A���B��B4  BO��Bk��B�  B���B�fgB���B�fgB�fgB�fgB�fgCfgCL�C�C�gC"�gC+�C4�C>  CHL�CQ�3C]�Cg� Ct�C�  C���C�� C�� C�&fC���C�s3C�ٙC���C�s3Cә�C�Y�C��C�  D33D�fDfD9�D  D&L�D.��D7��DAfDJ�3DU��D`��Dl�Dx�D�fgD�#4D��D��4D�<�D��4D���D�#4D£4D̀ D�	�D�gD�4D�Fg111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˲-A˶FA˰!A˺^A˼jA���A���Aʲ-A�l�A�1A�  A�|�A�M�A�M�A� �A�O�A��PA�K�A�bNA���A���A�O�A��uA��A�  A�l�A��Ak/A[G�AJ$�A?oA6�A-|�A'C�A��AffA��A
�A��@�v�@�v�@��@���@ա�@�n�@�E�@��;@�~�@�9X@�~�@�%@�E�@���@�/@��m@��+@��^@��/@~�y@tI�@i�#@_�w@Up�@L��@B^5@9��@0A�@$��@j@�@z�@l�111111111111111111111111111111111111111111411111111111111111111111111111A˲-A˶FA˰!A˺^A˼jA���A���Aʲ-A�l�A�1A�  A�|�A�M�A�M�A� �A�O�A��PA�K�A�bNA���A���A�O�A��uA��A�  A�l�A��Ak/A[G�AJ$�A?oA6�A-|�A'C�A��AffA��A
�A��@�v�@�v�@��G�O�@ա�@�n�@�E�@��;@�~�@�9X@�~�@�%@�E�@���@�/@��m@��+@��^@��/@~�y@tI�@i�#@_�w@Up�@L��@B^5@9��@0A�@$��@j@�@z�@l�111111111111111111111111111111111111111111411111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�jB�jB�jB�jB�jB�jB�jBǮBĜB��B��B�XB�B�B�BbB%B�B�ZB�LBx�BE�B�#B��BJB
��B
T�B	��B	,B�)B�B��B�^B��B��B�7B� B�PB�B�=BffBffB[#B��B�#B	B	"�B	@�B	cTB	�B	��B	�!B	ƨB	�BB	��B	��B

=B
{B
�B
(�B
33B
<jB
E�B
L�B
W
B
^5B
ffB
s�B
z�B
�B
�7B
�\111111111111111111111111111111111111111111411111111111111111111111111111B�B�B�B�B�B�B�B�LB�?B�!B�-B�B�sB�fB	7BB��B�HB�
B��BjB9XB��B��B
��B
��B
J�B	�VB	�B��B��BB�B��B�bB{�Bq�B~�Bv�B{�BXBXG�O�B��B��B�B	uB	1'B	S�B	t�B	�bB	��B	�LB	��B	�`B	�B	��B
B
\B
�B
#�B
-B
6FB
=qB
G�B
N�B
W
B
dZB
k�B
r�B
y�B
� 111111111111111111111111111111111111111111411111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.2 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9996 (+/-0), vertically averaged dS = -0.015 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201801031711462018010317114620180103171146  Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                APEX                            3147                            052405                          AO  ARGQ                                                                        20111220100406  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111220100406  QCF$                G�O�G�O�G�O�0               AO  ARCAADJP                                                                    20111220100406    IP                G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20120319125512  QC  PRES            @�33D�L�                    PM  ARSQCTM V1.1                                                                20120319125512  QC  PSAL            @�33D�L�                    PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120319132549  CF  TEMP            C��C��?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120319132550  CF  PSAL            C��C��?�                  PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20180103222818  IP                  G�O�G�O�G�O�                