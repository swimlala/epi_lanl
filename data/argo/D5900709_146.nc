CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2009-04-07T10:16:15Z creation; 2015-01-26T13:24:26Z updated; 2015-04-03T17:20:20Z converted from 2.2   
references        (http://www.argodatamgt.org/Documentation   comment              user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7D   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8    	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8(   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8H   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8h   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   axis      T      units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8l   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8t   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8x   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   axis      Y      units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      axis      X      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   axis      Z      	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ;   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @l   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        D\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  E|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    E�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    H�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    K�   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  N�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    O(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    O8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         OL   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         OP   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        OT   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OXArgo profile    3.1 1.2 19500101000000  5900709 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  20090407101615  20151106181248  0991_54086_146                  2C  D   APEX                            1773                            040804                          846 @�#@��I1   @�#A=�/�@9�I�  �cw+   1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                B   B   B   @���A!��A�  A�ffA�  B33B4  BPffBl��B���B�33B���B�  B���B���B�ffB�33C33C
��C�CL�C#  C+L�C433C>33CHL�CRffC]��Ch�CtL�C��E^!�D�6fC�ffC���C��C���C�33C���C�� C�� C�� C�33C��DL�D�D  D@ DfD&S3D.�3D7�3DAfDK�DUy�D`� Dl  Dx�D�p D�)�D�#3D��3D�C3D��fD�3D��D£3D�|�D�fD��D홚D�p 111111111111111111111111111111144411111111111111111111111111111111111111@���A!��A�  A�ffA�  B33B4  BPffBl��B���B�33B���B�  B���B���B�ffB�33C33C
��C�CL�C#  C+L�C433C>33CHL�CRffC]��Ch�CtL�C��G�O�G�O�C�ffC���C��C���C�33C���C�� C�� C�� C�33C��DL�D�D  D@ DfD&S3D.�3D7�3DAfDK�DUy�D`� Dl  Dx�D�p D�)�D�#3D��3D�C3D��fD�3D��D£3D�|�D�fD��D홚D�p 111111111111111111111111111111144111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��^A��FA��wA�A��9A��-A��FA��9A��-A��FA�jA�K�A�33A�VA�?}A�z�A�ffA���A���A���A�`BA��RA�^5A���A�{A��7A��A�=qA}/As�Ai|�B/AAR^5AJ=qA6�9A. �A(�/A$Q�A�\AM�A�jA\)@�p�@��@ج@���@���@���@�n�@�^5@�z�@�+@���@���@��H@�@y��@n��@h��@`bN@XA�@O\)@FE�@>v�@5�-@+��@!�7@��@C�@ƨ@/111111111111111111111111111114444411111111111111441111111111111111111111A��^A��FA��wA�A��9A��-A��FA��9A��-A��FA�jA�K�A�33A�VA�?}A�z�A�ffA���A���A���A�`BA��RA�^5A���A�{A��7A��A�=qA}/As�Ai|�G�O�G�O�AR^5AJ=qA6�9A. �A(�/A$Q�A�\AM�A�jA\)@�p�@��@ج@���@���@���@�n�G�O�@�z�@�+@���@���@��H@�@y��@n��@h��@`bN@XA�@O\)@FE�@>v�@5�-@+��@!�7@��@C�@ƨ@/111111111111111111111111111111144111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�FB�FB�FB�LB�?B�?B�?B�?B�9B�3B�B� BdZBQ�BN�BB�BS�BM�B49BbB�yB��Bv�B<jBm�B�\B'�B
�{B
=qB	��BcTB	e`BZB	w�B	"�B�B\)B`BBZBM�BA�B7LB0!B$�B"�B#�B'�B33BO�Bm�?�A�B�LB�TB	�B	\)B	~�B	��B	�^B	�B	�B	��B
oB
 �B
.B
8RB
D�B
N�B
]/B
ffB
n�B
v�B
~�111111111111111111111111111114444411111111111111444411111111111111111111B�B�B�B�B�B�B�B�B�B��B��Bw�B\)BH�BF�B9XBJ�BE�B,B1B�HB��Bo�B33BgmB�1B"�B
�VB
6FB
JB	�TG�O�G�O�B	^5B	�Bz�BS�BXBR�BF�B9XB/B(�B�B�B�B�B#�G�O�G�O�G�O�B��B�#B	{B	S�B	v�B	�\B	�-B	��B	�sB	��B

=B
�B
%�B
0!B
<jB
F�B
T�B
]/B
e`B
m�B
u�111111111111111111111111111111144111111111111111444111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�`B@��wG�O�G�O�<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0 dbar                                                                                                                                                                                                                          none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0), vertically averaged dS = -0.009 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      200910131811302009101318113020091013181130  AO  ARGQ                                                                        20090407101615  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20090407101615  QCF$                G�O�G�O�G�O�84B40           AO  ARCAADJS                                                                    20090407101615    IP                G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20091013181130  QC  PRES            @���E^!�                    PM  ARSQCTM V1.1                                                                20091013181130  QC  PSAL            @���E^!�                    PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152454  CF  TEMP            CtL�C��@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152454  CF  TEMP            C�ffC�ff@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152454  CF  TEMP            DfD&S3@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152454  CF  TEMP            D.�3D.�3?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152455  CF  PSAL            CtL�C��@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152455  CF  PSAL            C�ffC�ff@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20091014152455  CF  PSAL            D7�3D7�3@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20150126131221  IP                  G�O�G�O�G�O�                