CDF   
   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2010-12-03T15:00:59Z creation; 2015-01-26T13:26:44Z updated; 2015-04-03T17:20:25Z converted from 2.2   
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
_FillValue                    OXArgo profile    3.1 1.2 19500101000000  5900709 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  20101203150059  20151106181257  0991_54086_203                  2C  D   APEX                            1773                            040804                          846 @պ�lGc�1   @պ��� @9����m�c?+I�1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                B   B   B   @�  A&ffA�  A�  A�33B��B4  BQ��Bn  B�  B�  B�33B���B�33B�33B�  B�33CL�C�C33CL�C#ffC+  C4ffG�O�G�O�G�O�G�O�    Ct�C�33C��3C��C�&fC��fC��C���C���C���CɦfCӳ3C�ffC�@ C�33DFfD�D�3DL�D�D&S3D.�fD7ٚDA�DK�DU�3D`�fDl�Dw��D�l�D�&fD�#3D��fD�S3D��3D�  D�#3D�D̉�D�3D��D홚D���111111111111111111111111999941111111111111111111111111111111111111111111@�  A&ffA�  A�  A�33B��B4  BQ��Bn  B�  B�  B�33B���B�33B�33B�  B�33CL�C�C33CL�C#ffC+  C4ffG�O�G�O�G�O�G�O�G�O�Ct�C�33C��3C��C�&fC��fC��C���C���C���CɦfCӳ3C�ffC�@ C�33DFfD�D�3DL�D�D&S3D.�fD7ٚDA�DK�DU�3D`�fDl�Dw��D�l�D�&fD�#3D��fD�S3D��3D�  D�#3D�D̉�D�3D��D홚D���111111111111111111111111999941111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��!A��9A��jA��-A��^A�A�ƨA���A���A���A���A���A��A��#A�oA�9XA���A�9XA�jA�VA�O�A�1    G�O�G�O�G�O�G�O�AhVA\�HAWl�AO�mABz�A<^5A49XA*z�A�9A��A�A�A
A�A��@�Z@��@�C�@�&�@���@��H@�I�@�Q�@��-@�o@���@���@�G�@x�@q��@j��@d�/@_K�@X��@R��@K��@A�^@7
=@/+@( �@ bN@��@9X@V@�P111111111111111111111114999944111111141111111111111111111111111111111111A���A��!A��9A��jA��-A��^A�A�ƨA���A���A���A���A���A��A��#A�oA�9XA���A�9XA�jA�VA�O�A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�AWl�AO�mABz�A<^5A49XA*z�A�9G�O�A�A�A
A�A��@�Z@��@�C�@�&�@���@��H@�I�@�Q�@��-@�o@���@���@�G�@x�@q��@j��@d�/@_K�@X��@R��@K��@A�^@7
=@/+@( �@ bN@��@9X@V@�P111111111111111111111114999944111111141111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJ�BJ�BJ�BJ�BI�BI�BI�BI�BI�BI�BH�BD�BcTBW
B1'B��B�B��B�RB�BXB{B�/B�=G�O�G�O�G�O�G�O�    B	O�B	:^B��BƨB�FB��B�%Bu�B
S�BVBI�BA�B6FB(�B�B�B�B&�B2-BW
Bq�B�uB�jB�yB	�B	C�B	_;B	�uB	��B	�#B	�B
B
uB
-B
49B
C�B
M�B
W
B
bNB
hsB
o�B
v�B
� 111111111111111111111114999944111111141111111111111111111111111111111111B=qB=qB=qB=qB<jB<jB<jB<jB<jB<jB;dB?}BYBJ�B%�B�B�TBĜB�Bw�BL�B1B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	/B�B�dB�B��Bz�BiyG�O�BJ�B@�B5?B)�B�B\B
=BDB�B%�BJ�Be`B�+B�!B�/B	uB	7LB	R�B	�%B	�9B	��B	�NB	��B
%B
�B
&�B
6FB
@�B
I�B
T�B
[#B
bNB
iyB
r�111111111111111111111114999944111111141111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0 dbar                                                                                                                                                                                                                          none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9997 (+/-0), vertically averaged dS = -0.013 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201208031650222012080316502220120803165022  AO  ARGQ                                                                        20101203150059  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20101203150059  QCF$                G�O�G�O�G�O�4A40            PM  ARSQPADJV1.1                                                                20120803165022  QC  PRES                G�O�                    PM  ARSQCTM V1.1                                                                20120803165022  QC  PSAL                G�O�                    PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120810181541  CF  TEMP            Ct�Ct�?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120810181542  CF  TEMP            C���C���?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120810181542  CF  TEMP            CɦfCɦf@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120810181542  CF  PSAL            C4ffC4ff?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20120810181542  CF  PSAL            CɦfCɦf@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20150126131403  IP                  G�O�G�O�G�O�                