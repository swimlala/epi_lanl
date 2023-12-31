CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   A   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       MEDS   source        
Argo float     history       2015-05-22T20:17:08Z creation      
references        (http://www.argodatamgt.org/Documentation   comment              user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  8   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8\   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9    JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�����h�        9$   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9,   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        90   LATITUDE            	   	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y      	reference         WGS84      coordinate_reference_frame        urn:ogc:crs:EPSG::4326          98   	LONGITUDE               	   	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X      	reference         WGS84      coordinate_reference_frame        urn:ogc:crs:EPSG::4326          9@   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9H   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9L   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9T   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9X   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9\   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :`   PRES         
         	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z      coordinate_reference_frame        urn:ogc:crs:EPSG::5113         :d   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  ;h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      X          ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  <�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature          ?@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  @D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity         B�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  C�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  E    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    E�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    K�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Q�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        T  W�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    W�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    W�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    W�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    W�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  W�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    X$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    X4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         XH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         XL   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        XP   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    XTArgo profile    3.1 1.2 19500101000000  20150522201708  20150522201708  4900631 Canadian Argo                                                   Blair Greenan                                                   PRES            TEMP            PSAL               A   ME  49006319994PF                   2C+ D   APEX-SBE                        1997                            n/a                             846 @���[1   @���[@8�d`   �d��`   1   ARGOS   A   A   A   Primary sampling: discrete                                                                                                                                                                                                                                          @�  A33A�ffA���B33BE��Bm��B�ffB���B�ffB�  B���B�33C33CffC�C� C)ffC3ffC=L�CG  CQ  C[L�Ce�Co33Cx��C���C�s3C���C��fC���C��3C���C�� C���C�� C���C��fCǳ3C�3C���D	Y�DٚD"` D.� D;ffDG��DT@ D`ٚDm` DyٚD�,�D�p D��3D��3D�  D�c3D���D��3D�0 D��3D�&fD�Y�D��D���11111111111111111111111111111111111111111111111111111111111111111   @�  A33A�ffA���B33BC��Bk��B�ffB���B�ffB�  B���B�33C �3C
�fC��C  C(�fC2�fC<��CF� CP� CZ��Cd��Cn�3CxL�C�Y�C�33C�L�C�ffC�Y�C�s3C�Y�C�� C�Y�C�� C�Y�C�ffC�s3C�s3C�Y�D	9�D��D"@ D.� D;FfDG��DT  D`��Dm@ Dy��D��D�` D��3D��3D� D�S3D���D��3D�  D��3D�fD�I�D��D���11111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�A�1A�  A�
=A�VAǟ�A�O�A�E�A�\)A���A�JA�-A�|�A���A�K�A���A��^A�bNA��A��-A�ƨA��-A~�`Ay�^Av~�AtjAp��Ah�yAcK�AXJAU\)AR  AP�+AJ�9AE��A=�TA5A,��A�`A`B@�=q@�"�@��+@�E�@��@���@�"�@�M�@��@��@y�^@r^5@iG�@aX@TZ@K"�@EO�@=�h@7
=@M�@�j@	�7@+@�m11111111111111111111111111111111111111111111111111111111111111111   A���A�A�1A�  A�
=A�VAǟ�A�O�A�E�A�\)A���A�JA�-A�|�A���A�K�A���A��^A�bNA��A��-A�ƨA��-A~�`Ay�^Av~�AtjAp��Ah�yAcK�AXJAU\)AR  AP�+AJ�9AE��A=�TA5A,��A�`A`B@�=q@�"�@��+@�E�@��@���@�"�@�M�@��@��@y�^@r^5@iG�@aX@TZ@K"�@EO�@=�h@7
=@M�@�j@	�7@+@�m11111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�B�B�B�B�yB�DB��BƨB��BZB>wB+B�}B�PBo�BgmB;dB0!B�B
�B
�ZB
�B
�XB
��B
�B
x�B
T�B
B	ÖB	r�B	ffB	C�B	P�B	'�B	DB�B�B��B��BbNBC�BJ�BYB�DB�dB�fB	�B	XB	�DB	�!B	ȴB	�TB	��B
B
JB
hB
 �B
#�B
'�B
ffB
m�B
q�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B�B�mB��B��B�)BZLB>�B+,B��B��Bo�Bg�B;�B0UB�B
��B
�B
�8B
��B
��B
�VB
yB
U7B
UB	��B	r�B	f�B	C�B	Q(B	(5B	�B��B�ZB�B�EBb�BC�BKBYoB��B��B�B	B	X[B	��B	�iB	��B	�B	�B
aB
�B
�B
!B
$B
(9B
f�B
m�B
q�B
s�B
x11111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJUSTED is calculated from a conductivity multiplicative adjustment term r.                                                                                                                                                                               PRES_ADJUSTED=PRES + coefficient (see procedure 3.2.1 in Argo DMQC manual v2.4)                                                                                                                                                                                                                                                                                                                                                                                                                                                 PSAL_ADJUSTED is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     No significant salinity drift detected; r=1.000000                                                                                                                                                                                                              ADDITIVE COEFFICIENT FOR PRESSURE ADJUSTMENT IS -0.5db                                                                                                                                                                                                                                                                                                                                                                                                                                                                          r=0.999995, � 0.0001311303                                                                                                                                                                                                                                      Calibration error is manufacturers specified PRES accuracy at time of lab calibration                                                                                                                                                                           Calibration error is manufacturers specified TEMP accuracy at time of lab calibration                                                                                                                                                                           No salinity adjustment needed according to WJO(2003). Ref. Data are SeHyD ver. 1;WOD 2001; IOS BIO, IFR ctds; unadjusted d-mode ago                                                                                                                             PRES_ADJUSTED is calculated following the 3.2.1 procedure in the Argo Quality Control Manual version 2.5. No significant pressure drift was detected.Pressure evaluation done on 24-Aug-2010 16:03:44                                                           No approved method for delayed-mode qc on TEMP is available                                                                                                                                                                                                     No adjustment is needed on this parameter because no significant sensor drift has been detected.                                                                                                                                                                200511180947372005111809473720051118094737201008241707212010082417072120100824170721ME  RFMT    1.0                                                                 20051004000000  CR  RCRD            G�O�G�O�G�O�0               ME  ARDP    1.0                                                                 20051004000000  CR  RCRD            G�O�G�O�G�O�0               ME  ARGQ    1.0                                                                 20051004000000  QCF$RCRD            G�O�G�O�G�O�00000000        ME  ARGQ    1.0                                                                 20100824170721  QCP$RCRD            G�O�G�O�G�O�000007FBFE      ME  ARUP    1.0                                                                 20051004000000  UP  RCRD            G�O�G�O�G�O�0               CI  ARSQWJO 2.0bWOD01:SeHyD:CTD WITH MIN_MAP_ERR = 0.008                        20051118000000  CR  PSAL            G�O�G�O�G�O�0               ME  ARDU    1.0                                                                 20060509000000  UP  RCRD            G�O�G�O�G�O�0               ME  ARSQWJO 2.0bCTD&BOTTLE_2008V1                                               20100824170721  QCCV                G�O�G�O�G�O�                