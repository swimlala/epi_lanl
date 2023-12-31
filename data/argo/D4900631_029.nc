CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   =   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       MEDS   source        
Argo float     history       2015-05-22T20:17:11Z creation      
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
resolution        =���   axis      Z      coordinate_reference_frame        urn:ogc:crs:EPSG::5113        �  :d   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  @  ;X   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      X         �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  @  <�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  @  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature         �  >�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  @  ?�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  @(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  A   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  @  B   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity        �  BP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  @  CD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  C�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  Dx   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    D�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    J�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    P�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        T  V�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    W,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    W0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    W4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    W8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  W<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    W|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    W�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    W�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         W�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         W�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        W�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    W�Argo profile    3.1 1.2 19500101000000  20150522201711  20150522201711  4900631 Canadian Argo                                                   Blair Greenan                                                   PRES            TEMP            PSAL               A   ME  49006319834PF                   2C+ D   APEX-SBE                        1997                            n/a                             846 @������1   @������@:2n�   �dg
@   1   ARGOS   A   A   A   Primary sampling: discrete                                                                                                                                                                                                                                          @�33A33A���B�33B�ffB�  B�  BCffC
�fC�3C�fC)� C333C=L�CGffCQ33CZ�fCeL�CoL�CyffC���C��fC��fC��3C��3C���C��3C�� C�� C���C���C�s3Cǀ C�� C��fD	9�D��D"L�D.��D;L�DG��DTS3D`� DmFfDy��D��fD���D�&fD�i�D�� D���D�)�D�ffDک�D��fD�#3D�c3D�fD��D��f1111111111111111111111111111111111111111111111111111111111111   @���A  A�34B���B�  Bř�Bٙ�B�34C �3C
33C  C33C(��C2� C<��CF�3CP� CZ33Cd��Cn��Cx�3C�@ C�L�C�L�C�Y�C�Y�C�33C�Y�C�&fC�ffC�33C�@ C��C�&fC�ffC�L�D	�D� D"  D.��D;  DG� DT&fD`�3Dm�Dy��D�� D��gD� D�S4D���D��gD�4D�P Dړ4D�� D��D�L�D� D��4D�� 1111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�1A�
=A���A�dZA�JA��DA��FA�+A�+A��
A���A�M�A���A���A�~�AuK�Apz�Ail�Ag|�Ab�A\�+AX1'AT�+AR�AP��ALn�AI"�AE�ADffA@�\A>I�A6A�A-`BA�#AO�@�E�@�C�@���@��j@��m@�1@�(�@���@|Z@sdZ@=�-@8�u@3�@-��@( �@!�^@33@K�@�@Q�@I�@�P@t�?���?��1111111111111111111111111111111111111111111111111111111111111   A�A�1A�
=A���A�dZA�JA��DA��FA�+A�+A��
A���A�M�A���A���A�~�AuK�Apz�Ail�Ag|�Ab�A\�+AX1'AT�+AR�AP��ALn�AI"�AE�ADffA@�\A>I�A6A�A-`BA�#AO�@�E�@�C�@���@��j@��m@�1@�(�@���@|Z@sdZ@=�-@8�u@3�@-��@( �@!�^@33@K�@�@Q�@I�@�P@t�?���?��1111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B{�B�PBQ�B-B�B�hB�VBgmB>wBVB
�)B
�B
��B
ffB
I�B
,B
!�B
JB	�ZB	��B	��B	�dB	�B	�hB	z�B	^5B	]/B	Q�B	=qB	�B�B��B`BBF�B>wB[#B�B�XB�B	B	49B	iyB	��B
%�B
-B
6FB
>wB
F�B
N�B
YB
]/B
bNB
gmB
l�B
s�B
x�B
}�B
~�1111111111111111111111111111111111111111111111111111111111111   B�B�B�B|(B��BR+B-PB�QB��B��Bg�B>�B�B
�qB
�NB
� B
f�B
JB
,WB
"B
�B	�B	�B	��B	��B	�\B	��B	{9B	^�B	]�B	RFB	=�B	�B�B�B`�BGB>�B[�B��B��BچB	qB	4�B	i�B	��B
&EB
-pB
6�B
>�B
G	B
O:B
YxB
]�B
b�B
g�B
l�B
tB
y2B
~RB
Y1111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            Pcorrected = Praw(n) - surface_pres_offset(n)+5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 PSAL_ADJUSTED is calculated from a conductivity multiplicative adjustment term r.                                                                                                                                                                               PRES_ADJUSTED=PRES + coefficient (see procedure 3.2.1 in Argo DMQC manual v2.4)                                                                                                                                                                                                                                                                                                                                                                                                                                                 PSAL_ADJUSTED is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                     surface_pres_offset = 5.8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       COEFFICIENT r FOR CONDUCTIVITY IS 0.99987, +/- 0.0001396491                                                                                                                                                                                                     ADDITIVE COEFFICIENT FOR PRESSURE ADJUSTMENT IS -0.7db                                                                                                                                                                                                                                                                                                                                                                                                                                                                          r=0.999995, � 0.0001311303                                                                                                                                                                                                                                      Calibration error is manufacturers specified PRES accuracy at time of lab calibration                                                                                                                                                                           Calibration error is manufacturers specified TEMP accuracy at time of lab calibration                                                                                                                                                                           Adjusted salinity to climatology according to WJO(2003). Ref. Data are WOD 2001.                                                                                                                                                                                PRES_ADJUSTED is calculated following the 3.2.1 procedure in the Argo Quality Control Manual version 2.5. No significant pressure drift was detected.Pressure evaluation done on 24-Aug-2010 16:03:44                                                           No approved method for delayed-mode qc on TEMP is available                                                                                                                                                                                                     No adjustment is needed on this parameter because no significant sensor drift has been detected.                                                                                                                                                                200903051539022007020203281020070202032810201008241707212010082417072120100824170721ME  RFMT    1.0                                                                 20060523000000  CR  RCRD            G�O�G�O�G�O�                ME  ARDP    1.0                                                                 20060523000000  CR  RCRD            G�O�G�O�G�O�                ME  ARGQ    1.0                                                                 20100824170721  QCP$RCRD            G�O�G�O�G�O�0007FBFE        ME  ARGQ    1.0                                                                 20060523000000  QCF$RCRD            G�O�G�O�G�O�0               ME  ARUP    1.0                                                                 20060523000000  UP  RCRD            G�O�G�O�G�O�                ME  ARCAWJO 1.0 WOD01 WITH MIN_MAP_ERR = -1                                     20061002000000  CV  CNDC            ?�  ?�  ?�                  ME  ARUP    1.0                                                                 20061002000000  UP  RCRD            G�O�G�O�G�O�                CI  ARSQWJO 2.0bWOD01 WITH MIN_MAP_ERR = -1                                     20070912000000  CR  PSAL            G�O�G�O�G�O�                ME  ARDU    1.0                                                                 20071024000000  UP  RCRD            G�O�G�O�G�O�                ME  ARSQWJO 2.0bWOD01 WITH MIN_MAP_ERR = -1                                     20081016000000  QC  PSAL            G�O�G�O�G�O�                ME  ARDU    1.0                                                                 20090305000000  UP  RCRD            G�O�G�O�G�O�                ME  ARSQWJO 2.0bCTD&BOTTLE_2008V1                                               20100824170721  QCCV                G�O�G�O�G�O�                