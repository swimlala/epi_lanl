CDF      
      STRING16      STRING4       	DATE_TIME         N_PROF        STRING8       STRING64   @   N_PARAM       STRING2       STRING32       N_LEVELS   H   N_CALIB       	STRING256         	N_HISTORY             	   history       d2005-09-27T10:16:46Z creation; 2006-12-08T15:21:25Z updated; 2019-01-29T12:38:58Z converted from 2.2   title         Argo float vertical profile    institution       AOML   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                   	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7@   STATION_PARAMETERS                        	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                  	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   axis      T      
resolution        >�E�vQ�        8   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        8   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8$   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8,   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    80   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    88   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8<   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8@   PRES         	      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           8D   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9d   PRES_ADJUSTED            	      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ;   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        <4   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  =T   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?   PSAL         	      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @$   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  AD   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   	PARAMETER            
                	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  D   SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    DD   SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    GD   SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    JD   SCIENTIFIC_CALIB_DATE            
               
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  MD   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                     	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    O$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    O4   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         OH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         OL   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        OP   HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OT   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    Mp   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        Np   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     Nt   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     N�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     N�Argo profile    3.1 1.2 19500101000000  5900888 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  20050927101646  20190129124038  0998_54093_013                  2C  D   846 @��% �@ 1   @��%PȀ @1��   �c=��   1   ARGOS   A   A   A   @���A(  A�33A�33A�  B��B533BR  Bl��B���B�ffB�ffB�  B�ffB�33B�  B���CL�C�C��CffC#ffC+��C4� C>33CHffCR�C]L�Ch��Ct33C�33C��C�L�C�33C���C��C�s3C�33C��fCɦfCә�CަfC��3C�&fDffD�D3DL�D&fD&Y�D.�fD7�3DA  DK3DU� D`� Dl  Dx3D�c3D�&fD�#3D���D�P D��3D�3D�&fD�D̐ D��D�#3D��3D��111111111111111111111111111111111111111111111111111111111111111111111111@�34A��A���A���A�ffB��B2ffBO33Bj  B�34B�  B�  B���B�  B���B噚B�fgC��C
fgC�gC�3C"�3C*�gC3��C=� CG�3CQfgC\��Cg�gCs� C�3C��3C��3C�ٙC�33C�� C��C�ٙC�L�C�L�C�@ C�L�C陙C���D9�D��D�fD  D��D&,�D.��D7�fD@�3DJ�fDUs3D`S3Dk�3Dw�fD�L�D� D��D�vgD�9�D�l�D���D� DgD�y�D��gD��D��D��g111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�M�Aק�A�$�A֕�A�v�A�ZA�5?A���A�C�A�VA�%A�A�-A��A�\)A�S�A�I�A�7LA�(�A��A���A��DA���A�=qA�l�A��PA�A�Ax��Ae��AM�wA@VA7t�A1
=A&�`A!l�A��A��Al�AĜA+@��@��@��m@��@�"�@���@�33@�b@�V@��/@��@�{@���@��@���@��h@�b@��@��@q�#@e�@Z-@K��@@��@8��@1%@)hs@"M�@�9@�;@��111111111111111111111111111111111111111111111111111111111111111111111111A��A�M�Aק�A�$�A֕�A�v�A�ZA�5?A���A�C�A�VA�%A�A�-A��A�\)A�S�A�I�A�7LA�(�A��A���A��DA���A�=qA�l�A��PA�A�Ax��Ae��AM�wA@VA7t�A1
=A&�`A!l�A��A��Al�AĜA+@��@��@��m@��@�"�@���@�33@�b@�V@��/@��@�{@���@��@���@��h@�b@��@��@q�#@e�@Z-@K��@@��@8��@1%@)hs@"M�@�9@�;@��111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�'B
�-B
�'B
�!B
�'B
�XB/BF�B7LB
��B#�B�BVB�yB�B~�Br�BiyB�Bq�BE�BVB�}B/B
�{B
T�B	ĜB	B�B�Bo�Bk�BhsB~�B��B��B��B�B�1B�B�!B�^B�B�NB	hB	5?B	\)B	z�B	�VB	�B	��B	��B	��B	�B	��B
bB
�B
 �B
&�B
)�B
/B
8RB
G�B
Q�B
YB
aHB
hsB
n�B
u�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111B
�`B
��B
��B
��B
�yB
�rB
�B
��B/�BI#B;yB
�EB$�B�"B�B�B7B��BtBk
B��Bt>BHMBdB��B4IB
��B
Y|B	�%B	G�B�/Bq�Bm?Bj�B�WB�B��B�B��B�HB��B�AB�<B��B�B	.B	5�B	\�B	{�B	��B	��B	�eB	��B	�;B	�FB	�tB
�B
B
!JB
'B
*�B
/�B
8�B
H3B
RfB
Y�B
a�B
h�B
oB
v8B
lB
��111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.7 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  200611061255302006110612553020061106125530  Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                APEX                            1780                            040804                          AO  ARGQ                                                                        20050927101646  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20050927101646  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20061106125530  QC  PRES            @���D��@                  PM  ARSQCTM V1.1                                                                20061106125530  QC  PSAL            @���D��@                  PM  ARSQOWGUV1.0                                                                20190129124038  IP                  G�O�G�O�G�O�                