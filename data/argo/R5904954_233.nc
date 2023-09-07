CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:42Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191742  20181005191742  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�餢R��1   @��-��@4�j~��#�d�p��
=1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A>ffA`  A~ffA�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B'��B/��B7��B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CK�fCM�fCP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C��C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C��C�  C��3C��3C��3C��C�  C�  C��C�  C��3C��C��C��3C��3C�  C��C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C�  C��C��C��C�  C��3C�  C��C��C��C��C��C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C��3C��C�  C�  C�  C�  D fD � D  D�fDfD�fD  Dy�DfD�fDfD�fD  D� DfD� D��Dy�D	  D	� D
  D
� D  D�fD  D� D��Dy�D��D�fDfD� D��Dy�D  D�fD  Dy�D��Dy�D��Ds3D  D�fDfD� D��Dy�D  D� D��Ds3D��Dy�D  D�fD  Dy�D��D� DfD�fDfD�fD   D y�D!fD!�fD!��D"y�D#fD#� D#��D$�fD$��D%y�D&  D&�fD&��D'� D(fD(y�D)fD)y�D*  D*�fD*��D+s3D+�3D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2y�D3fD3� D4  D4y�D5  D5� D6  D6� D7fD7� D8  D8y�D9  D9� D:  D:� D;  D;�fD<  D<� D<��D=� D>  D>� D>��D?� D@  D@� DA  DA�fDB  DB�fDCfDCy�DD  DD� DD��DE� DF  DF� DG  DG�fDG��DH� DI  DI� DJ  DJ�fDJ��DK� DL  DL�fDM  DM� DNfDN� DO  DO�fDP  DP� DQ  DQy�DR  DR� DS  DS� DT  DTy�DU  DU�fDV  DV�fDW  DWy�DX  DX� DY  DY� DY��DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da�fDb  Dby�Dc  Dc� Dd  Dd� DefDey�Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� DnfDny�DofDo� Dp  Dpy�DqfDq��DrfDry�Ds  Ds�fDt  Dt� Du  Du�fDu��Dv�fDw  Dw� Dw��Dy�qD�B�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ə�A��A$��AC33Ad��A���A�ffA�ffA�ffA���A�ffA�ffA�ffB33B	33B33B33B!33B(��B0��B8��BA33BI33BQ33BY��Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���B���Bę�Bș�B̙�B���Bԙ�Bؙ�B���B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�C33C33CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CF33CHL�CJL�CL33CN33CPL�CRL�CTL�CVL�CXfgCZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�Cp33Cr33CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�33C�33C�33C�&fC�&fC�&fC�33C�&fC�&fC�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�33C�&fC�&fC�33C�&fC��C�&fC�33C�33C�&fC��C��C��C�33C�&fC�&fC�33C�&fC��C�33C�33C��C��C�&fC�33C�&fC��C�&fC�33C�&fC��C��C�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC��C��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�33C�33C�&fC�&fC�33C�33C�33C�&fC��C�&fC�33C�33C�33C�@ C�33C�33C�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�33C�&fC��C�33C�&fC�&fC�&fC�&fD �D �3D3D��D�D��D3D��D�D��D�D��D3D�3D�D�3D�D��D	3D	�3D
3D
�3D3D��D3D�3D�D��D�D��D�D�3D�D��D3D��D3D��D�D��D�D�fD3D��D�D�3D�D��D3D�3D�D�fD�D��D3D��D3D��D�D�3D�D��D�D��D 3D ��D!�D!��D"�D"��D#�D#�3D$�D$��D%�D%��D&3D&��D'�D'�3D(�D(��D)�D)��D*3D*��D+�D+�fD,fD,�3D-3D-��D.3D.�3D/3D/�3D03D0�3D13D1��D23D2��D3�D3�3D43D4��D53D5�3D63D6�3D7�D7�3D83D8��D93D9�3D:3D:�3D;3D;��D<3D<�3D=�D=�3D>3D>�3D?�D?�3D@3D@�3DA3DA��DB3DB��DC�DC��DD3DD�3DE�DE�3DF3DF�3DG3DG��DH�DH�3DI3DI�3DJ3DJ��DK�DK�3DL3DL��DM3DM�3DN�DN�3DO3DO��DP3DP�3DQ3DQ��DR3DR�3DS3DS�3DT3DT��DU3DU��DV3DV��DW3DW��DX3DX�3DY3DY�3DZ�DZ��D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`�D`�3Da3Da��Db3Db��Dc3Dc�3Dd3Dd�3De�De��Df3Df�3Dg3Dg�3Dh3Dh��Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm�Dm�3Dn�Dn��Do�Do�3Dp3Dp��Dq�Dq� Dr�Dr��Ds3Ds��Dt3Dt�3Du3Du��Dv�Dv��Dw3Dw�3Dx�Dy��D�L)D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AɓuAɗ�Aɕ�AɑhA�r�A��A��AȑhA�1'A���A�n�Aƕ�A��Aŉ7A�;dA�bA�A��A���A�A�%A�A�%A�A���A���A��A��`A��#A���A�ƨA���Aĺ^Aě�AąA�VA�oAÑhA���A�A�v�A�hsA�E�A�{A��^A�VA���A�{A�z�A�A��A��-A��-A��A��FA��A��7A�dZA�-A���A�1A��A�r�A�z�A�ȴA��;A���A��A��RA��A�VA�A��hA��RA�l�A�`BA���A��A��A�A�^5A�JA�5?A�5?A��jA�n�A�?}A�9XA�1A��`A��A��A��FA�(�A���A��A���A�ƨA�I�A���A�x�A�jA�A�A�ffA���A�M�A�t�AG�A~=qA|JAzJAvv�Ar�Ap^5AnjAjVAg\)Af��Ae�wAb9XAaG�A_�;A^��A]��A\{AX~�AW��AV�\AU�AU�FAT�AS�TASx�ARn�AQ�hAPz�ANQ�AM
=ALbNAK�AJ��AI��AH��AG��AG?}AG�AF��AEx�AC�A@��A?��A?�hA>��A=�A;�#A:JA8��A8n�A7��A5�PA4��A4A3t�A3;dA2z�A0�`A/�mA/\)A-�wA,�!A+�;A*1'A'��A&�RA&ffA&{A$�`A$  A#��A"��A"M�A!��A �/A�mA��AA�A�DA�mA��A��AVAjA��A��AJA�A\)A�A9XA��A��A�mA^5A��A
~�A��At�AffA�TA�-AXA
=A�A�A��A&�A �HA (�@���@��7@�%@�I�@��R@��@��u@�=q@�j@��@��@�?}@�@�b@��H@�D@柾@�p�@�b@���@�w@�|�@◍@���@��;@��#@�bN@�  @ۮ@�t�@���@�x�@؋D@�(�@�  @��@ו�@�o@�K�@ם�@�X@ӕ�@ҸR@�ff@ѡ�@ЋD@��
@���@Ώ\@́@�\)@ɉ7@� �@��@���@�  @\@�`B@��@�j@���@���@�33@�v�@��@��@�%@�?}@�X@���@�b@�|�@���@�M�@�{@���@���@�Q�@�|�@�\)@�K�@�C�@�l�@�;d@���@���@���@�ff@���@�7L@���@�z�@�(�@�b@��@��;@��w@�+@��@�ff@�M�@�V@�$�@���@��@�G�@���@��@��
@�o@���@��@�/@���@�Z@�1'@�ƨ@�@��\@�5?@��@���@���@���@���@�@��@��T@���@�X@��`@��j@��@� �@�  @��m@��
@�@���@��u@�b@�\)@���@��\@��#@�~�@��@�Ĝ@��@��P@�;d@��R@���@��h@�hs@�&�@�V@��@�r�@�Q�@�I�@� �@���@��@���@�9X@��m@��P@�t�@�\)@��y@���@���@�G�@�V@��@�1'@�C�@��@���@�ȴ@�n�@�E�@��@�x�@�%@��@��`@��u@�bN@�b@�S�@�"�@�
=@��y@���@�n�@�=q@��@��^@��@�`B@�/@���@��/@��9@�j@�(�@��w@��P@�K�@��@�ȴ@�M�@��@���@��@�p�@�&�@�&�@��@��@�bN@�1@��@�C�@�+@�33@�33@�o@�ȴ@��\@�v�@�E�@�{@���@���@��-@���@��-@��#@���@��@��7@�X@���@��j@��@�z�@�r�@�9X@�1@��;@���@�|�@�dZ@�C�@�33@�+@�o@��@��H@�ȴ@�v�@�5?@�$�@�J@��@��#@��h@�7L@��@��@��9@��u@�r�@�1@���@u�h@b��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AɓuAɗ�Aɕ�AɑhA�r�A��A��AȑhA�1'A���A�n�Aƕ�A��Aŉ7A�;dA�bA�A��A���A�A�%A�A�%A�A���A���A��A��`A��#A���A�ƨA���Aĺ^Aě�AąA�VA�oAÑhA���A�A�v�A�hsA�E�A�{A��^A�VA���A�{A�z�A�A��A��-A��-A��A��FA��A��7A�dZA�-A���A�1A��A�r�A�z�A�ȴA��;A���A��A��RA��A�VA�A��hA��RA�l�A�`BA���A��A��A�A�^5A�JA�5?A�5?A��jA�n�A�?}A�9XA�1A��`A��A��A��FA�(�A���A��A���A�ƨA�I�A���A�x�A�jA�A�A�ffA���A�M�A�t�AG�A~=qA|JAzJAvv�Ar�Ap^5AnjAjVAg\)Af��Ae�wAb9XAaG�A_�;A^��A]��A\{AX~�AW��AV�\AU�AU�FAT�AS�TASx�ARn�AQ�hAPz�ANQ�AM
=ALbNAK�AJ��AI��AH��AG��AG?}AG�AF��AEx�AC�A@��A?��A?�hA>��A=�A;�#A:JA8��A8n�A7��A5�PA4��A4A3t�A3;dA2z�A0�`A/�mA/\)A-�wA,�!A+�;A*1'A'��A&�RA&ffA&{A$�`A$  A#��A"��A"M�A!��A �/A�mA��AA�A�DA�mA��A��AVAjA��A��AJA�A\)A�A9XA��A��A�mA^5A��A
~�A��At�AffA�TA�-AXA
=A�A�A��A&�A �HA (�@���@��7@�%@�I�@��R@��@��u@�=q@�j@��@��@�?}@�@�b@��H@�D@柾@�p�@�b@���@�w@�|�@◍@���@��;@��#@�bN@�  @ۮ@�t�@���@�x�@؋D@�(�@�  @��@ו�@�o@�K�@ם�@�X@ӕ�@ҸR@�ff@ѡ�@ЋD@��
@���@Ώ\@́@�\)@ɉ7@� �@��@���@�  @\@�`B@��@�j@���@���@�33@�v�@��@��@�%@�?}@�X@���@�b@�|�@���@�M�@�{@���@���@�Q�@�|�@�\)@�K�@�C�@�l�@�;d@���@���@���@�ff@���@�7L@���@�z�@�(�@�b@��@��;@��w@�+@��@�ff@�M�@�V@�$�@���@��@�G�@���@��@��
@�o@���@��@�/@���@�Z@�1'@�ƨ@�@��\@�5?@��@���@���@���@���@�@��@��T@���@�X@��`@��j@��@� �@�  @��m@��
@�@���@��u@�b@�\)@���@��\@��#@�~�@��@�Ĝ@��@��P@�;d@��R@���@��h@�hs@�&�@�V@��@�r�@�Q�@�I�@� �@���@��@���@�9X@��m@��P@�t�@�\)@��y@���@���@�G�@�V@��@�1'@�C�@��@���@�ȴ@�n�@�E�@��@�x�@�%@��@��`@��u@�bN@�b@�S�@�"�@�
=@��y@���@�n�@�=q@��@��^@��@�`B@�/@���@��/@��9@�j@�(�@��w@��P@�K�@��@�ȴ@�M�@��@���@��@�p�@�&�@�&�@��@��@�bN@�1@��@�C�@�+@�33@�33@�o@�ȴ@��\@�v�@�E�@�{@���@���@��-@���@��-@��#@���@��@��7@�X@���@��j@��@�z�@�r�@�9X@�1@��;@���@�|�@�dZ@�C�@�33@�+@�o@��@��H@�ȴ@�v�@�5?@�$�@�J@��@��#@��h@�7L@��@��@��9@��u@�r�@�1@���@u�h@b��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�bB�bB�bB�bB�uB��B��B�qBB�B�B�B9XB?}B<jB;dB;dB=qBA�BF�BH�BH�BH�BH�BH�BH�BH�BH�BH�BG�BG�BF�BF�BH�BK�BQ�B^5Bx�B��B��B�9B�^B��B�;B�B�B�fB�B�RB��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�Bx�Bn�B]/BXBVBW
BVBT�BS�BN�BD�B49B(�B!�B�BoB	7B��B�yB�;B��B�}B��B~�BffBP�B;dBVB
��B
�B
�B
�TB
ǮB
�B
�B
��B
��B
��B
��B
��B
��B
p�B
z�B
�uB
�=B
� B
n�B
]/B
G�B
0!B
!�B
hB	��B	�yB	�`B	�)B	ƨB	�qB	�'B	��B	��B	�uB	�B	{�B	s�B	p�B	n�B	gmB	bNB	_;B	YB	R�B	L�B	@�B	8RB	5?B	1'B	.B	(�B	$�B	 �B	�B	�B	�B	uB		7B��B��B��B��B�B�B�`B�BB�;B�;B�)B�B�B�
B��B��B��B��B��BɺBĜBB�}B�jB�^B�XB�LB�9B�'B�-B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B�hB�\B�PB�DB�=B�7B�1B�+B�B�B�B�B�B~�B}�B}�B� B�B� B~�B}�B}�B}�B|�B{�B{�B{�Bz�Bz�Bz�By�Bx�Bw�Bv�Bx�Bz�B{�Bz�Bz�Bz�B{�B{�B{�B}�B|�B|�B|�B{�B|�B{�B~�B}�B}�B}�B}�B{�Bz�By�By�By�By�By�B}�B�B�7B�JB�DB�7B�7B�%B�B�B�B�B~�Bz�Bx�B{�B�%B�1B�+B�7B�DB�PB�JB�JB�\B��B��B��B�B�9B�RB��BǮB��B��B��B��B��B��B��B�B�B�B�B�)B�mB�B�B�B��B��B��B��B	B	B	%B	+B	+B		7B		7B	DB	JB	PB	bB	hB	oB	{B	�B	�B	�B	�B	#�B	%�B	&�B	,B	1'B	5?B	6FB	7LB	:^B	>wB	@�B	@�B	@�B	A�B	B�B	B�B	D�B	F�B	G�B	G�B	G�B	H�B	H�B	I�B	K�B	K�B	M�B	O�B	P�B	S�B	S�B	N�B	L�B	I�B	G�B	E�B	D�B	L�B	[#B	iyB	k�B	l�B	n�B	u�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�1B	�1B	�+B	�+B	�1B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�LB	�RB	�XB	�dB	�}B	��B	B	ÖB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�5B	�BB	�HB	�TB	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

B
'�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�bB�bB�bB�bB�uB��B��B�qBB�B�B�B9XB?}B<jB;dB;dB=qBA�BF�BH�BH�BH�BH�BH�BH�BH�BH�BH�BG�BG�BF�BF�BH�BK�BQ�B^5Bx�B��B��B�9B�^B��B�;B�B�B�fB�B�RB��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�Bx�Bn�B]/BXBVBW
BVBT�BS�BN�BD�B49B(�B!�B�BoB	7B��B�yB�;B��B�}B��B~�BffBP�B;dBVB
��B
�B
�B
�TB
ǮB
�B
�B
��B
��B
��B
��B
��B
��B
p�B
z�B
�uB
�=B
� B
n�B
]/B
G�B
0!B
!�B
hB	��B	�yB	�`B	�)B	ƨB	�qB	�'B	��B	��B	�uB	�B	{�B	s�B	p�B	n�B	gmB	bNB	_;B	YB	R�B	L�B	@�B	8RB	5?B	1'B	.B	(�B	$�B	 �B	�B	�B	�B	uB		7B��B��B��B��B�B�B�`B�BB�;B�;B�)B�B�B�
B��B��B��B��B��BɺBĜBB�}B�jB�^B�XB�LB�9B�'B�-B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B�hB�\B�PB�DB�=B�7B�1B�+B�B�B�B�B�B~�B}�B}�B� B�B� B~�B}�B}�B}�B|�B{�B{�B{�Bz�Bz�Bz�By�Bx�Bw�Bv�Bx�Bz�B{�Bz�Bz�Bz�B{�B{�B{�B}�B|�B|�B|�B{�B|�B{�B~�B}�B}�B}�B}�B{�Bz�By�By�By�By�By�B}�B�B�7B�JB�DB�7B�7B�%B�B�B�B�B~�Bz�Bx�B{�B�%B�1B�+B�7B�DB�PB�JB�JB�\B��B��B��B�B�9B�RB��BǮB��B��B��B��B��B��B��B�B�B�B�B�)B�mB�B�B�B��B��B��B��B	B	B	%B	+B	+B		7B		7B	DB	JB	PB	bB	hB	oB	{B	�B	�B	�B	�B	#�B	%�B	&�B	,B	1'B	5?B	6FB	7LB	:^B	>wB	@�B	@�B	@�B	A�B	B�B	B�B	D�B	F�B	G�B	G�B	G�B	H�B	H�B	I�B	K�B	K�B	M�B	O�B	P�B	S�B	S�B	N�B	L�B	I�B	G�B	E�B	D�B	L�B	[#B	iyB	k�B	l�B	n�B	u�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�1B	�1B	�+B	�+B	�1B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�LB	�RB	�XB	�dB	�}B	��B	B	ÖB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�5B	�BB	�HB	�TB	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

B
'�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191742                              AO  ARCAADJP                                                                    20181005191742    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191742  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191742  QCF$                G�O�G�O�G�O�8000            