CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:37Z AOML 3.0 creation; 2016-05-31T21:48:57Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230637  20160531144857  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_005                   2C  D   APEX                            5370                            041511                          846 @�7
��o�1   @�7m	�@8�S����c��l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�9�D�� D���D�� D�VfD�� D�� D�	�D�<�D���D�ɚD���D�I�D�y�D�� D��D�C3D�vfD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�fg@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dtl�Dy�gD�gD�6gD�|�D��gD���D�S3D�|�D���D�gD�9�D���D��gD���D�FgD�vgD��D�	�D�@ D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�;dA��A���A���A��7A��uA�O�A�;dA�1'A�(�A�"�A��A��A�bA�JA�JA�1A�%A�A�  A�  A�  A�  A���A���A���A���A���A��A��A��;A���A��^A�z�A��A��A�ffA�M�A��A�{A��HA��!A�p�A�-A��TA��!A��7A�hsA�S�A�A�A�33A� �A��mA��A�&�A�A���A��uA�z�A�"�A��TA�v�A��A��wA��TA�hsA�G�A��A�r�A�
=A���A��FA���A�$�A�S�A���A�O�A�p�A�O�A�?}A�x�A�dZA�hsA���A��A���A��TA��-A�oA�l�A�\)A�ƨA���A��`A�K�A��
A��`A��uA�XA�Q�A�bNA���A��hA�(�A�XA�A��
A��A���A��+A��A��A��
A���A���A~A�A{�Ay&�Aw\)Au\)At5?Ar�+Ap�AmO�AkoAjJAh�Ag
=AeG�Ac�PAbv�Aal�A`bNA^1'A];dA\�`A\A�A[/AZ�/AZM�AXbNAWXAV�yAV�AT�`ATJAR~�AQ�mAP�ANA�AL�`AK�PAJr�AI`BAHȴAH~�AHJAFQ�AEoAD-AC�ABĜAA��A@jA?7LA>r�A=��A=+A:��A9�A7��A5S�A4��A3�mA1�
A1`BA0~�A.��A-�A-&�A+�A+
=A*z�A)dZA'��A';dA&�HA&ffA#hsA Q�A�A  AI�A�^A/AffA��AjA�
A+A�`A��A1A�!A=qA��A�wA�`A&�A
5?A	�A	p�A�+A�^A��AO�A��A$�A�;A+A33A j@���@���@�@�%@�j@��F@��@�7L@�|�@���@��@���@���@�F@�n�@�-@�V@�5?@��^@�7L@�j@��@�z�@�K�@�u@��T@�D@�b@�!@�r�@�l�@��D@�/@��#@�Ĝ@ޟ�@� �@�(�@�33@���@Ձ@�Z@� �@�ȴ@���@�?}@Ѻ^@Ϯ@��@�ȴ@�{@���@͉7@�Q�@��@���@�b@�
=@�^5@ř�@���@�
=@��h@�?}@�%@�dZ@���@�hs@� �@��m@�b@��@�C�@���@��h@�%@���@�ƨ@���@�dZ@�
=@��@�S�@���@�  @��P@��@�%@��j@��@�hs@�j@�  @�33@��-@�/@���@�V@�z�@�Z@�1@�  @���@��@�@��+@��@�=q@�ff@�5?@��^@�?}@��@��@�V@�%@�V@��/@�r�@�9X@���@���@���@��m@���@�ƨ@�l�@�;d@��y@���@���@�ff@�-@�@���@���@��@���@�9X@�o@��#@�bN@�z�@��D@��w@���@��R@���@��@�t�@�~�@��h@��@�9X@���@��w@���@��@�~�@�-@��@��#@���@��7@�X@�7L@�V@���@�Ĝ@��@���@�r�@�A�@�(�@��@��
@�ƨ@��@��
@��m@��F@���@�ȴ@��\@�n�@�n�@�^5@�5?@�n�@�v�@�^5@�J@��h@�p�@�p�@��7@��7@���@��^@��7@�7L@�/@�&�@�%@���@�Ĝ@��j@���@���@��D@�bN@�b@��@�dZ@�\)@�;d@�33@���@��F@�Q�@���@���@���@��@�bN@�A�@��;@��F@���@���@��
@�K�@��@��@��@�/@�j@�(�@��@�1@��@���@�b@� �@� �@�b@�  @��@��
@���@��@���@�dZ@�C�@��R@�V@�-@�@��T@��#@��#@�@��h@�hs@�`B@�v�@~E�@v@o;d@iX@bJ@Z�@Q%@J^5@C�F@=?}@6ff@1G�@-�@(��@"��@p�@�@�@  @��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�;dA��A���A���A��7A��uA�O�A�;dA�1'A�(�A�"�A��A��A�bA�JA�JA�1A�%A�A�  A�  A�  A�  A���A���A���A���A���A��A��A��;A���A��^A�z�A��A��A�ffA�M�A��A�{A��HA��!A�p�A�-A��TA��!A��7A�hsA�S�A�A�A�33A� �A��mA��A�&�A�A���A��uA�z�A�"�A��TA�v�A��A��wA��TA�hsA�G�A��A�r�A�
=A���A��FA���A�$�A�S�A���A�O�A�p�A�O�A�?}A�x�A�dZA�hsA���A��A���A��TA��-A�oA�l�A�\)A�ƨA���A��`A�K�A��
A��`A��uA�XA�Q�A�bNA���A��hA�(�A�XA�A��
A��A���A��+A��A��A��
A���A���A~A�A{�Ay&�Aw\)Au\)At5?Ar�+Ap�AmO�AkoAjJAh�Ag
=AeG�Ac�PAbv�Aal�A`bNA^1'A];dA\�`A\A�A[/AZ�/AZM�AXbNAWXAV�yAV�AT�`ATJAR~�AQ�mAP�ANA�AL�`AK�PAJr�AI`BAHȴAH~�AHJAFQ�AEoAD-AC�ABĜAA��A@jA?7LA>r�A=��A=+A:��A9�A7��A5S�A4��A3�mA1�
A1`BA0~�A.��A-�A-&�A+�A+
=A*z�A)dZA'��A';dA&�HA&ffA#hsA Q�A�A  AI�A�^A/AffA��AjA�
A+A�`A��A1A�!A=qA��A�wA�`A&�A
5?A	�A	p�A�+A�^A��AO�A��A$�A�;A+A33A j@���@���@�@�%@�j@��F@��@�7L@�|�@���@��@���@���@�F@�n�@�-@�V@�5?@��^@�7L@�j@��@�z�@�K�@�u@��T@�D@�b@�!@�r�@�l�@��D@�/@��#@�Ĝ@ޟ�@� �@�(�@�33@���@Ձ@�Z@� �@�ȴ@���@�?}@Ѻ^@Ϯ@��@�ȴ@�{@���@͉7@�Q�@��@���@�b@�
=@�^5@ř�@���@�
=@��h@�?}@�%@�dZ@���@�hs@� �@��m@�b@��@�C�@���@��h@�%@���@�ƨ@���@�dZ@�
=@��@�S�@���@�  @��P@��@�%@��j@��@�hs@�j@�  @�33@��-@�/@���@�V@�z�@�Z@�1@�  @���@��@�@��+@��@�=q@�ff@�5?@��^@�?}@��@��@�V@�%@�V@��/@�r�@�9X@���@���@���@��m@���@�ƨ@�l�@�;d@��y@���@���@�ff@�-@�@���@���@��@���@�9X@�o@��#@�bN@�z�@��D@��w@���@��R@���@��@�t�@�~�@��h@��@�9X@���@��w@���@��@�~�@�-@��@��#@���@��7@�X@�7L@�V@���@�Ĝ@��@���@�r�@�A�@�(�@��@��
@�ƨ@��@��
@��m@��F@���@�ȴ@��\@�n�@�n�@�^5@�5?@�n�@�v�@�^5@�J@��h@�p�@�p�@��7@��7@���@��^@��7@�7L@�/@�&�@�%@���@�Ĝ@��j@���@���@��D@�bN@�b@��@�dZ@�\)@�;d@�33@���@��F@�Q�@���@���@���@��@�bN@�A�@��;@��F@���@���@��
@�K�@��@��@��@�/@�j@�(�@��@�1@��@���@�b@� �@� �@�b@�  @��@��
@���@��@���@�dZ@�C�@��R@�V@�-@�@��T@��#@��#@�@��h@�hs@�`B@�v�@~E�@v@o;d@iX@bJ@Z�@Q%@J^5@C�F@=?}@6ff@1G�@-�@(��@"��@p�@�@�@  @��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�3B�-B�'B�'B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�B�B�B�B�B�!B�?B�^BB��B�B�BB�B �B(�B1'BB�BM�BYBbNBgmBjBgmBffBgmBhsBiyBjBiyBhsBx�B�\B�oB�hB�VB�\B�=B�7B|�Bp�Br�BaHBXB>wB'�B!�B�B��B��B�}B�9B��B��B�\B�Bt�B[#BI�BF�B/B!�B{BoB1B�BǮB��B�bBn�BZBI�B>wBW
BS�BQ�BR�B\)BT�BF�BA�B8RB(�BoB
��B
�/B
�B
ĜB
�FB
��B
�=B
y�B
cTB
=qB
(�B
�B
JB	��B	��B	�sB	�B	ƨB	�RB	�'B	��B	��B	�uB	�=B	�B	�B	� B	u�B	t�B	p�B	iyB	hsB	hsB	hsB	cTB	aHB	^5B	ZB	T�B	O�B	G�B	D�B	;dB	+B	!�B	�B	�B	hB	hB	hB	PB	B��B��B��B	1B	%B	  B��B��B�B�B�TB�B��B�RB�qB�qB�?B�3B�B��B��B��B��B�uB�VB�+B�B�+B�7B�%BiyBQ�BQ�BS�BP�BQ�BO�BQ�BM�BM�BI�BE�B=qB;dB:^B:^B8RB9XB:^B8RB8RB8RB7LB5?B49B49B33B49B33B33B2-B0!B/B.B-B.B.B.B-B-B.B/B-B)�B(�B&�B'�B)�B+B.B0!B1'B2-B49B7LB:^B;dB<jB>wB;dB9XB8RB5?B49B7LBD�BL�BQ�BR�BO�BK�BG�BI�BM�BJ�BK�BM�BN�BP�BT�BdZB`BB_;B_;BcTBgmBjBiyBgmBe`BffBffBe`BdZBaHBcTBiyBjBiyBjBiyBjBjBjBn�Bp�Bp�Bq�Bq�Bt�Bx�By�Bz�B{�B}�B� B�B�1B�VB�uB�{B��B��B�JB|�Bt�Bu�Bw�B|�B� B�B�uB��B��B��B��B��B��B�3B�LB�^B�}BÖBĜBƨBȴB��B��B��B��B�B�)B�)B�/B�5B�TB�yB�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	+B	+B	B	B	B	B	1B	+B	B	%B	JB	\B	VB	PB	JB	DB	JB	JB	JB	DB	VB	hB	oB	uB	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	'�B	,B	0!B	33B	6FB	8RB	9XB	;dB	C�B	G�B	H�B	J�B	M�B	P�B	R�B	S�B	T�B	[#B	`BB	bNB	cTB	e`B	jB	m�B	t�B	w�B	|�B	� B	�B	�B	�=B	�=B	�=B	�=B	�DB	�JB	�JB	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�XB	�^B	�^B	�XB	�XB	�XB	�}B	B	ÖB	ŢB	ŢB	ĜB	ÖB	��B	��B	��B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�NB	��B	��B
	7B
hB
�B
"�B
-B
49B
:^B
A�B
G�B
L�B
O�B
R�B
ZB
_;B
e`B
iyB
l�B
q�11111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�FB�>B�8B�0B�-B�&B�$B�%B�B�B�B�B�B�B�B�B� B�B�B�B�G�O�B� B� B� B� B�"B�$B�+B�JB�jBB��B�B�B"B�B �B)B17BB�BM�BY#Bb\BgzBj�BgwBfrBgwBhBi�Bj�Bi�Bh}Bx�B�jB�B�uB�cB�jB�IB�HB}Bp�Br�BaUBXB>�B'�B!�B�B��B��B��B�EB��B��B�gB�Bt�B[,BI�BF�B/#B!�B�ByB7B�BǷB��B�mBn�BZ)BI�B>�BWBS�BQ�BR�B\2BUBF�BA�B8]B)B}B
��B
�;B
�B
īB
�TB
��B
�KB
y�B
cdB
=B
)B
�B
ZB	�B	��B	�B	�#B	ƻB	�fB	�<B	�
B	��B	��B	�RB	�0B	�(B	�B	u�B	t�B	p�B	i�B	h�B	h�B	h�B	ckB	a^B	^MB	Z5B	UB	O�B	G�B	D�B	;~B	+B	!�B	�B	�B	�B	�B	�B	lB	4B�B�B�B	NB	AB	 B��B��B��B�B�sB�<B��B�qB��B��B�_B�RB�#B��B��B��B��B��B�vB�MB�;B�NB�YB�GBi�BRBRBTBQ
BRBPBRBM�BM�BI�BE�B=|B;�B:�B:�B8vB9}B:�B8vB8wB8xB7rB5dB4`B4_B3>B4^B3XB3ZB2SB0EB/AB.7B-3B.9B.9B.9B-4B-6B.8B/(B-3B*!B)B'B'�B*"B+(B.9B0GB1PB2SB4^B7qB:�B;�B<�B>�B;�B9B8wB5dB4]B7qBD�BL�BRBSBPBK�BG�BI�BM�BJ�BK�BM�BN�BQBU"Bd{B`dB_\B_^BcuBg�Bj�Bi�Bg�Be�Bf�Bf�Be�Bd}BakBcuBi�Bj�Bi�Bj�Bi�Bj�Bj�Bj�Bn�Bp�Bp�Bq�Bq�Bt�Bx�By�B{B|
B~B�#B�?B�RB�vB��B��B��B��B�jB}Bt�Bu�Bw�B}B�#B�9B��B��B��B��B��B��B�
B�PB�iB�|B��BöBĹB��B��B��B��B��B�B�;B�FB�FB�JB�RB�oB�B�B�B��B��B��B��B��B��B�B�B	'B	,B	-B	>B	EB	GB	6B	)B	"B	:B	NB	FB	:B	@B	cB	vB	rB	mB	dB	_B	eB	gB	dB	]B	tB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	(
B	, B	0;B	3MB	6_B	8mB	9pB	;}B	C�B	G�B	H�B	J�B	M�B	P�B	SB	TB	UB	[<B	`ZB	beB	cmB	exB	j�B	m�B	t�B	w�B	}B	�B	�&B	�4B	�UB	�VB	�VB	�TB	�ZB	�cB	�cB	�nB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�1B	�>B	�RB	�lB	�sB	�tB	�mB	�lB	�kB	��B	¤B	ëB	ŷB	ŸB	İB	ëB	��B	��B	��B	��B	£B	ıB	ƼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	� B	�$B	�*B	�9B	�BB	�bB	��B	�B
	JB
|B
�B
"�B
-!B
4KB
:pB
A�B
G�B
L�B
O�B
SB
Z,B
_KB
epB
i�B
l�B
q�11111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448572016053114485720160531144857  AO  ARCAADJP                                                                    20140721230637    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230637  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230637  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144857  IP                  G�O�G�O�G�O�                