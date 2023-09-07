CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:59Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135733  20190522121825  1727_5046_030                   2C  D   APEX                            2143                            040306                          846 @�<|ٱ��1   @�<}����@7�`A�7L�c�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BzffB~  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�3D�#3D�c3D�� D��fD�fD�S3D��fD���D��D�I�D�� D��3D�)�D�ffDڠ D�� D��D�<�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bz  B}��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC��C!��C#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC:  C;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCV  CW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D&  D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4�3D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DJ  DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm�3Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy��D�  D�` D���D��3D�3D�P D��3D��D�fD�FfD���D�� D�&fD�c3Dڜ�D���D�	�D�9�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-AϓuA�t�A�\)A�VA�Q�A�O�A�G�A�A�A�9XA�9XA�5?A�33A�5?A�5?A�33A�33A�33A�1'A�1'A�/A�-A�-A�+A�(�A�(�A��A�VA�JAήA�
=A¾wA�v�A�bA�O�A��
A�Q�A�z�A��RA�&�A��A���A�t�A�JA���A�oA�ZA���A�{A���A�O�A���A�9XA�1A���A�n�A��A�1A�ȴA���A�\)A���A�l�A���A�|�A�hsA�"�A���A���A�`BA�;dA��yA��hA�/A���A��A��#A���A�ffA�I�A�+A��/A��A���A���A�bNA�C�A�5?A�oA���A��A��+A�Q�A��A��FA�~�A��^A�?}A��A���A���A��RA�^5A���A�v�A��^A�v�A�A�A��yA�A�A�p�A��HA��DA�A���A�9XA�+A��\A�5?A�M�A�ȴA��mA���A�z�A���A���A�$�A�t�A�=qA��^A�9XA��mA�7LA��A���A�1'A��^A��uA��/A~��A{K�Az�`AzffAx�+AtI�AsdZAp�yAoG�AmK�Al  AkoAi�7Ag�Af��Ae�mAeK�Ad��Ad^5AcdZAa
=A`A_VA]��A]S�A]?}A\�RA[�TA[%AY�PAX��AX^5AW�#AV�HAVE�AU�mAU�ATjAS�PAS+AS�AR��AR�AMƨAKK�AG��AB~�A?�A=��A;�mA:��A:-A9p�A8�A733A6bNA5�A5��A5VA49XA3��A2�jA1��A1�A0��A0�A0=qA0  A/��A/;dA.�A-�wA,��A+x�A*�A)VA'��A'x�A&�DA%A$��A$��A$~�A#��A"�jA!�A ~�A�TA\)A��AƨA�9A�A�hA�AAVAVA�mA��A��A��AI�AZA5?A��A�hAA�yA�9A�\A=qA�A-A�7A
�A�A�;A^5A�A�\A�#A�A$�A ��@��@��T@�M�@��w@�/@��9@��;@���@�5?@��`@�V@�$�@���@�\)@��@�v�@�$�@�Q�@�9X@���@��y@���@���@�l�@�A�@ش9@��@���@�ȴ@�{@�(�@�dZ@�-@��@�
=@�33@�o@�A�@�%@��m@�Z@ϝ�@���@��@˕�@�
=@ʏ\@�~�@�V@�J@ɩ�@���@��;@�V@�l�@�=q@��7@��@���@��D@��D@���@�@��@���@�V@���@���@�`B@�/@��j@��@�C�@��@�\)@��@��\@��+@��\@�^5@�J@��#@���@��@��`@��@���@�Q�@�Q�@�"�@��\@�{@��h@�G�@�Z@��@�n�@��@���@��@��D@��@���@��^@�7L@�V@��j@�(�@�1@�b@�I�@�bN@�(�@��@��
@���@�33@�~�@�G�@���@��@�J@�p�@�?}@�G�@�X@�/@��j@��
@�"�@��@�=q@�@���@�@���@�G�@�Z@� �@�(�@���@��w@���@�dZ@���@��@���@���@�ff@�$�@���@���@�r�@�Z@�9X@��@�b@�1@�1@���@�ƨ@���@�S�@�C�@�33@�"�@���@�E�@�@��@���@��-@�x�@�?}@�&�@���@���@�  @�ƨ@��
@��F@��@�l�@�S�@�+@�@���@�n�@�@���@�`B@�r�@�1@���@�"�@���@�=q@�{@��@��-@�7L@���@�I�@�Q�@�b@��;@�l�@�
=@��H@���@���@�5?@��@�@���@��7@�p�@�O�@�7L@��@�V@��@��H@~$�@p�9@h��@`��@ZJ@Rn�@L��@D��@@A�@;"�@49X@-�@(��@$�D@?}@�@Z@Ĝ@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-AϓuA�t�A�\)A�VA�Q�A�O�A�G�A�A�A�9XA�9XA�5?A�33A�5?A�5?A�33A�33A�33A�1'A�1'A�/A�-A�-A�+A�(�A�(�A��A�VA�JAήA�
=A¾wA�v�A�bA�O�A��
A�Q�A�z�A��RA�&�A��A���A�t�A�JA���A�oA�ZA���A�{A���A�O�A���A�9XA�1A���A�n�A��A�1A�ȴA���A�\)A���A�l�A���A�|�A�hsA�"�A���A���A�`BA�;dA��yA��hA�/A���A��A��#A���A�ffA�I�A�+A��/A��A���A���A�bNA�C�A�5?A�oA���A��A��+A�Q�A��A��FA�~�A��^A�?}A��A���A���A��RA�^5A���A�v�A��^A�v�A�A�A��yA�A�A�p�A��HA��DA�A���A�9XA�+A��\A�5?A�M�A�ȴA��mA���A�z�A���A���A�$�A�t�A�=qA��^A�9XA��mA�7LA��A���A�1'A��^A��uA��/A~��A{K�Az�`AzffAx�+AtI�AsdZAp�yAoG�AmK�Al  AkoAi�7Ag�Af��Ae�mAeK�Ad��Ad^5AcdZAa
=A`A_VA]��A]S�A]?}A\�RA[�TA[%AY�PAX��AX^5AW�#AV�HAVE�AU�mAU�ATjAS�PAS+AS�AR��AR�AMƨAKK�AG��AB~�A?�A=��A;�mA:��A:-A9p�A8�A733A6bNA5�A5��A5VA49XA3��A2�jA1��A1�A0��A0�A0=qA0  A/��A/;dA.�A-�wA,��A+x�A*�A)VA'��A'x�A&�DA%A$��A$��A$~�A#��A"�jA!�A ~�A�TA\)A��AƨA�9A�A�hA�AAVAVA�mA��A��A��AI�AZA5?A��A�hAA�yA�9A�\A=qA�A-A�7A
�A�A�;A^5A�A�\A�#A�A$�A ��@��@��T@�M�@��w@�/@��9@��;@���@�5?@��`@�V@�$�@���@�\)@��@�v�@�$�@�Q�@�9X@���@��y@���@���@�l�@�A�@ش9@��@���@�ȴ@�{@�(�@�dZ@�-@��@�
=@�33@�o@�A�@�%@��m@�Z@ϝ�@���@��@˕�@�
=@ʏ\@�~�@�V@�J@ɩ�@���@��;@�V@�l�@�=q@��7@��@���@��D@��D@���@�@��@���@�V@���@���@�`B@�/@��j@��@�C�@��@�\)@��@��\@��+@��\@�^5@�J@��#@���@��@��`@��@���@�Q�@�Q�@�"�@��\@�{@��h@�G�@�Z@��@�n�@��@���@��@��D@��@���@��^@�7L@�V@��j@�(�@�1@�b@�I�@�bN@�(�@��@��
@���@�33@�~�@�G�@���@��@�J@�p�@�?}@�G�@�X@�/@��j@��
@�"�@��@�=q@�@���@�@���@�G�@�Z@� �@�(�@���@��w@���@�dZ@���@��@���@���@�ff@�$�@���@���@�r�@�Z@�9X@��@�b@�1@�1@���@�ƨ@���@�S�@�C�@�33@�"�@���@�E�@�@��@���@��-@�x�@�?}@�&�@���@���@�  @�ƨ@��
@��F@��@�l�@�S�@�+@�@���@�n�@�@���@�`B@�r�@�1@���@�"�@���@�=q@�{@��@��-@�7L@���@�I�@�Q�@�b@��;@�l�@�
=@��H@���@���@�5?@��@�@���@��7@�p�@�O�@�7L@��@�V@��@��H@~$�@p�9@h��@`��@ZJ@Rn�@L��@D��@@A�@;"�@49X@-�@(��@$�D@?}@�@Z@Ĝ@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBv�Bu�Bu�Bu�Bv�Bv�Bw�Bx�Bx�Bx�Bx�Bw�Bw�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bw�Bt�Bl�BXB_;Bu�B�DB�bB�VB�uB��B��B�-B�FB�^BBĜB��B�)B�mB�B��B��B  B1BDB\B{B�B�B�B�B!�B$�B)�B0!B2-B2-B2-B33B6FB7LB7LB8RB7LB7LB8RB8RB8RB8RB9XB:^B<jB@�BB�BG�BJ�BL�BN�BP�BN�BL�BJ�BK�BI�BC�B?}B:^B/B,B'�B�B�B{BVBB�B�B�ZB�/B�B��B�}B�?B�B��B��B�JBo�BP�B8RB#�B�BB�fB��B�B��B�JBk�BS�B9XB�B+B
��B
�sB
ĜB
�B
n�B
jB
bNB
;dB
 �B
�B
�B
+B	�yB	�BB	��B	ŢB	�RB	�!B	��B	��B	�uB	�bB	�PB	�JB	�JB	�=B	�JB	��B	��B	��B	��B	�oB	�bB	�PB	�7B	�%B	�B	� B	}�B	z�B	v�B	u�B	s�B	r�B	q�B	m�B	jB	iyB	gmB	^5B	;dB	�B��B�BŢB�dB�dB�FB�-B�B�B��B��B�B��B��B��B�B�'B�'B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�hB�\B�JB�DB�7B�+B�%B�B�B� B~�B}�B{�By�Bx�Bx�Bv�Bv�Bu�Bt�Br�Bo�Bl�Be`BcTBaHBaHBaHB`BB_;B[#BbNBZBW
BT�BR�BP�BM�BJ�BH�BE�BC�B@�B<jB:^B7LB49B33B2-B1'B0!B/B.B+B(�B&�B'�B&�B&�B&�B$�B!�B!�B!�B#�B%�B&�B)�B9XBB�BA�BI�BL�BL�BI�BH�BG�BE�BJ�Bp�Bw�B�DB��B�{B�JB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�?B�FB�FB�LB�LB�LB�FB�LB�?B�FB�LB�LB�RB�RB�XB�^B�jB�qB��BBĜBǮB��B��B��B��B��B�B�B�B�B�/B�/B�5B�;B�BB�NB�`B�yB�B�B�B�B�B��B��B��B	B	B	B	B	B	B	B	%B		7B	PB	PB	VB	hB	{B	�B	{B	�B	�B	!�B	'�B	'�B	'�B	(�B	(�B	.B	1'B	49B	7LB	8RB	9XB	9XB	:^B	:^B	;dB	;dB	<jB	=qB	@�B	D�B	H�B	J�B	J�B	K�B	L�B	L�B	M�B	M�B	N�B	O�B	P�B	R�B	R�B	S�B	S�B	W
B	ZB	]/B	]/B	^5B	_;B	`BB	cTB	dZB	ffB	hsB	jB	m�B	r�B	v�B	y�B	z�B	{�B	}�B	~�B	�B	�B	�B	�%B	�+B	�1B	�7B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�^B	�jB	�qB	�wB	�wB	�}B	B	ÖB	ĜB	ĜB	ĜB	��B	�B
B
VB
�B
$�B
.B
6FB
=qB
B�B
F�B
M�B
R�B
W
B
ZB
_;B
dZB
hsB
l�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By�Bv�Bv�Bu�Bv�Bv�Bw�Bx�Bx�Bx�Bx�Bw�Bw�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�By�Bv�Bs�B[#BcTB{�B�hB�oB�bB��B��B��B�3B�LB�jBÖBǮB��B�;B�B�B��B  BB	7BJBhB�B�B�B�B!�B#�B'�B-B2-B33B33B33B5?B7LB8RB9XB:^B9XB8RB8RB9XB:^B9XB:^B;dB>wBA�BB�BG�BK�BM�BN�BQ�BO�BN�BK�BL�BK�BD�B@�B=qB1'B-B)�B"�B�B�BoB
=B��B�B�`B�;B�#B��BB�LB�B��B��B�oBx�BYB=qB&�B�B
=B�BŢB�'B��B��Bs�B]/BB�B'�BDB
��B
��B
�/B
�JB
o�B
m�B
k�B
A�B
!�B
�B
�B
bB	�B	�fB	��B	��B	�jB	�3B	�B	��B	��B	�oB	�\B	�VB	�PB	�VB	�{B	��B	��B	��B	��B	�uB	�oB	�bB	�JB	�DB	�%B	�B	� B	}�B	x�B	w�B	v�B	u�B	t�B	n�B	jB	jB	jB	hsB	@�B	%�B	B�#B��B�}B�qB�RB�9B�-B�B�B�B�B�B�B�B�!B�3B�3B�-B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�oB�oB�hB�bB�\B�JB�7B�1B�+B�+B�B�B� B~�B{�B{�Bz�Bw�Bw�Bw�Bu�Bs�Bt�Bq�BhsBgmBcTBbNBbNBaHB`BB`BBdZB\)B\)BYBW
BVBP�BM�BK�BH�BF�BD�B?}B>wB=qB8RB7LB33B33B2-B0!B0!B/B/B+B(�B'�B'�B'�B'�B'�B%�B%�B%�B&�B(�B(�B9XBE�BA�BJ�BM�BO�BK�BJ�BK�BF�BD�Bp�Bu�B�=B��B��B�JB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�9B�?B�FB�FB�LB�RB�RB�RB�LB�^B�LB�LB�RB�LB�RB�RB�^B�^B�qB�wB��BBŢBȴB��B��B��B��B��B�
B�B�B�)B�/B�5B�;B�BB�NB�ZB�fB�B�B�B�B�B�B��B��B��B	B	B	B	B	B	B	B	1B	DB	VB	PB	VB	hB	{B	�B	�B	�B	�B	"�B	(�B	(�B	'�B	(�B	)�B	0!B	1'B	49B	7LB	9XB	9XB	:^B	;dB	:^B	;dB	;dB	=qB	>wB	A�B	F�B	I�B	J�B	J�B	K�B	L�B	L�B	M�B	M�B	O�B	P�B	Q�B	R�B	R�B	S�B	T�B	XB	ZB	]/B	]/B	^5B	_;B	`BB	cTB	dZB	gmB	iyB	jB	m�B	r�B	v�B	y�B	z�B	{�B	}�B	� B	�B	�B	�B	�+B	�7B	�7B	�=B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�9B	�?B	�?B	�FB	�dB	�jB	�qB	�wB	�wB	�}B	B	ÖB	ĜB	ĜB	ĜB	��B	�B
B
VB
�B
$�B
.B
6FB
=qB
B�B
G�B
M�B
R�B
W
B
ZB
_;B
dZB
hsB
l�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<ě�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446442012010314464420120103144644  AO  ARGQ                                                                        20111130135733  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135733  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144644  IP                  G�O�G�O�G�O�                