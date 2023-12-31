CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:10Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               EA   AO  20111130140703  20190522121826  1727_5046_069                   2C  D   APEX                            2143                            040306                          846 @�n�v_�1   @�n���@7G-�c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	y�D
  D
� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy&fD�,�D�i�D���D��3D�33D�L�D�i�D��3D�fD�c3D�� D��fD�3D�C3Dڙ�D��3D�#3D�Y�D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���A��A>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU��CW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�3D	s3D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D  D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%s3D%��D&y�D&��D'y�D'��D(y�D(��D)y�D*  D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm�3Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy  D�)�D�ffD��fD�� D�0 D�I�D�ffD�� D�3D�` D���D��3D� D�@ DږfD�� D�  D�VfD�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�7LA�9XA�;dA�9XA�;dA�?}A�C�A�C�A�C�A�=qA�-A�{A���A��#A��A��\A�33A��TA�E�A�33A��A�ƨA�S�A�ZA��+A�;dA��A��A�{A���A�7LA��7A��PA�A��/A�33A�
=A�Q�A�C�A�t�A���A�t�A�C�A��A��7A���A�x�A�hsA�C�A��yA���A���A�l�A��jA�33A��A�x�A�z�A���A�`BA���A�bNA��A�ĜA��
A�  A�Q�A��A��!A�l�A�E�A���A�$�A��`A��9A��DA�K�A��A�A��A�E�A���A�t�A��DA�E�A�oA��A�$�A�p�A��wA�$�A��HA��A�dZA�1A�Q�A��A��;A��A��PA�A�A��9A�t�A��#A}hsAx~�Aw�Avz�AuoAo��Al�9Al=qAk��AkC�Ajv�Ah�yAg`BAe��Ad�uAdz�Af�uAg�Ae�mAeO�Ad~�Ac;dAa�-Aa33Aa%A_��A]�;A[x�AZ^5AZI�AZ(�AY�AX��AW�^AV�AVM�AUl�AT9XAS|�AQ��AO��AN�AM�AM7LALn�AK��AJ��AHA�AG��AG;dAEC�AChsAB�9AB{AA�PAA
=A@{A>�/A>r�A=�A=\)A<�A<bNA;�
A:~�A9��A7`BA6VA5�TA5l�A5XA5oA3��A2^5A1�-A0��A/�#A/33A.5?A-��A-33A,�+A,1A+O�A*A�A(1'A'%A&$�A%hsA$z�A$(�A#��A#A"=qA!"�A r�A|�AO�AM�A�;A��AK�A��A�7A��AjA�A�A�AA�hA�AS�A1'A��A�A�A�^A�AVAC�A��AAO�A
A�A	��A��A5?AA�A��AC�A��A A�@�&�@�V@�+@�?}@��@�~�@�x�@�7L@��/@��D@��@�@�ff@���@�@�^5@�Q�@��y@���@�ƨ@�
=@��@�\)@��@ؼj@ם�@�\)@�33@���@��T@�1@��@ҏ\@�^5@�E�@�{@щ7@�%@Ь@��@���@���@Χ�@���@�7L@�z�@�M�@�%@�t�@���@ļj@� �@���@���@Å@�ȴ@¸R@�@�ff@���@�V@��u@�1'@���@��y@�J@���@�hs@�?}@�b@��@�n�@�@�z�@�I�@�ƨ@�@�M�@��#@��j@���@��@��+@��@��P@�o@�~�@��@�hs@�%@��@�@�1@�=q@�G�@��D@�I�@�I�@�9X@���@�dZ@��@�&�@�33@��P@��@���@���@�v�@�V@��@���@�=q@�v�@��h@��j@��@��;@�;d@���@�$�@���@��j@�A�@�ƨ@���@�t�@�+@��@���@���@�~�@���@���@�b@�
=@��\@�~�@�~�@�E�@���@�hs@�/@��`@��@�bN@�Q�@�I�@�9X@�  @��@�K�@�C�@�+@�33@�
=@���@�$�@��@�j@�K�@�{@��^@�V@�V@�bN@�b@� �@�  @��w@�t�@��@���@���@���@��-@�@�5?@���@���@�x�@�hs@�7L@��@��@��@��/@���@�Q�@�A�@���@�S�@��H@���@�ȴ@���@��\@�n�@�-@��^@���@��-@��7@��@�hs@�%@���@�(�@�1@��;@�dZ@�C�@��y@��R@���@��\@�~�@�v�@�ff@�M�@�5?@��@���@�7L@���@�9X@�  @���@��F@�|�@�\)@�K�@�C�@�33@��@�@��R@�~�@�n�@�n�@�ff@�5?@��@���@�hs@��@��@��D@}/@vV@ol�@h��@_�;@Xr�@P �@I&�@@�`@9�@2��@.$�@( �@$j@!7L@��@��@dZ@��@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�7LA�9XA�;dA�9XA�;dA�?}A�C�A�C�A�C�A�=qA�-A�{A���A��#A��A��\A�33A��TA�E�A�33A��A�ƨA�S�A�ZA��+A�;dA��A��A�{A���A�7LA��7A��PA�A��/A�33A�
=A�Q�A�C�A�t�A���A�t�A�C�A��A��7A���A�x�A�hsA�C�A��yA���A���A�l�A��jA�33A��A�x�A�z�A���A�`BA���A�bNA��A�ĜA��
A�  A�Q�A��A��!A�l�A�E�A���A�$�A��`A��9A��DA�K�A��A�A��A�E�A���A�t�A��DA�E�A�oA��A�$�A�p�A��wA�$�A��HA��A�dZA�1A�Q�A��A��;A��A��PA�A�A��9A�t�A��#A}hsAx~�Aw�Avz�AuoAo��Al�9Al=qAk��AkC�Ajv�Ah�yAg`BAe��Ad�uAdz�Af�uAg�Ae�mAeO�Ad~�Ac;dAa�-Aa33Aa%A_��A]�;A[x�AZ^5AZI�AZ(�AY�AX��AW�^AV�AVM�AUl�AT9XAS|�AQ��AO��AN�AM�AM7LALn�AK��AJ��AHA�AG��AG;dAEC�AChsAB�9AB{AA�PAA
=A@{A>�/A>r�A=�A=\)A<�A<bNA;�
A:~�A9��A7`BA6VA5�TA5l�A5XA5oA3��A2^5A1�-A0��A/�#A/33A.5?A-��A-33A,�+A,1A+O�A*A�A(1'A'%A&$�A%hsA$z�A$(�A#��A#A"=qA!"�A r�A|�AO�AM�A�;A��AK�A��A�7A��AjA�A�A�AA�hA�AS�A1'A��A�A�A�^A�AVAC�A��AAO�A
A�A	��A��A5?AA�A��AC�A��A A�@�&�@�V@�+@�?}@��@�~�@�x�@�7L@��/@��D@��@�@�ff@���@�@�^5@�Q�@��y@���@�ƨ@�
=@��@�\)@��@ؼj@ם�@�\)@�33@���@��T@�1@��@ҏ\@�^5@�E�@�{@щ7@�%@Ь@��@���@���@Χ�@���@�7L@�z�@�M�@�%@�t�@���@ļj@� �@���@���@Å@�ȴ@¸R@�@�ff@���@�V@��u@�1'@���@��y@�J@���@�hs@�?}@�b@��@�n�@�@�z�@�I�@�ƨ@�@�M�@��#@��j@���@��@��+@��@��P@�o@�~�@��@�hs@�%@��@�@�1@�=q@�G�@��D@�I�@�I�@�9X@���@�dZ@��@�&�@�33@��P@��@���@���@�v�@�V@��@���@�=q@�v�@��h@��j@��@��;@�;d@���@�$�@���@��j@�A�@�ƨ@���@�t�@�+@��@���@���@�~�@���@���@�b@�
=@��\@�~�@�~�@�E�@���@�hs@�/@��`@��@�bN@�Q�@�I�@�9X@�  @��@�K�@�C�@�+@�33@�
=@���@�$�@��@�j@�K�@�{@��^@�V@�V@�bN@�b@� �@�  @��w@�t�@��@���@���@���@��-@�@�5?@���@���@�x�@�hs@�7L@��@��@��@��/@���@�Q�@�A�@���@�S�@��H@���@�ȴ@���@��\@�n�@�-@��^@���@��-@��7@��@�hs@�%@���@�(�@�1@��;@�dZ@�C�@��y@��R@���@��\@�~�@�v�@�ff@�M�@�5?@��@���@�7L@���@�9X@�  @���@��F@�|�@�\)@�K�@�C�@�33@��@�@��R@�~�@�n�@�n�@�ff@�5?@��@���@�hs@��@��@��D@}/@vV@ol�@h��@_�;@Xr�@P �@I&�@@�`@9�@2��@.$�@( �@$j@!7L@��@��@dZ@��@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�bB��B��B��B��B�B�XB��BƨBȴB��B��B��B��B��B��BŢBB�wB�^B�FB�9B�B��B��B��B�{B�PB�7B�+B�B� By�Bw�Bu�Bs�Bn�Be`B`BB]/BVBN�BC�B;dB(�B�B	7B��B�mB�5B��BŢB�dB�-B�B�B��B��B��B��B�uB�bB�PB�%BiyB[#BI�B5?B/B&�B�B�BuB	7B
��B
�B
�`B
�5B
�B
�
B
��B
��B
��B
�B
��B
�uB
�PB
�+B
~�B
y�B
q�B
O�B
/B
)�B
 �B
hB	�sB	ɺB	ÖB	�jB	�?B	��B	��B	�PB	�B	z�B	�hB	��B	�B	�ZB	�;B	�B	��B	ĜB	��B	�wB	�RB	�'B	��B	��B	��B	��B	��B	��B	��B	�uB	�VB	�1B	�B	~�B	t�B	gmB	bNB	^5B	[#B	W
B	Q�B	I�B	?}B	=qB	9XB	.B	"�B	�B	�B	�B	uB	PB		7B	%B	B	B��B��B��B�B�B�ZB�BB�5B�/B�)B�B��B��B��B��BȴBƨBB��B�}B�qB�XB�FB�-B�B�B��B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�DB�=B�+B�B�B�B�B� B}�B{�By�Bv�Bs�Bq�Bp�Bo�Bm�Bk�BjBhsBgmBe`BbNBaHB_;B]/B[#BXBS�BQ�BO�BL�BJ�BI�BH�BH�BG�BH�BH�BI�BI�BH�BH�BG�BF�BE�BB�B@�B=qB;dB9XB7LB6FB6FB49B49B33B49B6FB6FB6FB6FB8RB?}BC�BH�BJ�BL�BM�BO�BP�BQ�BR�BR�BVBXBYBZBZB\)B]/B^5B]/B\)B\)B]/B]/B]/BbNBcTBdZBe`BiyBl�Bl�Bl�Bm�Bp�Bq�Br�Bs�Br�Bt�Bt�Bs�Bp�Bs�By�B{�B{�Bz�Bz�B{�B}�B~�B~�B�B�B�+B�DB�VB�bB�bB�VB�PB�bB��B��B��B�B�B�!B�FB�qBĜB��B�/B�B�B�B��B��B��B��B��B	B	JB	VB	bB	hB	bB	bB	hB	hB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	/B	1'B	33B	33B	49B	49B	6FB	8RB	9XB	:^B	?}B	A�B	A�B	A�B	B�B	D�B	J�B	M�B	O�B	Q�B	VB	W
B	XB	YB	ZB	\)B	[#B	XB	W
B	YB	]/B	[#B	[#B	^5B	bNB	cTB	cTB	dZB	dZB	dZB	hsB	k�B	m�B	s�B	r�B	w�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�JB	�JB	�PB	�PB	�PB	�VB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�LB	�jB	�wB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�;B	�B
B
JB
�B
 �B
,B
49B
<jB
D�B
K�B
P�B
VB
ZB
^5B
cTB
gmB
l�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�{B��B��B��B��B�'B�qBĜBȴB��B��B��B��B��B��B��BɺBƨBÖB�qB�LB�RB�3B�B��B��B��B�bB�=B�1B�+B�B{�Bw�Bv�Bu�Bs�BgmBaHBaHBYBT�BG�B@�B.B!�BoB��B�B�NB�
BɺB�wB�9B�B�B��B��B��B��B�{B�hB�\B�oBn�BaHBR�B7LB2-B,B�B�B�BbB
��B
�B
�sB
�BB
�#B
�B
��B
��B
ȴB
�?B
��B
��B
�\B
�=B
�B
}�B
~�B
^5B
1'B
-B
#�B
�B	�B	��B	ĜB	�qB	�LB	�B	��B	�bB	�1B	z�B	�JB	ɺB	�B	�fB	�NB	�)B	��B	ƨB	B	B	�wB	�RB	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�JB	�B	�B	z�B	jB	e`B	`BB	^5B	ZB	T�B	Q�B	A�B	?}B	@�B	49B	%�B	 �B	�B	�B	�B	hB	DB	1B	%B	B	  B��B��B��B��B�sB�NB�BB�5B�5B�;B�B��B��B��B��BɺBĜB��B��B�}B�dB�XB�LB�'B�B�B��B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�JB�DB�1B�B�B�B�B� B}�B|�B|�Bw�Br�Bq�Br�Bq�Bn�Bm�Bl�BjBgmBe`Be`BbNB`BB^5B_;BZBW
BVBQ�BP�BN�BN�BL�BJ�BJ�BJ�BJ�BJ�BI�BI�BH�BG�BG�BG�BD�B@�B=qB;dB:^B7LB9XB7LB6FB5?B6FB7LB7LB7LB8RB;dBA�BD�BI�BJ�BL�BM�BP�BQ�BR�BS�BT�BW
BZBZB\)B^5B\)B]/BaHB_;B]/B]/B]/B^5B^5BbNBcTBe`BffBjBm�Bm�Bm�Bn�Bq�Br�Br�Bt�Bt�Bv�Bu�Bt�Br�Bt�Bz�B|�B|�B{�B|�B}�B~�B� B�B�B�B�1B�JB�\B�hB�hB�uB�PB�uB��B��B��B�B�B�'B�LB�qBBɺB�/B�B�B�B��B��B��B��B��B	B	PB	\B	hB	oB	hB	hB	oB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	(�B	1'B	2-B	33B	33B	5?B	5?B	7LB	9XB	:^B	;dB	?}B	A�B	A�B	A�B	C�B	E�B	K�B	M�B	O�B	Q�B	VB	XB	YB	ZB	\)B	^5B	]/B	YB	XB	YB	^5B	\)B	[#B	^5B	cTB	dZB	dZB	dZB	dZB	ffB	hsB	k�B	l�B	t�B	r�B	x�B	y�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�7B	�DB	�JB	�JB	�PB	�PB	�VB	�\B	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�RB	�jB	�wB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�;B	�B
B
JB
�B
 �B
,B
49B
<jB
D�B
K�B
P�B
VB
ZB
^5B
cTB
gmB
l�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446582012010314465820120103144658  AO  ARGQ                                                                        20111130140703  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140703  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144658  IP                  G�O�G�O�G�O�                