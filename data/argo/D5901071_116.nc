CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:23Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               tA   AO  20111130141815  20190522121826  1727_5046_116                   2C  D   APEX                            2143                            040306                          846 @ԫ�<M_�1   @ԫ���`@6ܬ1&��c�S���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A���A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dny�Do  Do� Do��Dpy�Dq  Dq� Dr  Dr� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @333@s33@���@���AffA>ffA^ffA~ffA�33A�  A�33A�ffA�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx  B��B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC  C  C
  C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCX  CZ  C[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7s3D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DD� DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dc� Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm�3Dns3Dn��Doy�Do�3Dps3Dp��Dqy�Dq��Dry�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�|�AŃA�x�A�~�AōPAŏ\Aŏ\Aŏ\Aŏ\Aŏ\AőhAŏ\Aŏ\AőhAœuAœuAœuAŕ�Aŗ�Aŕ�Aŗ�Ař�Aś�Ař�Aś�Aŝ�Aş�Aş�Aš�Aš�Aţ�Ať�Aţ�Aš�Aţ�Aş�A�x�A�%A´9A��uA��TA�=qA���A��+A��!A��mA���A��A���A�~�A�n�A��7A�jA�  A���A���A�9XA�M�A���A�I�A���A�v�A�1'A�{A���A�t�A��A��A���A��+A�ȴA��9A��hA���A�{A�+A��9A�ZA��HA��-A�Q�A��A��;A��A��+A�G�A���A�hsA�XA��9A�33A��A���A���A���A���A�XA��/A��A��\A�A�r�A�/A��A���A�E�A�r�A�oA�ĜA��7A��A���A�|�A��7A��9A��A�p�A��A~��A}�FA|1'AyG�ArA�Ap��Ao�AmhsAl��AiC�Af�`Ad9XAb9XA`��A_�A]|�AXz�ASdZAP�AO?}AN(�AM;dALE�AJ�AG��AF-AEC�AC�mABA�AAp�A@��A?��A?K�A>��A>M�A=XA;��A:z�A9A8�HA8z�A6��A4��A4�A3�^A3;dA21'A1XA0��A/K�A.VA-��A-"�A,�A,I�A+33A*�\A)��A'��A&�/A&^5A%�mA%
=A"�/A!?}A 1'At�A�/A�A33AVAv�AS�A�Al�A�A�HA�+AI�A��Al�A��A��Az�A�A��A1'A�RA��A=qA��A��A�AJA
��A	��A�mA�AA9XA��A��A�A n�@�?}@�t�@�?}@�v�@��j@��@�&�@�Ĝ@�|�@�b@ꟾ@��@�j@�~�@���@�V@���@�9X@߮@�33@�o@���@ݩ�@�r�@�|�@�v�@��#@ؼj@ו�@׍P@ם�@�33@պ^@���@Լj@�bN@Ӿw@�K�@�"�@�ȴ@�E�@щ7@�Z@�|�@Η�@�-@͉7@���@�Ĝ@�;d@š�@�X@���@�Ĝ@���@���@ÍP@�V@��@���@���@��@�9X@�C�@�n�@�?}@��@�7L@�hs@�b@��@��
@�r�@�1'@� �@��m@���@��@�Q�@�
=@�+@��@��@�+@�j@��P@���@�ff@��@��w@�  @�r�@��@�?}@�G�@�x�@��@�X@�7L@�7L@�\)@�x�@��@��@� �@�(�@��@�z�@��u@��D@��D@���@���@���@�t�@��@���@�M�@�@���@���@���@���@��`@���@�%@��@�Ĝ@�r�@�z�@��@�hs@���@�@�@���@�x�@�O�@�1'@�K�@�o@�v�@��-@�G�@��`@�j@���@���@�ȴ@���@���@��R@���@�V@��@���@���@��h@��7@�`B@�?}@��@���@��j@�A�@�\)@���@��\@�~�@�v�@�n�@�ff@�V@�-@��^@�hs@�`B@��@�Ĝ@�z�@�|�@���@��@���@�&�@�Q�@���@�l�@�
=@�ff@�{@���@���@���@��h@�p�@�7L@���@�Ĝ@��D@�r�@�bN@�b@�ƨ@���@���@��@���@���@�t�@�S�@��@�
=@�"�@�33@�"�@��@��y@�ff@��#@�`B@��@��@��`@��/@���@�Ĝ@��9@��u@��P@�=q@�@�`B@�&�@���@��9@�9X@�(�@�(�@�9X@�(�@��@�  @��@��m@��
@���@�;d@�o@�ȴ@�n�@�n�@�ff@�E�@�@��-@��h@��@�X@�G�@�7L@�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�|�AŃA�x�A�~�AōPAŏ\Aŏ\Aŏ\Aŏ\Aŏ\AőhAŏ\Aŏ\AőhAœuAœuAœuAŕ�Aŗ�Aŕ�Aŗ�Ař�Aś�Ař�Aś�Aŝ�Aş�Aş�Aš�Aš�Aţ�Ať�Aţ�Aš�Aţ�Aş�A�x�A�%A´9A��uA��TA�=qA���A��+A��!A��mA���A��A���A�~�A�n�A��7A�jA�  A���A���A�9XA�M�A���A�I�A���A�v�A�1'A�{A���A�t�A��A��A���A��+A�ȴA��9A��hA���A�{A�+A��9A�ZA��HA��-A�Q�A��A��;A��A��+A�G�A���A�hsA�XA��9A�33A��A���A���A���A���A�XA��/A��A��\A�A�r�A�/A��A���A�E�A�r�A�oA�ĜA��7A��A���A�|�A��7A��9A��A�p�A��A~��A}�FA|1'AyG�ArA�Ap��Ao�AmhsAl��AiC�Af�`Ad9XAb9XA`��A_�A]|�AXz�ASdZAP�AO?}AN(�AM;dALE�AJ�AG��AF-AEC�AC�mABA�AAp�A@��A?��A?K�A>��A>M�A=XA;��A:z�A9A8�HA8z�A6��A4��A4�A3�^A3;dA21'A1XA0��A/K�A.VA-��A-"�A,�A,I�A+33A*�\A)��A'��A&�/A&^5A%�mA%
=A"�/A!?}A 1'At�A�/A�A33AVAv�AS�A�Al�A�A�HA�+AI�A��Al�A��A��Az�A�A��A1'A�RA��A=qA��A��A�AJA
��A	��A�mA�AA9XA��A��A�A n�@�?}@�t�@�?}@�v�@��j@��@�&�@�Ĝ@�|�@�b@ꟾ@��@�j@�~�@���@�V@���@�9X@߮@�33@�o@���@ݩ�@�r�@�|�@�v�@��#@ؼj@ו�@׍P@ם�@�33@պ^@���@Լj@�bN@Ӿw@�K�@�"�@�ȴ@�E�@щ7@�Z@�|�@Η�@�-@͉7@���@�Ĝ@�;d@š�@�X@���@�Ĝ@���@���@ÍP@�V@��@���@���@��@�9X@�C�@�n�@�?}@��@�7L@�hs@�b@��@��
@�r�@�1'@� �@��m@���@��@�Q�@�
=@�+@��@��@�+@�j@��P@���@�ff@��@��w@�  @�r�@��@�?}@�G�@�x�@��@�X@�7L@�7L@�\)@�x�@��@��@� �@�(�@��@�z�@��u@��D@��D@���@���@���@�t�@��@���@�M�@�@���@���@���@���@��`@���@�%@��@�Ĝ@�r�@�z�@��@�hs@���@�@�@���@�x�@�O�@�1'@�K�@�o@�v�@��-@�G�@��`@�j@���@���@�ȴ@���@���@��R@���@�V@��@���@���@��h@��7@�`B@�?}@��@���@��j@�A�@�\)@���@��\@�~�@�v�@�n�@�ff@�V@�-@��^@�hs@�`B@��@�Ĝ@�z�@�|�@���@��@���@�&�@�Q�@���@�l�@�
=@�ff@�{@���@���@���@��h@�p�@�7L@���@�Ĝ@��D@�r�@�bN@�b@�ƨ@���@���@��@���@���@�t�@�S�@��@�
=@�"�@�33@�"�@��@��y@�ff@��#@�`B@��@��@��`@��/@���@�Ĝ@��9@��u@��P@�=q@�@�`B@�&�@���@��9@�9X@�(�@�(�@�9X@�(�@��@�  @��@��m@��
@���@�;d@�o@�ȴ@�n�@�n�@�ff@�E�@�@��-@��h@��@�X@�G�@�7L@�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDBDBDBDBDBDB
=B
=B
=B
=B
=B
=BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
=B
=B
=B
=B
=B
=BDBJB+BJ�BW
B_;BgmBhsBl�Bl�Bk�B��B��BÖB��B�B�B�B�
B�B�B�5B�5B�
B�B�B�B�B�B�/B�5B�)B�B�B��BŢBB�RB�B��B��B�uB�PB�1B� BjBZBL�B9XB33B'�B�BJB%BB��B�B�B�B�TB�B��B�^B�{Bx�B`BBJ�B8RB!�BPBB
��B
��B
�B
�mB
��B
�B
�%B
_;B
N�B
J�B
B�B
8RB
,B
�B	��B	��B	��B	�RB	��B	��B	�B	m�B	YB	I�B	?}B	5?B	"�B��B��BƨB�wB�XB�?B�'B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�uB�hB�\B�uB��B��B�B�B�B��B��B��B��B��B��B��B��B�uB�\B�JB�=B�+B�B� By�Bu�Bs�Bq�Bp�Bo�Bo�Bk�BgmBffBjBiyBiyBhsBgmBgmBe`BdZBcTBbNBaHB`BB]/B[#BZBW
BW
BVBT�BR�BP�BN�BL�BK�BL�BK�BL�BK�BK�BJ�BG�BC�BA�B>wB>wB?}B;dB=qB<jB8RB7LB9XB;dB;dB=qB=qB?}BA�BB�BA�B@�B@�B?}B@�B@�B?}B?}B>wB=qB?}BD�BG�BJ�BJ�BM�BO�BN�BM�BN�BO�BQ�BS�BT�BT�BS�BVBXBZBZB[#B^5B]/BbNBbNBe`BiyBo�Bq�Bq�Bn�Bm�Bk�BjBiyBq�Br�Br�Bw�B|�B�B��B��B�B�3B�-B�'B�B��B��B��B��B��B��B��B��B�-B�!B�'B�9B�wB��B��B�B�BB�ZB�mB�B�B�B�B��B�B�B�TB�5B�;B�`B�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	%B	
=B	PB	PB	VB	\B	bB	�B	�B	 �B	!�B	 �B	�B	�B	!�B	$�B	%�B	&�B	'�B	(�B	(�B	)�B	/B	7LB	9XB	9XB	9XB	:^B	;dB	>wB	A�B	D�B	E�B	F�B	G�B	G�B	H�B	I�B	J�B	K�B	M�B	R�B	VB	XB	ZB	[#B	\)B	]/B	^5B	`BB	bNB	e`B	ffB	gmB	hsB	hsB	l�B	q�B	u�B	w�B	y�B	� B	�B	�%B	�7B	�PB	�bB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�LB	�XB	�^B	�dB	��B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ƨB	ƨB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�5B	�BB	�HB	�NB	�ZB	�ZB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BDBDBDBDBDBDB
=B
=B
=B
=B
=B
=BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
=B
=B
=B
=B
=BDBPB�B49BM�BZBaHBhsBjBm�Bn�Bq�B��B��B��B��B�B�#B�B�B�B�5B�NB�fB�#B�B�)B�#B�#B�/B�;B�;B�/B�B�#B��BƨBŢB�dB�!B��B��B��B�VB�=B�%Bo�B`BBR�B:^B6FB.B�B\B	7B%B��B�B�B�B�fB�#B��BƨB��B�BgmBQ�BA�B)�BhBB
��B
��B
�B
�B
�#B
�?B
�uB
dZB
O�B
L�B
E�B
;dB
0!B
"�B
\B	��B	ÖB	�qB	�B	��B	�1B	u�B	_;B	M�B	B�B	<jB	2-B	B�/B��BB�jB�XB�LB�3B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B�{B��B�uB��B��B��B�B�'B�B�B��B��B��B��B��B��B��B��B��B�\B�JB�7B�1B�+B~�Bx�Bu�Bs�Bs�Br�Br�Bq�Bk�BjBl�BjBjBiyBhsBiyBffBffBdZBcTBcTBaHBbNB`BB^5B\)BYBW
BW
BW
BVBR�BR�BN�BQ�BP�BO�BL�BM�BM�BL�BE�BD�BB�BA�BB�B=qB>wB>wB=qB9XB;dB=qB>wB@�BA�BB�BB�BC�BB�BA�BA�BA�BC�BB�BA�BA�B@�B?}B?}BD�BH�BM�BL�BN�BP�BP�BN�BO�BP�BR�BVBW
BW
BT�BW
BYB^5B]/B]/B`BB^5BcTBcTBe`BiyBq�Bs�Bs�Bs�Bo�Bn�Bn�Bk�Br�Bt�Br�Bw�B|�B~�B��B��B�B�9B�-B�-B�3B��B��B��B��B��B��B��B��B�9B�-B�'B�3B�qB��B��B�
B�BB�ZB�mB�B�B�B�B��B�B�B�`B�;B�;B�`B�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	%B	
=B	VB	VB	VB	\B	\B	�B	�B	 �B	!�B	 �B	 �B	 �B	#�B	%�B	&�B	'�B	(�B	(�B	)�B	+B	1'B	7LB	9XB	9XB	9XB	:^B	<jB	?}B	B�B	D�B	E�B	F�B	G�B	G�B	H�B	I�B	K�B	L�B	O�B	S�B	W
B	XB	ZB	[#B	\)B	]/B	^5B	aHB	cTB	e`B	gmB	hsB	iyB	jB	m�B	r�B	v�B	x�B	z�B	�B	�B	�+B	�=B	�VB	�hB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�LB	�XB	�^B	�jB	B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ƨB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�BB	�NB	�TB	�ZB	�ZB	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447142012010314471420120103144714  AO  ARGQ                                                                        20111130141815  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141815  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144714  IP                  G�O�G�O�G�O�                