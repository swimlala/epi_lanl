CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:22Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               qA   AO  20111130141733  20190522121826  1727_5046_113                   2C  D   APEX                            2143                            040306                          846 @ԧ�ꒀ1   @ԧ���?�@6ě��S��c�dZ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG��CI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D � D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DD  DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQs3DQ�3DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[�3D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Db  Db� Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aț�Aȝ�Aȝ�Aȉ7A�x�A�n�A�n�A�ffA�\)A�XA�VA�VA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�M�A�K�A�I�A�G�A�E�A�C�A�C�A�C�A�E�A�E�A�E�A�C�A�?}A�?}A�=qA�9XA�7LA�1'A�oA���Ař�A���A�~�A���A�;dA���A�l�A���A�r�A�p�A�
=A��-A�VA��mA���A�?}A�A��\A��A�+A��A��wA��A�z�A��A��!A�;dA��A�1A�x�A���A��^A��A��A�JA��A�ffA�dZA���A�&�A���A�t�A�A��A�&�A�1A���A��A��A��
A�$�A���A��+A��A�bNA�r�A���A�$�A��+A�oA���A�+A�\)A��DA���A�bNA���A�t�A�A�ȴA�(�A���A�^5A��A��A�ƨA�K�A��A�;dA��wA}��A{�PAy��Ax~�Ax1Aw��Au33Ar�HAq"�An��AlffAj��Ai��Ah�/Ag��Ae|�Ab-A_�A\ZAXA�AR{AO�AN^5AN  AM|�AK��AJ�9AJ-AH�!AF�AEABbNA@�HA@bA>��A>{A<bA;�-A;`BA:ffA8�yA6��A5�A5l�A4��A4M�A3�7A2��A1�wA1"�A/�TA/7LA.�uA.�A,��A*�A)�wA)G�A)A($�A'p�A&v�A$��A#��A"1A�A%A�wAv�A�AO�A�A��A�A  A�A�Ar�A��A`BA�A�;Ap�A`BAO�A��AjA�A�A�+A�
A�A
1A��A�PA�^A��AbA��@�33@�G�@���@��j@��9@��@��\@��@�p�@�K�@��@�O�@�9X@��@�/@�u@�@�@�~�@�9X@ް!@��@ف@�ƨ@ՙ�@ӝ�@�~�@�%@ϝ�@�~�@��`@�A�@�ƨ@�\)@�;d@�p�@�K�@���@Ƈ+@��@š�@�/@Ĭ@���@�hs@�A�@��
@�\)@�M�@��/@�Q�@��@��m@�o@��+@��+@�V@���@���@���@���@�hs@��`@���@��w@�;d@��T@�`B@�V@���@�r�@�1@�;d@���@���@���@��@�x�@�z�@�r�@�bN@�1'@�1@�ƨ@��@�l�@���@��u@��-@��7@�V@��`@�O�@�x�@��@��/@��D@��@�l�@�+@�@�ȴ@���@�E�@��#@�@�{@�?}@���@�Z@��@�K�@��@��!@�^5@�5?@�{@��h@�(�@���@�l�@�K�@�;d@�+@�o@�@�@���@�=q@�-@�5?@���@���@���@��7@�hs@�?}@�V@���@��`@��`@��`@��`@��`@�Ĝ@�I�@��F@�t�@�;d@��@���@��!@��!@���@�ff@��@��h@��/@��@�t�@�K�@�l�@��
@��m@���@�l�@�C�@�33@��@���@���@�G�@�&�@���@���@���@��`@��/@�z�@� �@�S�@�"�@��@��y@�^5@�{@���@��#@�p�@�V@���@���@�ff@��T@�hs@�&�@��@��9@�I�@�I�@�9X@���@�33@�
=@��y@�@��P@�I�@��`@�E�@��P@��F@�33@�E�@�{@���@��#@��#@��^@���@���@��^@���@���@���@��7@�G�@�G�@�&�@���@��@���@�r�@�I�@�C�@���@��D@�bN@�A�@�1'@�1'@�1'@�9X@�9X@�1'@�(�@�1@�ƨ@�K�@�\)@�
=@���@�@�@��-@���@�p�@��@��@���@���@��/@��@��`@�Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aț�Aȝ�Aȝ�Aȉ7A�x�A�n�A�n�A�ffA�\)A�XA�VA�VA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�M�A�K�A�I�A�G�A�E�A�C�A�C�A�C�A�E�A�E�A�E�A�C�A�?}A�?}A�=qA�9XA�7LA�1'A�oA���Ař�A���A�~�A���A�;dA���A�l�A���A�r�A�p�A�
=A��-A�VA��mA���A�?}A�A��\A��A�+A��A��wA��A�z�A��A��!A�;dA��A�1A�x�A���A��^A��A��A�JA��A�ffA�dZA���A�&�A���A�t�A�A��A�&�A�1A���A��A��A��
A�$�A���A��+A��A�bNA�r�A���A�$�A��+A�oA���A�+A�\)A��DA���A�bNA���A�t�A�A�ȴA�(�A���A�^5A��A��A�ƨA�K�A��A�;dA��wA}��A{�PAy��Ax~�Ax1Aw��Au33Ar�HAq"�An��AlffAj��Ai��Ah�/Ag��Ae|�Ab-A_�A\ZAXA�AR{AO�AN^5AN  AM|�AK��AJ�9AJ-AH�!AF�AEABbNA@�HA@bA>��A>{A<bA;�-A;`BA:ffA8�yA6��A5�A5l�A4��A4M�A3�7A2��A1�wA1"�A/�TA/7LA.�uA.�A,��A*�A)�wA)G�A)A($�A'p�A&v�A$��A#��A"1A�A%A�wAv�A�AO�A�A��A�A  A�A�Ar�A��A`BA�A�;Ap�A`BAO�A��AjA�A�A�+A�
A�A
1A��A�PA�^A��AbA��@�33@�G�@���@��j@��9@��@��\@��@�p�@�K�@��@�O�@�9X@��@�/@�u@�@�@�~�@�9X@ް!@��@ف@�ƨ@ՙ�@ӝ�@�~�@�%@ϝ�@�~�@��`@�A�@�ƨ@�\)@�;d@�p�@�K�@���@Ƈ+@��@š�@�/@Ĭ@���@�hs@�A�@��
@�\)@�M�@��/@�Q�@��@��m@�o@��+@��+@�V@���@���@���@���@�hs@��`@���@��w@�;d@��T@�`B@�V@���@�r�@�1@�;d@���@���@���@��@�x�@�z�@�r�@�bN@�1'@�1@�ƨ@��@�l�@���@��u@��-@��7@�V@��`@�O�@�x�@��@��/@��D@��@�l�@�+@�@�ȴ@���@�E�@��#@�@�{@�?}@���@�Z@��@�K�@��@��!@�^5@�5?@�{@��h@�(�@���@�l�@�K�@�;d@�+@�o@�@�@���@�=q@�-@�5?@���@���@���@��7@�hs@�?}@�V@���@��`@��`@��`@��`@��`@�Ĝ@�I�@��F@�t�@�;d@��@���@��!@��!@���@�ff@��@��h@��/@��@�t�@�K�@�l�@��
@��m@���@�l�@�C�@�33@��@���@���@�G�@�&�@���@���@���@��`@��/@�z�@� �@�S�@�"�@��@��y@�^5@�{@���@��#@�p�@�V@���@���@�ff@��T@�hs@�&�@��@��9@�I�@�I�@�9X@���@�33@�
=@��y@�@��P@�I�@��`@�E�@��P@��F@�33@�E�@�{@���@��#@��#@��^@���@���@��^@���@���@���@��7@�G�@�G�@�&�@���@��@���@�r�@�I�@�C�@���@��D@�bN@�A�@�1'@�1'@�1'@�9X@�9X@�1'@�(�@�1@�ƨ@�K�@�\)@�
=@���@�@�@��-@���@�p�@��@��@���@���@��/@��@��`@�Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB;dB;dB;dB=qBA�BE�BF�BQ�B]/B_;BaHBbNBcTBdZBe`BffBgmBgmBhsBiyBiyBiyBjBjBjBk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bm�Bm�Bn�Bm�Bm�Bl�BiyB� B��B�!B�RBƨBǮBǮBǮB��BɺBƨBǮBǮBǮBȴBȴBȴBȴBƨBƨBȴBǮBŢB�}B�qB�jB�dB�RB�LB�3B�'B�B��B��B��B�\B�VB�Bw�Bo�BjBe`B]/BN�BB�B2-B"�BoB
=B  B�B�mB��B��BȴB�}B�FB�B��B�=B�Bx�Bk�B]/BM�B7LB�B
=B
��B
�B
�ZB
�5B
��B
B
��B
{�B
u�B
m�B
aHB
I�B
1'B
#�B
{B
JB
+B
B	�B	�HB	�B	ÖB	�3B	�B	��B	��B	�{B	�B	k�B	YB	:^B	�B�B�;B�B�
B��B��BB��B�RB�B��B��B��B��B��B��B��B��B��B�hB�DB�B� B~�B{�Bz�Bw�Bv�Bv�Bt�Bs�Br�Bq�Bq�Bp�Bm�Bn�Bm�Bl�Bm�Bn�Bl�Bk�BgmBcTBcTBbNBcTBdZBdZBcTBbNBbNBaHB`BB`BB^5B_;B^5B]/B\)B\)B\)B[#BXBW
BW
BVBS�BR�BP�BN�BK�BI�BE�BC�B@�B<jB9XB8RB8RB8RB8RB7LB49B33B0!B.B,B)�B)�B(�B'�B'�B'�B%�B#�B"�B"�B�B�B�B�B�B!�B!�B"�B%�B'�B+B-B.B/B-B/B5?B8RB8RB9XB:^B:^B:^B9XB>wBA�BA�BA�BC�BI�BK�BK�BK�BM�BO�BO�BO�BR�B\)BaHBjBo�Bv�B}�B|�B|�B{�Bz�Bz�B{�B�B�B�%B�+B�7B�\B��B��B��B�B�-B�FB�RB�jB�}BÖBĜB��B�qB�qB�qBĜBǮBɺBɺB��B��B��B��B��B��B��B��B�B�B�;B�HB�ZB�TB�NB�HB�BB�BB�BB�NB�NB�TB�B��B��B��B	  B	  B	B	B	B	B	+B	PB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	(�B	)�B	,B	,B	,B	-B	,B	+B	+B	33B	49B	7LB	;dB	>wB	D�B	H�B	J�B	K�B	L�B	M�B	O�B	S�B	VB	W
B	W
B	W
B	W
B	W
B	XB	YB	ZB	`BB	bNB	dZB	e`B	gmB	l�B	n�B	p�B	q�B	o�B	jB	iyB	l�B	r�B	t�B	v�B	w�B	x�B	|�B	|�B	}�B	�B	�%B	�1B	�DB	�VB	�uB	��B	��B	��B	�B	�'B	�-B	�-B	�-B	�'B	�'B	�'B	�'B	�'B	�'B	�RB	�dB	�dB	�qB	�qB	�qB	�}B	�}B	��B	B	ÖB	ÖB	ÖB	B	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�5B	�;B	�;B	�;B	�HB	�`B	�fB	�fB	�fB	�mB	�mB	�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B;dB;dB;dB=qBA�BE�BF�BQ�B]/B_;BaHBbNBcTBdZBe`BffBgmBgmBhsBiyBiyBiyBjBjBjBk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bm�Bm�Bn�Bm�Bm�Bm�Bp�B�\B�B�XBÖBȴBɺB��B��B��B��B��B��B��B��B��BɺB��B��B��BǮBɺB��BǮB��B�}B�wB�qB�dB�XB�?B�-B�3B��B��B��B�uB�uB�B{�Bq�Bl�BhsBbNBT�BH�B8RB(�B�BVBB��B�B�B��B��BÖB�^B�B��B�PB�B|�Bo�BaHBQ�B=qB�BPB
��B
�B
�fB
�BB
�
B
��B
��B
}�B
w�B
q�B
iyB
R�B
7LB
(�B
�B
PB
1B
	7B	��B	�fB	�)B	ɺB	�LB	�B	��B	��B	��B	�JB	r�B	dZB	G�B	+B��B�HB�#B�B�
B��BÖBĜB�qB�3B�B��B��B��B��B��B��B��B��B��B�bB�+B�B�B|�B|�Bz�By�Bx�Bx�Bu�Bt�Bs�Bu�Bv�Bq�Bp�Bn�Bo�Bo�Bq�Bq�Bn�Bl�BiyBffBffBgmBgmBe`Be`BffBe`BdZBdZBdZBcTBaHB`BB_;B`BB^5B]/B\)B^5B]/BYBXBXBVBS�BS�BQ�BN�BL�BH�BC�BE�BB�B<jB9XB8RB8RB8RB8RB8RB6FB33B33B/B-B,B+B(�B)�B(�B'�B%�B$�B#�B!�B!�B �B"�B#�B#�B$�B'�B)�B,B.B/B/B/B2-B6FB9XB9XB:^B;dB;dB;dB=qB@�BB�BB�BC�BE�BJ�BL�BL�BM�BN�BO�BP�BP�BS�B\)BaHBk�Bp�Bx�B~�B}�B~�B|�B{�B{�B|�B�B�+B�+B�1B�=B�\B��B��B��B�B�3B�FB�XB�jB��BŢBȴBƨB�wB�wB�qBÖBǮB��B��B��B��B��B��B��B��B��B��B�
B�B�;B�NB�`B�ZB�TB�NB�BB�HB�HB�NB�NB�ZB�B��B��B��B	  B	  B	B	B	B	B	1B	PB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	%�B	(�B	)�B	,B	,B	-B	.B	-B	,B	-B	49B	49B	7LB	:^B	>wB	E�B	I�B	J�B	K�B	L�B	N�B	O�B	T�B	VB	W
B	W
B	W
B	W
B	W
B	YB	ZB	[#B	`BB	bNB	dZB	ffB	hsB	l�B	n�B	q�B	r�B	q�B	k�B	jB	m�B	s�B	u�B	v�B	w�B	x�B	|�B	|�B	~�B	�B	�%B	�1B	�DB	�PB	�oB	��B	��B	��B	�B	�-B	�9B	�-B	�-B	�'B	�'B	�'B	�'B	�'B	�'B	�RB	�dB	�dB	�qB	�wB	�qB	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�5B	�;B	�BB	�BB	�HB	�`B	�fB	�fB	�fB	�mB	�mB	�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447132012010314471320120103144713  AO  ARGQ                                                                        20111130141733  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141733  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144713  IP                  G�O�G�O�G�O�                