CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-24T06:50:51Z creation;2023-05-24T06:50:51Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230524065051  20230524065957  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�-�[�j11   @�-��^o�@0)x����c�-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B虚B�33B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C33C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @   @y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B@  BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B�  B�33B���B���B���B���B���B���B���B���B���B���B���B���B���B�fgB�  BB���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�C��C�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC@  CA�fCC�fCE�fCG�fCI�fCK��CM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C�  C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DM  DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�N<A�C�A�<6A�.�A�&�A�%A�(XA�%A��A��A�JA��A��A���A��lA��>A�ΥA���Aη�AάAΩ*AΦAΥzAΤ@AΠ\AΠ'AΟVAΟ�AΟVAΠ'AΛ�AΓ�A�yrA�=A���A�U2A��dA��cAɊ=A�LdA���A��A�f2AŜxA�u%Aø�A��A�#:A���A�	�A���A�TaA���A�WsA��:A��A��A�m�A���A��WA�`�A��tA� �A���A��A��A�7LA�6�A�}�A��tA�_A��?A��_A�kA�L�A���A�/OA�d�A���A��JA�oA�͟A�4�A�qA�A�3hA���A���A�`A�*�A���A�1A�W�A�`A��OA�~�A��.A�4�A�#nA�b�A�ߤA|PHAz��Aw<6Au�Ar1'An��Akh�AiGEAdB�A`D�A]cAY�AS�8AS�AR�YAPm�ANI�AM�AL33AHR�AGP�AF��AET�A>jA=�A;��A:�A9%�A6M�A5J#A3��A0��A-�jA,��A*�)A(O�A'n/A$W�A"p�A"�A"A �xA��A�A~(AJ#AIRAxlA��A4�A!A�
A+AQ�A�`AaA�A�dAV�A�[A8A�XAc�A��A33A��A��A��A��AcAAB�A�[A�?AԕA�-A6�A)_A��A�6Ae,APHAF�Aa�A�Ap;Ax�A�PA��A�AB�A	�WA	~(A��A�KA0�A��A� ASAT�A��A�A�jAS�AE�A �;A ��@���@�!-@�tT@�&�@��H@��4@�.�@��@��@��@��@��h@��3@�P�@�B�@���@��@���@��@�V@�@��@��@�(�@�z@��@���@��d@�P@�|@�<6@��@@���@�!@�V@� @�X@��@��@�'@�iD@�0�@�#:@��@Ⲗ@�L0@�@��.@�E9@���@���@�<6@���@۲�@۷@���@��@�1'@�xl@ۓ@ٜ�@�6@�o@֩�@ֲ�@��f@��y@�ff@�4�@�m�@�s�@�:�@�|�@��v@�ԕ@�Ĝ@�c�@�c @̮}@�@�@��#@ˬq@ˎ"@�|�@�c�@ˉ7@�^�@��@�G�@�hs@�L�@��@ʯO@��}@���@ǅ@���@ŝ�@�B[@��B@�p;@��@���@�e�@�Dg@�4n@�k�@�@���@�!�@�*0@��o@���@�ѷ@�*�@���@��M@��.@�)�@��N@���@���@�B�@���@��@��@���@�YK@�=q@�~@��@�4@���@�l�@�7�@���@��w@���@�(�@��z@��@�_@���@��)@���@���@�C@��@��@��W@�c@�f�@��@��L@�Ov@���@�%@���@�M@���@���@�X�@��@�Q�@���@��[@��	@���@�zx@�&@��@�Ĝ@��@���@��U@�K^@���@�ݘ@��a@���@�0�@��@���@��E@���@���@���@���@���@���@�m�@�ϫ@�L�@��y@��j@�q@�@��$@�X�@�=�@��@��s@���@�u�@��@��;@���@�Z�@�ی@��b@��@�{�@�@�@���@���@�4@���@���@�z�@�u@�˒@���@�|@�~�@��C@�w2@�33@��@��H@���@�ߤ@��B@���@��@�ں@���@���@��b@��o@�1'@�{J@�*0@�/@�0�@�)_@��@�S�@�~@�)�@���@�4�@�ff@�%�@�b@��C@���@���@��@��j@�
=@��@��@�YK@�-�@���@���@�,=@��Z@���@���@�1�@�q@��@�Y@��@�(�@�,�@���@� \@�%F@�#�@�
=@��@��9@���@�W�@��:@�4�@�&�@��8@���@�_�@�'R@�!@�~�@��/@�;�@���@��D@��@��7@��@��@�֡@��@��@��5@���@�}V@�n�@��@��-@��7@�e,@�O@�/@��@�~�@�;�@�#:@�4@��@��]@���@�o�@�&@�o@���@�!@��@�s@�@��@��@�9X@��@��@��@@�E9@�"�@��@�|�@�	@���@���@��@��6@���@��X@�	l@�z@���@���@���@�e�@�rG@�b�@�U�@��B@�]d@�	@���@�A�@���@o�@�@~_@~��@�@�*@l�@~�}@~ �@}�o@}�h@|�[@|��@|@{��@{�@z��@y��@y�~@y<6@x1'@w��@w��@we�@w�@v��@v=q@u�@u�#@u4@t�4@t�@s�}@s�@s��@r��@q�.@q�N@p�@p9X@oƨ@o�@n�]@n�F@n:*@m��@mc�@m7L@l�	@l��@lbN@l1@k�w@kb�@k�@j�<@k9�@j�2@j҉@j��@j!�@i��@i�@hĜ@h<�@ga@g6z@f �@eQ�@e�@d��@dg8@d,=@c�{@c�@b^5@a��@bc @b5?@b	@a�N@aJ�@ap�@ak�@a5�@a�@`��@``�@_�@^�"@^��@^�b@]��@]c�@\�@\ѷ@\�/@\��@]IR@]m]@]0�@]�@\�f@\Ɇ@\�9@[��@Z��@Z�@Z��@Y�@Y��@Ys�@Yq@X��@Xc�@W�6@WF�@V{@U	l@U��@Uhs@U�=@U|@UN<@U%F@Ty>@T:�@T�@S�@S��@Sb�@S$t@R�'@R�A@Ri�@R�@Q�@Qe,@QQ�@QG�@P��@PN�@P$@O��@O��@O��@O.I@O(@N��@Ni�@M�@MVm@L`�@L  @K��@K�;@K� @K��@KdZ@K@J��@JkQ@J
�@I@Ip�@I!�@H1@G��@Gv`@G@O@G�@G�@F��@F��@F#:@E��@Ej@E�@D��@Dw�@C�6@Cs@C+@B��@B��@BYK@A��@A�H@A��@Ac�@AO�@A=�@@��@@bN@@N�@?�m@?;d@>�]@>�A@>e@=��@= \@<�D@<x@;�@;{J@:�!@:R�@:-@9��@9�@9�N@9�-@9�@8��@8�@8oi@8*�@7�W@7��@7�[@7��@7�@6�L@6L0@5�)@5��@5�~@5zx@5?}@4�K@4�e@4e�@4,=@3�$@3dZ@38@2ߤ@2��@2�\@2J�@2{@20U@1�@1G�@1%@0�j@0[�@0�@/��@/�k@/�:@/�@.��@.�r@.s�@.ff@.d�@.Z�@.Q@.8�@-�@-�M@-s�@-X@-G�@-@,�)@,�u@,r�@,-�@, �@,b@+�Q@+��@+X�@+(@*�y@*��@*��@)�@)��@)s�@)@@(��@(��@(m�@(A�@(�@'�m@'�	@'@&�h@&:*@&�@%�^@%�7@%^�@%IR@%!�@$��@$�)@$w�@$x@#�6@#��@#��@#{J@#�@"�2@"��@"Ov@!�#@!�~@![W@!+@ �e@ w�@ A�@��@�@��@o�@9�@Y@�@�@4@��@�=@a�@�j@]d@<�@,=@M@��@� @�w@�F@��@�4@l�@F�@$t@�@�6@d�@�@��@zx@[W@<6@��@�E@�e@��@4n@��@�F@�F@��@�F@g�@.I@�@�@�,@�R@�\@~�@W�@J@@�S@j@�@�@��@�z@Ft@�@ƨ@��@��@��@qv@8@��@�@�#@��@��@e,@5�@V@�E@�@�@oi@M@G@�g@�@�*@�{@~�@iD@,�@҉@�b@.�@ �@��@0�@�@��@�@��@�I@��@�@r�@S�@-�@�@G@�m@��@��@|�@|�@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�N<A�C�A�<6A�.�A�&�A�%A�(XA�%A��A��A�JA��A��A���A��lA��>A�ΥA���Aη�AάAΩ*AΦAΥzAΤ@AΠ\AΠ'AΟVAΟ�AΟVAΠ'AΛ�AΓ�A�yrA�=A���A�U2A��dA��cAɊ=A�LdA���A��A�f2AŜxA�u%Aø�A��A�#:A���A�	�A���A�TaA���A�WsA��:A��A��A�m�A���A��WA�`�A��tA� �A���A��A��A�7LA�6�A�}�A��tA�_A��?A��_A�kA�L�A���A�/OA�d�A���A��JA�oA�͟A�4�A�qA�A�3hA���A���A�`A�*�A���A�1A�W�A�`A��OA�~�A��.A�4�A�#nA�b�A�ߤA|PHAz��Aw<6Au�Ar1'An��Akh�AiGEAdB�A`D�A]cAY�AS�8AS�AR�YAPm�ANI�AM�AL33AHR�AGP�AF��AET�A>jA=�A;��A:�A9%�A6M�A5J#A3��A0��A-�jA,��A*�)A(O�A'n/A$W�A"p�A"�A"A �xA��A�A~(AJ#AIRAxlA��A4�A!A�
A+AQ�A�`AaA�A�dAV�A�[A8A�XAc�A��A33A��A��A��A��AcAAB�A�[A�?AԕA�-A6�A)_A��A�6Ae,APHAF�Aa�A�Ap;Ax�A�PA��A�AB�A	�WA	~(A��A�KA0�A��A� ASAT�A��A�A�jAS�AE�A �;A ��@���@�!-@�tT@�&�@��H@��4@�.�@��@��@��@��@��h@��3@�P�@�B�@���@��@���@��@�V@�@��@��@�(�@�z@��@���@��d@�P@�|@�<6@��@@���@�!@�V@� @�X@��@��@�'@�iD@�0�@�#:@��@Ⲗ@�L0@�@��.@�E9@���@���@�<6@���@۲�@۷@���@��@�1'@�xl@ۓ@ٜ�@�6@�o@֩�@ֲ�@��f@��y@�ff@�4�@�m�@�s�@�:�@�|�@��v@�ԕ@�Ĝ@�c�@�c @̮}@�@�@��#@ˬq@ˎ"@�|�@�c�@ˉ7@�^�@��@�G�@�hs@�L�@��@ʯO@��}@���@ǅ@���@ŝ�@�B[@��B@�p;@��@���@�e�@�Dg@�4n@�k�@�@���@�!�@�*0@��o@���@�ѷ@�*�@���@��M@��.@�)�@��N@���@���@�B�@���@��@��@���@�YK@�=q@�~@��@�4@���@�l�@�7�@���@��w@���@�(�@��z@��@�_@���@��)@���@���@�C@��@��@��W@�c@�f�@��@��L@�Ov@���@�%@���@�M@���@���@�X�@��@�Q�@���@��[@��	@���@�zx@�&@��@�Ĝ@��@���@��U@�K^@���@�ݘ@��a@���@�0�@��@���@��E@���@���@���@���@���@���@�m�@�ϫ@�L�@��y@��j@�q@�@��$@�X�@�=�@��@��s@���@�u�@��@��;@���@�Z�@�ی@��b@��@�{�@�@�@���@���@�4@���@���@�z�@�u@�˒@���@�|@�~�@��C@�w2@�33@��@��H@���@�ߤ@��B@���@��@�ں@���@���@��b@��o@�1'@�{J@�*0@�/@�0�@�)_@��@�S�@�~@�)�@���@�4�@�ff@�%�@�b@��C@���@���@��@��j@�
=@��@��@�YK@�-�@���@���@�,=@��Z@���@���@�1�@�q@��@�Y@��@�(�@�,�@���@� \@�%F@�#�@�
=@��@��9@���@�W�@��:@�4�@�&�@��8@���@�_�@�'R@�!@�~�@��/@�;�@���@��D@��@��7@��@��@�֡@��@��@��5@���@�}V@�n�@��@��-@��7@�e,@�O@�/@��@�~�@�;�@�#:@�4@��@��]@���@�o�@�&@�o@���@�!@��@�s@�@��@��@�9X@��@��@��@@�E9@�"�@��@�|�@�	@���@���@��@��6@���@��X@�	l@�z@���@���@���@�e�@�rG@�b�@�U�@��B@�]d@�	@���@�A�@���@o�@�@~_@~��@�@�*@l�@~�}@~ �@}�o@}�h@|�[@|��@|@{��@{�@z��@y��@y�~@y<6@x1'@w��@w��@we�@w�@v��@v=q@u�@u�#@u4@t�4@t�@s�}@s�@s��@r��@q�.@q�N@p�@p9X@oƨ@o�@n�]@n�F@n:*@m��@mc�@m7L@l�	@l��@lbN@l1@k�w@kb�@k�@j�<@k9�@j�2@j҉@j��@j!�@i��@i�@hĜ@h<�@ga@g6z@f �@eQ�@e�@d��@dg8@d,=@c�{@c�@b^5@a��@bc @b5?@b	@a�N@aJ�@ap�@ak�@a5�@a�@`��@``�@_�@^�"@^��@^�b@]��@]c�@\�@\ѷ@\�/@\��@]IR@]m]@]0�@]�@\�f@\Ɇ@\�9@[��@Z��@Z�@Z��@Y�@Y��@Ys�@Yq@X��@Xc�@W�6@WF�@V{@U	l@U��@Uhs@U�=@U|@UN<@U%F@Ty>@T:�@T�@S�@S��@Sb�@S$t@R�'@R�A@Ri�@R�@Q�@Qe,@QQ�@QG�@P��@PN�@P$@O��@O��@O��@O.I@O(@N��@Ni�@M�@MVm@L`�@L  @K��@K�;@K� @K��@KdZ@K@J��@JkQ@J
�@I@Ip�@I!�@H1@G��@Gv`@G@O@G�@G�@F��@F��@F#:@E��@Ej@E�@D��@Dw�@C�6@Cs@C+@B��@B��@BYK@A��@A�H@A��@Ac�@AO�@A=�@@��@@bN@@N�@?�m@?;d@>�]@>�A@>e@=��@= \@<�D@<x@;�@;{J@:�!@:R�@:-@9��@9�@9�N@9�-@9�@8��@8�@8oi@8*�@7�W@7��@7�[@7��@7�@6�L@6L0@5�)@5��@5�~@5zx@5?}@4�K@4�e@4e�@4,=@3�$@3dZ@38@2ߤ@2��@2�\@2J�@2{@20U@1�@1G�@1%@0�j@0[�@0�@/��@/�k@/�:@/�@.��@.�r@.s�@.ff@.d�@.Z�@.Q@.8�@-�@-�M@-s�@-X@-G�@-@,�)@,�u@,r�@,-�@, �@,b@+�Q@+��@+X�@+(@*�y@*��@*��@)�@)��@)s�@)@@(��@(��@(m�@(A�@(�@'�m@'�	@'@&�h@&:*@&�@%�^@%�7@%^�@%IR@%!�@$��@$�)@$w�@$x@#�6@#��@#��@#{J@#�@"�2@"��@"Ov@!�#@!�~@![W@!+@ �e@ w�@ A�@��@�@��@o�@9�@Y@�@�@4@��@�=@a�@�j@]d@<�@,=@M@��@� @�w@�F@��@�4@l�@F�@$t@�@�6@d�@�@��@zx@[W@<6@��@�E@�e@��@4n@��@�F@�F@��@�F@g�@.I@�@�@�,@�R@�\@~�@W�@J@@�S@j@�@�@��@�z@Ft@�@ƨ@��@��@��@qv@8@��@�@�#@��@��@e,@5�@V@�E@�@�@oi@M@G@�g@�@�*@�{@~�@iD@,�@҉@�b@.�@ �@��@0�@�@��@�@��@�I@��@�@r�@S�@-�@�@G@�m@��@��@|�@|�@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
I�B
JrB
J�B
J#B
J	B
I�B
I�B
I�B
I�B
I�B
IlB
IlB
IRB
IlB
IB
IB
IRB
I�B
IlB
I�B
J=B
J�B
J�B
KDB
K�B
LJB
L~B
L~B
L�B
M6B
M�B
M�B
MB
IlB
JXB
U�B
sMB
{JB
�BB
��B
��B
�RB
�FB
��B)_B4TB:�BA�BHBJ=BP}Bi�B��B��B�B�:B�dB��B�'B��B��B�qB�QB�aB��B��B�B�mBοBϫB�BB�B��B�DB�XB�'B�^B��B�8B��B�B|�Bc�BZ�BKB.�BBB
��B
�pB
�}B�B
�B
�B
� B
� B
��B
�=B
vzB
g�B
\�B
RB
3�B
�B
	�B	�fB	�WB	ںB	�B	��B	��B	�aB	�;B	p!B	_pB	Q B	L�B	J=B	E9B	>(B	:*B	5%B	1�B	-)B	+B	,�B	&�B	�B	9B	B	�B	�B��B�dB�`B��B�ZB�B�KB��B�IB��B̈́B��B�HB�YB��B��B�[B��B��B�uB��B�
B��BݘB	hB	8B	;dB	6+B	9�B	i_B	c�B	f�B	h
B	l�B	h�B	dB	\�B	TaB	PHB	S�B	dtB	p�B	w�B	�iB	�zB	��B	��B	��B	�[B	t�B	uB	��B	�B	��B	�B	�yB	��B	��B	�{B	��B	�B	��B	��B	��B	�2B	��B	�]B	��B	��B	�B	��B	�B	�nB	�fB	�mB	�_B	�KB	�B	��B	�qB	��B	�B	��B	��B	��B	��B	��B	��B	�aB	��B	��B	��B	��B	��B	�9B	��B	��B	��B	��B	��B	�2B	��B	��B	�$B	��B	�^B	�B	�HB	��B	�-B	�B	�MB	��B	�B	��B	��B	��B	� B	�UB	�oB	�'B	�SB	��B	�BB	�BB	��B	��B	ĶB	�uB	�}B	��B	��B	��B	�B	āB	��B	�B	�B	� B	�B	ȴB	��B	�MB	��B	ɆB	οB	��B	бB	̈́B	̳B	�vB	�,B	�FB	�B	ѝB	�xB	�+B	��B	�[B	�[B	��B	�B	�)B	�(B	ӏB	��B	ԕB	�5B	�B	�ZB	�B	��B	��B	�zB	��B	�jB	�B	�mB	�&B	ѝB	�bB	��B	�vB	ϑB	��B	��B	��B	��B	רB	��B	ڠB	�KB	��B	�
B	�B	�B	��B	چB	��B	��B	��B	�qB	��B	�B	��B	�CB	�dB	ݲB	�B	�;B	�B	�4B	�hB	��B	�B	��B	�@B	�B	��B	�8B	�RB	�RB	�8B	�8B	��B	�B	�
B	�B	��B	��B	�B	�"B	��B	�B	��B	� B	�B	��B	�TB	��B	��B	�tB	�2B	��B	�xB	�B	�B
;B
B
'B
B
[B
�B
�B
	7B
KB
	�B

�B
�B
6B
B
B
6B
�B
�B
�B
~B
B

�B
B
VB
(B
BB
�B
�B
.B
 B
�B
�B
�B
B
�B
�B
�B
�B
uB
B
�B
aB
FB
,B
�B
uB
uB
[B
@B
B
[B
�B
�B
�B
gB
YB
eB
�B
�B
dB
�B
"NB
$�B
%�B
'mB
)*B
(�B
*B
*B
)yB
(�B
(
B
&�B
&�B
&�B
&�B
&�B
%�B
#�B
$&B
%FB
&B
%�B
#�B
#�B
%,B
%zB
#TB
#�B
%B
&LB
)�B
,=B
,"B
+�B
.B
,qB
)�B
(�B
(>B
'�B
(sB
)B
)B
)*B
)DB
)yB
)�B
*0B
+�B
/�B
1�B
2|B
4B
4TB
4�B
5B
5?B
5?B
5%B
5?B
5ZB
5%B
5%B
4�B
4nB
49B
2�B
2-B
1'B
1�B
2B
1�B
3B
3�B
3�B
4�B
5tB
5tB
5tB
5�B
6�B
8B
72B
7LB
7LB
7fB
7fB
7B
6`B
5�B
5�B
5�B
5�B
5tB
4�B
4B
3�B
3�B
2�B
2�B
2aB
2�B
2�B
2|B
2�B
2�B
3B
3hB
3�B
4B
4�B
4�B
49B
3�B
4TB
4�B
5%B
5�B
5�B
7�B
6zB
5�B
6�B
7�B
8lB
>]B
@4B
AoB
A�B
A�B
A�B
BAB
BuB
A�B
@�B
>BB
=�B
=�B
?�B
F%B
G�B
G+B
F�B
FYB
H1B
IlB
H�B
K)B
J�B
JrB
J#B
J�B
J=B
J	B
J=B
J=B
K)B
KDB
K^B
K^B
K�B
K�B
LB
LB
M6B
L�B
K�B
K�B
L�B
LdB
LJB
LB
LdB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
LdB
MB
L�B
L�B
L�B
L�B
L�B
MB
MjB
NB
P�B
Q�B
R B
RB
Q�B
R:B
R�B
S@B
R�B
Q�B
R B
RoB
Q�B
R B
TB
T�B
U�B
UgB
U�B
T�B
U2B
XEB
X�B
Y1B
YB
X�B
Y�B
Z�B
Z�B
[	B
[#B
[�B
[�B
[=B
\)B
\]B
[WB
[WB
[�B
[�B
]B
]~B
`'B
a|B
aHB
aB
aB
`�B
`�B
_pB
_!B
_VB
_�B
_;B
_!B
_!B
_!B
_�B
a-B
a-B
`�B
`BB
`'B
a�B
b�B
d&B
dB
c�B
dB
dZB
dtB
d�B
ezB
e`B
e`B
eFB
eFB
d�B
e`B
ezB
e�B
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
hsB
hXB
h>B
h
B
hXB
h$B
g�B
h�B
hsB
h�B
hsB
hsB
hXB
hXB
h�B
h�B
h�B
h�B
h>B
g�B
gmB
g�B
h�B
h�B
h�B
hXB
h$B
h$B
hsB
h�B
iB
iyB
i_B
h�B
iB
i*B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
k�B
k�B
k�B
k�B
lWB
l=B
l=B
l=B
l"B
l"B
k�B
kkB
k6B
k�B
l=B
lWB
l"B
l�B
l�B
mCB
m]B
m�B
m�B
mwB
mwB
n�B
oB
oOB
o�B
pB
p;B
p;B
p�B
qB
qAB
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
r�B
s3B
sB
r�B
raB
rGB
rGB
rGB
rGB
r�B
sMB
tB
u%B
t�B
t�B
t�B
tB
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v�B
v�B
v�B
v�B
wB
w2B
wLB
wLB
w2B
wLB
xB
xB
xRB
x�B
yrB
yXB
y�B
z*B
z�B
z�B
z�B
z�B
z�B
{B
{�B
|B
|6B
|�B
|�B
|�B
|�B
}"B
}�B
~]B
~]B
~BB
~wB
~wB
~�B
~�B
~wB
~�B
~�B
.B
HB
�B
�B
�B
�OB
�iB
��B
�iB
��B
��B
��B
��B
��B
��B
�'B
�'B
�AB
��B
�GB
�{B
�{B
��B
��B
��B
��B
��B
�B
�MB
�gB
�gB
�gB
��B
��B
�SB
��B
�%B
�YB
��B
��B
�B
�+B
�zB
�zB
��B
�KB
�fB
��B
��B
�B
��B
�#B
�=B
�rB
�rB
��B
��B
��B
��B
�DB
��B
��B
��B
�dB
��B
�~B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�"B
�pB
��B
��B
��B
��B
��B
�B
�BB
�BB
�vB
��B
��B
��B
��B
�.B
�bB
��B
��B
��B
��B
��B
��B
�NB
�4B
�B
�B
� B
��B
��B
�B
�4B
�B
�4B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
I�B
JrB
J�B
J#B
J	B
I�B
I�B
I�B
I�B
I�B
IlB
IlB
IRB
IlB
IB
IB
IRB
I�B
IlB
I�B
J=B
J�B
J�B
KDB
K�B
LJB
L~B
L~B
L�B
M6B
M�B
M�B
MB
IlB
JXB
U�B
sMB
{JB
�BB
��B
��B
�RB
�FB
��B)_B4TB:�BA�BHBJ=BP}Bi�B��B��B�B�:B�dB��B�'B��B��B�qB�QB�aB��B��B�B�mBοBϫB�BB�B��B�DB�XB�'B�^B��B�8B��B�B|�Bc�BZ�BKB.�BBB
��B
�pB
�}B�B
�B
�B
� B
� B
��B
�=B
vzB
g�B
\�B
RB
3�B
�B
	�B	�fB	�WB	ںB	�B	��B	��B	�aB	�;B	p!B	_pB	Q B	L�B	J=B	E9B	>(B	:*B	5%B	1�B	-)B	+B	,�B	&�B	�B	9B	B	�B	�B��B�dB�`B��B�ZB�B�KB��B�IB��B̈́B��B�HB�YB��B��B�[B��B��B�uB��B�
B��BݘB	hB	8B	;dB	6+B	9�B	i_B	c�B	f�B	h
B	l�B	h�B	dB	\�B	TaB	PHB	S�B	dtB	p�B	w�B	�iB	�zB	��B	��B	��B	�[B	t�B	uB	��B	�B	��B	�B	�yB	��B	��B	�{B	��B	�B	��B	��B	��B	�2B	��B	�]B	��B	��B	�B	��B	�B	�nB	�fB	�mB	�_B	�KB	�B	��B	�qB	��B	�B	��B	��B	��B	��B	��B	��B	�aB	��B	��B	��B	��B	��B	�9B	��B	��B	��B	��B	��B	�2B	��B	��B	�$B	��B	�^B	�B	�HB	��B	�-B	�B	�MB	��B	�B	��B	��B	��B	� B	�UB	�oB	�'B	�SB	��B	�BB	�BB	��B	��B	ĶB	�uB	�}B	��B	��B	��B	�B	āB	��B	�B	�B	� B	�B	ȴB	��B	�MB	��B	ɆB	οB	��B	бB	̈́B	̳B	�vB	�,B	�FB	�B	ѝB	�xB	�+B	��B	�[B	�[B	��B	�B	�)B	�(B	ӏB	��B	ԕB	�5B	�B	�ZB	�B	��B	��B	�zB	��B	�jB	�B	�mB	�&B	ѝB	�bB	��B	�vB	ϑB	��B	��B	��B	��B	רB	��B	ڠB	�KB	��B	�
B	�B	�B	��B	چB	��B	��B	��B	�qB	��B	�B	��B	�CB	�dB	ݲB	�B	�;B	�B	�4B	�hB	��B	�B	��B	�@B	�B	��B	�8B	�RB	�RB	�8B	�8B	��B	�B	�
B	�B	��B	��B	�B	�"B	��B	�B	��B	� B	�B	��B	�TB	��B	��B	�tB	�2B	��B	�xB	�B	�B
;B
B
'B
B
[B
�B
�B
	7B
KB
	�B

�B
�B
6B
B
B
6B
�B
�B
�B
~B
B

�B
B
VB
(B
BB
�B
�B
.B
 B
�B
�B
�B
B
�B
�B
�B
�B
uB
B
�B
aB
FB
,B
�B
uB
uB
[B
@B
B
[B
�B
�B
�B
gB
YB
eB
�B
�B
dB
�B
"NB
$�B
%�B
'mB
)*B
(�B
*B
*B
)yB
(�B
(
B
&�B
&�B
&�B
&�B
&�B
%�B
#�B
$&B
%FB
&B
%�B
#�B
#�B
%,B
%zB
#TB
#�B
%B
&LB
)�B
,=B
,"B
+�B
.B
,qB
)�B
(�B
(>B
'�B
(sB
)B
)B
)*B
)DB
)yB
)�B
*0B
+�B
/�B
1�B
2|B
4B
4TB
4�B
5B
5?B
5?B
5%B
5?B
5ZB
5%B
5%B
4�B
4nB
49B
2�B
2-B
1'B
1�B
2B
1�B
3B
3�B
3�B
4�B
5tB
5tB
5tB
5�B
6�B
8B
72B
7LB
7LB
7fB
7fB
7B
6`B
5�B
5�B
5�B
5�B
5tB
4�B
4B
3�B
3�B
2�B
2�B
2aB
2�B
2�B
2|B
2�B
2�B
3B
3hB
3�B
4B
4�B
4�B
49B
3�B
4TB
4�B
5%B
5�B
5�B
7�B
6zB
5�B
6�B
7�B
8lB
>]B
@4B
AoB
A�B
A�B
A�B
BAB
BuB
A�B
@�B
>BB
=�B
=�B
?�B
F%B
G�B
G+B
F�B
FYB
H1B
IlB
H�B
K)B
J�B
JrB
J#B
J�B
J=B
J	B
J=B
J=B
K)B
KDB
K^B
K^B
K�B
K�B
LB
LB
M6B
L�B
K�B
K�B
L�B
LdB
LJB
LB
LdB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
LdB
MB
L�B
L�B
L�B
L�B
L�B
MB
MjB
NB
P�B
Q�B
R B
RB
Q�B
R:B
R�B
S@B
R�B
Q�B
R B
RoB
Q�B
R B
TB
T�B
U�B
UgB
U�B
T�B
U2B
XEB
X�B
Y1B
YB
X�B
Y�B
Z�B
Z�B
[	B
[#B
[�B
[�B
[=B
\)B
\]B
[WB
[WB
[�B
[�B
]B
]~B
`'B
a|B
aHB
aB
aB
`�B
`�B
_pB
_!B
_VB
_�B
_;B
_!B
_!B
_!B
_�B
a-B
a-B
`�B
`BB
`'B
a�B
b�B
d&B
dB
c�B
dB
dZB
dtB
d�B
ezB
e`B
e`B
eFB
eFB
d�B
e`B
ezB
e�B
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
hsB
hXB
h>B
h
B
hXB
h$B
g�B
h�B
hsB
h�B
hsB
hsB
hXB
hXB
h�B
h�B
h�B
h�B
h>B
g�B
gmB
g�B
h�B
h�B
h�B
hXB
h$B
h$B
hsB
h�B
iB
iyB
i_B
h�B
iB
i*B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
k�B
k�B
k�B
k�B
lWB
l=B
l=B
l=B
l"B
l"B
k�B
kkB
k6B
k�B
l=B
lWB
l"B
l�B
l�B
mCB
m]B
m�B
m�B
mwB
mwB
n�B
oB
oOB
o�B
pB
p;B
p;B
p�B
qB
qAB
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
r�B
s3B
sB
r�B
raB
rGB
rGB
rGB
rGB
r�B
sMB
tB
u%B
t�B
t�B
t�B
tB
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v�B
v�B
v�B
v�B
wB
w2B
wLB
wLB
w2B
wLB
xB
xB
xRB
x�B
yrB
yXB
y�B
z*B
z�B
z�B
z�B
z�B
z�B
{B
{�B
|B
|6B
|�B
|�B
|�B
|�B
}"B
}�B
~]B
~]B
~BB
~wB
~wB
~�B
~�B
~wB
~�B
~�B
.B
HB
�B
�B
�B
�OB
�iB
��B
�iB
��B
��B
��B
��B
��B
��B
�'B
�'B
�AB
��B
�GB
�{B
�{B
��B
��B
��B
��B
��B
�B
�MB
�gB
�gB
�gB
��B
��B
�SB
��B
�%B
�YB
��B
��B
�B
�+B
�zB
�zB
��B
�KB
�fB
��B
��B
�B
��B
�#B
�=B
�rB
�rB
��B
��B
��B
��B
�DB
��B
��B
��B
�dB
��B
�~B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�"B
�pB
��B
��B
��B
��B
��B
�B
�BB
�BB
�vB
��B
��B
��B
��B
�.B
�bB
��B
��B
��B
��B
��B
��B
�NB
�4B
�B
�B
� B
��B
��B
�B
�4B
�B
�4B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230524065037  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230524065051  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230524065051  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230524065051                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230524065052  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230524065052  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230524065957                      G�O�G�O�G�O�                