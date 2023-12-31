CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-05-22T00:35:21Z creation;2017-05-22T00:35:24Z conversion to V3.1;2019-12-19T08:07:00Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170522003521  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA  I2_0577_121                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�	5�� 1   @�	6�u� @2�Q�_�daZ����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@333@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B8  B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%��C'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D6  D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dx  Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�9�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aհ!Aմ9Aղ-Aհ!Aգ�Aա�A՟�Aՙ�AՓuAՑhAՏ\AՏ\AՏ\AՏ\AՏ\AՍPAՍPAՍPAՏ\AՏ\AՍPA�~�A�^5A�E�A�ȴAӝ�A�A�Aҥ�A��A�A�dZA���A���A�jA��`Aͣ�A͓uA�7LA��A̩�A̝�Ạ�A̕�A�hsA�bNAʮA�O�A� �A�ZA��Aś�A�t�A���A�XA�M�A�hsA��A��!A�XA��A��A�C�A��`A�|�A�1A���A�~�A�ZA��#A���A�?}A��/A�=qA��A�?}A��jA���A��A��FA�5?A��-A���A��DA���A�M�A���A�A��9A���A�E�A�5?A���A�x�A�  A�C�A�jA�  A���A��TA��\A���A��A�O�A���A��\A��PA�A���A��\A�x�A���A�v�A��
A�C�A��A�{A�$�A�5?A��`A�E�A{�mAz�+Ay%Aw�^As�hArffAp��Ao�PAnM�AlVAi�Af��Aex�Ad^5AcS�Abr�A`��A_;dA]"�AY��AX��AT��ATQ�AR�HAPv�AO��AN(�ALv�ALAK�hAJ��AI�mAHr�AF�`AE`BAB{A@�`A@1A??}A=�-A<E�A;��A;;dA9C�A6bNA4=qA2��A1�A1dZA0VA/XA.��A-XA+A)�PA(��A'��A&9XA$�A#ƨA"�\A Q�A+A%AAI�AS�A��AffA�hAZA�A�`AbNA��A�PA�7Ap�A��A��A�A��AA�A+A
-A��A�#A�FA`BA�AA�AS�A�#A�HA��A�7A|�AO�A n�A 5?A �@���@�&�@��!@��+@��/@��@�V@�h@�@�"�@��@��#@�^@�D@��@���@�v�@�E�@�-@�z�@�33@��H@��@�hs@��@ߝ�@�+@��H@�v�@��@��@���@�G�@�%@��/@��`@�/@�G�@�  @�-@�G�@�j@׍P@���@֧�@���@��@�S�@�E�@�J@�@�p�@�/@мj@��;@�^5@�$�@�@̴9@�bN@�+@���@ʗ�@�n�@�{@ɑh@ʗ�@���@�~�@�O�@�V@�r�@Ǿw@�|�@�\)@�o@Ɨ�@ă@�?}@�l�@���@��@��
@���@�%@�x�@�/@�7L@�&�@�?}@�&�@�&�@���@�I�@���@�E�@�j@�V@�I�@��w@�ȴ@��+@�v�@���@�$�@��`@��@�bN@�Z@�Z@��
@���@��^@�V@��!@���@���@�p�@�&�@��@�V@���@��u@���@�;d@�ff@�{@��@��^@���@�Ĝ@�bN@���@���@�|�@�;d@�
=@��@�ȴ@���@�^5@�E�@�5?@���@�X@�V@�Ĝ@��@�j@�Z@�Q�@��@�\)@��@��\@�~�@�=q@�-@��@��@��-@���@�x�@�p�@�hs@�hs@�hs@�x�@��/@���@�l�@�t�@�|�@�t�@�S�@���@���@�V@�x�@��u@��P@�S�@�33@��R@�V@�-@�J@���@��-@���@��@��u@�9X@���@���@��P@�|�@�\)@�@���@��@���@�hs@�?}@��@�V@���@��@���@�b@�(�@��w@�\)@�33@��@��@���@�M�@��#@���@��h@��7@��@�p�@�%@�Ĝ@���@�bN@���@���@�ƨ@��w@��F@��w@��P@�33@��@��!@���@�J@��#@��#@���@��#@���@���@�J@���@���@���@���@�O�@���@���@���@��u@��D@�z�@�j@� �@��@�S�@�;d@�+@�
=@��+@�-@�{@��h@�p�@�V@��@�z�@�Z@�A�@�1@��@��@��R@�v�@�=q@�{@��#@��-@�x�@�X@��@�%@��@��/@��9@��u@��@�j@�Z@�1'@��m@���@���@�K�@���@�V@�J@�@�x�@�?}@���@��j@��D@�Z@�1'@K�@~��@~{@}p�@}?}@|�@|�@{�m@{33@zn�@y�@y��@y��@y7L@x�9@xb@wK�@v�@v�R@v��@v5?@u?}@t��@tj@s��@sƨ@st�@sC�@so@r��@r~�@rn�@rM�@r�@r-@q��@qX@p�u@o�w@o�P@n�@n{@mp�@mV@l�@m�@m?}@l��@l�j@l��@l(�@kS�@j�\@j-@h��@hQ�@h1'@g�@gK�@f��@fV@e�@d��@dZ@d�@c��@c�m@c�F@c�@ct�@cdZ@c33@b�H@b�!@b^5@b-@a�^@aX@aG�@a&�@`�@` �@_�P@_|�@_+@^�R@^E�@^$�@]�h@]/@]/@]V@\�@\�j@\I�@[�@[33@Z�@Z~�@Z=q@ZJ@YX@Y�@XĜ@XbN@W��@W;d@V��@Vv�@VV@U��@U�-@U�-@U��@U�h@U�@Up�@U`B@T�j@S��@SS�@S33@So@Rn�@RJ@Q�@Q��@Qhs@Q%@Pr�@Pb@P  @O�;@O�w@O�w@O|�@O+@N��@N��@NV@M�T@M�h@MO�@M/@L��@L��@Lj@L(�@K��@K��@K"�@J��@I��@I7L@H�9@H�u@HQ�@H1'@G�;@G|�@FE�@E��@D�@D�D@DI�@D�@C�m@C�@Co@A��@A��@A��@A��@A�7@Ax�@Ahs@A7L@@��@@r�@@  @?;d@?+@?�@?
=@>��@>�@>�R@>��@>��@>��@>��@>�+@>E�@>$�@>{@=�@=?}@<�@<j@<�@;t�@;o@:�H@:�\@9�#@9x�@9G�@9&�@9%@8�`@8��@8r�@8bN@8 �@7�@7�@7;d@6v�@6@5��@5�@5?}@5/@5V@4�/@4�D@4j@4Z@4�@3��@3�
@3��@333@2�@2��@2��@2-@1��@1�#@1�^@1&�@0Ĝ@0Q�@0b@/�@/�P@/;d@/�@.�y@.�R@.�+@.ff@.$�@-��@-`B@-V@,z�@+ƨ@+��@+"�@*�H@*��@*�\@*~�@*^5@*^5@*M�@*=q@*=q@*-@*�@*J@*J@)��@)�#@)��@)��@)��@)X@(�`@(r�@(A�@(b@'�@'l�@'K�@'�@&��@&��@&��@&��@&�y@&�@&�R@&�+@&v�@&v�@&v�@&5?@&{@%��@%V@$��@$��@$Z@$9X@$�@$1@#��@#�m@#�@#"�@#o@"��@"�\@"M�@"J@!x�@!%@ ��@ Ĝ@ Ĝ@ �9@ r�@�@l�@;d@ȴ@��@�+@ff@{@p�@?}@V@��@I�@1@�m@��@��@��@��@��@�@dZ@dZ@S�@�@��@�!@��@�\@~�@n�@n�@^5@^5@-@��@�#@��@��@x�@G�@�@�@%@�9@r�@A�@  @��@|�@K�@+@�y@��@ff@@`B@�@�@�/@�/@�j@�@��@�D@�D@�D@�D@z�@Z@I�@�
@��@��@t�@�@�\@^5@^5@-@J@��@�@��@x�@X@&�@�`@��@�@bN@b@�;@�@��@�P@\)@�@�y@��@ȴ@��@$�@{@@��@��@p�@V@��@��@j@9X@(�@(�@�@1@�m@�F@dZ@S�@C�@33@33@@
��@
^5@
M�@	�@	��@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aհ!Aմ9Aղ-Aհ!Aգ�Aա�A՟�Aՙ�AՓuAՑhAՏ\AՏ\AՏ\AՏ\AՏ\AՍPAՍPAՍPAՏ\AՏ\AՍPA�~�A�^5A�E�A�ȴAӝ�A�A�Aҥ�A��A�A�dZA���A���A�jA��`Aͣ�A͓uA�7LA��A̩�A̝�Ạ�A̕�A�hsA�bNAʮA�O�A� �A�ZA��Aś�A�t�A���A�XA�M�A�hsA��A��!A�XA��A��A�C�A��`A�|�A�1A���A�~�A�ZA��#A���A�?}A��/A�=qA��A�?}A��jA���A��A��FA�5?A��-A���A��DA���A�M�A���A�A��9A���A�E�A�5?A���A�x�A�  A�C�A�jA�  A���A��TA��\A���A��A�O�A���A��\A��PA�A���A��\A�x�A���A�v�A��
A�C�A��A�{A�$�A�5?A��`A�E�A{�mAz�+Ay%Aw�^As�hArffAp��Ao�PAnM�AlVAi�Af��Aex�Ad^5AcS�Abr�A`��A_;dA]"�AY��AX��AT��ATQ�AR�HAPv�AO��AN(�ALv�ALAK�hAJ��AI�mAHr�AF�`AE`BAB{A@�`A@1A??}A=�-A<E�A;��A;;dA9C�A6bNA4=qA2��A1�A1dZA0VA/XA.��A-XA+A)�PA(��A'��A&9XA$�A#ƨA"�\A Q�A+A%AAI�AS�A��AffA�hAZA�A�`AbNA��A�PA�7Ap�A��A��A�A��AA�A+A
-A��A�#A�FA`BA�AA�AS�A�#A�HA��A�7A|�AO�A n�A 5?A �@���@�&�@��!@��+@��/@��@�V@�h@�@�"�@��@��#@�^@�D@��@���@�v�@�E�@�-@�z�@�33@��H@��@�hs@��@ߝ�@�+@��H@�v�@��@��@���@�G�@�%@��/@��`@�/@�G�@�  @�-@�G�@�j@׍P@���@֧�@���@��@�S�@�E�@�J@�@�p�@�/@мj@��;@�^5@�$�@�@̴9@�bN@�+@���@ʗ�@�n�@�{@ɑh@ʗ�@���@�~�@�O�@�V@�r�@Ǿw@�|�@�\)@�o@Ɨ�@ă@�?}@�l�@���@��@��
@���@�%@�x�@�/@�7L@�&�@�?}@�&�@�&�@���@�I�@���@�E�@�j@�V@�I�@��w@�ȴ@��+@�v�@���@�$�@��`@��@�bN@�Z@�Z@��
@���@��^@�V@��!@���@���@�p�@�&�@��@�V@���@��u@���@�;d@�ff@�{@��@��^@���@�Ĝ@�bN@���@���@�|�@�;d@�
=@��@�ȴ@���@�^5@�E�@�5?@���@�X@�V@�Ĝ@��@�j@�Z@�Q�@��@�\)@��@��\@�~�@�=q@�-@��@��@��-@���@�x�@�p�@�hs@�hs@�hs@�x�@��/@���@�l�@�t�@�|�@�t�@�S�@���@���@�V@�x�@��u@��P@�S�@�33@��R@�V@�-@�J@���@��-@���@��@��u@�9X@���@���@��P@�|�@�\)@�@���@��@���@�hs@�?}@��@�V@���@��@���@�b@�(�@��w@�\)@�33@��@��@���@�M�@��#@���@��h@��7@��@�p�@�%@�Ĝ@���@�bN@���@���@�ƨ@��w@��F@��w@��P@�33@��@��!@���@�J@��#@��#@���@��#@���@���@�J@���@���@���@���@�O�@���@���@���@��u@��D@�z�@�j@� �@��@�S�@�;d@�+@�
=@��+@�-@�{@��h@�p�@�V@��@�z�@�Z@�A�@�1@��@��@��R@�v�@�=q@�{@��#@��-@�x�@�X@��@�%@��@��/@��9@��u@��@�j@�Z@�1'@��m@���@���@�K�@���@�V@�J@�@�x�@�?}@���@��j@��D@�Z@�1'@K�@~��@~{@}p�@}?}@|�@|�@{�m@{33@zn�@y�@y��@y��@y7L@x�9@xb@wK�@v�@v�R@v��@v5?@u?}@t��@tj@s��@sƨ@st�@sC�@so@r��@r~�@rn�@rM�@r�@r-@q��@qX@p�u@o�w@o�P@n�@n{@mp�@mV@l�@m�@m?}@l��@l�j@l��@l(�@kS�@j�\@j-@h��@hQ�@h1'@g�@gK�@f��@fV@e�@d��@dZ@d�@c��@c�m@c�F@c�@ct�@cdZ@c33@b�H@b�!@b^5@b-@a�^@aX@aG�@a&�@`�@` �@_�P@_|�@_+@^�R@^E�@^$�@]�h@]/@]/@]V@\�@\�j@\I�@[�@[33@Z�@Z~�@Z=q@ZJ@YX@Y�@XĜ@XbN@W��@W;d@V��@Vv�@VV@U��@U�-@U�-@U��@U�h@U�@Up�@U`B@T�j@S��@SS�@S33@So@Rn�@RJ@Q�@Q��@Qhs@Q%@Pr�@Pb@P  @O�;@O�w@O�w@O|�@O+@N��@N��@NV@M�T@M�h@MO�@M/@L��@L��@Lj@L(�@K��@K��@K"�@J��@I��@I7L@H�9@H�u@HQ�@H1'@G�;@G|�@FE�@E��@D�@D�D@DI�@D�@C�m@C�@Co@A��@A��@A��@A��@A�7@Ax�@Ahs@A7L@@��@@r�@@  @?;d@?+@?�@?
=@>��@>�@>�R@>��@>��@>��@>��@>�+@>E�@>$�@>{@=�@=?}@<�@<j@<�@;t�@;o@:�H@:�\@9�#@9x�@9G�@9&�@9%@8�`@8��@8r�@8bN@8 �@7�@7�@7;d@6v�@6@5��@5�@5?}@5/@5V@4�/@4�D@4j@4Z@4�@3��@3�
@3��@333@2�@2��@2��@2-@1��@1�#@1�^@1&�@0Ĝ@0Q�@0b@/�@/�P@/;d@/�@.�y@.�R@.�+@.ff@.$�@-��@-`B@-V@,z�@+ƨ@+��@+"�@*�H@*��@*�\@*~�@*^5@*^5@*M�@*=q@*=q@*-@*�@*J@*J@)��@)�#@)��@)��@)��@)X@(�`@(r�@(A�@(b@'�@'l�@'K�@'�@&��@&��@&��@&��@&�y@&�@&�R@&�+@&v�@&v�@&v�@&5?@&{@%��@%V@$��@$��@$Z@$9X@$�@$1@#��@#�m@#�@#"�@#o@"��@"�\@"M�@"J@!x�@!%@ ��@ Ĝ@ Ĝ@ �9@ r�@�@l�@;d@ȴ@��@�+@ff@{@p�@?}@V@��@I�@1@�m@��@��@��@��@��@�@dZ@dZ@S�@�@��@�!@��@�\@~�@n�@n�@^5@^5@-@��@�#@��@��@x�@G�@�@�@%@�9@r�@A�@  @��@|�@K�@+@�y@��@ff@@`B@�@�@�/@�/@�j@�@��@�D@�D@�D@�D@z�@Z@I�@�
@��@��@t�@�@�\@^5@^5@-@J@��@�@��@x�@X@&�@�`@��@�@bN@b@�;@�@��@�P@\)@�@�y@��@ȴ@��@$�@{@@��@��@p�@V@��@��@j@9X@(�@(�@�@1@�m@�F@dZ@S�@C�@33@33@@
��@
^5@
M�@	�@	��@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
JB

=B

=B

=B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B
	7B
	7B
	7B

=B
	7B
	7B
	7B

=B

=B

=B
JB
JB
!�B
R�B
aHB
u�B
�B
�oB
��B
��B
��B
�3B
�LB
�jB
ƨB
�TB
�B
=BD�BcTBv�B�\B�DB��B�9B�B��B+B;dBE�BE�BE�BC�BN�B[#BiyBu�By�B�B�B|�Bx�Bp�BgmBW
BQ�BP�BN�BK�BL�BD�BB�BD�BG�BJ�BJ�BI�BH�BC�B<jB7LB49B0!B(�B�BoBbBVBJB	7BB��B�B�yB��B�jB��B�=By�BffB\)BD�B)�BbBB
�ZB
��B
ǮB
�'B
��B
�B
Q�B
9XB
@�B
iyB
� B
|�B
iyB
B�B
2-B
#�B
�B	�B	�B	�mB	�mB	�NB	�B	ĜB	�RB	�B	��B	��B	��B	�+B	t�B	cTB	L�B	A�B	2-B	'�B	#�B	�B	oB	uB	JB	DB	DB	PB	JB		7B��B�B�B�yB�fB�TB�5B�B�B��B��B��BŢB�}B�qB�dB�XB�FB�9B�-B�B�B��B��B��B��B��B�!B�'B�B�B�B�'B�9B�9B�?B�LB�XB�dB�dB�dB�^B�dB�jB�wBÖBĜBǮBȴBȴBȴBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�FB�3B�B�B�B�B�RB�dB�qB��BÖBÖBŢBǮBȴBƨBƨBȴB��B��B��B��B��B�B�5B�NB�ZB�ZB�`B�fB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B�B�B�B�B��B��B	  B	B	%B		7B	PB	�B	#�B	'�B	,B	0!B	0!B	0!B	1'B	1'B	0!B	/B	+B	 �B	�B	hB	\B	VB	uB	�B	�B	"�B	%�B	&�B	+B	,B	-B	.B	0!B	/B	2-B	/B	49B	33B	8RB	:^B	;dB	@�B	A�B	E�B	C�B	F�B	G�B	H�B	I�B	I�B	F�B	H�B	L�B	O�B	P�B	R�B	T�B	T�B	T�B	T�B	T�B	T�B	S�B	XB	YB	YB	XB	YB	_;B	_;B	aHB	bNB	bNB	bNB	cTB	cTB	ffB	iyB	n�B	r�B	s�B	u�B	w�B	y�B	z�B	}�B	�B	�B	�B	�B	�%B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�!B	�3B	�9B	�FB	�FB	�LB	�LB	�LB	�XB	�dB	�jB	�jB	�}B	��B	��B	��B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
	7B

=B
DB
DB
JB
DB
DB
JB
PB
PB
PB
JB
JB
PB
VB
\B
\B
bB
bB
oB
oB
oB
uB
uB
{B
{B
{B
�B
{B
�B
�B
�B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
JB

XB

XB

XB

XB

XB
	RB
	lB
	7B
	7B
	7B

=B
	RB
	7B
	7B

=B
	7B
	7B
	RB

XB

rB

�B
�B
�B
$&B
S�B
b�B
wLB
�B
��B
�B
��B
��B
�B
��B
��B
�_B
�B
�[B
�BD�Bc�Bw�B� B�B��B��B�/B�6B-�B=�BF�BGBG�BE9BO�B[�Bj0Bv`Bz�B��B��B~Bz^BsMBjBYKBSBQ�BPbBN�BO�BFYBF�BIRBI�BK�BK�BK�BLJBG�B>�B8�B6`B3�B+�BSBB�B BvB�BB�"B��B�}B��B��B��B�VB}�Bh�B_�BG+B,=BTB�B
�>B
�
B
�lB
�3B
�B
�rB
T�B
;JB
AB
i�B
��B
�B
m�B
D�B
4TB
&fB
]B	�B	��B	�B	�yB	�B	یB	�B	�*B	��B	�LB	�'B	��B	��B	w�B	gB	O(B	E9B	3�B	*B	&�B	�B	aB	2B	6B	B	dB	�B	VB	^B�wB�%B�)B�B��B�FB��B�#B�sB��B�mB�VB�_B��B��B��B��B��B��B��B��B�=B��B�=B��B��B��B��B�3B�cB��B�B�|B�%B�?B��B�$B�B��B�6B�PB��B��B��B��B��BżBȀBɆBɆB��B�7B̈́B��B�\B�pB��B��BуBҽB�PB��B�jB�B�pB� B�TB�{B�9B׍B�B�{B�VB�"B�'B�B�TB��B��B�wB��B�$B�B��B�B��B�3BƨBȀB�B�BǔB�lB̘B�HB�4B�TB�TB�EB�jB�B�B�tB�zB�B��B�B��B�cB�CB�IB�B�B�]B�B�5B�OB��B��B��B� B�TB��B��B�-B�B�B�TB��B�<B	 4B	aB	�B		lB	�B	�B	$@B	(�B	,WB	0�B	0�B	0oB	1[B	1�B	1B	0�B	-B	"B	�B	TB	bB	�B	�B	gB	�B	"�B	&B	'B	+6B	,"B	-wB	.�B	0�B	0;B	2�B	/B	4�B	3�B	8�B	:�B	;�B	@�B	B'B	FYB	C�B	F�B	G�B	H�B	JrB	J�B	F�B	H�B	L�B	O�B	Q4B	S�B	U2B	U2B	U2B	U2B	UgB	U�B	T�B	X�B	YKB	YeB	X_B	Y�B	_�B	_�B	a�B	b�B	b�B	b�B	c�B	c�B	f�B	i�B	n�B	r�B	tB	v+B	x8B	zB	{0B	~BB	� B	�GB	�3B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�WB	��B	��B	�/B	�B	�;B	�MB	��B	��B	�zB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	�B	�1B	�)B	�6B	��B	��B	�B	�B	�.B	�:B	�uB	�B	�WB	�CB	�OB	�VB	�vB	�\B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�"B	�BB	�(B	�.B
 iB
GB
B
B
B
B
B
+B
	lB

XB
xB
^B
~B
xB
�B
~B
�B
jB
jB
dB
�B
�B
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
!B
 B
 B
 B
 �B
 �B
!�B
#B
#:B
#:B
#B
#B
#B
$B
#B
#:B
#:B
#B
#B
#B
#B
#:B
$B
$&B
%B
$�B
$�B
%B
%�B
&B
%�B
'B
'B
'B
(
B
)*B
*KB
*KB
*eB
*B
*KB
+kB
+6B
+B
+B
+B
-)B
-CB
./B
.IB
.IB
.}B
/iB
/iB
/�B
0UB
0;B
0;B
0UB
1[B
1vB
1�B
1AB
2aB
3MB
3MB
4nB
4TB
4TB
4TB
4nB
4nB
4nB
4nB
4nB
4TB
4�B
5ZB
5tB
5tB
5tB
6�B
6�B
7fB
6zB
7�B
7�B
7fB
7�B
7fB
8RB
8�B
8�B
8lB
8�B
9�B
9rB
9�B
9�B
9�B
9�B
9�B
9rB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:xB
:xB
;B
;dB
;B
;B
;B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?}B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
EB
E�B
E�B
E�B
E�B
F�B
F�B
F�B
GB
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
LB
K�B
L�B
L�B
L�B
L�B
L�B
MB
MB
L�B
L�B
L�B
L�B
MB
M�B
M�B
NB
N<B
NB
N�B
OB
O(B
OB
O�B
PB
P.B
QB
QB
QB
Q B
Q B
RB
R B
R B
RB
RB
RB
R:B
S@B
S@B
TB
TB
T,B
T�B
U2B
UB
UB
U2B
U2B
UB
V9B
V9B
VB
V9B
VB
VB
W?B
WYB
W$B
W$B
W$B
WYB
X_B
XEB
YKB
YKB
YKB
Y1B
Z7B
ZQB
ZQB
ZQB
ZQB
ZQB
[=B
[qB
[qB
[WB
\xB
]IB
]dB
]IB
]IB
]IB
^5B
^jB
^OB
^OB
^5B
^OB
^5B
^OB
^5B
^OB
^OB
^OB
^5B
^jB
_VB
^jB
_pB
_pB
`vB
`vB
`vB
`\B
a|B
a|B
abB
aHB
aHB
abB
abB
aHB
a|B
a|B
bhB
bNB
abB
abB
b�B
b�B
b�B
cnB
c�B
c�B
c�B
c�B
dtB
dtB
dtB
d�B
d�B
dtB
d�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
ffB
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
xB
w�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
{B
{B
z�B
z�B
z�B
z�B
z�B
|B
|B
|B
|B
}"B
|�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705260037192017052600371920170526003719201806221313472018062213134720180622131347201804050715192018040507151920180405071519  JA  ARFMdecpA19c                                                                20170522093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170522003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170522003523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170522003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170522003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170522003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170522003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170522003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170522003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170522003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20170522010803                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170522153155  CV  JULD            G�O�G�O�F�I�                JM  ARGQJMQC2.0                                                                 20170522153155  CV  JULD_LOCATION   G�O�G�O�F�I�                JM  ARGQJMQC2.0                                                                 20170522153155  CV  LATITUDE        G�O�G�O�A�ȴ                JM  ARGQJMQC2.0                                                                 20170522153155  CV  LONGITUDE       G�O�G�O��#                JM  ARCAJMQC2.0                                                                 20170525153719  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170525153719  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221519  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041347  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                