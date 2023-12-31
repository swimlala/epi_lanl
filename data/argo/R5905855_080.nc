CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:24:32Z creation;2022-06-04T19:24:32Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192432  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               PA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�n���=�1   @�n��0�@-n��P�dO�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   CL�C33C��C�fC	�fC�fC�fC  C  C  C�fC  C  C  C  C   C"  C$  C&�C(  C*  C,�C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|��C}ffC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@y��@���@���AffA>ffA^ffA|��A�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B˙�Bϙ�B���B���B���B���B���B���B���B���B���B���B���B���C33C�C�3C��C	��C��C��C�fC�fC�fC��C�fC�fC�fC�fC�fC!�fC#�fC&  C'�fC)�fC,  C.  C/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]��C_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC|� C}L�C�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#s4D#��D$y�D$��D%y�D%��D&y�D&��D'y�D(  D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^�A�]�A�^A�\)A�7LA�.�A�	lA�;A��yAɸRAɞ�A�~�A�W?A�,qA�(A��A��)A��)A��2A��BA���A��XA��aAȾAȻ0Aȹ�AȵAȱ�Aȯ�AȬ=AȪ0AȨXAȖAȋDA�n/A�5?AǾwA�k�A��Aƭ�A�qA�NA�AUA�9�A�:A�ϫAųhAŬqAŧA���A��A��QA�)*A�A�	lA�e,A���A�G�A�H�A�<�A��OA�dZA�)*A�ߤA�{A���A��oA���A��A�V�A��^A�IA�XEA��]A��=A�$�A��pA��IA�:�A��A��bA���A�WsA�*�A���A���A�G�A�(�A��A��#A���A��A���A�.}A��A�ѷA�� A�t�A��A�m�A�H�A�oA���A�VA��oA��IA�J#A��MA�u�A�a�A���A���A�Z�A�	lA��7A���A�6FA���A{��Aw��At��Ap?AgTaAaA^�AS��AR�SAL��AJ(�AGJ�ADc ABw�A@�=A?HA=0�A<A;XyA:�<A9  A7V�A61'A5��A3qvA.҉A*��A*��A*��A*~(A,�AA.��A2�A2�9A3'RA2�QA1�A0ffA06zA0K�A/��A.�A-��A-
�A+�	A+�*A+R�A*�hA)خA)I�A(MA'�]A'�A'�A'*�A&��A&s�A%�A%/�A% �A#�]A#1A"��A"-A!�4A ��A �A?AȴAbA�4AbA��A�KA	lAL�Ac�A�A�A�XAa�A�9A#:A�A�7A�/A��A��AVA��A��A��A*�A҉A}VAZ�A/�AAA�A
�A�8A��AߤA�nAL0A?}A  A�uAC�A��A+�AیA��A=�A\)A��A[WA+�A��A��A�YAW�A.�A��A�A�A��A�\AuA��AB[AA�A
xA	��A	H�A�AX�A�A|�A�^A��AO�A��Ap�A�[Av�AAo A!A�=A.�A��A��AD�A7�AA ��A �jA �uA �@�@��d@�#�@���@���@���@�Z�@��H@�{�@���@��S@���@�o@�@��@��@�p�@��@�c @�C-@�x@�F�@���@�b�@�o@��@�!�@�=@�R�@땁@�@��@�M@迱@��@�@�M@�W?@��@�@䖼@��@��@�:�@�"�@�GE@�j@�^�@�2a@��]@�x@�&@���@܁o@�Xy@�4@ۭC@���@�S�@�^�@�O@�4@�	@��;@�B�@�:�@���@Շ�@�N�@Ӡ�@Ӊ7@�:�@��M@��@ҩ�@Ґ.@�C-@�|@�@Сb@�6�@�@ϥ�@λ�@Χ�@ΐ.@ͺ^@�6z@�҉@�:*@��s@Ɉf@�A�@���@�r�@��@�ݘ@ǃ{@�1�@��/@Ɛ.@�Xy@���@ū�@��5@�j@�($@���@¼j@���@���@��S@�v`@�-w@���@��@��@�hs@�@@���@�:*@���@��@�#�@�ߤ@���@�m�@�I�@�3�@��@�a�@��@���@�a|@�5?@��@�6z@���@�tT@�8�@��Z@��@�1�@���@��@��@���@�33@��@�*�@���@��@��E@��A@��T@�g�@�S@��@�ȴ@�L0@��g@�|�@�X�@�S@��9@���@��@�{�@��@�S&@�@���@�U2@��]@��^@��7@�J#@�$t@��@��$@���@�V@��@��@��6@���@�]d@�Z�@�!@��.@��)@��;@���@��@�Q@��@��@��3@�}�@�(�@��B@�}V@�x@�y�@�hs@�Q�@�@�Ɇ@���@�c�@�B[@�D�@�7�@�,=@�$�@��W@���@�l�@�<6@�
=@���@���@�bN@���@�e,@�4@���@��B@���@�~�@�$@���@�a@�C�@��c@��Y@�@��'@�k�@�Y�@�(�@��4@�]d@�  @���@�}�@��@�}V@�H�@�-�@���@��@��V@��k@���@��$@�y�@�+�@�H@�M@�
�@���@��@�PH@�!�@���@��~@�8�@��@���@�Xy@��@��3@�~�@�!�@��@���@�$@��a@�a�@��@�ی@���@��@���@�r�@�*�@��@�@���@�\)@�)_@��@���@�_�@��@�ϫ@�Vm@�%F@���@���@���@��@�h�@�J�@��@�ݘ@���@�}�@�O�@�%F@��f@��h@�^5@� �@��>@���@���@�F�@��P@�Ĝ@�a|@�($@�%�@�#:@�_@��#@���@��"@�s�@�O�@���@���@�m�@�6@���@�� @��d@���@�c�@�O@�@��5@��m@��6@��\@��Y@�q@�V@��@~�"@~v�@~C�@}��@}�3@}�@|�@|$@|�@{�@{�f@{K�@z�B@z}V@zq�@y�.@y�~@y�@xɆ@xV�@w�w@w�4@v��@v�@vL0@v!�@v@u�^@u!�@tq@s��@s&@r!�@q�C@qY�@p��@pu�@o��@oJ#@o�@n�@n1�@m�3@l�	@l��@lg8@l�@k�4@k�@j�1@j1�@i�@i��@ik�@h�/@h!@gݘ@g�:@g"�@f�1@e��@e�M@eIR@d��@d��@d9X@c��@c{J@c;d@b��@b_�@b_@a��@`�P@`z�@`"h@_ƨ@_e�@_A�@_8@^��@^�x@^u%@^1�@]�@]�@]��@]O�@]�@\��@\q@\�@[dZ@Z�@Zc @Z�@Y��@Y��@Yk�@Yq@XtT@W�&@W��@WW?@V� @Uϫ@U��@U|@U<6@U/@U�@T��@TA�@T@S�A@S�&@S��@S�@SZ�@R��@R�<@R��@R��@R.�@R �@Q��@Qk�@P�z@O�@O�:@Oe�@ORT@O9�@N��@N��@N#:@M��@M�C@Ms�@L��@L]d@L,=@L  @K�g@K�@KMj@J�@J{�@I�@I\�@IL�@IJ�@I@H�9@H[�@Hx@G�r@G�;@G�g@G��@F�<@FR�@E��@E	l@D:�@D�@C�g@C�$@C'�@B�x@BkQ@BQ@B0U@A�@AO�@A�@@��@@r�@@q@@Xy@@b@?��@?iD@?(@>�X@>��@=��@=��@=�@<<�@;�V@;,�@:�@:��@:;�@9��@9hs@9�@8�v@8��@8��@8��@86@7�@7�:@7j�@7Mj@6��@6��@6�R@6:*@6@5��@52a@4��@4��@4u�@4�@3�m@3�@3E9@2��@2.�@1zx@1F@0�|@0�9@0y>@/�g@/��@/s@/.I@/�@.ں@.��@.
�@-��@- \@,Ĝ@,��@,*�@+�@+خ@+�a@+�@+��@+��@+�@+_p@+&@*�s@*�x@*l�@*Ov@*#:@*	@*@)�Z@)��@)��@)e,@)@@(�K@(��@(|�@([�@(�@'�r@'�$@'>�@&��@&҉@&l�@&L0@%�>@%�^@%�h@%e,@%0�@$�f@$�O@$�_@$��@$6@#�K@#��@#��@#iD@#F�@#=@#.I@#�@"�H@"�<@"�@!�M@!Dg@!8�@!2a@!+@ �|@ �U@ 2�@��@��@�[@�:@g�@'�@��@��@�@��@�@@�@	@J@�.@�Z@��@p�@e,@8�@�	@�$@z�@2�@�@~�@�@�y@�X@}V@
�@��@��@��@�X@�7@Dg@*0@@@�	@��@I�@1@�g@��@v`@!-@��@��@�r@z@�@��@�@�j@�@/@q@@@��@�e@�e@�4@r�@M@4n@~@خ@v`@6z@�@�m@+k@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^�A�]�A�^A�\)A�7LA�.�A�	lA�;A��yAɸRAɞ�A�~�A�W?A�,qA�(A��A��)A��)A��2A��BA���A��XA��aAȾAȻ0Aȹ�AȵAȱ�Aȯ�AȬ=AȪ0AȨXAȖAȋDA�n/A�5?AǾwA�k�A��Aƭ�A�qA�NA�AUA�9�A�:A�ϫAųhAŬqAŧA���A��A��QA�)*A�A�	lA�e,A���A�G�A�H�A�<�A��OA�dZA�)*A�ߤA�{A���A��oA���A��A�V�A��^A�IA�XEA��]A��=A�$�A��pA��IA�:�A��A��bA���A�WsA�*�A���A���A�G�A�(�A��A��#A���A��A���A�.}A��A�ѷA�� A�t�A��A�m�A�H�A�oA���A�VA��oA��IA�J#A��MA�u�A�a�A���A���A�Z�A�	lA��7A���A�6FA���A{��Aw��At��Ap?AgTaAaA^�AS��AR�SAL��AJ(�AGJ�ADc ABw�A@�=A?HA=0�A<A;XyA:�<A9  A7V�A61'A5��A3qvA.҉A*��A*��A*��A*~(A,�AA.��A2�A2�9A3'RA2�QA1�A0ffA06zA0K�A/��A.�A-��A-
�A+�	A+�*A+R�A*�hA)خA)I�A(MA'�]A'�A'�A'*�A&��A&s�A%�A%/�A% �A#�]A#1A"��A"-A!�4A ��A �A?AȴAbA�4AbA��A�KA	lAL�Ac�A�A�A�XAa�A�9A#:A�A�7A�/A��A��AVA��A��A��A*�A҉A}VAZ�A/�AAA�A
�A�8A��AߤA�nAL0A?}A  A�uAC�A��A+�AیA��A=�A\)A��A[WA+�A��A��A�YAW�A.�A��A�A�A��A�\AuA��AB[AA�A
xA	��A	H�A�AX�A�A|�A�^A��AO�A��Ap�A�[Av�AAo A!A�=A.�A��A��AD�A7�AA ��A �jA �uA �@�@��d@�#�@���@���@���@�Z�@��H@�{�@���@��S@���@�o@�@��@��@�p�@��@�c @�C-@�x@�F�@���@�b�@�o@��@�!�@�=@�R�@땁@�@��@�M@迱@��@�@�M@�W?@��@�@䖼@��@��@�:�@�"�@�GE@�j@�^�@�2a@��]@�x@�&@���@܁o@�Xy@�4@ۭC@���@�S�@�^�@�O@�4@�	@��;@�B�@�:�@���@Շ�@�N�@Ӡ�@Ӊ7@�:�@��M@��@ҩ�@Ґ.@�C-@�|@�@Сb@�6�@�@ϥ�@λ�@Χ�@ΐ.@ͺ^@�6z@�҉@�:*@��s@Ɉf@�A�@���@�r�@��@�ݘ@ǃ{@�1�@��/@Ɛ.@�Xy@���@ū�@��5@�j@�($@���@¼j@���@���@��S@�v`@�-w@���@��@��@�hs@�@@���@�:*@���@��@�#�@�ߤ@���@�m�@�I�@�3�@��@�a�@��@���@�a|@�5?@��@�6z@���@�tT@�8�@��Z@��@�1�@���@��@��@���@�33@��@�*�@���@��@��E@��A@��T@�g�@�S@��@�ȴ@�L0@��g@�|�@�X�@�S@��9@���@��@�{�@��@�S&@�@���@�U2@��]@��^@��7@�J#@�$t@��@��$@���@�V@��@��@��6@���@�]d@�Z�@�!@��.@��)@��;@���@��@�Q@��@��@��3@�}�@�(�@��B@�}V@�x@�y�@�hs@�Q�@�@�Ɇ@���@�c�@�B[@�D�@�7�@�,=@�$�@��W@���@�l�@�<6@�
=@���@���@�bN@���@�e,@�4@���@��B@���@�~�@�$@���@�a@�C�@��c@��Y@�@��'@�k�@�Y�@�(�@��4@�]d@�  @���@�}�@��@�}V@�H�@�-�@���@��@��V@��k@���@��$@�y�@�+�@�H@�M@�
�@���@��@�PH@�!�@���@��~@�8�@��@���@�Xy@��@��3@�~�@�!�@��@���@�$@��a@�a�@��@�ی@���@��@���@�r�@�*�@��@�@���@�\)@�)_@��@���@�_�@��@�ϫ@�Vm@�%F@���@���@���@��@�h�@�J�@��@�ݘ@���@�}�@�O�@�%F@��f@��h@�^5@� �@��>@���@���@�F�@��P@�Ĝ@�a|@�($@�%�@�#:@�_@��#@���@��"@�s�@�O�@���@���@�m�@�6@���@�� @��d@���@�c�@�O@�@��5@��m@��6@��\@��Y@�q@�V@��@~�"@~v�@~C�@}��@}�3@}�@|�@|$@|�@{�@{�f@{K�@z�B@z}V@zq�@y�.@y�~@y�@xɆ@xV�@w�w@w�4@v��@v�@vL0@v!�@v@u�^@u!�@tq@s��@s&@r!�@q�C@qY�@p��@pu�@o��@oJ#@o�@n�@n1�@m�3@l�	@l��@lg8@l�@k�4@k�@j�1@j1�@i�@i��@ik�@h�/@h!@gݘ@g�:@g"�@f�1@e��@e�M@eIR@d��@d��@d9X@c��@c{J@c;d@b��@b_�@b_@a��@`�P@`z�@`"h@_ƨ@_e�@_A�@_8@^��@^�x@^u%@^1�@]�@]�@]��@]O�@]�@\��@\q@\�@[dZ@Z�@Zc @Z�@Y��@Y��@Yk�@Yq@XtT@W�&@W��@WW?@V� @Uϫ@U��@U|@U<6@U/@U�@T��@TA�@T@S�A@S�&@S��@S�@SZ�@R��@R�<@R��@R��@R.�@R �@Q��@Qk�@P�z@O�@O�:@Oe�@ORT@O9�@N��@N��@N#:@M��@M�C@Ms�@L��@L]d@L,=@L  @K�g@K�@KMj@J�@J{�@I�@I\�@IL�@IJ�@I@H�9@H[�@Hx@G�r@G�;@G�g@G��@F�<@FR�@E��@E	l@D:�@D�@C�g@C�$@C'�@B�x@BkQ@BQ@B0U@A�@AO�@A�@@��@@r�@@q@@Xy@@b@?��@?iD@?(@>�X@>��@=��@=��@=�@<<�@;�V@;,�@:�@:��@:;�@9��@9hs@9�@8�v@8��@8��@8��@86@7�@7�:@7j�@7Mj@6��@6��@6�R@6:*@6@5��@52a@4��@4��@4u�@4�@3�m@3�@3E9@2��@2.�@1zx@1F@0�|@0�9@0y>@/�g@/��@/s@/.I@/�@.ں@.��@.
�@-��@- \@,Ĝ@,��@,*�@+�@+خ@+�a@+�@+��@+��@+�@+_p@+&@*�s@*�x@*l�@*Ov@*#:@*	@*@)�Z@)��@)��@)e,@)@@(�K@(��@(|�@([�@(�@'�r@'�$@'>�@&��@&҉@&l�@&L0@%�>@%�^@%�h@%e,@%0�@$�f@$�O@$�_@$��@$6@#�K@#��@#��@#iD@#F�@#=@#.I@#�@"�H@"�<@"�@!�M@!Dg@!8�@!2a@!+@ �|@ �U@ 2�@��@��@�[@�:@g�@'�@��@��@�@��@�@@�@	@J@�.@�Z@��@p�@e,@8�@�	@�$@z�@2�@�@~�@�@�y@�X@}V@
�@��@��@��@�X@�7@Dg@*0@@@�	@��@I�@1@�g@��@v`@!-@��@��@�r@z@�@��@�@�j@�@/@q@@@��@�e@�e@�4@r�@M@4n@~@خ@v`@6z@�@�m@+k@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
dB
�B
IB
/B
5B
B
!B
�B
 �B
!�B
!�B
!|B
!bB
"B
!�B
 �B
 \B
 \B
 �B
 �B
!|B
"B
!�B
!-B
 �B
!B
 �B
 �B
!-B
!|B
!�B
!bB
�B
OB
�B
/B
(�B
6+B
?�B
J�B
S�B
Y�B
`�B
e,B
o B
utB
z^B
{�B
{�B
��B
� B
�B
�%B
� B
��B
�CB�B�B0UB<B=�B=�B<�BI7BX�B[=B[�Br�B��B�*B��B�B�OB�|B�B��B�qB�RB�wB��B��B�?B�tB�FB�B��B�B�B��B�vB�*B  B 4B�B�B'�B-�B/�BBBM�BN"BR�B]�BiyBw�B��B|�Bu?BhXBB�B�B�B�EButB;B
��B
R:B
$B	��B	�]B	��B	�B	r�B	O\B	C�B	pB	�B�B�CB�B�2B�`B�eB�=B�MB��B�6B��B	B	B	�B	B	<B�B�'B�$B	�B	�B	RTB	��B	�>B	��B
dB
{B
�B
VB
B
&B
8RB
=�B
@4B
CB
@iB
D�B
E�B
D�B
EB
DgB
C-B
C�B
H1B
K^B
L0B
M�B
N�B
OB
RTB
X_B
Q�B
M�B
M�B
MPB
Q�B
R:B
RB
M�B
J�B
JrB
I�B
H�B
K�B
K�B
Q�B
Y�B
[�B
Z�B
]�B
\�B
\)B
Z�B
X�B
V�B
T�B
RTB
Q�B
Q4B
R:B
Q B
P�B
PB
P}B
P.B
O�B
O�B
P.B
PbB
QNB
Q�B
Q�B
R B
Q�B
QNB
Q B
O�B
N�B
OB
OB
M�B
N�B
L~B
J�B
IlB
G�B
E�B
A�B
?�B
>wB
=qB
<�B
<B
;0B
;�B
9�B
8B
72B
6FB
4nB
3�B
2-B
1�B
2B
2-B
2�B
/B
.cB
-�B
-�B
-�B
.�B
0B
/�B
0�B
2aB
2|B
1�B
1[B
0;B
.�B
./B
.IB
,�B
+�B
+6B
+QB
+kB
+B
*�B
)�B
)DB
(
B
&fB
%�B
#�B
"�B
"�B
!�B
!bB
!-B
 BB
VB
OB
B
)B
�B
B
7B
1B
�B
YB
�B
�B
B
9B
B
FB
�B
TB
B
NB
�B
bB
�B
VB
)B

rB

rB
	RB
+B
�B
�B
3B
B
AB
 �B	��B	�cB	�.B	�B	��B	��B	�dB	��B	�^B	�B	��B	�rB	��B	��B	�B	�2B	��B
 iB
  B	��B
 4B	�B	�<B	��B	��B	��B	��B	�0B	��B	�B	��B	��B	��B	��B	��B	�wB	�(B	��B	�<B	��B	�B	��B	��B	�wB	��B	�<B	�B	�]B	��B	��B	�VB	�jB	�PB	��B	��B	�HB	��B	��B	��B	��B	�BB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�cB	�HB	��B	�}B	�HB	��B	�}B	�cB	�HB	��B	��B
  B
B
UB
�B
�B
�B
�B
�B
�B
�B
�B
B
aB
B
B
GB
{B
�B
9B
mB
�B
�B
B
�B
�B
B
zB
_B
EB
�B
1B
�B
�B
�B
	B
	lB
	�B
	�B

	B

#B
B
�B
B
0B
�B
�B
�B
jB
�B
�B
"B
VB
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
vB
B
\B
B
�B
vB
�B
�B
�B
\B
(B
BB
�B
B
�B
�B
�B
�B
HB
�B
@B
aB
aB
aB
�B
2B
�B
�B
�B
gB
MB
�B
B
SB
9B
�B
�B
9B
�B
�B
sB
YB
�B
�B
KB
�B
�B
�B
B
KB
eB
1B
eB
B
�B
QB
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
;B
�B
�B
 BB
 vB
 �B
 �B
 �B
!HB
!�B
!�B
"NB
"�B
"�B
#B
#�B
$@B
$�B
%B
%`B
%�B
%zB
%zB
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&fB
&�B
'�B
'�B
($B
'�B
(
B
*B
*B
*0B
*�B
*�B
+B
+B
+B
+kB
+�B
+�B
+�B
,B
,qB
,�B
-B
,�B
-wB
-�B
.B
.cB
.�B
/B
/ B
.�B
/B
/ B
/ B
.�B
.�B
/5B
/�B
/�B
/�B
0!B
0;B
0;B
0B
0UB
0UB
0UB
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0oB
0�B
1AB
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
2�B
33B
3B
33B
3�B
3�B
4B
5B
6FB
6zB
72B
7�B
7�B
7fB
7�B
8RB
8lB
8lB
8�B
9$B
9�B
:DB
:DB
:�B
:DB
:xB
:�B
;B
;dB
;�B
<B
<jB
=�B
=�B
>]B
>]B
>�B
>�B
?.B
?}B
?�B
@B
@OB
@OB
@OB
@�B
A�B
AoB
A�B
A�B
BAB
B�B
B�B
B�B
C-B
CGB
C�B
DB
DB
D3B
D�B
D�B
D�B
EB
E�B
E�B
FYB
FYB
FtB
FtB
FtB
F�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
G�B
HB
HB
HfB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
K�B
LB
K�B
LB
L~B
L�B
L�B
L�B
L�B
L�B
L�B
M6B
M�B
M�B
M�B
M�B
N"B
N"B
N"B
N�B
O\B
O�B
P.B
PHB
P.B
P.B
P}B
PbB
P}B
PbB
P�B
P�B
RB
Q�B
Q�B
R B
RB
R B
R B
RoB
R�B
R�B
SB
S&B
S&B
S[B
SuB
S�B
T,B
TFB
TaB
TFB
TaB
T{B
T�B
T�B
T�B
UB
UB
UB
U2B
U�B
VSB
V9B
VB
VSB
W$B
W$B
W�B
W�B
WsB
W�B
W�B
W�B
X�B
X�B
Y1B
Y1B
YKB
Y�B
Y�B
ZB
Z�B
Z�B
[#B
[�B
[�B
\B
\]B
\]B
\�B
\�B
\�B
]�B
^B
^5B
^jB
^�B
^�B
^�B
^�B
^�B
_B
_VB
_VB
_�B
_�B
`B
`B
`BB
`\B
`�B
`�B
`�B
`�B
aB
a�B
a|B
a|B
abB
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b�B
c:B
cnB
c�B
d&B
d@B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
e�B
e�B
f2B
ffB
ffB
ffB
f�B
f�B
gB
g8B
gRB
g�B
g�B
h>B
h�B
h�B
h�B
h�B
i*B
i*B
i�B
i�B
i�B
jB
jKB
jeB
j�B
j�B
kB
kB
kB
kQB
k�B
k�B
k�B
l"B
l"B
l"B
l"B
lWB
lWB
l"B
lqB
l�B
l�B
l�B
mB
m)B
m]B
mwB
nIB
ncB
ncB
n}B
n�B
n�B
n�B
o B
oB
oB
o B
oOB
o�B
o�B
o�B
o�B
o�B
poB
pUB
pUB
p�B
p�B
p�B
p�B
q'B
qAB
q[B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
sMB
s�B
shB
s�B
tB
t9B
t�B
t�B
t�B
uB
uB
u%B
uZB
uZB
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
wB
wB
v�B
w2B
wLB
wLB
wfB
w�B
xB
x8B
xRB
x�B
y>B
y�B
z^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
dB
�B
IB
/B
5B
B
!B
�B
 �B
!�B
!�B
!|B
!bB
"B
!�B
 �B
 \B
 \B
 �B
 �B
!|B
"B
!�B
!-B
 �B
!B
 �B
 �B
!-B
!|B
!�B
!bB
�B
OB
�B
/B
(�B
6+B
?�B
J�B
S�B
Y�B
`�B
e,B
o B
utB
z^B
{�B
{�B
��B
� B
�B
�%B
� B
��B
�CB�B�B0UB<B=�B=�B<�BI7BX�B[=B[�Br�B��B�*B��B�B�OB�|B�B��B�qB�RB�wB��B��B�?B�tB�FB�B��B�B�B��B�vB�*B  B 4B�B�B'�B-�B/�BBBM�BN"BR�B]�BiyBw�B��B|�Bu?BhXBB�B�B�B�EButB;B
��B
R:B
$B	��B	�]B	��B	�B	r�B	O\B	C�B	pB	�B�B�CB�B�2B�`B�eB�=B�MB��B�6B��B	B	B	�B	B	<B�B�'B�$B	�B	�B	RTB	��B	�>B	��B
dB
{B
�B
VB
B
&B
8RB
=�B
@4B
CB
@iB
D�B
E�B
D�B
EB
DgB
C-B
C�B
H1B
K^B
L0B
M�B
N�B
OB
RTB
X_B
Q�B
M�B
M�B
MPB
Q�B
R:B
RB
M�B
J�B
JrB
I�B
H�B
K�B
K�B
Q�B
Y�B
[�B
Z�B
]�B
\�B
\)B
Z�B
X�B
V�B
T�B
RTB
Q�B
Q4B
R:B
Q B
P�B
PB
P}B
P.B
O�B
O�B
P.B
PbB
QNB
Q�B
Q�B
R B
Q�B
QNB
Q B
O�B
N�B
OB
OB
M�B
N�B
L~B
J�B
IlB
G�B
E�B
A�B
?�B
>wB
=qB
<�B
<B
;0B
;�B
9�B
8B
72B
6FB
4nB
3�B
2-B
1�B
2B
2-B
2�B
/B
.cB
-�B
-�B
-�B
.�B
0B
/�B
0�B
2aB
2|B
1�B
1[B
0;B
.�B
./B
.IB
,�B
+�B
+6B
+QB
+kB
+B
*�B
)�B
)DB
(
B
&fB
%�B
#�B
"�B
"�B
!�B
!bB
!-B
 BB
VB
OB
B
)B
�B
B
7B
1B
�B
YB
�B
�B
B
9B
B
FB
�B
TB
B
NB
�B
bB
�B
VB
)B

rB

rB
	RB
+B
�B
�B
3B
B
AB
 �B	��B	�cB	�.B	�B	��B	��B	�dB	��B	�^B	�B	��B	�rB	��B	��B	�B	�2B	��B
 iB
  B	��B
 4B	�B	�<B	��B	��B	��B	��B	�0B	��B	�B	��B	��B	��B	��B	��B	�wB	�(B	��B	�<B	��B	�B	��B	��B	�wB	��B	�<B	�B	�]B	��B	��B	�VB	�jB	�PB	��B	��B	�HB	��B	��B	��B	��B	�BB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�cB	�HB	��B	�}B	�HB	��B	�}B	�cB	�HB	��B	��B
  B
B
UB
�B
�B
�B
�B
�B
�B
�B
�B
B
aB
B
B
GB
{B
�B
9B
mB
�B
�B
B
�B
�B
B
zB
_B
EB
�B
1B
�B
�B
�B
	B
	lB
	�B
	�B

	B

#B
B
�B
B
0B
�B
�B
�B
jB
�B
�B
"B
VB
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
vB
B
\B
B
�B
vB
�B
�B
�B
\B
(B
BB
�B
B
�B
�B
�B
�B
HB
�B
@B
aB
aB
aB
�B
2B
�B
�B
�B
gB
MB
�B
B
SB
9B
�B
�B
9B
�B
�B
sB
YB
�B
�B
KB
�B
�B
�B
B
KB
eB
1B
eB
B
�B
QB
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
;B
�B
�B
 BB
 vB
 �B
 �B
 �B
!HB
!�B
!�B
"NB
"�B
"�B
#B
#�B
$@B
$�B
%B
%`B
%�B
%zB
%zB
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&fB
&�B
'�B
'�B
($B
'�B
(
B
*B
*B
*0B
*�B
*�B
+B
+B
+B
+kB
+�B
+�B
+�B
,B
,qB
,�B
-B
,�B
-wB
-�B
.B
.cB
.�B
/B
/ B
.�B
/B
/ B
/ B
.�B
.�B
/5B
/�B
/�B
/�B
0!B
0;B
0;B
0B
0UB
0UB
0UB
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0oB
0�B
1AB
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
2�B
33B
3B
33B
3�B
3�B
4B
5B
6FB
6zB
72B
7�B
7�B
7fB
7�B
8RB
8lB
8lB
8�B
9$B
9�B
:DB
:DB
:�B
:DB
:xB
:�B
;B
;dB
;�B
<B
<jB
=�B
=�B
>]B
>]B
>�B
>�B
?.B
?}B
?�B
@B
@OB
@OB
@OB
@�B
A�B
AoB
A�B
A�B
BAB
B�B
B�B
B�B
C-B
CGB
C�B
DB
DB
D3B
D�B
D�B
D�B
EB
E�B
E�B
FYB
FYB
FtB
FtB
FtB
F�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
G�B
HB
HB
HfB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
K�B
LB
K�B
LB
L~B
L�B
L�B
L�B
L�B
L�B
L�B
M6B
M�B
M�B
M�B
M�B
N"B
N"B
N"B
N�B
O\B
O�B
P.B
PHB
P.B
P.B
P}B
PbB
P}B
PbB
P�B
P�B
RB
Q�B
Q�B
R B
RB
R B
R B
RoB
R�B
R�B
SB
S&B
S&B
S[B
SuB
S�B
T,B
TFB
TaB
TFB
TaB
T{B
T�B
T�B
T�B
UB
UB
UB
U2B
U�B
VSB
V9B
VB
VSB
W$B
W$B
W�B
W�B
WsB
W�B
W�B
W�B
X�B
X�B
Y1B
Y1B
YKB
Y�B
Y�B
ZB
Z�B
Z�B
[#B
[�B
[�B
\B
\]B
\]B
\�B
\�B
\�B
]�B
^B
^5B
^jB
^�B
^�B
^�B
^�B
^�B
_B
_VB
_VB
_�B
_�B
`B
`B
`BB
`\B
`�B
`�B
`�B
`�B
aB
a�B
a|B
a|B
abB
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b�B
c:B
cnB
c�B
d&B
d@B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
e�B
e�B
f2B
ffB
ffB
ffB
f�B
f�B
gB
g8B
gRB
g�B
g�B
h>B
h�B
h�B
h�B
h�B
i*B
i*B
i�B
i�B
i�B
jB
jKB
jeB
j�B
j�B
kB
kB
kB
kQB
k�B
k�B
k�B
l"B
l"B
l"B
l"B
lWB
lWB
l"B
lqB
l�B
l�B
l�B
mB
m)B
m]B
mwB
nIB
ncB
ncB
n}B
n�B
n�B
n�B
o B
oB
oB
o B
oOB
o�B
o�B
o�B
o�B
o�B
poB
pUB
pUB
p�B
p�B
p�B
p�B
q'B
qAB
q[B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
sMB
s�B
shB
s�B
tB
t9B
t�B
t�B
t�B
uB
uB
u%B
uZB
uZB
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
wB
wB
v�B
w2B
wLB
wLB
wfB
w�B
xB
x8B
xRB
x�B
y>B
y�B
z^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105244  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192432  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192432  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192432                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042439  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042439  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                