CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-30T00:36:59Z creation;2019-01-30T00:37:04Z conversion to V3.1;2019-12-19T07:22:14Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190130003659  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              FA   JA  I2_0576_326                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @أ��o��1   @أ���� @9٭B����dT��*01   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�ffA�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DI  DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D�� D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�@ D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�@ D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�3D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA��^A���A��DA�z�A�r�A�l�A�dZA�^5A�ZA�Q�A�M�A�M�A�I�A�E�A�A�A�;dA�;dA�;dA�7LA�7LA�7LA�7LA�5?A�33A�"�A��A�oA�VA�A���A���A��9A���A�z�A�K�A���A��`A���A��A�ffA��DA�K�A�ZA�z�A�"�A��uA�33A�%A��jA�=qA�|�A���A�\)A�ȴA� �A�/A�VA��!A�=qA�|�A�t�A�-A��FA��A���A�;dA�E�A��9A�v�A���A��;A�A��+A���A�p�A���A�p�A��PA�33A��A�"�A���A�|�A�K�A�A�^5A���A��+A�~�A���A���A�|�A�A���A�S�A�~�A�?}A��A�bNA��-A���A�=qA���A�  A7LA~�!A~(�A}`BA|~�A{��AzZAx�/Aw�AvAs��An �Ak�Aj�HAj�Ai��Ai��Ai|�AihsAh�AhA�Ag�hAgC�Ag%Af��Af�Ac7LA`��A_�A]�
A\�\AX��AU�mAT�+ATffAT$�ASAO�#AM�ALjAK
=AJJAI�AH�+AG��AF5?AD=qACS�ABz�AA�wA@(�A>�A>jA>E�A=�mA=�PA:��A8jA7�A6^5A6  A5%A41A3p�A3oA2��A2��A2�A2 �A1�A0��A/�-A.E�A+�A+�A+l�A+XA+
=A*  A)p�A)
=A(~�A(ffA'�A&1'A%A$�A$9XA"��A!��A �jA -A%A�A��A�uA(�A�;A�-A��Ap�AO�A��A�AI�A5?A�A�7A%A�9AZA$�AG�At�A{AS�A��A�9Ar�A  A��AbNA��A33A9XA�A	x�A�wA33A=qA�7A��AjA"�AbNA�A z�A J@���@�=q@���@�I�@��;@�dZ@��\@���@���@�@�@�Ĝ@�V@��@陚@���@��
@��@�P@��y@��@�h@�@ݺ^@�ȴ@���@ف@���@� �@�=q@У�@��@�33@�
=@ΰ!@�ff@͑h@̃@�;d@��@�;d@��@�x�@�/@�%@ģ�@ċD@�j@�bN@�Z@��
@�C�@�@�5?@��h@�@��@� �@��y@�=q@��#@�`B@��9@�\)@��@�v�@��h@��@��@�^5@�=q@�V@�ff@�~�@�n�@�=q@���@��@�bN@��@���@��P@�o@�ȴ@���@�@�X@�&�@�V@�Ĝ@�Z@�  @�\)@��H@�5?@��@� �@��@�b@��@��m@�dZ@���@�V@�{@��-@�?}@��u@��@�o@�$�@��^@�p�@�?}@�?}@��@��@���@�(�@�1@�ƨ@�1@�1@�  @��
@�ƨ@�;d@��@�x�@���@�Q�@��;@��R@��^@��@�j@�t�@�33@�o@�@��@��+@�^5@���@���@��/@��w@��@��w@��@��P@�S�@��y@��+@�5?@�$�@�{@��#@��h@��@���@�Z@�1'@��F@��@��@��y@�ff@��T@�p�@�`B@���@���@��D@�r�@�Q�@�1'@� �@� �@�b@���@��m@���@��@��P@�t�@�S�@�33@��y@���@��\@�~�@�$�@��@��@�@�x�@��@��`@��@�j@�1'@��@�ƨ@���@�|�@�K�@�;d@�33@���@��@��P@�t�@�C�@��@��@���@���@��\@�ff@��@�p�@��/@��@� �@�  @�1@�@
=@~ff@}��@}�h@}�@|��@|z�@{t�@z~�@y��@y�7@y�@y�@y�@xĜ@x�@x �@w�;@w��@wK�@w+@v��@v�y@v�R@vff@v@u`B@u�@t��@t�/@t�/@tj@tI�@t(�@st�@r��@r~�@rn�@rM�@q��@q%@pr�@o|�@n��@n�@nE�@n@m�T@m�h@mp�@l�/@l�D@l�D@lj@l1@kƨ@kt�@k33@j�@j�\@j�@i��@i�7@h��@hĜ@h��@h�u@hQ�@h1'@hb@g��@g�@g��@g�@f�+@f{@e�-@eO�@d�@d�D@d�D@d�D@dz�@dj@dI�@dI�@c�m@cdZ@co@b�H@b��@b��@b��@b^5@b-@a��@a�#@aX@`��@`r�@`1'@_��@_l�@_\)@_\)@_K�@^��@^{@]�T@]�-@]�@]V@\��@\�@\(�@[�F@[S�@[o@Z�!@Y�#@Yhs@X��@X�@W�;@W|�@V�+@VE�@V@U@UO�@U/@U�@UV@T�/@T�@Tz�@T9X@T1@S�m@Sƨ@S��@S"�@R�!@R^5@Q�7@Q%@P��@P�`@PA�@Ol�@NV@M�@MO�@L�j@LZ@K��@K��@KS�@K33@K33@Ko@J�@J�!@J�\@J�\@J^5@J�@I��@I�7@I&�@H1'@G�w@G�P@GK�@Fv�@F@E?}@DZ@C�
@C�@CdZ@Co@B�H@B��@BJ@A��@A��@AX@@��@@��@?��@>��@>5?@=�-@=�h@=`B@=V@<j@;�F@;�@;C�@;"�@:��@:n�@:-@:J@9��@9�#@9�7@97L@8��@8�u@7�;@7�@7��@7|�@7\)@7;d@7
=@6��@6ȴ@6��@5��@5�h@5�h@5�@5p�@5`B@5O�@4��@4��@4��@4�D@4Z@41@3�F@3dZ@2��@2��@2�\@2M�@1�#@1�7@1hs@17L@1&�@1&�@1&�@1�@0�9@/�@/��@/��@/;d@/
=@.��@.ȴ@.E�@-�h@,��@,j@,1@+��@+S�@+33@+o@*��@*�\@*~�@*�@)�^@)hs@)G�@)�@(��@(�`@(Ĝ@(�@(A�@'�;@'��@'K�@&��@&��@&�+@&E�@&{@&{@&{@&@%�T@%@%@%�h@%�@%?}@$�/@$�j@$Z@#ƨ@#t�@#o@#@#@"��@"~�@"=q@!�@!�#@!��@!�^@!�^@!��@!x�@ ��@ ��@ �@ �@ r�@ bN@ bN@ bN@ Q�@ Q�@ A�@ A�@ 1'@ b@�;@�P@\)@\)@K�@K�@K�@��@��@�+@V@{@�T@p�@��@��@z�@j@j@Z@Z@1@ƨ@��@��@t�@dZ@o@�!@~�@n�@�@�#@��@��@7L@�@b@��@�w@�@�@�+@V@{@�@�-@�@�@j@Z@9X@��@�
@ƨ@��@t�@33@�@��@�\@^5@��@��@��@�7@7L@��@��@��@�9@��@�@1'@b@�@��@l�@
=@V@E�@$�@@�T@��@p�@?}@��@�/@��@�@�D@Z@I�@9X@�@1@��@��@S�@C�@"�@
�@
��@
�\@
M�@	��@	��@	hs@	X@	G�@	G�@	G�@	&�@��@�`@�9@r�@Q�@�;@�@|�@;d@
=@��@��@�@ȴ@��@v�@V@5?@{@��@�-@�-@��@��@��@�h@p�@`B@O�@?}@/@�@V@��@�/@�j@�@j@9X@9X@(�@(�@�@1@1@��@�
@��@�@t�@dZ@S�@"�@�@��@��@n�@M�@�@J@��@�#@��@x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA��^A���A��DA�z�A�r�A�l�A�dZA�^5A�ZA�Q�A�M�A�M�A�I�A�E�A�A�A�;dA�;dA�;dA�7LA�7LA�7LA�7LA�5?A�33A�"�A��A�oA�VA�A���A���A��9A���A�z�A�K�A���A��`A���A��A�ffA��DA�K�A�ZA�z�A�"�A��uA�33A�%A��jA�=qA�|�A���A�\)A�ȴA� �A�/A�VA��!A�=qA�|�A�t�A�-A��FA��A���A�;dA�E�A��9A�v�A���A��;A�A��+A���A�p�A���A�p�A��PA�33A��A�"�A���A�|�A�K�A�A�^5A���A��+A�~�A���A���A�|�A�A���A�S�A�~�A�?}A��A�bNA��-A���A�=qA���A�  A7LA~�!A~(�A}`BA|~�A{��AzZAx�/Aw�AvAs��An �Ak�Aj�HAj�Ai��Ai��Ai|�AihsAh�AhA�Ag�hAgC�Ag%Af��Af�Ac7LA`��A_�A]�
A\�\AX��AU�mAT�+ATffAT$�ASAO�#AM�ALjAK
=AJJAI�AH�+AG��AF5?AD=qACS�ABz�AA�wA@(�A>�A>jA>E�A=�mA=�PA:��A8jA7�A6^5A6  A5%A41A3p�A3oA2��A2��A2�A2 �A1�A0��A/�-A.E�A+�A+�A+l�A+XA+
=A*  A)p�A)
=A(~�A(ffA'�A&1'A%A$�A$9XA"��A!��A �jA -A%A�A��A�uA(�A�;A�-A��Ap�AO�A��A�AI�A5?A�A�7A%A�9AZA$�AG�At�A{AS�A��A�9Ar�A  A��AbNA��A33A9XA�A	x�A�wA33A=qA�7A��AjA"�AbNA�A z�A J@���@�=q@���@�I�@��;@�dZ@��\@���@���@�@�@�Ĝ@�V@��@陚@���@��
@��@�P@��y@��@�h@�@ݺ^@�ȴ@���@ف@���@� �@�=q@У�@��@�33@�
=@ΰ!@�ff@͑h@̃@�;d@��@�;d@��@�x�@�/@�%@ģ�@ċD@�j@�bN@�Z@��
@�C�@�@�5?@��h@�@��@� �@��y@�=q@��#@�`B@��9@�\)@��@�v�@��h@��@��@�^5@�=q@�V@�ff@�~�@�n�@�=q@���@��@�bN@��@���@��P@�o@�ȴ@���@�@�X@�&�@�V@�Ĝ@�Z@�  @�\)@��H@�5?@��@� �@��@�b@��@��m@�dZ@���@�V@�{@��-@�?}@��u@��@�o@�$�@��^@�p�@�?}@�?}@��@��@���@�(�@�1@�ƨ@�1@�1@�  @��
@�ƨ@�;d@��@�x�@���@�Q�@��;@��R@��^@��@�j@�t�@�33@�o@�@��@��+@�^5@���@���@��/@��w@��@��w@��@��P@�S�@��y@��+@�5?@�$�@�{@��#@��h@��@���@�Z@�1'@��F@��@��@��y@�ff@��T@�p�@�`B@���@���@��D@�r�@�Q�@�1'@� �@� �@�b@���@��m@���@��@��P@�t�@�S�@�33@��y@���@��\@�~�@�$�@��@��@�@�x�@��@��`@��@�j@�1'@��@�ƨ@���@�|�@�K�@�;d@�33@���@��@��P@�t�@�C�@��@��@���@���@��\@�ff@��@�p�@��/@��@� �@�  @�1@�@
=@~ff@}��@}�h@}�@|��@|z�@{t�@z~�@y��@y�7@y�@y�@y�@xĜ@x�@x �@w�;@w��@wK�@w+@v��@v�y@v�R@vff@v@u`B@u�@t��@t�/@t�/@tj@tI�@t(�@st�@r��@r~�@rn�@rM�@q��@q%@pr�@o|�@n��@n�@nE�@n@m�T@m�h@mp�@l�/@l�D@l�D@lj@l1@kƨ@kt�@k33@j�@j�\@j�@i��@i�7@h��@hĜ@h��@h�u@hQ�@h1'@hb@g��@g�@g��@g�@f�+@f{@e�-@eO�@d�@d�D@d�D@d�D@dz�@dj@dI�@dI�@c�m@cdZ@co@b�H@b��@b��@b��@b^5@b-@a��@a�#@aX@`��@`r�@`1'@_��@_l�@_\)@_\)@_K�@^��@^{@]�T@]�-@]�@]V@\��@\�@\(�@[�F@[S�@[o@Z�!@Y�#@Yhs@X��@X�@W�;@W|�@V�+@VE�@V@U@UO�@U/@U�@UV@T�/@T�@Tz�@T9X@T1@S�m@Sƨ@S��@S"�@R�!@R^5@Q�7@Q%@P��@P�`@PA�@Ol�@NV@M�@MO�@L�j@LZ@K��@K��@KS�@K33@K33@Ko@J�@J�!@J�\@J�\@J^5@J�@I��@I�7@I&�@H1'@G�w@G�P@GK�@Fv�@F@E?}@DZ@C�
@C�@CdZ@Co@B�H@B��@BJ@A��@A��@AX@@��@@��@?��@>��@>5?@=�-@=�h@=`B@=V@<j@;�F@;�@;C�@;"�@:��@:n�@:-@:J@9��@9�#@9�7@97L@8��@8�u@7�;@7�@7��@7|�@7\)@7;d@7
=@6��@6ȴ@6��@5��@5�h@5�h@5�@5p�@5`B@5O�@4��@4��@4��@4�D@4Z@41@3�F@3dZ@2��@2��@2�\@2M�@1�#@1�7@1hs@17L@1&�@1&�@1&�@1�@0�9@/�@/��@/��@/;d@/
=@.��@.ȴ@.E�@-�h@,��@,j@,1@+��@+S�@+33@+o@*��@*�\@*~�@*�@)�^@)hs@)G�@)�@(��@(�`@(Ĝ@(�@(A�@'�;@'��@'K�@&��@&��@&�+@&E�@&{@&{@&{@&@%�T@%@%@%�h@%�@%?}@$�/@$�j@$Z@#ƨ@#t�@#o@#@#@"��@"~�@"=q@!�@!�#@!��@!�^@!�^@!��@!x�@ ��@ ��@ �@ �@ r�@ bN@ bN@ bN@ Q�@ Q�@ A�@ A�@ 1'@ b@�;@�P@\)@\)@K�@K�@K�@��@��@�+@V@{@�T@p�@��@��@z�@j@j@Z@Z@1@ƨ@��@��@t�@dZ@o@�!@~�@n�@�@�#@��@��@7L@�@b@��@�w@�@�@�+@V@{@�@�-@�@�@j@Z@9X@��@�
@ƨ@��@t�@33@�@��@�\@^5@��@��@��@�7@7L@��@��@��@�9@��@�@1'@b@�@��@l�@
=@V@E�@$�@@�T@��@p�@?}@��@�/@��@�@�D@Z@I�@9X@�@1@��@��@S�@C�@"�@
�@
��@
�\@
M�@	��@	��@	hs@	X@	G�@	G�@	G�@	&�@��@�`@�9@r�@Q�@�;@�@|�@;d@
=@��@��@�@ȴ@��@v�@V@5?@{@��@�-@�-@��@��@��@�h@p�@`B@O�@?}@/@�@V@��@�/@�j@�@j@9X@9X@(�@(�@�@1@1@��@�
@��@�@t�@dZ@S�@"�@�@��@��@n�@M�@�@J@��@�#@��@x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bp�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Bs�Br�Br�Br�Bu�By�B}�B� B�B�B�B�B�B�+B�+B�+B�1B�1B�7B�=B�=B�=B�DB�DB�DB�DB�=B�7B�=B�JB�JB�DB�=B�7B�PB�PB�VB�7B�B{�B��B��B�uB~�B�JBp�BgmBW
BM�Bo�B^5BJ�BhsB^5BS�BI�B�B�#BVB�B�B&�B�BB{B1B��B�B��BÖB��B��B�wB��B��B��B��B��B�oBn�BffBXBM�BI�BP�B2-B�B1'B-B#�B�B
��B
�B
�TB
��B
�;B
�TB
��B
�wB
ȴB
��B
��B
�bB
q�B
>wB
5?B
dZB
gmB
dZB
`BB
T�B
L�B
?}B
33B
%�B
�B
+B	�B	�B	�FB	�fB	�HB	�fB	�mB	�fB	�NB	�B	��B	��B	��B	��B	��B	��B	}�B	n�B	v�B	r�B	^5B	6FB	,B	D�B	^5B	P�B	6FB	1B		7B	�B	PB	�B	�B	DB	%B��B�ZB�B�B�ZB��B��B�;B�ZB�
BĜB��B�DB��B�!B�9B��B��B�B�B�9B�-B��B��B�JB�bB�Bs�BgmB�PB��B�oB�=Bz�B~�B�B}�B�Bu�B[#Bq�Bm�BbNB^5BQ�B[#B]/BO�B<jBffBgmBbNBbNBdZBcTB]/BT�B?}B?}BS�BYBR�BM�BG�BK�BE�B@�B.B�B&�B49B@�B>wB<jB33B%�B.B+B$�B�B�B��BB"�B�B�B�B�B\B�B�BuB�B�B�B%BJB)�B'�B�B�B�B�B�B�B��B
=B�B�B�BbBbB$�B$�B�B�B+B	7B"�B"�BJBJB�B �B.B0!B7LB33B1'B(�B"�B!�B�B&�B5?BB�BG�BH�BF�BJ�BJ�BJ�BH�BB�B@�B>wB=qB33B�B�B.BG�BS�BW
BVBT�BN�B[#B]/BVBW
BR�BdZBm�Bq�Bq�Bp�Bn�BjBhsBhsBl�Bu�B{�Bz�By�B}�B�B|�B|�B�B�B�B� B�B~�B�B�B|�B�DB�{B��B�{B�hB�VB�JB�uB��B�{B��B��B�uB��B��B��B�B�!B�9B�-B�3B�'B�B�XB�}BȴBƨBĜBÖBĜB�wB�XBƨBǮBŢBǮBÖB��B�B�;B�NB�B�B�B�B�B�B�B�B�B�B	B	B	B	B	B	B	1B	
=B	\B	\B	VB	PB	VB	PB	uB	�B	�B	�B	�B	&�B	!�B	�B	"�B	&�B	&�B	)�B	,B	.B	/B	0!B	2-B	33B	2-B	33B	49B	49B	5?B	6FB	8RB	8RB	:^B	9XB	;dB	>wB	?}B	=qB	B�B	H�B	G�B	I�B	K�B	O�B	Q�B	S�B	VB	ZB	]/B	_;B	`BB	cTB	gmB	iyB	m�B	m�B	l�B	l�B	l�B	m�B	m�B	q�B	p�B	n�B	n�B	l�B	iyB	l�B	r�B	r�B	u�B	w�B	u�B	r�B	s�B	t�B	x�B	y�B	}�B	{�B	{�B	� B	�+B	�7B	�DB	�\B	�VB	�PB	�PB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�oB	�hB	�oB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�3B	�-B	�'B	�-B	�9B	�9B	�9B	�?B	�FB	�RB	�LB	�RB	�qB	�wB	�}B	�}B	��B	B	ĜB	ǮB	ǮB	ŢB	ȴB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�)B	�/B	�5B	�/B	�/B	�5B	�BB	�BB	�5B	�;B	�HB	�TB	�TB	�ZB	�fB	�fB	�`B	�TB	�ZB	�sB	�mB	�sB	�mB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B
  B
B
B
B
1B
1B
	7B

=B
JB
PB
JB
JB
DB
PB
PB
JB
DB
JB
DB

=B
	7B
PB
\B
VB
JB
VB
VB
VB
uB
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
 �B
 �B
"�B
$�B
$�B
#�B
"�B
#�B
"�B
$�B
#�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
&�B
)�B
-B
,B
,B
,B
+B
+B
+B
,B
-B
,B
+B
,B
,B
+B
/B
0!B
/B
.B
0!B
1'B
2-B
2-B
33B
2-B
1'B
/B
-B
2-B
2-B
2-B
33B
33B
2-B
0!B
/B
1'B
49B
5?B
6FB
8RB
9XB
9XB
8RB
9XB
:^B
9XB
9XB
;dB
<jB
<jB
<jB
=qB
<jB
;dB
<jB
;dB
<jB
=qB
=qB
>wB
@�B
?}B
A�B
B�B
B�B
B�B
A�B
B�B
B�B
A�B
A�B
@�B
?}B
A�B
@�B
?}B
B�B
C�B
E�B
E�B
D�B
C�B
E�B
E�B
G�B
G�B
G�B
G�B
F�B
E�B
E�B
F�B
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
I�B
I�B
H�B
H�B
J�B
K�B
K�B
K�B
J�B
I�B
H�B
K�B
J�B
J�B
J�B
I�B
J�B
L�B
N�B
O�B
O�B
O�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
N�B
N�B
P�B
Q�B
O�B
P�B
Q�B
Q�B
N�B
N�B
P�B
S�B
T�B
S�B
Q�B
Q�B
VB
VB
VB
T�B
S�B
VB
YB
ZB
ZB
YB
ZB
[#B
[#B
ZB
YB
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
`BB
`BB
_;B
_;B
_;B
^5B
_;B
_;B
^5B
_;B
^5B
]/B
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
e`B
dZB
dZB
e`B
e`B
ffB
e`B
e`B
e`B
dZB
dZB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
iyB
jB
jB
jB
iyB
iyB
jB
iyB
iyB
iyB
hsB
jB
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
o�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
q�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bp�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Bs�Br�Br�Br�BvBzB~B�B�'B�-B�MB�B�9B�+B�+B�EB�KB�1B�7B�XB�=B�=B�DB�^B�DB�DB�XB�RB�XB�dB�dB�^B�rB��B��B��B��B�	B�YB}qB��B�)B�FB��B�6BsBi�BZBP�Bp�B`�BM�Bi�B_�BU�BK^B �B�B.BWB=B($BYBgB�B	�B�rB�UB�B�?B��B�B�cB��B�6B�[B��B�FB�Bq�Bh�B[=BP�BK�BR�B4�B�B2B./B%B�B
�B
��B
��B
ՁB
�BB
��B
�mB
�OB
�RB
�oB
��B
��B
t�B
DB
9$B
ezB
hsB
eB
aB
VB
NB
@�B
4�B
'�B
 �B
	�B	��B	��B	�XB	�RB	�4B	��B	�B	�B	�B	��B	ԕB	ϫB	�NB	�6B	�'B	��B	��B	q�B	x�B	tnB	`vB	:xB	/�B	F%B	^jB	Q�B	8RB	JB	�B	KB	B	�B	WB	�B	�B��B��B��B��B�B�BՁB߾B�B��B��B�]B�VB��B�B��B�sB�B��B��B�TB�GB��B��B��B�NB��Bu�BjKB��B��B��B��B|PB�B��B~�B�oBv�B]~Br�Bn�Bc�B_�BS�B\]B^OBQ�B>�Bf�Bg�Bb�Bb�Bd�Bc�B]�BU�BA�B@�BT{BYeBSuBNpBH�BLJBF?BA B/�BB(�B5?BA B?B<�B49B'�B/ B,B%�B�B�B��B9B#nB�B�B�B vBB�B�B�B BBxB�BfB�B*0B(sB �B�B�B vB \B�B�B�BkBjByBB�B%`B%`B�BB	RBB#nB#�B<B�B�B!�B.�B0�B7�B3�B1�B)�B#�B#BIB(XB6BB�BG�BIBGBJ�BKBJ�BIBC-BAB?B>B4TBB �B0BH�BT{BW�BV�BU�BO�B[�B]�BV�BW�BT,Bd�Bm�Bq�Bq�Bp�Bn�Bj�Bh�Bi*Bm)BvB|B{JBz^B~BB�[B}qB}qB�aB�MB�oB��B��B�B��B��B~B��B��B��B��B��B��B��B��B��B��B��B�9B�{B�tB��B�XB�]B�oB�nB�aB�MB�[B��B�rB��BȀB��B��B��B��B�.B�*B�B�B�YB�KBĜBϑBخBߤB��B��B��B��B��B��B�B�B�B�OB�iB	B	B	MB	aB	aB	�B	�B	
rB	�B	vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	'B	"4B	 'B	# B	'B	'8B	*KB	,WB	.IB	/5B	0;B	2GB	3MB	2aB	3MB	4TB	4TB	5tB	6zB	8�B	8�B	:xB	9�B	;�B	>�B	?�B	=�B	B�B	H�B	G�B	J	B	LB	P.B	R:B	TFB	VSB	ZkB	]dB	_pB	`vB	c�B	gmB	i�B	mwB	m�B	l�B	l�B	l�B	m�B	m�B	q�B	p�B	n�B	n�B	l�B	i�B	l�B	r�B	sB	u�B	xB	u�B	sB	tB	t�B	x�B	zB	~(B	|PB	|jB	�iB	�zB	��B	��B	�\B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�4B	�:B	�>B	�B	�DB	�=B	�CB	�"B	�)B	�]B	�5B	�MB	�GB	�vB	�aB	�TB	�nB	��B	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	ªB	ĶB	��B	��B	��B	�B	��B	�B	�B	�:B	�$B	�+B	�B	�EB	�+B	�+B	�EB	�SB	�YB	�QB	�CB	�dB	�OB	�IB	�dB	�jB	�\B	�vB	ބB	�pB	�B	�nB	�B	�B	�fB	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�BB	�.B	�BB
 4B
3B
GB
 iB	�}B
 iB
SB
mB
SB
fB
�B
	lB

rB
~B
jB
~B
~B
^B
PB
PB
dB
xB
~B
xB

�B
	�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
 �B
 �B
!�B
 �B
!B
"�B
%B
%B
#�B
# B
#�B
# B
$�B
$&B
(
B
)*B
)B
)B
)*B
)*B
)*B
(
B
(
B
'8B
*B
-B
,"B
,"B
,B
+6B
+B
+6B
,"B
-CB
,=B
+QB
,=B
,"B
+6B
/5B
0!B
/OB
.cB
0;B
1AB
2GB
2-B
33B
2-B
1AB
/OB
-]B
2-B
2GB
2GB
3MB
3MB
2aB
0UB
/iB
1�B
4nB
5�B
6zB
8lB
9rB
9rB
8�B
9rB
:�B
9�B
9�B
;B
<�B
<�B
<�B
=�B
<�B
;�B
<�B
;�B
<�B
=�B
=�B
>�B
@�B
?�B
A�B
B�B
B�B
B�B
A�B
B�B
B�B
A�B
A�B
@�B
?�B
A�B
@�B
?�B
B�B
C�B
E�B
E�B
D�B
C�B
E�B
E�B
G�B
G�B
G�B
G�B
F�B
E�B
E�B
F�B
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
I�B
I�B
H�B
H�B
J�B
K�B
K�B
K�B
J�B
I�B
H�B
K�B
J�B
J�B
J�B
I�B
J�B
L�B
N�B
O�B
O�B
O�B
N�B
M�B
N�B
O�B
O�B
PB
O�B
OB
OB
QB
R B
PB
QB
RB
RB
OB
OBB
Q4B
TB
U2B
TB
R B
R:B
VB
VB
V9B
UB
T,B
V9B
Y1B
Z7B
Z7B
YKB
Z7B
[#B
[=B
Z7B
YKB
Z7B
[WB
[WB
[WB
[WB
\]B
]IB
]dB
]dB
]IB
`\B
`\B
_pB
_VB
_pB
^OB
_VB
_pB
^jB
_VB
^jB
]~B
bNB
bhB
bhB
b�B
b�B
b�B
cnB
cnB
d�B
e`B
d�B
dtB
ezB
e`B
f�B
ezB
ezB
ezB
d�B
dtB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
h�B
iyB
j�B
jB
jB
i�B
i�B
jB
i�B
i�B
i�B
h�B
j�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
o�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
q�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902030036162019020300361620190203003616201902030200172019020302001720190203020017201902040020452019020400204520190204002045  JA  ARFMdecpA19c                                                                20190130093658  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190130003659  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190130003703  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190130003703  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190130003704  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190130003704  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190130003704  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190130003704  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190130003704  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190130003704                      G�O�G�O�G�O�                JA  ARUP                                                                        20190130005849                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190130153510  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190202153616  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190202153616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190202170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190203152045  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                