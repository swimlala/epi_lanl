CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-24T00:42:05Z creation;2018-07-24T00:42:09Z conversion to V3.1;2019-12-19T07:37:10Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180724004205  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_261                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�r��g� 1   @�r���-�@9������dR�!-w1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)��C+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:s3D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D�  D�@ D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D�  D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AˬAˡ�AˋDA�x�A�v�A�r�A�n�A�l�A�`BA�Q�A�O�A�9XA�JA�ĜA�S�A�~�A���A�A�ZA��A��FA�~�A�n�A�ffA�C�A�
=A�A�S�A���A�33A�XA��A�{A��A�A�r�A�7LA��mA���A�\)A�hsA�A��A�VA���A�E�A�VA�5?A�1'A��A�|�A� �A�=qA�A�/A���A�n�A��A��/A�Q�A���A��+A�;dA��-A��hA���A�\)A�/A��!A�O�A��wA� �A���A��/A���A��A�E�A�C�A�1A��A��7A�A�A���A�hsA�K�A��9A�+A��9A���A��
A�-A���A��A��\A��A�(�A��HA��+A��A�p�A��A��A�^5A��#AoA{�mAz�!Ay"�Aw��AwK�Au�wAt��AtAs7LAr^5AqAqhsAp�Ao�-Am��Am�hAm
=Aj1AhQ�Ag"�Ad�Ac�Ab�uAbjAax�A_��A^$�A\�!A[\)AZVAXM�AV�DATQ�AS
=AQ��AP�DAP  AOx�ANJAL��AJ�AH1AG�AF~�AF�AE�AC;dAAAA�A@��A@E�A?��A?"�A>A<E�A:��A9��A8��A7A6��A6VA6JA5�wA3�;A3oA2��A2��A1��A1VA0jA/?}A.{A-K�A,�jA,I�A+�mA+��A+|�A*r�A*  A)��A)|�A(�/A'S�A&�DA%A#�;A"�A"jA"M�A!\)A ȴA�#A�A��A��A�/A�A?}An�A�hA�DAjAI�A�jA1'A�A�A��A�/Av�A�A��A1'AG�A�/AI�A��A�PA�A��A
E�A��AK�A��A�RA$�AG�AffA-A�mA��A�
A"�@�+@�@��@�X@�?}@�&�@���@�S�@���@�9X@�S�@�{@���@� �@��@��@�hs@�(�@��@�R@�^@��m@柾@�^@�j@�
=@◍@�{@��#@� �@��@�;d@ڰ!@�p�@�K�@֗�@�@�{@�1'@ҏ\@���@ϥ�@Ώ\@�5?@͡�@��@� �@ˍP@ˍP@�dZ@ʸR@��@�p�@�  @�
=@���@�|�@�ȴ@���@���@�A�@�  @���@��+@�E�@�E�@�-@�/@�ȴ@��u@�\)@��R@���@��
@��y@�E�@���@�9X@��@��@��`@�I�@�K�@�$�@��@�9X@�b@���@�@��^@���@��7@�G�@�&�@��9@�1'@��;@��
@���@�o@�M�@���@�x�@�?}@���@��9@�Z@�1'@���@��@�K�@��R@�@���@�p�@�Ĝ@��F@�ȴ@���@�v�@��@�Ĝ@��D@�bN@�b@�ƨ@�|�@�"�@��!@�@�`B@��D@�A�@� �@��m@���@���@��@��@�^5@�5?@���@��@��@�b@�ƨ@��y@��+@�ff@���@���@�9X@��@��w@�S�@�
=@��!@��#@�%@�Ĝ@�z�@�I�@�Q�@��@��@�\)@�33@��@��@��R@�ff@��T@���@�x�@�O�@�O�@�%@��9@��D@�z�@�bN@�9X@�b@�dZ@�V@�J@��-@�x�@�p�@�X@��`@��@��@�bN@�1'@�  @�;@K�@
=@~ȴ@~E�@}�T@}��@}/@|I�@{�m@{�
@{��@{��@{dZ@{o@z�@zn�@zJ@y��@y��@yx�@yhs@yX@y&�@x�9@xb@w�@w|�@wK�@v��@v�+@v5?@v5?@u�T@u�@t�/@s�
@s33@r�@r�!@rM�@r�@q��@q�#@q��@q�7@q�7@qx�@qG�@p�`@pĜ@pQ�@o�@o�P@o�@o
=@n�@n$�@mO�@l��@l��@lj@l�@k��@kdZ@j�H@j��@j��@j^5@jM�@j-@jJ@i��@i7L@h��@hA�@g�;@g;d@f�y@f�R@f�+@f5?@e��@e�@ep�@d�@d��@dz�@dI�@c�m@cdZ@co@b��@bM�@a��@a�^@a��@a%@`��@`�@`bN@`A�@` �@_�@_��@_�@_��@_��@_�P@_l�@^�+@]��@]@]�@\�D@[�m@["�@Z��@Z�@Y��@Y��@Y�7@YX@Y&�@XĜ@X�u@XQ�@X1'@X  @W�@W�@W�;@W;d@V�y@V�@Vȴ@V��@V��@V�+@V�+@Vv�@VE�@U�-@U?}@Tz�@S�F@R��@Rn�@R-@R�@RJ@Q�^@Q��@Qx�@Qhs@QG�@Q%@P��@P�@O�@O�w@O��@O|�@Ol�@O+@Nff@M�h@M?}@M�@MV@MV@MV@L��@Lj@K�F@KS�@K"�@J�@J��@JJ@IX@H��@H�9@H�@G�;@G�w@G�P@G\)@G+@G
=@F�y@F�@F�R@F�+@F5?@E��@E��@E`B@E?}@E�@D��@D��@D�@DZ@C�
@C��@C@B^5@A��@AX@@�`@@�9@@r�@?|�@?�@?
=@>ȴ@>v�@>V@>E�@=��@=?}@<��@<(�@<�@<�@<�@<�@<�@<1@;�F@;33@:�@:��@:n�@9hs@8��@8�u@8bN@8  @7�@7�P@6��@6E�@65?@6$�@5�-@5p�@5?}@4�@41@3�m@3��@3C�@333@3o@2�@2�!@2~�@2J@1��@1G�@1%@0��@0bN@01'@0b@/�;@/��@/|�@/;d@/
=@.�@.��@.5?@-�T@-�-@-�@-p�@-�@,�/@,�@,j@,I�@,�@+�m@+�F@+��@+�@+S�@+"�@*�@*�@*��@*�!@*�\@*^5@*=q@*J@)��@(�`@(Q�@(  @'��@'�P@'|�@'K�@'
=@&�R@&�+@&$�@%�T@%@%�-@%��@%�@%�@%`B@%/@%�@%�@%�@%V@%V@$��@$��@$��@$�D@$z�@$I�@$�@#�F@#o@"��@"n�@!�@!��@!G�@!�@!%@!%@ �9@ r�@ Q�@ Q�@ A�@ b@�@�@�@+@�@�h@p�@?}@V@��@�D@�D@(�@�m@�
@��@S�@33@o@�!@^5@-@-@�@��@x�@X@G�@�@%@�`@��@Ĝ@�9@��@bN@b@�@�w@��@;d@ȴ@��@�+@V@$�@�@��@��@@@@p�@`B@?}@?}@V@�@j@9X@1@�F@S�@@�!@��@��@�\@=q@�@�@�7@%@Ĝ@�@r�@bN@A�@1'@1'@ �@�;@;d@
=@ȴ@�R@��@V@$�@{@@��@�@p�@?}@�@��@�@�/@��@�j@�@��@��@j@(�@1@�m@��@dZ@C�@o@
�@
�H@
�H@
��@
��@
M�@	��@	x�@	&�@	�@��@Ĝ@r�@1'@�@��@K�@
=@�@ff@{@@@��@�h@�@`B@?}@V@��@�@�/@��@��@�j@�@�@��@�D@I�@9X@(�@�@1@1@��@��@�@dZ@dZ@S�@"�@"�@"�@@�H@�H@��@~�@=q@�@�@��@��@�7@x�@hs@7L@7L@&�@ ��@ Ĝ@ �u@ bN@ Q�@ 1'@  �@ b?��;?��w?���?���?�\)?�;d?��?��?��R?�v�?�5??���?��-?��-?�p�?�/?�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AˬAˡ�AˋDA�x�A�v�A�r�A�n�A�l�A�`BA�Q�A�O�A�9XA�JA�ĜA�S�A�~�A���A�A�ZA��A��FA�~�A�n�A�ffA�C�A�
=A�A�S�A���A�33A�XA��A�{A��A�A�r�A�7LA��mA���A�\)A�hsA�A��A�VA���A�E�A�VA�5?A�1'A��A�|�A� �A�=qA�A�/A���A�n�A��A��/A�Q�A���A��+A�;dA��-A��hA���A�\)A�/A��!A�O�A��wA� �A���A��/A���A��A�E�A�C�A�1A��A��7A�A�A���A�hsA�K�A��9A�+A��9A���A��
A�-A���A��A��\A��A�(�A��HA��+A��A�p�A��A��A�^5A��#AoA{�mAz�!Ay"�Aw��AwK�Au�wAt��AtAs7LAr^5AqAqhsAp�Ao�-Am��Am�hAm
=Aj1AhQ�Ag"�Ad�Ac�Ab�uAbjAax�A_��A^$�A\�!A[\)AZVAXM�AV�DATQ�AS
=AQ��AP�DAP  AOx�ANJAL��AJ�AH1AG�AF~�AF�AE�AC;dAAAA�A@��A@E�A?��A?"�A>A<E�A:��A9��A8��A7A6��A6VA6JA5�wA3�;A3oA2��A2��A1��A1VA0jA/?}A.{A-K�A,�jA,I�A+�mA+��A+|�A*r�A*  A)��A)|�A(�/A'S�A&�DA%A#�;A"�A"jA"M�A!\)A ȴA�#A�A��A��A�/A�A?}An�A�hA�DAjAI�A�jA1'A�A�A��A�/Av�A�A��A1'AG�A�/AI�A��A�PA�A��A
E�A��AK�A��A�RA$�AG�AffA-A�mA��A�
A"�@�+@�@��@�X@�?}@�&�@���@�S�@���@�9X@�S�@�{@���@� �@��@��@�hs@�(�@��@�R@�^@��m@柾@�^@�j@�
=@◍@�{@��#@� �@��@�;d@ڰ!@�p�@�K�@֗�@�@�{@�1'@ҏ\@���@ϥ�@Ώ\@�5?@͡�@��@� �@ˍP@ˍP@�dZ@ʸR@��@�p�@�  @�
=@���@�|�@�ȴ@���@���@�A�@�  @���@��+@�E�@�E�@�-@�/@�ȴ@��u@�\)@��R@���@��
@��y@�E�@���@�9X@��@��@��`@�I�@�K�@�$�@��@�9X@�b@���@�@��^@���@��7@�G�@�&�@��9@�1'@��;@��
@���@�o@�M�@���@�x�@�?}@���@��9@�Z@�1'@���@��@�K�@��R@�@���@�p�@�Ĝ@��F@�ȴ@���@�v�@��@�Ĝ@��D@�bN@�b@�ƨ@�|�@�"�@��!@�@�`B@��D@�A�@� �@��m@���@���@��@��@�^5@�5?@���@��@��@�b@�ƨ@��y@��+@�ff@���@���@�9X@��@��w@�S�@�
=@��!@��#@�%@�Ĝ@�z�@�I�@�Q�@��@��@�\)@�33@��@��@��R@�ff@��T@���@�x�@�O�@�O�@�%@��9@��D@�z�@�bN@�9X@�b@�dZ@�V@�J@��-@�x�@�p�@�X@��`@��@��@�bN@�1'@�  @�;@K�@
=@~ȴ@~E�@}�T@}��@}/@|I�@{�m@{�
@{��@{��@{dZ@{o@z�@zn�@zJ@y��@y��@yx�@yhs@yX@y&�@x�9@xb@w�@w|�@wK�@v��@v�+@v5?@v5?@u�T@u�@t�/@s�
@s33@r�@r�!@rM�@r�@q��@q�#@q��@q�7@q�7@qx�@qG�@p�`@pĜ@pQ�@o�@o�P@o�@o
=@n�@n$�@mO�@l��@l��@lj@l�@k��@kdZ@j�H@j��@j��@j^5@jM�@j-@jJ@i��@i7L@h��@hA�@g�;@g;d@f�y@f�R@f�+@f5?@e��@e�@ep�@d�@d��@dz�@dI�@c�m@cdZ@co@b��@bM�@a��@a�^@a��@a%@`��@`�@`bN@`A�@` �@_�@_��@_�@_��@_��@_�P@_l�@^�+@]��@]@]�@\�D@[�m@["�@Z��@Z�@Y��@Y��@Y�7@YX@Y&�@XĜ@X�u@XQ�@X1'@X  @W�@W�@W�;@W;d@V�y@V�@Vȴ@V��@V��@V�+@V�+@Vv�@VE�@U�-@U?}@Tz�@S�F@R��@Rn�@R-@R�@RJ@Q�^@Q��@Qx�@Qhs@QG�@Q%@P��@P�@O�@O�w@O��@O|�@Ol�@O+@Nff@M�h@M?}@M�@MV@MV@MV@L��@Lj@K�F@KS�@K"�@J�@J��@JJ@IX@H��@H�9@H�@G�;@G�w@G�P@G\)@G+@G
=@F�y@F�@F�R@F�+@F5?@E��@E��@E`B@E?}@E�@D��@D��@D�@DZ@C�
@C��@C@B^5@A��@AX@@�`@@�9@@r�@?|�@?�@?
=@>ȴ@>v�@>V@>E�@=��@=?}@<��@<(�@<�@<�@<�@<�@<�@<1@;�F@;33@:�@:��@:n�@9hs@8��@8�u@8bN@8  @7�@7�P@6��@6E�@65?@6$�@5�-@5p�@5?}@4�@41@3�m@3��@3C�@333@3o@2�@2�!@2~�@2J@1��@1G�@1%@0��@0bN@01'@0b@/�;@/��@/|�@/;d@/
=@.�@.��@.5?@-�T@-�-@-�@-p�@-�@,�/@,�@,j@,I�@,�@+�m@+�F@+��@+�@+S�@+"�@*�@*�@*��@*�!@*�\@*^5@*=q@*J@)��@(�`@(Q�@(  @'��@'�P@'|�@'K�@'
=@&�R@&�+@&$�@%�T@%@%�-@%��@%�@%�@%`B@%/@%�@%�@%�@%V@%V@$��@$��@$��@$�D@$z�@$I�@$�@#�F@#o@"��@"n�@!�@!��@!G�@!�@!%@!%@ �9@ r�@ Q�@ Q�@ A�@ b@�@�@�@+@�@�h@p�@?}@V@��@�D@�D@(�@�m@�
@��@S�@33@o@�!@^5@-@-@�@��@x�@X@G�@�@%@�`@��@Ĝ@�9@��@bN@b@�@�w@��@;d@ȴ@��@�+@V@$�@�@��@��@@@@p�@`B@?}@?}@V@�@j@9X@1@�F@S�@@�!@��@��@�\@=q@�@�@�7@%@Ĝ@�@r�@bN@A�@1'@1'@ �@�;@;d@
=@ȴ@�R@��@V@$�@{@@��@�@p�@?}@�@��@�@�/@��@�j@�@��@��@j@(�@1@�m@��@dZ@C�@o@
�@
�H@
�H@
��@
��@
M�@	��@	x�@	&�@	�@��@Ĝ@r�@1'@�@��@K�@
=@�@ff@{@@@��@�h@�@`B@?}@V@��@�@�/@��@��@�j@�@�@��@�D@I�@9X@(�@�@1@1@��@��@�@dZ@dZ@S�@"�@"�@"�@@�H@�H@��@~�@=q@�@�@��@��@�7@x�@hs@7L@7L@&�@ ��@ Ĝ@ �u@ bN@ Q�@ 1'@  �@ b?��;?��w?���?���?�\)?�;d?��?��?��R?�v�?�5??���?��-?��-?�p�?�/?�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�?B�?B�FB�RB�RB�RB�RB�LB�FB�RB�FB�?B�?B�'B��B��B�+B�/B!�B'�BM�Bo�BjBW
BJ�BR�BiyBcTBhsB]/BT�BR�B[#B[#BjBp�Bm�BffB_;BF�B%�B&�BB8RB;dB8RB+B�B,B�BoB�B�mB�B�B�B�fB��B�
B�B��B��B�}B�!B�wB�HB�NB�B��B��BȴB��BɺB�}B��B��B�=Bv�Bn�B[#BffBP�BG�BP�BA�B5?B-B�B1B
��B
��B
�fB
�;B
�}B
�oB
�oB
��B
��B
��B
��B
�=B
�B
p�B
Q�B
0!B
@�B
49B
+B
.B
�B
�B
uB
PB
	7B
B
B	��B	��B	�B	��B	�B	��B	��B	��B	�!B	��B	�}B	�jB	�B	�{B	�1B	�B	w�B	t�B	]/B	T�B	D�B	F�B	B�B	<jB	@�B	6FB	%�B	�B		7B	B	\B	bB	\B	B��B�B��B��B��B�B�B�BB��BȴB��B��BȴBȴB��B��BǮB�9B�RB��B�qB�-B�'B�B��B��B��B��B��B��B�B��B��B��B��B��B�bB�B�B� Br�B{�B�B�%Bz�Bz�Bw�Bw�Bs�BffBgmBe`BgmBcTBbNB^5BjBffBT�B_;BgmBcTB^5BW
BXBT�BH�BJ�BI�BL�BG�B>wB,B7LB33B"�B�B:^B<jB;dB49B/B,B6FB2-B%�B�B"�B�B+B0!B49B49B2-B-B!�B�B$�B"�B�B�B�BVB
=B{B�B�B�B�BPBbBoBbBbB�B�BuB1B��B+BoBPB%B�B�B�BbBPBPB\B{B�B�B{B!�B%�B,B)�B&�B%�B$�B!�B#�B�B'�B.B/B0!B7LB8RB8RB49B;dB<jB8RB/B!�B'�B49B:^B6FB=qBB�BF�BE�B@�BB�BI�BI�BQ�BO�BQ�BXBZBbNB\)B_;Bm�Bm�Bm�Bl�Bm�Bl�Bl�Bo�Br�Bp�Bl�Bn�Bu�Bw�By�Bx�Bz�By�B|�Bz�B|�Bz�By�B{�B�B�B~�B� B�+B�bB�bB�PB�7B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�-B�B�9B�^B�^B�RB�XB�LBBBɺB��B��B��B��B�/B�;B�;B�HB�NB�TB�mB�B�B��B��B��B��B	B	B	B	B	B	%B	%B	DB	DB	VB	bB	VB	\B	uB	{B	{B	{B	uB	hB	uB	#�B	%�B	+B	/B	.B	-B	2-B	5?B	6FB	7LB	8RB	;dB	;dB	>wB	?}B	@�B	C�B	F�B	G�B	I�B	P�B	T�B	VB	W
B	W
B	XB	ZB	ZB	]/B	`BB	aHB	cTB	dZB	dZB	cTB	cTB	ffB	iyB	l�B	m�B	m�B	o�B	q�B	s�B	r�B	r�B	w�B	v�B	|�B	�B	�B	�%B	�7B	�=B	�=B	�DB	�JB	�VB	�VB	�VB	�VB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�LB	�LB	�dB	�jB	�qB	�wB	��B	B	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	�B	�/B	�)B	�B	�/B	�5B	�NB	�HB	�fB	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
  B
B
B
%B
%B
B
B
B
1B

=B

=B
	7B
PB
PB
PB
VB
VB
\B
\B
\B
VB
VB
\B
hB
hB
oB
oB
oB
oB
oB
hB
hB
oB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
!�B
!�B
 �B
�B
!�B
!�B
 �B
�B
"�B
$�B
%�B
$�B
%�B
&�B
$�B
'�B
)�B
)�B
(�B
(�B
)�B
(�B
(�B
,B
,B
,B
.B
-B
-B
-B
-B
,B
-B
-B
.B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
1'B
2-B
49B
49B
5?B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
9XB
9XB
9XB
9XB
8RB
8RB
7LB
6FB
7LB
9XB
;dB
;dB
<jB
<jB
;dB
<jB
=qB
<jB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
?}B
B�B
B�B
B�B
C�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
H�B
H�B
G�B
G�B
H�B
F�B
D�B
C�B
I�B
K�B
K�B
K�B
J�B
M�B
M�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
P�B
P�B
P�B
O�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
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
VB
W
B
W
B
W
B
W
B
VB
VB
W
B
XB
W
B
XB
YB
YB
[#B
[#B
[#B
ZB
ZB
\)B
ZB
ZB
\)B
]/B
^5B
_;B
_;B
_;B
_;B
^5B
]/B
\)B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
`BB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
ffB
ffB
e`B
e`B
e`B
ffB
hsB
hsB
hsB
gmB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
l�B
m�B
l�B
l�B
m�B
m�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
q�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
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
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
x�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�tB�tB�`B�RB�RB�lB�RB�fB�`B��B��B��B�+B��B��B�B�4B�B%�B,�BO�Bo�Bk6BY�BM�BUMBjeBd�Bi�B_pBW�BUgB]IB]~Bk�Bq'BnIBg8B`�BIRB)�B*0B1B9>B<PB9$B-B�B,�B �B�B�B��B�UB��B�[B��B�FB�EB�
B��B��B� B��B�iB��B�B�eB��B�B��B�6B�#B�OB��B��B��By�BqAB]�BgmBS�BH�BQNBB�B6�B.cB�B
=B �B
�2B
�>B
��B
��B
��B
�B
�vB
�8B
�4B
�yB
�^B
�B
r|B
U2B
3�B
BB
6+B
,�B
.�B
~B
�B
�B
pB

XB
�B
�B
  B	�fB	��B	�qB	�'B	уB	� B	͹B	�MB	��B	�4B	�B	��B	��B	�=B	��B	y�B	v�B	_�B	W?B	G_B	HfB	DMB	=�B	A;B	7LB	'�B	�B	~B	�B	�B	hB	.B	�B��B�[B��B��B�xB��B�B�B�2B��B�HB�(B�#B��B̳B�~BȚB��B�rB��B��B��B�B�UB��B�mB��B��B��B�B�kB�yB�B��B�HB�IB��B�B�GB�oBuB}"B��B��B|6B{�Bx�Bx�Bt�Bh
Bh�Bf�Bh�Bd�Bc�B_pBj�BgBW
B_�Bg�Bc�B^�BX+BX�BU�BJXBK�BJ�BM�BH�B?�B.�B8�B4�B%FB�B:�B=B<B5?B0UB-)B6�B2�B'RB!bB$&B�B+�B0�B4nB4nB2aB-�B# BB%`B#�B�BBjB.BB�BqBkB BB_BpB4B&BhB4B�B�B�B	�B��BfB�BVB�B�B$B�B�BpB�BHB2BB7B�B!�B&LB,=B*eB'�B&�B%�B"�B$�B \B(�B.�B/�B0�B7�B8�B8�B5B;�B<�B8�B0;B#�B)yB5%B;0B7�B>BBCaBGEBF?BA�BC{BJ�BJ�BR�BP�BR�BX�BZ�Bb�B]B`Bm�Bm�Bm�Bl�Bm�Bl�BmBpBr�Bp�Bm)BoOBv+Bx8Bz*By>B{0Bz*B}"B{JB}"B{JBzxB|�B�[B�oB�B��B��B��B��B��B�	B��B��B��B��B�B�B�	B�=B�IB�hB�6B�5B�OB�oB�MB�|B��B��B��B��B��B��B�8B��B�-B�#B�B˒B�bB�uB�~B�pBߤB�B��B��B�$B��B��B��B�	B�rB�B	;B	aB	MB	gB	�B	tB	�B	^B	�B	�B	}B	�B	�B	�B	�B	�B	�B	�B	 B	FB	#�B	&LB	+QB	/OB	.IB	-�B	2|B	5tB	6zB	7�B	8�B	;B	;�B	>�B	?�B	@�B	C�B	F�B	G�B	J#B	Q B	U2B	V9B	W?B	W$B	X+B	ZQB	ZkB	]dB	`\B	abB	cnB	d�B	dtB	c�B	c�B	f�B	i�B	l�B	m�B	m�B	o�B	q�B	s�B	r�B	r�B	xB	w2B	}<B	�-B	�9B	�YB	�RB	�rB	�rB	�^B	�~B	�pB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�0B	�B	�)B	�OB	�;B	�vB	�|B	�hB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�"B	��B	�.B	�B	�B	�B	�@B	�2B	�$B	�$B	�EB	�+B	�+B	�7B	�7B	�=B	�QB	�7B	�+B	�mB	�yB	�IB	�xB	ٚB	�~B	ޞB	�hB	�B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�	B	�B	�B	�B	��B	��B	��B	�*B	�$B	�B	�"B	�(B	�B	�6B	�JB	�VB
 B
-B
3B
B
B
AB
 iB
UB
3B
YB
YB
SB
gB
�B
KB

rB

XB
	�B
�B
�B
�B
pB
pB
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
!�B
"�B
"�B
"�B
!�B
!�B
 �B
�B
!�B
!�B
 �B
 'B
# B
$�B
&B
%B
%�B
'B
%FB
(
B
*B
*B
)DB
)*B
*B
)*B
)DB
,"B
,"B
,=B
./B
-)B
-)B
-CB
-)B
,=B
-CB
-]B
.IB
/5B
0;B
1[B
1[B
1[B
1[B
2aB
2GB
2aB
2aB
2aB
1[B
2GB
4TB
4TB
5ZB
4TB
5tB
5ZB
5ZB
6zB
6zB
7fB
7fB
8lB
8lB
8lB
9rB
9rB
:xB
9�B
9�B
9�B
9rB
8lB
8lB
7�B
6�B
7�B
9rB
;B
;�B
<�B
<�B
;�B
<�B
=�B
<�B
>�B
?�B
?�B
?}B
?�B
@�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
?�B
B�B
B�B
B�B
C�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
H�B
H�B
G�B
G�B
H�B
F�B
EB
DB
I�B
K�B
K�B
K�B
J�B
M�B
M�B
MB
MB
M�B
NB
M�B
OB
N�B
NB
N�B
Q B
P�B
Q B
PB
O�B
Q�B
RB
R B
SB
SB
R�B
SB
R�B
SB
RB
RB
SB
SB
SB
S@B
S@B
UB
UB
U2B
U2B
VB
W
B
W
B
W
B
W$B
W
B
VB
W$B
W$B
W
B
W$B
VB
V9B
W?B
XEB
WYB
XEB
YKB
Y1B
[#B
[=B
[WB
Z7B
ZQB
\CB
ZkB
ZkB
\CB
]dB
^5B
_;B
_pB
_;B
_VB
^jB
]dB
\xB
_pB
_pB
`BB
`vB
`vB
`vB
aHB
`\B
abB
bNB
bhB
bhB
bhB
cTB
cTB
cTB
cnB
dtB
dtB
dZB
dtB
c�B
c�B
dtB
dtB
d�B
e�B
e�B
f�B
f�B
ffB
g�B
f�B
f�B
ezB
e�B
e�B
f�B
h�B
h�B
h�B
g�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
l�B
m�B
l�B
l�B
m�B
m�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
q�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
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
v�B
v�B
w�B
w�B
xB
xB
w�B
x�B
w�B
x�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807230035142018072300351420180723003514201807230200172018072302001720180723020017201807240023212018072400232120180724002321  JA  ARFMdecpA19c                                                                20180724093532  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180724004205  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180724004208  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180724004208  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180724004209  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180724004209  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180724004209  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180724004209  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180724004209                      G�O�G�O�G�O�                JA  ARUP                                                                        20180724005603                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180719154447  CV  JULD            G�O�G�O�F×�                JM  ARCAJMQC2.0                                                                 20180722153514  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180722153514  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180722170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180723152321  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                