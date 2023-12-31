CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-09-08T09:02:04Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        P  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  o   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220908090204  20220908090204  4903175 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @���&�T1   @��8㘖@3M�����c҇+J1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�fg@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC<  C=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Dd� Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D�  D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A܁A܇+A܏\A܋DA܍PA܍PAܑhA܏\AܓuA�~�Aۣ�A��A�"�A��A�G�Aղ-A�ZA�I�A�+A�bAԅA���A�VA�ȴA���A̝�A�dZA�?}A���Aǡ�A�C�A�A�A�A���A�|�A�9XA�O�A��A���A��;A���A�7LA�VA��A��#A�v�A��uA��`A�=qA��!A�M�A���A��yA���A���A�r�A�$�A��PA�{A��A��RA�A��TA���A�C�A�C�A���A��A��wA���A���A�E�A���A�M�A�A��A�ffA��PA��!A�M�A�^5A�
=A�A�v�A�JA�|�A��A��`A}`BA{C�Az9XAz �Ay�Aw|�At�Ar�RAq\)Ap��Ao/Al5?Ah�/AgƨAe��Ab��A`(�A[��AX��AV1AUVAQ�
AO�AM��AI��AHr�AG��AF�\ADĜABA>jA<ffA;;dA9/A7�^A6��A5ƨA57LA3��A2�A1��A0A�A.jA,�9A+VA)K�A'?}A&�yA&JA%C�A$��A$VA"��A �A ��A�jA��AȴAG�Az�A�AQ�AjAVA|�A��AC�A%A�9A  A�TAA��A�7Ax�A33A�/A�DA(�A1A�RA�hAA��AAdZAC�A�A
�yA
�AVA5?A�PAQ�A`BA33AXAĜAffAdZA v�A 9X@�\)@�V@�^5@�V@�+@�r�@��P@�-@�bN@��@��@�7@���@��
@�"�@��H@@���@�@�@�@�1@�o@��@�&�@��`@�9@�r�@�@��T@��@��;@�dZ@�ȴ@��@ܼj@�K�@�{@�9X@�^5@��#@��#@���@���@թ�@Դ9@��
@�t�@��@җ�@�G�@��m@�l�@�=q@�&�@�1'@˾w@�33@�
=@�v�@�?}@�j@�9X@ǥ�@���@��@��`@ă@�t�@�=q@��@��@��#@���@��@�hs@��`@�b@��m@��@��@�S�@�"�@��H@���@�n�@�M�@�J@�5?@�ff@���@���@��+@�ff@�M�@�-@��h@��@�Z@�|�@�o@�@��R@�~�@�n�@�ff@�M�@�@�p�@���@�Ĝ@�j@�1@�l�@���@��!@�=q@���@��@���@���@���@�j@� �@��w@�t�@�S�@�;d@��@���@�n�@�$�@���@�`B@��@��@�Z@���@��w@���@�o@��T@���@���@�x�@��9@� �@���@��@��@��@��y@��y@��\@�E�@���@���@���@�ƨ@��F@��@�@���@���@�=q@��-@�X@�/@�V@��/@��@��@���@�K�@�+@�ȴ@�5?@��T@�x�@�G�@���@��`@��u@���@���@���@�\)@���@�v�@�V@�{@���@��-@���@�G�@�%@���@���@���@��j@��@�j@��@�b@�b@���@�ƨ@�;d@���@�v�@�ff@�M�@�E�@�-@���@��h@�x�@�O�@�?}@��@���@��u@��@��F@��@�C�@��y@�ȴ@��!@���@�$�@�@���@���@���@��@���@��D@� �@��m@���@�S�@�"�@�
=@���@��H@��@�ȴ@���@�ff@��T@��@�7L@�V@��/@�r�@�(�@�  @�ƨ@�|�@�l�@�\)@��R@�E�@�{@���@�`B@��@��/@���@��j@��u@��@��D@��D@��@�r�@�Q�@�|�@�33@�"�@���@��!@�=q@��^@�G�@��j@�Q�@�Q�@�Z@��@��P@�33@���@�ff@�$�@�J@���@��-@�&�@�%@��@��9@���@��u@��D@�bN@�A�@��@�ƨ@���@�|�@�;d@�o@�
=@��y@��@���@���@��+@�n�@�^5@�M�@�-@�@��@���@��^@�hs@�?}@���@��@�r�@�bN@�I�@� �@��@�1@�;@�w@��@K�@~�+@}/@|�/@|��@|z�@|I�@|9X@{��@{dZ@z�\@z-@y��@yhs@x��@xA�@x �@w�P@w
=@v�@v�R@v�R@v��@vv�@v5?@u�@u��@uO�@u/@t�@t9X@s�m@s��@sS�@r�!@r�\@r~�@r~�@rn�@rM�@q��@q�#@q��@q��@q�7@q7L@p��@p  @o+@n��@n�+@nff@nE�@m�-@l(�@k�F@ko@j�H@j��@j��@jn�@jn�@j=q@i��@i�^@i�@h�u@h�@h1'@g�;@g\)@f�y@f�@f�R@f��@f�+@f5?@f{@eV@d��@dj@d(�@c��@c��@cC�@b��@b=q@a�#@a�^@a�7@ahs@`�`@` �@_K�@]��@]��@]p�@]?}@\�@\�j@\j@[�
@[�@["�@Z�H@Z~�@Z=q@Y�@Y��@Y�#@Y�@Yhs@X�u@X1'@W�;@W+@V@U�@UV@T�@TI�@T�@T1@SdZ@S"�@R��@Q��@Q7L@P�9@P  @O�@O��@O�@O+@O
=@Nff@N5?@N{@M�@M��@M@M�-@M��@M�@MO�@L�@L�D@LI�@L�@K�
@KC�@J��@J�\@J~�@J=q@I�@I�@I�^@I&�@H��@HbN@H1'@H  @G�@G�P@Gl�@G\)@GK�@G+@G
=@F��@F�R@Fff@F{@E@D��@DZ@DI�@D�@Cƨ@C�F@CC�@B��@B�\@BM�@B�@BJ@A�@A�7@AX@@��@@��@@��@@�@@A�@?�@?l�@>��@>{@=�T@=��@=�-@=?}@=V@<��@<�D@<9X@;��@;33@:�@:��@:�!@:��@:�\@:^5@:M�@9�@9x�@8Ĝ@8A�@81'@8 �@7�;@7�w@7��@7l�@7+@6�@6v�@6ff@6E�@5�T@5@5�-@5�-@5��@5O�@4��@4�D@4(�@4�@3ƨ@333@2^5@1��@1��@1�7@0�9@0bN@0A�@01'@/K�@/�@.��@.�y@.�@.��@-�T@-�-@-�@-/@-/@-V@,�@,��@,I�@,�@+��@+�m@+�m@+�m@+ƨ@+ƨ@+��@+��@+t�@+S�@+@*��@*M�@)��@)�@(�`@(�9@(Q�@( �@'�;@'�@'l�@'l�@'K�@';d@&�y@&ff@%�@%�h@%O�@%O�@%�@%/@%?}@%�@$��@$�/@$�j@$�@$�D@$Z@$�@#�@#dZ@#dZ@#dZ@#C�@#o@"��@"=q@!��@ ��@ r�@ Q�@ A�@ b@|�@�@��@��@5?@5?@�T@�@O�@/@��@�/@Z@�@��@�
@��@o@��@^5@��@��@hs@hs@X@G�@&�@��@Ĝ@r�@Q�@Q�@A�@1'@b@�;@�w@�@�P@l�@\)@+@ȴ@��@��@��@��@�+@$�@�T@�@`B@�@��@z�@Z@(�@�@�@�
@�F@��@��@dZ@�@��@�\@=q@=q@-@�@J@��@��@��@x�@hs@X@G�@&�@&�@�@%@�`@��@Ĝ@�9@�9@��@��@r�@Q�@A�@b@  @�;@��@�@�@�@�@��@�P@|�@l�@\)@+@
=@ȴ@�+@V@5?@$�@�T@�-@�h@�@p�@/@/@�@V@V@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A܁A܇+A܏\A܋DA܍PA܍PAܑhA܏\AܓuA�~�Aۣ�A��A�"�A��A�G�Aղ-A�ZA�I�A�+A�bAԅA���A�VA�ȴA���A̝�A�dZA�?}A���Aǡ�A�C�A�A�A�A���A�|�A�9XA�O�A��A���A��;A���A�7LA�VA��A��#A�v�A��uA��`A�=qA��!A�M�A���A��yA���A���A�r�A�$�A��PA�{A��A��RA�A��TA���A�C�A�C�A���A��A��wA���A���A�E�A���A�M�A�A��A�ffA��PA��!A�M�A�^5A�
=A�A�v�A�JA�|�A��A��`A}`BA{C�Az9XAz �Ay�Aw|�At�Ar�RAq\)Ap��Ao/Al5?Ah�/AgƨAe��Ab��A`(�A[��AX��AV1AUVAQ�
AO�AM��AI��AHr�AG��AF�\ADĜABA>jA<ffA;;dA9/A7�^A6��A5ƨA57LA3��A2�A1��A0A�A.jA,�9A+VA)K�A'?}A&�yA&JA%C�A$��A$VA"��A �A ��A�jA��AȴAG�Az�A�AQ�AjAVA|�A��AC�A%A�9A  A�TAA��A�7Ax�A33A�/A�DA(�A1A�RA�hAA��AAdZAC�A�A
�yA
�AVA5?A�PAQ�A`BA33AXAĜAffAdZA v�A 9X@�\)@�V@�^5@�V@�+@�r�@��P@�-@�bN@��@��@�7@���@��
@�"�@��H@@���@�@�@�@�1@�o@��@�&�@��`@�9@�r�@�@��T@��@��;@�dZ@�ȴ@��@ܼj@�K�@�{@�9X@�^5@��#@��#@���@���@թ�@Դ9@��
@�t�@��@җ�@�G�@��m@�l�@�=q@�&�@�1'@˾w@�33@�
=@�v�@�?}@�j@�9X@ǥ�@���@��@��`@ă@�t�@�=q@��@��@��#@���@��@�hs@��`@�b@��m@��@��@�S�@�"�@��H@���@�n�@�M�@�J@�5?@�ff@���@���@��+@�ff@�M�@�-@��h@��@�Z@�|�@�o@�@��R@�~�@�n�@�ff@�M�@�@�p�@���@�Ĝ@�j@�1@�l�@���@��!@�=q@���@��@���@���@���@�j@� �@��w@�t�@�S�@�;d@��@���@�n�@�$�@���@�`B@��@��@�Z@���@��w@���@�o@��T@���@���@�x�@��9@� �@���@��@��@��@��y@��y@��\@�E�@���@���@���@�ƨ@��F@��@�@���@���@�=q@��-@�X@�/@�V@��/@��@��@���@�K�@�+@�ȴ@�5?@��T@�x�@�G�@���@��`@��u@���@���@���@�\)@���@�v�@�V@�{@���@��-@���@�G�@�%@���@���@���@��j@��@�j@��@�b@�b@���@�ƨ@�;d@���@�v�@�ff@�M�@�E�@�-@���@��h@�x�@�O�@�?}@��@���@��u@��@��F@��@�C�@��y@�ȴ@��!@���@�$�@�@���@���@���@��@���@��D@� �@��m@���@�S�@�"�@�
=@���@��H@��@�ȴ@���@�ff@��T@��@�7L@�V@��/@�r�@�(�@�  @�ƨ@�|�@�l�@�\)@��R@�E�@�{@���@�`B@��@��/@���@��j@��u@��@��D@��D@��@�r�@�Q�@�|�@�33@�"�@���@��!@�=q@��^@�G�@��j@�Q�@�Q�@�Z@��@��P@�33@���@�ff@�$�@�J@���@��-@�&�@�%@��@��9@���@��u@��D@�bN@�A�@��@�ƨ@���@�|�@�;d@�o@�
=@��y@��@���@���@��+@�n�@�^5@�M�@�-@�@��@���@��^@�hs@�?}@���@��@�r�@�bN@�I�@� �@��@�1@�;@�w@��@K�@~�+@}/@|�/@|��@|z�@|I�@|9X@{��@{dZ@z�\@z-@y��@yhs@x��@xA�@x �@w�P@w
=@v�@v�R@v�R@v��@vv�@v5?@u�@u��@uO�@u/@t�@t9X@s�m@s��@sS�@r�!@r�\@r~�@r~�@rn�@rM�@q��@q�#@q��@q��@q�7@q7L@p��@p  @o+@n��@n�+@nff@nE�@m�-@l(�@k�F@ko@j�H@j��@j��@jn�@jn�@j=q@i��@i�^@i�@h�u@h�@h1'@g�;@g\)@f�y@f�@f�R@f��@f�+@f5?@f{@eV@d��@dj@d(�@c��@c��@cC�@b��@b=q@a�#@a�^@a�7@ahs@`�`@` �@_K�@]��@]��@]p�@]?}@\�@\�j@\j@[�
@[�@["�@Z�H@Z~�@Z=q@Y�@Y��@Y�#@Y�@Yhs@X�u@X1'@W�;@W+@V@U�@UV@T�@TI�@T�@T1@SdZ@S"�@R��@Q��@Q7L@P�9@P  @O�@O��@O�@O+@O
=@Nff@N5?@N{@M�@M��@M@M�-@M��@M�@MO�@L�@L�D@LI�@L�@K�
@KC�@J��@J�\@J~�@J=q@I�@I�@I�^@I&�@H��@HbN@H1'@H  @G�@G�P@Gl�@G\)@GK�@G+@G
=@F��@F�R@Fff@F{@E@D��@DZ@DI�@D�@Cƨ@C�F@CC�@B��@B�\@BM�@B�@BJ@A�@A�7@AX@@��@@��@@��@@�@@A�@?�@?l�@>��@>{@=�T@=��@=�-@=?}@=V@<��@<�D@<9X@;��@;33@:�@:��@:�!@:��@:�\@:^5@:M�@9�@9x�@8Ĝ@8A�@81'@8 �@7�;@7�w@7��@7l�@7+@6�@6v�@6ff@6E�@5�T@5@5�-@5�-@5��@5O�@4��@4�D@4(�@4�@3ƨ@333@2^5@1��@1��@1�7@0�9@0bN@0A�@01'@/K�@/�@.��@.�y@.�@.��@-�T@-�-@-�@-/@-/@-V@,�@,��@,I�@,�@+��@+�m@+�m@+�m@+ƨ@+ƨ@+��@+��@+t�@+S�@+@*��@*M�@)��@)�@(�`@(�9@(Q�@( �@'�;@'�@'l�@'l�@'K�@';d@&�y@&ff@%�@%�h@%O�@%O�@%�@%/@%?}@%�@$��@$�/@$�j@$�@$�D@$Z@$�@#�@#dZ@#dZ@#dZ@#C�@#o@"��@"=q@!��@ ��@ r�@ Q�@ A�@ b@|�@�@��@��@5?@5?@�T@�@O�@/@��@�/@Z@�@��@�
@��@o@��@^5@��@��@hs@hs@X@G�@&�@��@Ĝ@r�@Q�@Q�@A�@1'@b@�;@�w@�@�P@l�@\)@+@ȴ@��@��@��@��@�+@$�@�T@�@`B@�@��@z�@Z@(�@�@�@�
@�F@��@��@dZ@�@��@�\@=q@=q@-@�@J@��@��@��@x�@hs@X@G�@&�@&�@�@%@�`@��@Ĝ@�9@�9@��@��@r�@Q�@A�@b@  @�;@��@�@�@�@�@��@�P@|�@l�@\)@+@
=@ȴ@�+@V@5?@$�@�T@�-@�h@�@p�@/@/@�@V@V@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�(�A�5?A�M�A�z�A��A� �A���A�K�A©�A��A�&�A��#A�\)A���A�VA�XA�  A�p�A�p�A�\)A�S�A���A��jA��A��RA�ƨA���A��HA�XA��HA�l�A�;dA���A�dZA�jA�K�A�  A��DA���A���A��A��FA�
=A���A��A��-A�r�A��A�JA���A��#A�JA��A��7A��PA��!A�dZA�oAǃAӺ^Aܧ�A��A�bNA�S�A�p�B
VB
bB	�B
B
  B	�/B	�B	�B	��B	��B	��B	��B	�)B	�
B	ɺB	�BB	�B	��B	��B	_;B	@�B	&�B	"�B	��B	�fB	��B	��B	��B	��B	�PB	��B	��B	�uB	n�B	Q�B	VB	cTB	33B	PB	
=B	B	9XB	H�B	dZB	;dB	S�B	C�B	>wB	v�B	{�B	]/B	@�B	)�B	.B	cTB	t�B	jB	�B	�\B	��B	��B	� B	�B	�DB	n�B	^5B	YB	R�B	Q�B	]/B	�B	w�B	y�B	w�B	iyB	L�B	VB	o�B	W
B	l�B	o�B	u�B	��B	�B	�?B	��B	�+B	aHB	��B	��B	�B	��B	��B	��B	��B	ȴB	ȴB	ÖB	�^B	�9B	�B	��B	��B	� B	�DB	��B	�%B	�{B	��B	�wB	�'B	��B	� B	l�B	z�B	��B	��B	�'B	ÖB	�wB	�B	�B	��B	��B	�LB	�B	�!B	�-B	��B	� B	�oB	�B	��B	�B	�FB	��B	ŢB	��B	�qB	ƨB	ȴB	ÖB	�RB	�3B	�XB	�3B	�^B	��B	��B	�B	�/B	�#B	��B	��B	ÖB	��B	�`B	�B	�`B	�TB	�`B	�mB	�B	�B

=B
$�B
.B
.B
.B
+B
#�B
'�B
/B
-B
(�B
!�B
%�B
9XB
7LB
?}B
J�B
YB
\)B
aHB
\)B
ZB
hsB
p�B
jB
hsB
jB
iyB
t�B
o�B
w�B
�1B
�DB
�1B
�+B
�%B
�B
� B
�B
�JB
�JB
�JB
�DB
�DB
�=B
�DB
�=B
�PB
�PB
�{B
�{B
�hB
�JB
�DB
�+B
�B
|�B
s�B
p�B
y�B
s�B
{�B
� B
{�B
|�B
}�B
{�B
w�B
s�B
q�B
u�B
y�B
v�B
t�B
u�B
x�B
y�B
v�B
w�B
w�B
�B
�B
�%B
�B
�B
�B
�+B
�+B
�+B
�B
�B
�B
�B
|�B
}�B
}�B
|�B
w�B
{�B
|�B
z�B
q�B
m�B
x�B
t�B
o�B
ffB
hsB
iyB
k�B
m�B
m�B
hsB
aHB
XB
Q�B
J�B
E�B
P�B
[#B
XB
R�B
ZB
ZB
T�B
N�B
O�B
R�B
Q�B
K�B
E�B
?}B
;dB
8RB
8RB
8RB
7LB
6FB
;dB
;dB
=qB
;dB
:^B
49B
2-B
8RB
5?B
2-B
1'B
=qB
?}B
=qB
?}B
B�B
A�B
@�B
D�B
G�B
E�B
C�B
C�B
C�B
B�B
F�B
L�B
I�B
E�B
@�B
<jB
>wB
D�B
E�B
F�B
E�B
D�B
A�B
@�B
C�B
A�B
B�B
?}B
;dB
;dB
7LB
:^B
<jB
<jB
;dB
>wB
>wB
<jB
8RB
;dB
?}B
=qB
;dB
7LB
2-B
7LB
<jB
A�B
A�B
C�B
E�B
H�B
L�B
K�B
H�B
D�B
?}B
;dB
9XB
<jB
?}B
?}B
=qB
;dB
=qB
>wB
<jB
:^B
;dB
7LB
0!B
49B
7LB
49B
2-B
5?B
;dB
:^B
9XB
9XB
:^B
9XB
8RB
5?B
1'B
-B
&�B
0!B
1'B
.B
-B
'�B
(�B
-B
0!B
49B
8RB
5?B
.B
-B
.B
.B
+B
0!B
2-B
1'B
-B
,B
33B
33B
2-B
2-B
0!B
/B
,B
,B
)�B
,B
-B
-B
,B
.B
/B
.B
.B
.B
.B
.B
.B
.B
-B
+B
)�B
)�B
)�B
'�B
$�B
&�B
$�B
%�B
(�B
)�B
)�B
)�B
+B
)�B
)�B
(�B
&�B
"�B
�B
�B
(�B
)�B
(�B
'�B
&�B
#�B
�B
 �B
"�B
"�B
�B
�B
"�B
#�B
 �B
!�B
$�B
$�B
#�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
bB
PB
PB
PB
bB
uB
bB
JB
%B
B
PB
PB
bB
hB
hB
\B
PB
DB
1B
%B
B
%B
	7B
%B
%B
B
%B
1B
1B
+B
B
B
B	��B
B
B
B
B
B
  B	��B
  B
  B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�TB	�TB	�HB	�;B	�HB	�`B	�TB	�NB	�NB	�BB	�;B	�NB	�HB	�HB	�HB	�;B	�/B	�5B	�/B	�/B	�)B	�#B	�B	�B	�B	��B	�B	�B	�B	�
B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	ɺB	ǮB	ĜB	ÖB	B	B	B	�wB	�jB	�jB	�}B	�}B	�wB	�dB	�}B	��B	�wB	�dB	��B	��B	��B	�wB	�dB	�XB	�qB	�jB	�jB	�qB	�jB	�dB	�^B	�^B	�jB	�jB	�jB	�jB	�dB	�^B	�XB	�RB	�LB	�FB	�9B	�-B	�'B	�B	�B	�!B	�-B	�-B	�'B	�-B	�'B	�'B	�'B	�-B	�!B	�B	�B	�B	�B	�B	�!B	�'B	�!B	�'B	�!B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	�{B	�{B	�uB	�uB	�uB	�{B	�uB	�uB	�oB	�oB	�oB	�hB	�hB	�hB	�bB	�bB	�bB	�\B	�VB	�PB	�VB	�PB	�JB	�PB	�PB	�JB	�JB	�JB	�JB	�DB	�=B	�=B	�7B	�1B	�+B	�%B	�%B	�B	�B	�%B	�%B	�B	�B	�B	�%B	�%B	�B	�B	�%B	�%B	�+B	�1B	�7B	�DB	�744444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444A�(�A�5?A�M�A�z�A��A� �A���A�K�A©�A��A�&�A��#A�\)A���A�VA�XA�  A�p�A�p�A�\)A�S�A���A��jA��A��RA�ƨA���A��HA�XA��HA�l�A�;dA���A�dZA�jA�K�A�  A��DA���A���A��A��FA�
=A���A��A��-A�r�A��A�JA���A��#A�JA��A��7A��PA��!A�dZA�oAǃAӺ^Aܧ�A��A�bNA�S�A�p�B
VB
bB	�B
B
  B	�/B	�B	�B	��B	��B	��B	��B	�)B	�
B	ɺB	�BB	�B	��B	��B	_;B	@�B	&�B	"�B	��B	�fB	��B	��B	��B	��B	�PB	��B	��B	�uB	n�B	Q�B	VB	cTB	33B	PB	
=B	B	9XB	H�B	dZB	;dB	S�B	C�B	>wB	v�B	{�B	]/B	@�B	)�B	.B	cTB	t�B	jB	�B	�\B	��B	��B	� B	�B	�DB	n�B	^5B	YB	R�B	Q�B	]/B	�B	w�B	y�B	w�B	iyB	L�B	VB	o�B	W
B	l�B	o�B	u�B	��B	�B	�?B	��B	�+B	aHB	��B	��B	�B	��B	��B	��B	��B	ȴB	ȴB	ÖB	�^B	�9B	�B	��B	��B	� B	�DB	��B	�%B	�{B	��B	�wB	�'B	��B	� B	l�B	z�B	��B	��B	�'B	ÖB	�wB	�B	�B	��B	��B	�LB	�B	�!B	�-B	��B	� B	�oB	�B	��B	�B	�FB	��B	ŢB	��B	�qB	ƨB	ȴB	ÖB	�RB	�3B	�XB	�3B	�^B	��B	��B	�B	�/B	�#B	��B	��B	ÖB	��B	�`B	�B	�`B	�TB	�`B	�mB	�B	�B

=B
$�B
.B
.B
.B
+B
#�B
'�B
/B
-B
(�B
!�B
%�B
9XB
7LB
?}B
J�B
YB
\)B
aHB
\)B
ZB
hsB
p�B
jB
hsB
jB
iyB
t�B
o�B
w�B
�1B
�DB
�1B
�+B
�%B
�B
� B
�B
�JB
�JB
�JB
�DB
�DB
�=B
�DB
�=B
�PB
�PB
�{B
�{B
�hB
�JB
�DB
�+B
�B
|�B
s�B
p�B
y�B
s�B
{�B
� B
{�B
|�B
}�B
{�B
w�B
s�B
q�B
u�B
y�B
v�B
t�B
u�B
x�B
y�B
v�B
w�B
w�B
�B
�B
�%B
�B
�B
�B
�+B
�+B
�+B
�B
�B
�B
�B
|�B
}�B
}�B
|�B
w�B
{�B
|�B
z�B
q�B
m�B
x�B
t�B
o�B
ffB
hsB
iyB
k�B
m�B
m�B
hsB
aHB
XB
Q�B
J�B
E�B
P�B
[#B
XB
R�B
ZB
ZB
T�B
N�B
O�B
R�B
Q�B
K�B
E�B
?}B
;dB
8RB
8RB
8RB
7LB
6FB
;dB
;dB
=qB
;dB
:^B
49B
2-B
8RB
5?B
2-B
1'B
=qB
?}B
=qB
?}B
B�B
A�B
@�B
D�B
G�B
E�B
C�B
C�B
C�B
B�B
F�B
L�B
I�B
E�B
@�B
<jB
>wB
D�B
E�B
F�B
E�B
D�B
A�B
@�B
C�B
A�B
B�B
?}B
;dB
;dB
7LB
:^B
<jB
<jB
;dB
>wB
>wB
<jB
8RB
;dB
?}B
=qB
;dB
7LB
2-B
7LB
<jB
A�B
A�B
C�B
E�B
H�B
L�B
K�B
H�B
D�B
?}B
;dB
9XB
<jB
?}B
?}B
=qB
;dB
=qB
>wB
<jB
:^B
;dB
7LB
0!B
49B
7LB
49B
2-B
5?B
;dB
:^B
9XB
9XB
:^B
9XB
8RB
5?B
1'B
-B
&�B
0!B
1'B
.B
-B
'�B
(�B
-B
0!B
49B
8RB
5?B
.B
-B
.B
.B
+B
0!B
2-B
1'B
-B
,B
33B
33B
2-B
2-B
0!B
/B
,B
,B
)�B
,B
-B
-B
,B
.B
/B
.B
.B
.B
.B
.B
.B
.B
-B
+B
)�B
)�B
)�B
'�B
$�B
&�B
$�B
%�B
(�B
)�B
)�B
)�B
+B
)�B
)�B
(�B
&�B
"�B
�B
�B
(�B
)�B
(�B
'�B
&�B
#�B
�B
 �B
"�B
"�B
�B
�B
"�B
#�B
 �B
!�B
$�B
$�B
#�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
bB
PB
PB
PB
bB
uB
bB
JB
%B
B
PB
PB
bB
hB
hB
\B
PB
DB
1B
%B
B
%B
	7B
%B
%B
B
%B
1B
1B
+B
B
B
B	��B
B
B
B
B
B
  B	��B
  B
  B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�TB	�TB	�HB	�;B	�HB	�`B	�TB	�NB	�NB	�BB	�;B	�NB	�HB	�HB	�HB	�;B	�/B	�5B	�/B	�/B	�)B	�#B	�B	�B	�B	��B	�B	�B	�B	�
B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	ɺB	ǮB	ĜB	ÖB	B	B	B	�wB	�jB	�jB	�}B	�}B	�wB	�dB	�}B	��B	�wB	�dB	��B	��B	��B	�wB	�dB	�XB	�qB	�jB	�jB	�qB	�jB	�dB	�^B	�^B	�jB	�jB	�jB	�jB	�dB	�^B	�XB	�RB	�LB	�FB	�9B	�-B	�'B	�B	�B	�!B	�-B	�-B	�'B	�-B	�'B	�'B	�'B	�-B	�!B	�B	�B	�B	�B	�B	�!B	�'B	�!B	�'B	�!B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	�{B	�{B	�uB	�uB	�uB	�{B	�uB	�uB	�oB	�oB	�oB	�hB	�hB	�hB	�bB	�bB	�bB	�\B	�VB	�PB	�VB	�PB	�JB	�PB	�PB	�JB	�JB	�JB	�JB	�DB	�=B	�=B	�7B	�1B	�+B	�%B	�%B	�B	�B	�%B	�%B	�B	�B	�B	�%B	�%B	�B	�B	�%B	�%B	�+B	�1B	�7B	�DB	�744444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220908090204                              AO  ARCAADJP                                                                    20220908090204    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220908090204  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20220908090204  QCF$                G�O�G�O�G�O�8800            