CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-18T09:00:49Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211018090049  20211018090049  5905729 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @ٛ��R1   @ٛӄ���@)u\(��dj^5?|�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BX  B`  Bh  Br  Bw33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BH  BO34BW��B_��Bg��Bq��Bv��B��B���B�fgB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�y�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aߕ�AߑhAߟ�Aߙ�A߶FA߸RA߾wAߺ^Aߴ9Aߧ�AߑhA�z�A�ffA�bNA�`BA�^5A�ZA�ZA�XA�S�A�Q�A��A���AۅA�=qA���Aװ!A֥�A�C�A���A�  A���A���A���A��yA���A��#A���A��A��wA��A�5?A�C�A��^A���A���A���A���A�ƨA���A|��Awl�AsG�Ao��Aj=qAd�DA]��AZ�AZjAU`BAQ�AP�AO|�AM��AJ��AH�!AGK�AD��ACVAB$�A?��A<�DA<=qA=�A=dZA=7LA<��A:A�A7�A6�A6jA5�A5VA4��A4��A4v�A4��A4�`A5"�A5S�A4I�A45?A2�yA1`BA0�/A0{A/��A.��A.�9A.��A.�A-��A-t�A,�uA,1A+S�A*�A*��A*9XA)�^A)p�A)�A(�`A(�/A(�DA(A'��A'
=A&��A&~�A%��A%��A%dZA%+A$��A$�A$ffA$  A#�A#"�A"�DA"1'A!��A!��A!dZA!O�A!33A �A ~�A M�A 1'A�mAƨAl�A��Az�AM�AJA�hA33A/A
=A�A�9A��AZA��A�
A�FAx�A;dA�A
=A�A�jA�DA^5AbA��A�A��A|�AO�AG�A��AffA�^Al�AO�A&�A�yA��A��A�uA~�An�AM�A$�A1A�#A?}Ar�AbA�#A�A7LA��AȴAv�AA�A��A��A|�Ap�AK�A�An�AAx�AG�A�A�`A�+A-A�mAƨAp�A�A�`A�uA^5A��A"�A
�A
^5A
A�A
-A
 �A
JA	�A	��A	��A�yA�9AVA��A��A��A\)A;dAoA�/A��A^5A9XA�FA/A�+A-AƨA33A��AZA1A��A�-A��A
=A ��A VA b@��;@�33@��\@�5?@��h@��@��u@�b@�\)@���@�J@���@��;@�33@��R@�~�@�$�@��#@���@�p�@��`@��u@�Q�@�b@��;@�V@�@��D@��D@��D@�j@��;@�@�v�@�$�@��@���@��-@��@�Ĝ@�bN@�1'@���@�P@��@�~�@�V@�J@��@�`B@���@�j@�Z@�b@�F@�t�@�o@�^5@�7L@�b@�C�@���@⟾@�=q@��T@ᙚ@�hs@��@�(�@�n�@��#@ݙ�@�X@���@�bN@�Q�@�t�@�E�@�O�@�Ĝ@� �@�t�@�S�@�;d@��@�@�^5@��T@ա�@�hs@�V@��@�t�@�;d@ҸR@�-@љ�@У�@�j@� �@�1@Ͼw@�o@�v�@�@��@�  @�K�@��y@�n�@�5?@�hs@�(�@�
=@Ɵ�@Ə\@�v�@��@�`B@���@ċD@�r�@�I�@�(�@� �@���@�|�@�+@��y@�ȴ@+@�J@��@���@��-@�r�@��@��;@���@�S�@���@��@�?}@���@�9X@�ƨ@�t�@�\)@�33@��H@���@��@��@�1'@��@�dZ@�K�@��@���@�v�@��@�O�@��j@�I�@�(�@�1@��;@��P@�dZ@��y@��#@�Ĝ@��P@�dZ@�dZ@�C�@�o@���@�V@��^@�/@��`@���@�r�@�A�@���@��@��\@��@��@��^@�x�@�/@��@��/@��u@�9X@��P@�"�@�@���@�{@��-@�hs@�&�@���@�1@�S�@��y@���@��R@�n�@�E�@�$�@�`B@�j@�A�@��@�  @��@��w@���@�|�@�dZ@�S�@�"�@���@��!@�E�@�@�?}@���@�Q�@��@�K�@�"�@��@��R@�ff@�$�@��#@��-@���@��h@�p�@��@���@���@��j@�bN@��@�"�@�ȴ@���@��@�@���@���@�`B@��@�z�@�9X@��@���@��w@���@��@�dZ@�;d@�
=@��y@��H@�ȴ@���@�V@��@�@���@�O�@��@��@���@���@��@�r�@�Z@�9X@�b@���@�|�@��@���@�^5@�{@��T@�@�X@���@���@�bN@��@��F@�K�@�@�n�@��@��#@��^@�hs@�/@�%@��/@�Ĝ@��@�j@���@�S�@��@��y@���@��R@�@��-@��h@��7@��7@�x�@�p�@�X@�7L@��@���@�Z@���@�t�@�dZ@�K�@��!@�ff@�5?@��@��#@�hs@��@���@��u@��D@�Z@�P@\)@+@~�y@~�@~�R@~ff@~5?@~{@}��@|�@{��@{ƨ@{33@zn�@z�@y�^@yG�@x��@x�@x�@xr�@x�@x�u@xQ�@w
=@v�R@v��@vv�@vE�@v{@v@v@v@u��@u/@uV@t�@t�/@tj@s�
@st�@r�\@q��@p�@o�@o�@n�y@m�-@l��@l��@lz�@l9X@k�m@kƨ@k�F@kS�@j��@j^5@i��@ihs@h��@hb@gl�@g�@f�@f��@e@e`B@e�@eV@d�j@dj@d1@cƨ@cS�@b-@a��@a�@`��@`Q�@_�@_+@^�R@^V@]�T@\��@\�@\Z@[�
@[@Z~�@Y�^@X��@X1'@W��@W��@W+@W
=@V�y@Vff@V{@U�T@U��@U��@Up�@U`B@U/@Tj@T�@S��@R�H@R�\@R�@Q�7@Q%@P��@P �@O�;@O�w@O\)@N�@N��@Nv�@N@M`B@MO�@L��@LI�@K�F@KC�@K@J��@J~�@JJ@I��@IG�@I7L@I7L@I7L@IG�@H��@G|�@F�y@F�+@F@E�h@EV@D�@D��@Dj@DZ@DI�@D�@Ct�@B��@B�\@B=q@A��@A�^@AG�@@��@@1'@?�@?;d@>��@>�@>�+@>v�@>V@>E�@>{@=�@=�-@=�@=/@<��@<�j@<�D@<9X@;ƨ@:�@:�\@:M�@:J@9�7@9G�@8��@8Q�@7l�@6�y@6��@6�R@6�R@6ff@6$�@6@5��@5��@5�-@5p�@5/@4��@4�@49X@3t�@3@2�!@2M�@2�@1x�@1&�@0�`@0�9@0r�@0bN@0Q�@/�;@/+@.ȴ@.��@.ff@.5?@-�T@-`B@,�/@,��@,Z@,(�@+�F@+"�@*��@*~�@*^5@*M�@*-@)��@)��@)��@)hs@)�@(��@(�9@(�@(A�@( �@( �@'�@'|�@'+@&��@&�y@&V@%@%p�@%`B@$��@$��@$Z@$1@#�m@#ƨ@#��@#t�@#dZ@#33@#"�@#o@#o@"�@"��@"��@"-@"J@!�@!��@!G�@ �`@ �9@  �@   @�w@�P@;d@�y@ȴ@ȴ@ȴ@ȴ@��@v�@ff@ff@E�@$�@{@�T@��@`B@?}@��@�D@j@j@I�@�@1@��@�
@ƨ@�F@��@S�@33@o@�@�!@M�@�@J@�#@��@�^@��@x�@G�@7L@��@�@Q�@b@��@�w@��@l�@;d@
=@�y@�R@v�@ff@5?@�T@�-@��@�h@�h@�@?}@�@��@��@�@z�@I�@�m@��@dZ@S�@C�@33@"�@@�@�@��@�\@J@��@��@�7@x�@hs@G�@7L@7L@�`@Ĝ@�9@bN@A�@�@�P@l�@+@��@�R@�+@$�@�@�-@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aߕ�AߑhAߟ�Aߙ�A߶FA߸RA߾wAߺ^Aߴ9Aߧ�AߑhA�z�A�ffA�bNA�`BA�^5A�ZA�ZA�XA�S�A�Q�A��A���AۅA�=qA���Aװ!A֥�A�C�A���A�  A���A���A���A��yA���A��#A���A��A��wA��A�5?A�C�A��^A���A���A���A���A�ƨA���A|��Awl�AsG�Ao��Aj=qAd�DA]��AZ�AZjAU`BAQ�AP�AO|�AM��AJ��AH�!AGK�AD��ACVAB$�A?��A<�DA<=qA=�A=dZA=7LA<��A:A�A7�A6�A6jA5�A5VA4��A4��A4v�A4��A4�`A5"�A5S�A4I�A45?A2�yA1`BA0�/A0{A/��A.��A.�9A.��A.�A-��A-t�A,�uA,1A+S�A*�A*��A*9XA)�^A)p�A)�A(�`A(�/A(�DA(A'��A'
=A&��A&~�A%��A%��A%dZA%+A$��A$�A$ffA$  A#�A#"�A"�DA"1'A!��A!��A!dZA!O�A!33A �A ~�A M�A 1'A�mAƨAl�A��Az�AM�AJA�hA33A/A
=A�A�9A��AZA��A�
A�FAx�A;dA�A
=A�A�jA�DA^5AbA��A�A��A|�AO�AG�A��AffA�^Al�AO�A&�A�yA��A��A�uA~�An�AM�A$�A1A�#A?}Ar�AbA�#A�A7LA��AȴAv�AA�A��A��A|�Ap�AK�A�An�AAx�AG�A�A�`A�+A-A�mAƨAp�A�A�`A�uA^5A��A"�A
�A
^5A
A�A
-A
 �A
JA	�A	��A	��A�yA�9AVA��A��A��A\)A;dAoA�/A��A^5A9XA�FA/A�+A-AƨA33A��AZA1A��A�-A��A
=A ��A VA b@��;@�33@��\@�5?@��h@��@��u@�b@�\)@���@�J@���@��;@�33@��R@�~�@�$�@��#@���@�p�@��`@��u@�Q�@�b@��;@�V@�@��D@��D@��D@�j@��;@�@�v�@�$�@��@���@��-@��@�Ĝ@�bN@�1'@���@�P@��@�~�@�V@�J@��@�`B@���@�j@�Z@�b@�F@�t�@�o@�^5@�7L@�b@�C�@���@⟾@�=q@��T@ᙚ@�hs@��@�(�@�n�@��#@ݙ�@�X@���@�bN@�Q�@�t�@�E�@�O�@�Ĝ@� �@�t�@�S�@�;d@��@�@�^5@��T@ա�@�hs@�V@��@�t�@�;d@ҸR@�-@љ�@У�@�j@� �@�1@Ͼw@�o@�v�@�@��@�  @�K�@��y@�n�@�5?@�hs@�(�@�
=@Ɵ�@Ə\@�v�@��@�`B@���@ċD@�r�@�I�@�(�@� �@���@�|�@�+@��y@�ȴ@+@�J@��@���@��-@�r�@��@��;@���@�S�@���@��@�?}@���@�9X@�ƨ@�t�@�\)@�33@��H@���@��@��@�1'@��@�dZ@�K�@��@���@�v�@��@�O�@��j@�I�@�(�@�1@��;@��P@�dZ@��y@��#@�Ĝ@��P@�dZ@�dZ@�C�@�o@���@�V@��^@�/@��`@���@�r�@�A�@���@��@��\@��@��@��^@�x�@�/@��@��/@��u@�9X@��P@�"�@�@���@�{@��-@�hs@�&�@���@�1@�S�@��y@���@��R@�n�@�E�@�$�@�`B@�j@�A�@��@�  @��@��w@���@�|�@�dZ@�S�@�"�@���@��!@�E�@�@�?}@���@�Q�@��@�K�@�"�@��@��R@�ff@�$�@��#@��-@���@��h@�p�@��@���@���@��j@�bN@��@�"�@�ȴ@���@��@�@���@���@�`B@��@�z�@�9X@��@���@��w@���@��@�dZ@�;d@�
=@��y@��H@�ȴ@���@�V@��@�@���@�O�@��@��@���@���@��@�r�@�Z@�9X@�b@���@�|�@��@���@�^5@�{@��T@�@�X@���@���@�bN@��@��F@�K�@�@�n�@��@��#@��^@�hs@�/@�%@��/@�Ĝ@��@�j@���@�S�@��@��y@���@��R@�@��-@��h@��7@��7@�x�@�p�@�X@�7L@��@���@�Z@���@�t�@�dZ@�K�@��!@�ff@�5?@��@��#@�hs@��@���@��u@��D@�Z@�P@\)@+@~�y@~�@~�R@~ff@~5?@~{@}��@|�@{��@{ƨ@{33@zn�@z�@y�^@yG�@x��@x�@x�@xr�@x�@x�u@xQ�@w
=@v�R@v��@vv�@vE�@v{@v@v@v@u��@u/@uV@t�@t�/@tj@s�
@st�@r�\@q��@p�@o�@o�@n�y@m�-@l��@l��@lz�@l9X@k�m@kƨ@k�F@kS�@j��@j^5@i��@ihs@h��@hb@gl�@g�@f�@f��@e@e`B@e�@eV@d�j@dj@d1@cƨ@cS�@b-@a��@a�@`��@`Q�@_�@_+@^�R@^V@]�T@\��@\�@\Z@[�
@[@Z~�@Y�^@X��@X1'@W��@W��@W+@W
=@V�y@Vff@V{@U�T@U��@U��@Up�@U`B@U/@Tj@T�@S��@R�H@R�\@R�@Q�7@Q%@P��@P �@O�;@O�w@O\)@N�@N��@Nv�@N@M`B@MO�@L��@LI�@K�F@KC�@K@J��@J~�@JJ@I��@IG�@I7L@I7L@I7L@IG�@H��@G|�@F�y@F�+@F@E�h@EV@D�@D��@Dj@DZ@DI�@D�@Ct�@B��@B�\@B=q@A��@A�^@AG�@@��@@1'@?�@?;d@>��@>�@>�+@>v�@>V@>E�@>{@=�@=�-@=�@=/@<��@<�j@<�D@<9X@;ƨ@:�@:�\@:M�@:J@9�7@9G�@8��@8Q�@7l�@6�y@6��@6�R@6�R@6ff@6$�@6@5��@5��@5�-@5p�@5/@4��@4�@49X@3t�@3@2�!@2M�@2�@1x�@1&�@0�`@0�9@0r�@0bN@0Q�@/�;@/+@.ȴ@.��@.ff@.5?@-�T@-`B@,�/@,��@,Z@,(�@+�F@+"�@*��@*~�@*^5@*M�@*-@)��@)��@)��@)hs@)�@(��@(�9@(�@(A�@( �@( �@'�@'|�@'+@&��@&�y@&V@%@%p�@%`B@$��@$��@$Z@$1@#�m@#ƨ@#��@#t�@#dZ@#33@#"�@#o@#o@"�@"��@"��@"-@"J@!�@!��@!G�@ �`@ �9@  �@   @�w@�P@;d@�y@ȴ@ȴ@ȴ@ȴ@��@v�@ff@ff@E�@$�@{@�T@��@`B@?}@��@�D@j@j@I�@�@1@��@�
@ƨ@�F@��@S�@33@o@�@�!@M�@�@J@�#@��@�^@��@x�@G�@7L@��@�@Q�@b@��@�w@��@l�@;d@
=@�y@�R@v�@ff@5?@�T@�-@��@�h@�h@�@?}@�@��@��@�@z�@I�@�m@��@dZ@S�@C�@33@"�@@�@�@��@�\@J@��@��@�7@x�@hs@G�@7L@7L@�`@Ĝ@�9@bN@A�@�@�P@l�@+@��@�R@�+@$�@�@�-@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B,B,B1'B2-B9XB:^B?}BA�BC�BE�BG�BH�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BG�BF�B�-B	  B	1'B	gmB	�VB	��B
@�B
m�B
N�B
>wB
L�B
=qB
D�B
;dB
�B	�-B	��B	�}B	��B
B	�B	��B	ĜB	�yB	��B	�sB	�`B	B	�B	��B	��B	gmB	e`B	=qB	T�B	o�B	N�B	YB	�PB	�B	�7B	}�B	y�B	�B	l�B	~�B	�B	v�B	n�B	�=B	��B	��B	�;B	�mB	�)B	��B	�B	�B	�B	�#B
hB
�B
 �B
(�B
.B
1'B
1'B
0!B
G�B
L�B
F�B
R�B
]/B
dZB
iyB
x�B
{�B
v�B
v�B
�\B
�PB
�uB
�hB
�{B
��B
��B
��B
��B
�B
�!B
�-B
�B
�B
�B
�!B
�-B
�FB
�9B
�9B
�FB
�FB
�9B
�?B
�3B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B
��B
��B
��B
��B
��B
��B
��B
�bB
�\B
�hB
�{B
�uB
�hB
�uB
�{B
��B
�{B
�{B
�uB
�bB
�VB
�DB
�B
�B
�%B
�7B
�%B
�B
�B
�B
�B
�B
�B
~�B
� B
� B
}�B
z�B
u�B
s�B
w�B
x�B
w�B
u�B
r�B
q�B
q�B
r�B
o�B
m�B
n�B
k�B
iyB
e`B
bNB
aHB
ffB
ffB
ffB
ffB
e`B
cTB
bNB
_;B
ZB
\)B
[#B
ZB
\)B
[#B
ZB
ZB
ZB
XB
W
B
T�B
S�B
N�B
J�B
I�B
K�B
I�B
G�B
G�B
E�B
G�B
G�B
H�B
G�B
A�B
?}B
C�B
B�B
B�B
@�B
>wB
@�B
>wB
=qB
=qB
<jB
:^B
:^B
9XB
7LB
5?B
8RB
:^B
;dB
:^B
:^B
:^B
:^B
9XB
9XB
9XB
8RB
6FB
1'B
,B
9XB
9XB
8RB
6FB
49B
1'B
33B
49B
5?B
5?B
49B
2-B
2-B
2-B
2-B
1'B
/B
-B
1'B
1'B
/B
/B
.B
.B
.B
-B
-B
,B
,B
)�B
'�B
$�B
#�B
%�B
'�B
+B
(�B
'�B
'�B
&�B
%�B
 �B
�B
$�B
%�B
%�B
$�B
"�B
#�B
�B
�B
�B
!�B
 �B
!�B
$�B
$�B
#�B
#�B
 �B
!�B
!�B
!�B
 �B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
�B
 �B
 �B
�B
�B
�B
!�B
$�B
#�B
#�B
"�B
"�B
�B
�B
�B
�B
!�B
#�B
#�B
"�B
!�B
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
 �B
�B
!�B
"�B
"�B
!�B
 �B
 �B
"�B
$�B
$�B
#�B
#�B
#�B
!�B
!�B
&�B
&�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
,B
,B
,B
,B
-B
-B
.B
/B
.B
.B
,B
/B
/B
/B
-B
,B
,B
.B
/B
.B
/B
1'B
1'B
0!B
/B
/B
1'B
2-B
33B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
2-B
2-B
1'B
33B
33B
33B
33B
49B
49B
33B
5?B
5?B
5?B
5?B
49B
49B
33B
33B
49B
6FB
5?B
6FB
5?B
33B
49B
5?B
6FB
5?B
7LB
6FB
7LB
6FB
9XB
:^B
;dB
:^B
:^B
;dB
;dB
;dB
;dB
:^B
9XB
:^B
;dB
;dB
<jB
<jB
;dB
;dB
=qB
=qB
>wB
=qB
=qB
=qB
=qB
=qB
<jB
;dB
:^B
>wB
>wB
>wB
<jB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
C�B
E�B
D�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
G�B
E�B
C�B
E�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
H�B
I�B
I�B
I�B
H�B
F�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
L�B
L�B
L�B
K�B
K�B
L�B
J�B
K�B
K�B
K�B
L�B
K�B
K�B
M�B
O�B
P�B
O�B
O�B
P�B
P�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
S�B
T�B
VB
T�B
S�B
VB
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
VB
T�B
W
B
W
B
XB
YB
YB
YB
YB
ZB
ZB
YB
\)B
\)B
[#B
[#B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
`BB
_;B
_;B
_;B
^5B
^5B
]/B
]/B
]/B
_;B
_;B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
aHB
bNB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
aHB
dZB
e`B
e`B
dZB
dZB
dZB
e`B
dZB
dZB
e`B
dZB
dZB
dZB
ffB
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
s�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
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
z�B
{�B
{�B
{�B
{�B
{�B
|�B
}�B
|�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�+B
�+B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�VB
�VB
�VB
�VB
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B,B,B1'B2-B9XB:^B?}BA�BC�BE�BG�BH�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BG�BF�B�-B	  B	1'B	gmB	�VB	��B
@�B
m�B
N�B
>wB
L�B
=qB
D�B
;dB
�B	�-B	��B	�}B	��B
B	�B	��B	ĜB	�yB	��B	�sB	�`B	B	�B	��B	��B	gmB	e`B	=qB	T�B	o�B	N�B	YB	�PB	�B	�7B	}�B	y�B	�B	l�B	~�B	�B	v�B	n�B	�=B	��B	��B	�;B	�mB	�)B	��B	�B	�B	�B	�#B
hB
�B
 �B
(�B
.B
1'B
1'B
0!B
G�B
L�B
F�B
R�B
]/B
dZB
iyB
x�B
{�B
v�B
v�B
�\B
�PB
�uB
�hB
�{B
��B
��B
��B
��B
�B
�!B
�-B
�B
�B
�B
�!B
�-B
�FB
�9B
�9B
�FB
�FB
�9B
�?B
�3B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B
��B
��B
��B
��B
��B
��B
��B
�bB
�\B
�hB
�{B
�uB
�hB
�uB
�{B
��B
�{B
�{B
�uB
�bB
�VB
�DB
�B
�B
�%B
�7B
�%B
�B
�B
�B
�B
�B
�B
~�B
� B
� B
}�B
z�B
u�B
s�B
w�B
x�B
w�B
u�B
r�B
q�B
q�B
r�B
o�B
m�B
n�B
k�B
iyB
e`B
bNB
aHB
ffB
ffB
ffB
ffB
e`B
cTB
bNB
_;B
ZB
\)B
[#B
ZB
\)B
[#B
ZB
ZB
ZB
XB
W
B
T�B
S�B
N�B
J�B
I�B
K�B
I�B
G�B
G�B
E�B
G�B
G�B
H�B
G�B
A�B
?}B
C�B
B�B
B�B
@�B
>wB
@�B
>wB
=qB
=qB
<jB
:^B
:^B
9XB
7LB
5?B
8RB
:^B
;dB
:^B
:^B
:^B
:^B
9XB
9XB
9XB
8RB
6FB
1'B
,B
9XB
9XB
8RB
6FB
49B
1'B
33B
49B
5?B
5?B
49B
2-B
2-B
2-B
2-B
1'B
/B
-B
1'B
1'B
/B
/B
.B
.B
.B
-B
-B
,B
,B
)�B
'�B
$�B
#�B
%�B
'�B
+B
(�B
'�B
'�B
&�B
%�B
 �B
�B
$�B
%�B
%�B
$�B
"�B
#�B
�B
�B
�B
!�B
 �B
!�B
$�B
$�B
#�B
#�B
 �B
!�B
!�B
!�B
 �B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
�B
 �B
 �B
�B
�B
�B
!�B
$�B
#�B
#�B
"�B
"�B
�B
�B
�B
�B
!�B
#�B
#�B
"�B
!�B
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
 �B
�B
!�B
"�B
"�B
!�B
 �B
 �B
"�B
$�B
$�B
#�B
#�B
#�B
!�B
!�B
&�B
&�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
,B
,B
,B
,B
-B
-B
.B
/B
.B
.B
,B
/B
/B
/B
-B
,B
,B
.B
/B
.B
/B
1'B
1'B
0!B
/B
/B
1'B
2-B
33B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
2-B
2-B
1'B
33B
33B
33B
33B
49B
49B
33B
5?B
5?B
5?B
5?B
49B
49B
33B
33B
49B
6FB
5?B
6FB
5?B
33B
49B
5?B
6FB
5?B
7LB
6FB
7LB
6FB
9XB
:^B
;dB
:^B
:^B
;dB
;dB
;dB
;dB
:^B
9XB
:^B
;dB
;dB
<jB
<jB
;dB
;dB
=qB
=qB
>wB
=qB
=qB
=qB
=qB
=qB
<jB
;dB
:^B
>wB
>wB
>wB
<jB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
B�B
C�B
E�B
D�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
G�B
E�B
C�B
E�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
H�B
I�B
I�B
I�B
H�B
F�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
L�B
L�B
L�B
K�B
K�B
L�B
J�B
K�B
K�B
K�B
L�B
K�B
K�B
M�B
O�B
P�B
O�B
O�B
P�B
P�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
S�B
T�B
VB
T�B
S�B
VB
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
VB
T�B
W
B
W
B
XB
YB
YB
YB
YB
ZB
ZB
YB
\)B
\)B
[#B
[#B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
`BB
_;B
_;B
_;B
^5B
^5B
]/B
]/B
]/B
_;B
_;B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
aHB
bNB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
aHB
dZB
e`B
e`B
dZB
dZB
dZB
e`B
dZB
dZB
e`B
dZB
dZB
dZB
ffB
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
s�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
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
z�B
{�B
{�B
{�B
{�B
{�B
|�B
}�B
|�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�+B
�+B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�VB
�VB
�VB
�VB
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211018090049                              AO  ARCAADJP                                                                    20211018090049    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211018090049  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211018090049  QCF$                G�O�G�O�G�O�0               