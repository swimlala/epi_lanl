CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-21T21:35:13Z creation;2016-08-21T21:35:15Z conversion to V3.1;2019-12-19T08:32:25Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160821213513  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_029                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���<� 1   @������ @;��]c�f�dg�[W>�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz�fD{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#�qC%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy�)Dz|)Dz�)D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�>D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AͲ-AͰ!AͮAͮAͬAͬAͰ!Aʹ9Aʹ9AͲ-Aͥ�A�l�A��mA̝�A�ZA�bA˲-A�^5A��TA�\)A�=qA�G�A���A���A��A�t�A�VA��A���A��\A��A��A�VA��A�-A��PA���A��uA�n�A��A�XA���A���A�&�A�VA�l�A�l�A�(�A�A��hA��+A���A�A�A���A�ZA�ZA�K�A��!A�-A��A��HA�33A�G�A�I�A�x�A��;A��DA���A���A�r�A�~�A���A��DA���A�A�+A�7LA��;A�`BA�r�A��mA���A��A�
A�A~��A}A}l�A|�A{ƨA{�AzȴAyAx��Ax(�Aw��AvȴAu�FAt��AtA�As��Aq�Ap-Ao��Ao%Am;dAlAj��AjAihsAh(�AfffAehsAc�;Ac?}Ab~�Aa��A_�mA^�uA\��AZĜAY�#AYG�AX=qAW�hAV5?ATr�ASt�AR�AQO�APȴAPn�AP �AOS�ANĜAM�wALn�AJ�/AI�AH��AH1'AGt�AF��AFffAEƨAD��AD�ABv�AA`BAA
=A@�/A@bA>��A>1A=/A<~�A<=qA;A;S�A;oA:Q�A9�A9�A8��A7��A6A37LA2�DA2�A1�TA1x�A1?}A0��A0bNA/�TA/dZA.��A-�A,��A+�7A*ĜA*bNA*A�A*5?A*$�A)t�A({A'��A&�+A&bA%O�A$��A$�A$-A#��A#�7A#�A#t�A#"�A"��A"�+A"I�A!�;A!dZA �`A ��A�
A��A��A�7AXA�yAA�A{A  A�At�AXA��AbNA�An�A�7A7LAr�A`BA��A  AO�A^5AoA	�hA��Az�A-A��A�A��AbA�A��A�AA�`A��A��AbNA1'A �`@��@���@�\)@�"�@��@���@���@�r�@���@�j@�33@�p�@�@�9@�ȴ@噚@�j@�A�@��@��y@���@��@��m@�J@��y@�%@�Q�@�K�@�"�@�o@��@֧�@�v�@�{@�l�@Л�@�|�@�@�@�hs@���@�Ĝ@ˮ@ʇ+@��@ȼj@�C�@��@�G�@Ĭ@î@§�@�n�@��@���@�  @�~�@���@���@���@��F@��@���@�{@�O�@��D@���@�=q@���@���@�X@�%@��j@�A�@���@�C�@�5?@���@�1@��@�@���@���@���@���@�G�@�Ĝ@�(�@�t�@��R@�n�@�V@��@��@��@�b@�"�@��y@��H@���@�M�@���@��@�O�@��@���@���@�I�@���@�|�@�dZ@��y@���@�~�@�v�@�v�@�v�@�n�@�^5@��7@���@�1@�ƨ@���@�l�@��@��+@��#@���@���@�hs@��j@�A�@��
@��
@�ƨ@���@��@�Ĝ@�9X@��m@��
@��m@�(�@�z�@���@�1@���@��-@��7@�hs@�X@�7L@�&�@��@�bN@�l�@�@�n�@��^@�?}@��@�Q�@��@�|�@�\)@�33@�@��H@���@�n�@�E�@���@��^@���@�`B@�%@�  @��F@���@�t�@��@�M�@�-@���@��^@��@�p�@�/@�Ĝ@�r�@� �@��@�C�@��P@��@�Q�@��
@�S�@���@��+@�^5@�-@�=q@�M�@�v�@��+@�^5@���@��@�P@�P@l�@~v�@~v�@
=@~�@}�T@}?}@|��@{�F@{�
@{�m@|1@|�@|Z@|z�@|�D@|z�@{��@{�m@{�m@{�F@{t�@{33@{@y��@xĜ@x�@x1'@x  @w�P@v�y@vff@u�h@u/@u/@u�@uV@t��@t�@t��@t�@r�H@r��@r~�@r=q@rJ@r-@r�@r-@r=q@r=q@r-@q�#@p�9@o|�@n��@n$�@n@n@n@nE�@m`B@l�@mV@l��@l��@l�j@l�/@l�@l�/@l��@l�/@l��@m/@mV@mV@l��@l�/@l��@k�m@k��@j�@j��@j~�@j^5@j-@iX@hĜ@h��@h�u@h�@g�@g�w@g�w@g�w@f��@f5?@e��@e�@e�@e`B@d9X@c�@cS�@b��@b��@bM�@a��@a�7@a7L@`�@`  @`  @_�@_�@_l�@_+@_�@^��@^�R@^$�@^{@]�h@]�@\��@\�D@[ƨ@[33@[@Z��@Zn�@Z^5@ZM�@Y�@YX@Y&�@X��@Xr�@Xb@W�@W�@Vff@U�T@U�-@T��@T9X@S�F@SdZ@So@R�H@R�!@R��@R�\@R^5@R=q@RJ@Q�#@Q�7@Q7L@P�`@Pr�@P �@P  @O�@O\)@N��@N�@Nv�@N$�@N@M@M�@L�@LZ@Kƨ@KdZ@K33@Ko@J�@J��@J~�@I�@IG�@I7L@I%@H�`@H��@H��@H��@H��@H�u@H�@Hr�@HQ�@H �@G�@G�P@F��@F��@F��@F�+@Fv�@Fff@FV@F5?@E��@Ep�@D��@D��@D�j@D�@D�D@DI�@C��@C�F@CS�@C33@C"�@B��@Bn�@B=q@A��@A�@Ax�@@��@@��@@�9@@�9@@bN@?�w@>�y@>$�@=�@=��@=��@=p�@<�/@<��@<��@<�j@<�@<��@<Z@;��@:��@:n�@:J@9�@9�#@9�7@9x�@9X@9&�@8�`@8bN@8A�@81'@8b@7�@7��@7�@6��@6ȴ@6��@6�+@6ff@5`B@4��@4z�@4j@4j@4I�@41@3�m@3ƨ@3�F@3�
@3ƨ@3��@3�@3t�@3dZ@3"�@2�@2�H@2�H@2��@2��@2��@2=q@2J@1�@1�#@1��@1�^@1�^@1��@1��@1�7@1�@0�9@0b@/��@/\)@/
=@.�@.�R@.�+@.ff@.V@.{@-��@-�@-O�@-?}@-�@,��@,�@,�@,z�@,Z@,9X@,1@+�
@+��@+S�@+@*��@*��@*~�@*^5@*=q@)�@)�^@)�7@)hs@)hs@)X@)�@(Ĝ@(�u@'�@'��@'�P@';d@'+@&��@&�@&ff@&E�@&5?@&@%��@%/@$I�@#�
@#ƨ@#ƨ@#ƨ@#�F@#�F@#�F@#��@#dZ@#S�@#C�@"��@"n�@"�@!�@!�^@!G�@ ��@ ��@ �@ A�@�;@�w@l�@�@ȴ@ff@$�@�T@�h@`B@�@�/@�/@��@�D@I�@�@�m@t�@"�@@�H@�!@n�@��@G�@�`@�u@bN@�@|�@K�@+@�@v�@V@$�@�@�@`B@?}@��@�j@��@�@�@�D@(�@�F@dZ@��@n�@^5@=q@��@�^@��@hs@X@�@��@��@�9@�u@�@ �@��@\)@K�@;d@�y@��@�+@$�@�T@�-@p�@O�@/@V@�@��@�@I�@�m@��@o@@
��@
��@
�!@
�\@
�@	��@	��@	x�@	&�@�`@��@r�@bN@A�@b@b@  @�@��@�P@|�@|�@l�@\)@;d@�@�@
=@�y@�@��@5?@{@@�T@@�-@��@p�@/@V@�@��@�@��@�D@(�@�F@t�@dZ@S�@S�@C�@33@"�@"�@o@@�@��@=q@�@�^@�^@��@�7@X@�@ �`@ ��@ �u@ �@ r�@ r�@ bN@ Q�@ A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AͲ-AͰ!AͮAͮAͬAͬAͰ!Aʹ9Aʹ9AͲ-Aͥ�A�l�A��mA̝�A�ZA�bA˲-A�^5A��TA�\)A�=qA�G�A���A���A��A�t�A�VA��A���A��\A��A��A�VA��A�-A��PA���A��uA�n�A��A�XA���A���A�&�A�VA�l�A�l�A�(�A�A��hA��+A���A�A�A���A�ZA�ZA�K�A��!A�-A��A��HA�33A�G�A�I�A�x�A��;A��DA���A���A�r�A�~�A���A��DA���A�A�+A�7LA��;A�`BA�r�A��mA���A��A�
A�A~��A}A}l�A|�A{ƨA{�AzȴAyAx��Ax(�Aw��AvȴAu�FAt��AtA�As��Aq�Ap-Ao��Ao%Am;dAlAj��AjAihsAh(�AfffAehsAc�;Ac?}Ab~�Aa��A_�mA^�uA\��AZĜAY�#AYG�AX=qAW�hAV5?ATr�ASt�AR�AQO�APȴAPn�AP �AOS�ANĜAM�wALn�AJ�/AI�AH��AH1'AGt�AF��AFffAEƨAD��AD�ABv�AA`BAA
=A@�/A@bA>��A>1A=/A<~�A<=qA;A;S�A;oA:Q�A9�A9�A8��A7��A6A37LA2�DA2�A1�TA1x�A1?}A0��A0bNA/�TA/dZA.��A-�A,��A+�7A*ĜA*bNA*A�A*5?A*$�A)t�A({A'��A&�+A&bA%O�A$��A$�A$-A#��A#�7A#�A#t�A#"�A"��A"�+A"I�A!�;A!dZA �`A ��A�
A��A��A�7AXA�yAA�A{A  A�At�AXA��AbNA�An�A�7A7LAr�A`BA��A  AO�A^5AoA	�hA��Az�A-A��A�A��AbA�A��A�AA�`A��A��AbNA1'A �`@��@���@�\)@�"�@��@���@���@�r�@���@�j@�33@�p�@�@�9@�ȴ@噚@�j@�A�@��@��y@���@��@��m@�J@��y@�%@�Q�@�K�@�"�@�o@��@֧�@�v�@�{@�l�@Л�@�|�@�@�@�hs@���@�Ĝ@ˮ@ʇ+@��@ȼj@�C�@��@�G�@Ĭ@î@§�@�n�@��@���@�  @�~�@���@���@���@��F@��@���@�{@�O�@��D@���@�=q@���@���@�X@�%@��j@�A�@���@�C�@�5?@���@�1@��@�@���@���@���@���@�G�@�Ĝ@�(�@�t�@��R@�n�@�V@��@��@��@�b@�"�@��y@��H@���@�M�@���@��@�O�@��@���@���@�I�@���@�|�@�dZ@��y@���@�~�@�v�@�v�@�v�@�n�@�^5@��7@���@�1@�ƨ@���@�l�@��@��+@��#@���@���@�hs@��j@�A�@��
@��
@�ƨ@���@��@�Ĝ@�9X@��m@��
@��m@�(�@�z�@���@�1@���@��-@��7@�hs@�X@�7L@�&�@��@�bN@�l�@�@�n�@��^@�?}@��@�Q�@��@�|�@�\)@�33@�@��H@���@�n�@�E�@���@��^@���@�`B@�%@�  @��F@���@�t�@��@�M�@�-@���@��^@��@�p�@�/@�Ĝ@�r�@� �@��@�C�@��P@��@�Q�@��
@�S�@���@��+@�^5@�-@�=q@�M�@�v�@��+@�^5@���@��@�P@�P@l�@~v�@~v�@
=@~�@}�T@}?}@|��@{�F@{�
@{�m@|1@|�@|Z@|z�@|�D@|z�@{��@{�m@{�m@{�F@{t�@{33@{@y��@xĜ@x�@x1'@x  @w�P@v�y@vff@u�h@u/@u/@u�@uV@t��@t�@t��@t�@r�H@r��@r~�@r=q@rJ@r-@r�@r-@r=q@r=q@r-@q�#@p�9@o|�@n��@n$�@n@n@n@nE�@m`B@l�@mV@l��@l��@l�j@l�/@l�@l�/@l��@l�/@l��@m/@mV@mV@l��@l�/@l��@k�m@k��@j�@j��@j~�@j^5@j-@iX@hĜ@h��@h�u@h�@g�@g�w@g�w@g�w@f��@f5?@e��@e�@e�@e`B@d9X@c�@cS�@b��@b��@bM�@a��@a�7@a7L@`�@`  @`  @_�@_�@_l�@_+@_�@^��@^�R@^$�@^{@]�h@]�@\��@\�D@[ƨ@[33@[@Z��@Zn�@Z^5@ZM�@Y�@YX@Y&�@X��@Xr�@Xb@W�@W�@Vff@U�T@U�-@T��@T9X@S�F@SdZ@So@R�H@R�!@R��@R�\@R^5@R=q@RJ@Q�#@Q�7@Q7L@P�`@Pr�@P �@P  @O�@O\)@N��@N�@Nv�@N$�@N@M@M�@L�@LZ@Kƨ@KdZ@K33@Ko@J�@J��@J~�@I�@IG�@I7L@I%@H�`@H��@H��@H��@H��@H�u@H�@Hr�@HQ�@H �@G�@G�P@F��@F��@F��@F�+@Fv�@Fff@FV@F5?@E��@Ep�@D��@D��@D�j@D�@D�D@DI�@C��@C�F@CS�@C33@C"�@B��@Bn�@B=q@A��@A�@Ax�@@��@@��@@�9@@�9@@bN@?�w@>�y@>$�@=�@=��@=��@=p�@<�/@<��@<��@<�j@<�@<��@<Z@;��@:��@:n�@:J@9�@9�#@9�7@9x�@9X@9&�@8�`@8bN@8A�@81'@8b@7�@7��@7�@6��@6ȴ@6��@6�+@6ff@5`B@4��@4z�@4j@4j@4I�@41@3�m@3ƨ@3�F@3�
@3ƨ@3��@3�@3t�@3dZ@3"�@2�@2�H@2�H@2��@2��@2��@2=q@2J@1�@1�#@1��@1�^@1�^@1��@1��@1�7@1�@0�9@0b@/��@/\)@/
=@.�@.�R@.�+@.ff@.V@.{@-��@-�@-O�@-?}@-�@,��@,�@,�@,z�@,Z@,9X@,1@+�
@+��@+S�@+@*��@*��@*~�@*^5@*=q@)�@)�^@)�7@)hs@)hs@)X@)�@(Ĝ@(�u@'�@'��@'�P@';d@'+@&��@&�@&ff@&E�@&5?@&@%��@%/@$I�@#�
@#ƨ@#ƨ@#ƨ@#�F@#�F@#�F@#��@#dZ@#S�@#C�@"��@"n�@"�@!�@!�^@!G�@ ��@ ��@ �@ A�@�;@�w@l�@�@ȴ@ff@$�@�T@�h@`B@�@�/@�/@��@�D@I�@�@�m@t�@"�@@�H@�!@n�@��@G�@�`@�u@bN@�@|�@K�@+@�@v�@V@$�@�@�@`B@?}@��@�j@��@�@�@�D@(�@�F@dZ@��@n�@^5@=q@��@�^@��@hs@X@�@��@��@�9@�u@�@ �@��@\)@K�@;d@�y@��@�+@$�@�T@�-@p�@O�@/@V@�@��@�@I�@�m@��@o@@
��@
��@
�!@
�\@
�@	��@	��@	x�@	&�@�`@��@r�@bN@A�@b@b@  @�@��@�P@|�@|�@l�@\)@;d@�@�@
=@�y@�@��@5?@{@@�T@@�-@��@p�@/@V@�@��@�@��@�D@(�@�F@t�@dZ@S�@S�@C�@33@"�@"�@o@@�@��@=q@�@�^@�^@��@�7@X@�@ �`@ ��@ �u@ �@ r�@ r�@ bN@ Q�@ A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B9XB8RB8RB8RB8RB9XB:^B:^B:^B;dB<jB@�BJ�BS�BZB`BBdZBiyBx�B�B� B{�Bo�Bk�BdZBVBE�B?}B:^B1'B+B�BuB
=BB��B�;B��B�?B��B��B�{B�7B{�Bq�BhsBaHBQ�BH�B9XB.B �B�BuB\BB��B�B�mB�5B��BĜB�LB�B��B�uB� Bm�BhsB_;BQ�BA�B.B%�BuBDB
��B
��B
�B
�BB
�B
��B
ĜB
�dB
�RB
�FB
�B
�B
��B
��B
��B
��B
�oB
�1B
�B
~�B
v�B
u�B
u�B
p�B
hsB
VB
E�B
A�B
?}B
/B
"�B
�B
VB
	7B	��B	�B	�mB	�)B	�
B	��B	��B	ĜB	�^B	�-B	��B	��B	��B	�{B	�JB	~�B	k�B	aHB	W
B	K�B	G�B	F�B	D�B	K�B	K�B	H�B	;dB	1'B	#�B	�B	�B	�B	�B	{B	{B	hB	VB	%B	  B��B��B��B��B��B�B�B�B��B��B��B	B	B	  B��B��B�B�;B�B�B��B��B��B��B��BɺBƨBÖB�wB�XB�?B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�7B}�Bz�By�Bw�Bv�Bv�Bu�Bs�Br�Bp�BhsBgmBcTBaHB`BB^5B\)BZBXBVBR�BQ�BN�BL�BK�BJ�BI�BH�BF�BF�BD�BC�BB�B?}B?}B?}B>wB>wB<jB<jB7LB2-B0!B0!B0!B/B/B.B-B,B)�B+B(�B(�B'�B&�B&�B%�B%�B$�B#�B#�B"�B"�B#�B"�B#�B$�B#�B#�B#�B#�B"�B"�B&�B&�B&�B'�B'�B'�B'�B&�B(�B)�B(�B,B-B-B/B/B1'B1'B1'B1'B/B2-B49B5?B49B6FB8RB7LB9XB:^B;dB<jB<jBA�BH�BI�BJ�BK�BK�BM�BM�BN�BQ�BT�BW
BZB^5B^5B_;B^5BcTBiyBk�Bm�Bn�Bn�Bm�Bm�Bm�Bm�Bl�Bn�Br�Bs�Bs�Bu�Bx�Bz�B|�B}�B~�B~�B� B�B�B�bB��B��B��B��B��B�B�B�B�B�B�!B�-B�3B�9B�9B�3B�3B�9B�LB�XB�qB�wBÖBBBŢBŢBɺB��B��B��B��B��B��B��B�B�#B�B�B�B�B�
B�
B�
B�B�B�)B�/B�BB�TB�`B�yB�B�B��B��B��B��B��B	  B	B	B	1B	
=B	DB	JB	VB	bB	bB	bB	hB	uB	�B	�B	$�B	%�B	'�B	)�B	+B	,B	,B	,B	0!B	49B	7LB	;dB	A�B	F�B	I�B	J�B	L�B	Q�B	T�B	VB	W
B	XB	[#B	\)B	]/B	_;B	]/B	bNB	cTB	dZB	e`B	k�B	m�B	m�B	l�B	m�B	q�B	v�B	w�B	x�B	x�B	z�B	{�B	{�B	}�B	� B	� B	� B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�1B	�7B	�=B	�JB	�VB	�\B	�\B	�bB	�bB	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�wB	��B	��B	��B	��B	��B	ÖB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�5B	�;B	�HB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
PB
PB
\B
\B
hB
hB
oB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
!�B
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
$�B
$�B
%�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
)�B
)�B
+B
+B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
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
?}B
?}B
?}B
@�B
@�B
@�B
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
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
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
VB
VB
VB
VB
W
B
W
B
YB
ZB
ZB
ZB
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
\)B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
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
iyB
jB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
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
o�B
p�B
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
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B9XB8lB8lB8RB8lB9rB:^B:xB:xB;�B=<BA�BKxBT�BZ�BabBfLBm�B�B��B��B~�Bq'Bn�BkQBZ�BHfBB[B=VB49B2aB&�BB�BtB�B�B�{B�B��B�qB�YB�dB~BtBkBd@BT�BK�B;�B/�B!�BB�B�BYB�RB�B��B�'B��B��B��B�B��B�mB�'BoiBjBa�BT,BC�B0UB($B2BPB
�(B
�`B
��B
��B
��B
�0B
��B
��B
�XB
��B
��B
�"B
��B
�vB
��B
��B
��B
�B
��B
�4B
xB
v�B
v�B
q�B
jeB
W�B
F�B
B�B
A�B
0�B
$tB
�B
�B

�B
 B	�B	�*B	�IB	�+B	�{B	��B	�tB	��B	�nB	�8B	��B	�B	��B	�<B	�B	mB	b�B	X_B	L�B	HfB	G_B	E�B	L�B	M6B	JXB	=qB	2�B	%,B	 �B	�B	kB	�B	�B	�B	�B	bB	zB	 �B��B�HB	 �B��B��B��B�-B�B�zB�fB��B	�B	�B	UB��B�B��B�\BٴBևBԯB҉BѝBΊB̘BʦB��B�B��B��B�+B��B�oB�iB��B�OB��B��B�2B��B�
B��B�KB��B��B�DB�>B�XB��B��B�zB�tB��B��B�pB�~B�)B��B��B��B~wB{�Bz�Bx8Bw2BwfBv`Bt�Bt�Br�Bi�Bh�BdtBa�Ba�B_�B]/B[qBYeBW�BT�BS�BO�BM�BL~BK�BJ�BI�BG�BG�BE�BE9BCGB?�B?�B@ B?B?cB>�B?�B9>B3B0�B0oB0�B/�B0oB/�B.�B-)B+�B,�B*�B*0B(�B'�B'mB&fB&�B%�B$�B%B$tB$�B%B#�B$tB%,B$&B$&B$@B$ZB#�B$�B(�B'�B'�B(�B(�B(sB(sB'�B)�B*�B)�B-)B-�B-�B/�B0B1�B1vB1�B1�B0UB33B4�B5�B5B7B8�B8B9�B;B<B=VB>(BC�BIRBJ=BK)BLBLJBNVBN�BO�BR�BU�BW�BZ�B^�B^jB_VB^�Bc�Bi�Bl"Bn/BoBo Bm�Bm�BnIBnBmCBo5Br�Bs�BtBvFBy>B{JB}<B~(BHBcB�iB�[B��B��B�!B� B�B�
B�B�6B�6B�kB��B��B��B�aB��B��B��B��B��B�nB�fB��B��B��B��B��B��B�?B�B�=B�)B��B�B��B��B��B�,B��B��B��B�SB�B�9B�?B�?B�?BּB��BܒB��B��B�B��B��B�B�B�B�B�$B�6B�<B	 4B	[B	mB	fB	
rB	�B	�B	�B	�B	�B	�B	B	�B	�B	B	%B	&B	(
B	*KB	+kB	,WB	,qB	,qB	0;B	4B	72B	;�B	BB	G+B	J#B	KB	MB	R B	T�B	VB	W$B	XEB	[qB	\�B	^B	`B	]~B	b�B	c�B	dtB	ezB	k�B	m�B	m�B	l�B	m�B	q�B	v�B	w�B	x�B	x�B	z�B	{�B	|B	~(B	� B	�4B	�4B	� B	�AB	�AB	��B	��B	�tB	�zB	�fB	��B	��B	��B	��B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�HB	�HB	�:B	�,B	�B	�
B	�
B	�B	�QB	�IB	�;B	�tB	�tB	�`B	�LB	�lB	��B	�XB	�rB	�xB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�"B	�B	�B	�B	�B	� B	� B	�&B	�[B	�gB	�$B	�?B	�EB	�eB	ٚB	ބB	�pB	�B	�B	�B	�tB	�B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�*B	�0B	�6B	�<B	�BB	�BB	�.B	�cB
oB
AB
AB
-B
GB
3B
3B
MB
MB
SB
SB
SB
YB
YB
zB
�B
	lB
	lB

rB

XB

�B
xB
�B
~B
dB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!B
!�B
!�B
!�B
"B
#B
#B
"�B
#B
# B
$&B
$@B
%,B
%B
%B
%B
%B
%,B
&B
(
B
(
B
)*B
)*B
)*B
)_B
*eB
+B
+B
+6B
+6B
+B
+B
*B
*0B
+B
+QB
-)B
-)B
-CB
-CB
-]B
.IB
.IB
.IB
/OB
/OB
.cB
.}B
/OB
/OB
/5B
/5B
/5B
0UB
0UB
1AB
1AB
1AB
2GB
3hB
3MB
3MB
4nB
4nB
4TB
5ZB
5?B
6`B
6`B
6zB
6zB
6zB
6zB
6`B
7fB
7fB
7fB
7fB
7�B
7�B
7�B
7�B
8�B
8�B
9rB
9�B
9�B
:xB
:�B
:�B
:�B
:�B
:�B
;B
;�B
;B
;B
;�B
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
?�B
?�B
?�B
@�B
@�B
@�B
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
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
KB
J�B
J�B
J�B
K�B
K�B
MB
L�B
NB
NB
MB
MB
M�B
OB
N�B
N�B
PB
O�B
O�B
PB
PB
O�B
O�B
PB
QB
Q B
RB
R B
R B
R B
R B
R:B
RB
R B
S@B
S&B
T,B
T,B
TB
U2B
UB
U2B
U2B
V9B
V9B
V9B
V9B
W?B
W
B
Y1B
Z7B
ZQB
ZQB
ZkB
ZQB
ZkB
ZQB
Z7B
ZQB
[WB
[=B
[=B
[WB
\CB
\]B
]dB
]/B
^jB
_pB
_VB
_�B
_pB
_pB
`\B
_pB
_pB
`vB
`vB
`vB
`vB
`vB
`vB
a|B
bhB
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
dtB
dtB
ezB
e�B
ezB
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
hsB
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
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
o�B
p�B
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
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608260032542016082600325420160826003254201806221212512018062212125120180622121251201804050405222018040504052220180405040522  JA  ARFMdecpA19c                                                                20160822063504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160821213513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160821213513  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160821213514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160821213514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160821213514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160821213514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160821213514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160821213515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160821213515                      G�O�G�O�G�O�                JA  ARUP                                                                        20160821222645                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160822153535  CV  JULD            G�O�G�O�F�'�                JM  ARCAJMQC2.0                                                                 20160825153254  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160825153254  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190522  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031251  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                