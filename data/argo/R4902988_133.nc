CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-01T06:58:47Z creation;2023-06-01T06:58:48Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230601065847  20230601070438  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�/��fff1   @�/�13�@;F�-�c��
=q1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8ffB@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C�C�C  C�fC	�fC  C�C  C  C  C  C  C�fC  C  C �C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Cg�fCj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��3C�  C�  C��3C�  C�  C��3C�  C��3C��C�  C��3C��C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C��D   D � D  Dy�D  D�fD  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D��D� D  D� D  D� D  D� D��D� D  D� D  D�fD   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6y�D6��D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DEy�DE��DF� DG  DG� DHfDH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� DZ��D[y�D\  D\� D]  D]� D]��D^y�D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� DffDf�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm�fDn  Dn� Do  Doy�Do��Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~y�D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D���D���D�@ D�� D���D�  D�@ D�|�D¼�D�  D�@ DÃ3D�� D�  D�@ D�|�D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D���D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�<�D�|�D�� D�  D�@ D�|�D�� D�  D�@ Dр D��3D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@��H@��HAp�A=p�A]p�A}p�A��A��RA��RA��RAθRA޸RA�RA��RB\)B\)BB\)B'\)B/\)B7B?\)BG\)BO\)BW\)B_\)BgBo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��GB��B��BîBǮBˮBϮB�z�B׮BۮB߮B�B�B�B�B�B��B��GB��HC�C�C�
C�pC	�pC�
C�C�
C�
C�
C�
C�
C�pC�
C�
C�C!�pC#�
C%�
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
Cc�Ce�
Cg�pCi�
Ck�
Cm�
Co�
Cq�
Cs�pCu�
Cw�
Cy�
C{�
C}�
C�
C��C��C�޸C��C��C��C�޸C��C�޸C��C��C�޸C��C��C�޸C��C�޸C��RC��C�޸C��RC��RC��C��C��C��C�޸C�޸C�޸C�޸C�޸C��C��C��C��C��C�޸C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��RC��C��C��C��C��RC��C��C��C��C��C��C��C��C��C�޸C��C��C��C��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��C��C��C��C��RC��C��C�޸C��C��C�޸C��C��C��C��RC��RC��D u�D ��Do]D��D|)D��Du�D��Du�D��Du�D�)Du�D��Du�D��Du�D��D	u�D	��D
|)D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D|)D�)Du�D�]Du�D��Du�D��Du�D��Du�D�]Du�D��Du�D��D|)D��D u�D �]D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)�)D*|)D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3o]D3��D4u�D4��D5u�D5��D6o]D6�]D7u�D7��D8u�D8��D9u�D9��D:o]D:��D;u�D;��D<u�D<��D=u�D=��D>|)D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD�]DEo]DE�]DFu�DF��DGu�DG�)DHu�DH��DIu�DI�]DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO�]DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DY|)DY��DZu�DZ�]D[o]D[��D\u�D\��D]u�D]�]D^o]D^�]D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd�)Deu�De�)Df|)Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dl|)Dl�)Dm|)Dm��Dnu�Dn��Doo]Do�]Dpu�Dp��Dqu�Dq�)Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du�]Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyo]Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~o]D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��D��D�:�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�~D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�~D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�~D���D���D�:�D�w�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�~D��D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�w�D·�D���D�:�D�~Dú�D���D�:�D�w�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˷�D���D�:�D�z�D̾D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�7�D�w�DϺ�D���D�:�D�w�Dк�D���D�:�D�z�DѾD���D�:�D�z�DҺ�D���D�:�D�w�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�>D�z�Dֺ�D���D�:�D�~D׺�D���D�:�D�z�Dط�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D�D���D�:�D�z�D��D���D�:�D�z�D��D��D�:�D�z�D�D��D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�~D�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA���A��MA��A��0A���A��-A��RA���A���A�p;A�v+A���A��lA�ŢA�e�A�cA��1A�*�A�{JA�gmA��/A���A�VmA���A��A�ܒA��qA�.}A�ĜA��]A��A�g�A���A��A���A��A��4A�p�A�B�A���A���A�8A�� A��.A���A��QA���A�?�A�q�A��A���A�.A��nA���A�ɆA�{A���A���A�!�A���A���A�FA��A���A��~A�#nA��vA���A�I�A�K^A���A�-�A�QNA��A�~A�u�A�\A��HA�\]A�r�A��A���A�%A��A�
�A�oiA��A�P�A�%zA��^A�r�A��A��7A��A~VmA}�\A})�A|�6A{�MAz�Ay��Ay=qAv��Au��As��Arm�AqƨAq�SApW�AmQ�Aly�Ak�&Ak*0Aj��Ai2aAg(�Af�Ae[�Ac��Ab�FAaX�A`��A_��A^H�A^�A]��A\�4A[VAY�oAYu%AX�;AX  AV�`AU�ATS�ARu%AQ~(AP�&APYKAPQ�AO�FAO+AM�)AM:�AL�AL.IAK��AJ�AJ�AIiDAG�AF�uAEiDAB)_A@FtA?�A?  A>CA=9XA<��A;��A:X�A9m�A6��A5<�A4�"A4��A4�A4^5A4&A4A3͟A3i�A3+A2��A0��A/��A.�UA-S�A,��A+�_A+'RA*�A*A)<�A(qA'�mA&˒A&FA%X�A$�ZA#��A"��A �fA HA�fA��A�A��A��AX�A�[AGEA/�A��A�AS�A��ARTA�A�A4�A;dA��A�AJ#Ao A�AffA��A��A��Ax�A�bA�mAN�A
y�A	�KA�pA��A�A�Ac�A�~A9XA4�A}VAخA �.A ��A  �@�	�@�l�@��@���@���@�4n@��<@�
�@���@�\�@��@�S@���@�,=@��@��@�YK@��@�|@�$t@�@�x@��@�$�@�P@�Z@�x�@�s�@ߠ�@�4@��@�f�@�M@ײ�@��@է�@Ӂ@��[@��@��@̀4@˫�@��@�J@ȋD@�-�@��@�e@ò-@�Q�@��P@°�@��Z@��w@���@�ѷ@�d�@���@��H@��[@��m@���@�K^@���@�� @�B�@��1@��*@�m]@�b�@�6z@�ff@���@�N�@��"@�Y@�PH@��@��L@���@��@��@���@�A @��+@��P@��m@���@�ff@�E�@�*�@��f@�c�@�&�@���@�g�@�<�@��@���@�p;@�Q�@�@�iD@�,�@�!�@��R@�-�@�G@�@�5?@�F�@�G�@�8@�<6@�8�@��@��O@�v�@��@��@��@��:@��s@�L0@��Q@�\�@�&@���@�B[@�|@�=�@��@��.@�oi@��+@�qv@�.I@��@��z@�B[@��z@�iD@��_@�~@�a�@� \@�@��5@���@�|�@�($@�N<@��@��@���@��+@�l�@�Q@�Ft@�3�@�4@��w@��C@��'@�f�@���@���@��@���@�r�@�N�@�7@��m@���@��-@��	@�F@���@���@�R�@��@��9@��X@�C�@��@���@��R@��z@���@�g8@��@�o@��|@�ߤ@���@���@��@�_�@���@��&@��}@��^@���@�6z@��K@���@���@��@��@�kQ@�D�@�~@�b@��@�u@��@��@�V@;d@~��@~B[@}��@}��@}��@}f�@}+@|��@|�9@|r�@{��@z� @y�Z@y��@yj@y/@x��@xN�@w�w@w=@v�<@v1�@v5?@v�@t��@sZ�@r�1@r-@q�@q@q��@qe,@q0�@p�9@pI�@o��@o�@nں@n�h@nz@n�@m@l��@l  @k�@k)_@jYK@i�z@i=�@h��@h�@g�K@g��@g.I@f��@f}V@fGE@f!�@e�.@e��@e|@eq@d�z@d-�@c˒@c�4@c9�@c�@cY@c�@b��@b�B@b�+@bTa@b\�@b$�@a��@a�T@a��@a��@aL�@`K^@_{J@_b�@_S�@^��@^�A@^8�@]�T@]L�@\�|@\�v@\��@\�p@\�4@[��@[1�@Z��@Z�x@ZH�@Z	@Y��@Y��@YIR@Y \@X�@X��@X�z@Xe�@W�@WY@Vff@V�@V4@U�9@U��@U�H@U�@U��@U��@U��@U�X@U��@U�=@U��@U��@Uzx@Uj@Uhs@Ua�@UVm@U+�@T��@T�@S9�@Rߤ@R�@R�L@Q��@Q�@Q�@P�@PH@O�Q@OO@O@O@OE9@N�2@N�B@Nd�@NQ@N4@M�@M�X@M-w@L�Y@L(�@Ky�@J��@JQ@J#:@Je@J_@I�@I��@I�M@I�@HɆ@H��@H��@Hh�@H7�@H�@G�m@G�*@GRT@F�@F��@Fp;@F.�@F	@E�9@Ej@E�@D�P@D�|@D�@D��@D�z@D`�@D�@C�K@C��@Cg�@CS@B�y@B�,@B��@B��@B��@B�@B��@Bz@BTa@B&�@A��@@�@@�@@G@?�@?~�@?W?@?&@>�y@>��@>ں@>ȴ@>_�@=�>@=X@<�@<�@<��@<r�@<~@;�@;�@;�P@;l�@;W?@;A�@;A�@;>�@;�@;S@:�8@:�s@:�!@:��@:Ta@:$�@:@9�@9�X@9Y�@9�@8��@8�I@8<�@7�	@7o@6�c@6�H@6��@65?@5�o@5�X@5k�@5L�@5;@4�U@4��@4w�@4Ft@4G@3l�@3o@2�@2v�@2v�@2u%@2q�@2p;@2^5@2Q@28�@25?@20U@2#:@1�j@0�	@0�Y@/��@/�@.�y@.�s@.@�@-��@-c@,�)@,��@,�o@,[�@,M@,H@,Ft@,A�@,1'@,�@+6z@*�@*_�@*.�@)��@)�d@)�h@):�@(�@(h�@(�@'�6@'g�@&�!@&�@%��@%��@%=�@%4@%(�@%@@%�@$�f@$Ĝ@$e�@$/�@#�@#��@#�$@#x@#�@"�m@"�h@"�!@"�h@"�}@"��@"��@"�F@"v�@"a|@"W�@"Q@"?@"=q@"8�@"3�@"1�@"($@"#:@!�@!��@!*0@ r�@ %�@��@y�@_p@P�@��@xl@|@-w@%@�@��@�)@��@tT@M@�;@˒@��@o�@��@�,@��@��@��@�r@q�@M�@6�@	@	@	@�Z@ϫ@Dg@��@�?@�u@��@�u@�o@:�@7@��@خ@�@�[@��@n/@�@� @R�@;�@	@�)@��@7L@	l@��@�@�$@\)@@�@҉@�x@kQ@l�@C�@0U@O@�@_@u@��@�Z@�H@�@��@]d@N�@7�@�@7@�@7@�@1@  @��@ݘ@��@�	@W?@�"@q�@@�@�d@�@�7@�7@�M@o @j@Vm@Dg@@@�@��@�@Ĝ@w�@Ft@�@��@�k@'�@
�h@
v�@
^5@
�@	�@	�^@	k�@	-w@	V@�@�.@��@�@�V@Z�@>�@�@ں@��@{�@d�@YK@C�@+k@�@{@�@��@��@��@ԕ@�@�'@��@`B@A @��@�j@��@�O@`�@C-@�@�}@��@]�@�y@��@�@z@($@�D@�)@�@�>@�@�9@�^@s�@J�@ �@ ��@ oi@ -�@ �?���?�b�?���?��F?�l�?�l�?�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA���A��MA��A��0A���A��-A��RA���A���A�p;A�v+A���A��lA�ŢA�e�A�cA��1A�*�A�{JA�gmA��/A���A�VmA���A��A�ܒA��qA�.}A�ĜA��]A��A�g�A���A��A���A��A��4A�p�A�B�A���A���A�8A�� A��.A���A��QA���A�?�A�q�A��A���A�.A��nA���A�ɆA�{A���A���A�!�A���A���A�FA��A���A��~A�#nA��vA���A�I�A�K^A���A�-�A�QNA��A�~A�u�A�\A��HA�\]A�r�A��A���A�%A��A�
�A�oiA��A�P�A�%zA��^A�r�A��A��7A��A~VmA}�\A})�A|�6A{�MAz�Ay��Ay=qAv��Au��As��Arm�AqƨAq�SApW�AmQ�Aly�Ak�&Ak*0Aj��Ai2aAg(�Af�Ae[�Ac��Ab�FAaX�A`��A_��A^H�A^�A]��A\�4A[VAY�oAYu%AX�;AX  AV�`AU�ATS�ARu%AQ~(AP�&APYKAPQ�AO�FAO+AM�)AM:�AL�AL.IAK��AJ�AJ�AIiDAG�AF�uAEiDAB)_A@FtA?�A?  A>CA=9XA<��A;��A:X�A9m�A6��A5<�A4�"A4��A4�A4^5A4&A4A3͟A3i�A3+A2��A0��A/��A.�UA-S�A,��A+�_A+'RA*�A*A)<�A(qA'�mA&˒A&FA%X�A$�ZA#��A"��A �fA HA�fA��A�A��A��AX�A�[AGEA/�A��A�AS�A��ARTA�A�A4�A;dA��A�AJ#Ao A�AffA��A��A��Ax�A�bA�mAN�A
y�A	�KA�pA��A�A�Ac�A�~A9XA4�A}VAخA �.A ��A  �@�	�@�l�@��@���@���@�4n@��<@�
�@���@�\�@��@�S@���@�,=@��@��@�YK@��@�|@�$t@�@�x@��@�$�@�P@�Z@�x�@�s�@ߠ�@�4@��@�f�@�M@ײ�@��@է�@Ӂ@��[@��@��@̀4@˫�@��@�J@ȋD@�-�@��@�e@ò-@�Q�@��P@°�@��Z@��w@���@�ѷ@�d�@���@��H@��[@��m@���@�K^@���@�� @�B�@��1@��*@�m]@�b�@�6z@�ff@���@�N�@��"@�Y@�PH@��@��L@���@��@��@���@�A @��+@��P@��m@���@�ff@�E�@�*�@��f@�c�@�&�@���@�g�@�<�@��@���@�p;@�Q�@�@�iD@�,�@�!�@��R@�-�@�G@�@�5?@�F�@�G�@�8@�<6@�8�@��@��O@�v�@��@��@��@��:@��s@�L0@��Q@�\�@�&@���@�B[@�|@�=�@��@��.@�oi@��+@�qv@�.I@��@��z@�B[@��z@�iD@��_@�~@�a�@� \@�@��5@���@�|�@�($@�N<@��@��@���@��+@�l�@�Q@�Ft@�3�@�4@��w@��C@��'@�f�@���@���@��@���@�r�@�N�@�7@��m@���@��-@��	@�F@���@���@�R�@��@��9@��X@�C�@��@���@��R@��z@���@�g8@��@�o@��|@�ߤ@���@���@��@�_�@���@��&@��}@��^@���@�6z@��K@���@���@��@��@�kQ@�D�@�~@�b@��@�u@��@��@�V@;d@~��@~B[@}��@}��@}��@}f�@}+@|��@|�9@|r�@{��@z� @y�Z@y��@yj@y/@x��@xN�@w�w@w=@v�<@v1�@v5?@v�@t��@sZ�@r�1@r-@q�@q@q��@qe,@q0�@p�9@pI�@o��@o�@nں@n�h@nz@n�@m@l��@l  @k�@k)_@jYK@i�z@i=�@h��@h�@g�K@g��@g.I@f��@f}V@fGE@f!�@e�.@e��@e|@eq@d�z@d-�@c˒@c�4@c9�@c�@cY@c�@b��@b�B@b�+@bTa@b\�@b$�@a��@a�T@a��@a��@aL�@`K^@_{J@_b�@_S�@^��@^�A@^8�@]�T@]L�@\�|@\�v@\��@\�p@\�4@[��@[1�@Z��@Z�x@ZH�@Z	@Y��@Y��@YIR@Y \@X�@X��@X�z@Xe�@W�@WY@Vff@V�@V4@U�9@U��@U�H@U�@U��@U��@U��@U�X@U��@U�=@U��@U��@Uzx@Uj@Uhs@Ua�@UVm@U+�@T��@T�@S9�@Rߤ@R�@R�L@Q��@Q�@Q�@P�@PH@O�Q@OO@O@O@OE9@N�2@N�B@Nd�@NQ@N4@M�@M�X@M-w@L�Y@L(�@Ky�@J��@JQ@J#:@Je@J_@I�@I��@I�M@I�@HɆ@H��@H��@Hh�@H7�@H�@G�m@G�*@GRT@F�@F��@Fp;@F.�@F	@E�9@Ej@E�@D�P@D�|@D�@D��@D�z@D`�@D�@C�K@C��@Cg�@CS@B�y@B�,@B��@B��@B��@B�@B��@Bz@BTa@B&�@A��@@�@@�@@G@?�@?~�@?W?@?&@>�y@>��@>ں@>ȴ@>_�@=�>@=X@<�@<�@<��@<r�@<~@;�@;�@;�P@;l�@;W?@;A�@;A�@;>�@;�@;S@:�8@:�s@:�!@:��@:Ta@:$�@:@9�@9�X@9Y�@9�@8��@8�I@8<�@7�	@7o@6�c@6�H@6��@65?@5�o@5�X@5k�@5L�@5;@4�U@4��@4w�@4Ft@4G@3l�@3o@2�@2v�@2v�@2u%@2q�@2p;@2^5@2Q@28�@25?@20U@2#:@1�j@0�	@0�Y@/��@/�@.�y@.�s@.@�@-��@-c@,�)@,��@,�o@,[�@,M@,H@,Ft@,A�@,1'@,�@+6z@*�@*_�@*.�@)��@)�d@)�h@):�@(�@(h�@(�@'�6@'g�@&�!@&�@%��@%��@%=�@%4@%(�@%@@%�@$�f@$Ĝ@$e�@$/�@#�@#��@#�$@#x@#�@"�m@"�h@"�!@"�h@"�}@"��@"��@"�F@"v�@"a|@"W�@"Q@"?@"=q@"8�@"3�@"1�@"($@"#:@!�@!��@!*0@ r�@ %�@��@y�@_p@P�@��@xl@|@-w@%@�@��@�)@��@tT@M@�;@˒@��@o�@��@�,@��@��@��@�r@q�@M�@6�@	@	@	@�Z@ϫ@Dg@��@�?@�u@��@�u@�o@:�@7@��@خ@�@�[@��@n/@�@� @R�@;�@	@�)@��@7L@	l@��@�@�$@\)@@�@҉@�x@kQ@l�@C�@0U@O@�@_@u@��@�Z@�H@�@��@]d@N�@7�@�@7@�@7@�@1@  @��@ݘ@��@�	@W?@�"@q�@@�@�d@�@�7@�7@�M@o @j@Vm@Dg@@@�@��@�@Ĝ@w�@Ft@�@��@�k@'�@
�h@
v�@
^5@
�@	�@	�^@	k�@	-w@	V@�@�.@��@�@�V@Z�@>�@�@ں@��@{�@d�@YK@C�@+k@�@{@�@��@��@��@ԕ@�@�'@��@`B@A @��@�j@��@�O@`�@C-@�@�}@��@]�@�y@��@�@z@($@�D@�)@�@�>@�@�9@�^@s�@J�@ �@ ��@ oi@ -�@ �?���?�b�?���?��F?�l�?�l�?�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BɆB�	BɆB�tBŢB�mB�mB�B��B�3B��B�gB�>B�hB�}B�B�qB�`B�EB��B�oB�\B��B�B��B��B��B�B{JBx�Bo�Ba-B^�BY�BX+BW�BXBa�Bg�Bf�Bc�BWYBM�BC�BA;B?.B7�B0�B)�B�BuB�B�B;B�B�#B�YB��B��B�2B��B|�Bj�B_BMPB:�B.IB($BpByB�BٚB��B��B�B�eB�B��B��B��B}<Bn}Bc:BYKBO\BG�B1AB"NBdB#BB�B	7BGB
�XB
��B
�]B
�B
�B
�hB
چB
ՁB
ЗB
żB
�B
�TB
��B
�FB
�B
��B
�JB
�gB
��B
�B
}�B
xlB
l�B
c:B
^�B
WYB
P�B
H�B
D3B
A�B
=VB
:�B
8�B
4�B
4�B
/�B
+B
(>B
!�B
�B
 B
jB
tB
�B	�cB	��B	�B	��B	�FB	��B	�B	�WB	�B	�$B	�FB	�B	ߤB	ٚB	уB	��B	�}B	��B	��B	�WB	��B	� B	�\B	�xB	��B	��B	�^B	�;B	�B	~�B	~BB	}qB	|�B	|B	{B	zDB	x�B	v�B	r�B	m�B	jB	dtB	]�B	YKB	U�B	T,B	R�B	O\B	MB	J#B	D�B	BAB	<�B	;�B	8�B	4�B	/�B	,B	)�B	(
B	$�B	#�B	#:B	"hB	!�B	�B	!-B	!HB	/B	B	�B	gB	B	gB	�B	�B	B	B	�B	?B	�B��B��B��B��B�GB�B��B�B�WB�QB�mB�`B�NB��B��B�VB�B��B��B��BרB�B��B�[BуBуB�bBЗB��B��B̈́B�JB�~BȴB�B��B�_B�B�mBĜB�3B�-BBB�;B�OB�BB�(B�qB�qB��B��B��B��B�B��B�DB��B�	B��B�RB�B�>B��B��B�lB��B��B��B�B�B��B�VB��B�VB�B��B�B��B�uB�uB��B��B��B�OB��B�UB��B��B��B�B��B��B��B��B��B�EB��B�B��B͹B�BՁB�9B׍B�#B�]B�B�HB�B��B��B�&B��B�fB�KB�B�B�)B�AB�9B�?B��B��B	 �B	�B	�B	�B	�B	�B	fB	<B	NB	B	�B	CB	OB	 B	#TB	$tB	%�B	'8B	+B	+�B	-�B	2aB	5ZB	7�B	:DB	;JB	>BB	@OB	FtB	I�B	KxB	M�B	NpB	Q�B	TFB	U2B	W
B	Y1B	[qB	^�B	a|B	g�B	i�B	mCB	n�B	n�B	p�B	tnB	vFB	z�B	��B	�RB	�^B	��B	��B	�B	�B	�\B	�.B	��B	�MB	�B	�mB	��B	��B	�B	�!B	�BB	��B	�`B	�mB	��B	�QB	��B	��B	�UB	�B	��B	��B	��B	��B	��B	āB	��B	�B	�RB	��B	ʦB	�xB	�<B	�{B	�2B	�9B	��B	��B	�B	�B	�B	�B	� B	�B	��B	�B	�B	��B	�IB	�B	�B	��B	�9B	��B	��B	�8B	�rB	�B	��B	�B	�B
 �B
AB
�B
�B
zB
fB
	�B
�B
�B
�B
�B
�B
B
�B
 �B
!�B
# B
&LB
(�B
+�B
.}B
1�B
1�B
2�B
7�B
<�B
>]B
@ B
A;B
A�B
B[B
D�B
EB
F�B
H1B
J�B
L�B
M�B
NB
N�B
PB
QNB
T�B
XB
ZB
[#B
]�B
^�B
`BB
abB
cB
c�B
d&B
ezB
g8B
g�B
h�B
iB
iyB
j�B
kB
lWB
m�B
n�B
pB
p�B
q�B
q�B
q�B
rGB
r�B
s�B
uB
u�B
utB
v+B
vzB
v�B
v�B
w�B
xlB
|B
~wB
~�B
~�B
�B
�oB
�AB
��B
�+B
�fB
��B
��B
��B
�B
�xB
��B
��B
�"B
�(B
��B
��B
��B
��B
�B
��B
�FB
�{B
�2B
�sB
��B
�7B
�=B
��B
��B
�B
�B
�/B
�IB
�IB
�~B
�dB
�~B
��B
��B
��B
�B
�OB
�5B
�OB
�jB
��B
��B
�-B
�B
��B
��B
�&B
�B
�RB
��B
�KB
�B
�"B
�B
�B
��B
�OB
�OB
��B
��B
�vB
��B
�-B
�MB
��B
�tB
�B
�$B
��B
�B
��B
�*B
��B
��B
�B
��B
�<B
��B
��B
�B
��B
��B
�cB
��B
��B
��B
�'B
�-B
�B
�tB
�zB
ȀB
ȀB
ȀB
��B
�B
�B
ɠB
�=B
��B
�xB
ˬB
�dB
�jB
͟B
͹B
��B
�B
�<B
�<B
�VB
ΊB
ΥB
�(B
�bB
ѷB
ҽB
�uB
�,B
�FB
�{B
��B
�B
�B
�2B
��B
��B
ּB
רB
��B
��B
��B
�+B
��B
�KB
��B
�B
�kB
ڠB
��B
��B
��B
�=B
�WB
�qB
یB
�B
�xB
�B
�dB
�5B
�jB
ޞB
�B
�|B
�B
��B
��B
�@B
�@B
�@B
�tB
�FB
��B
�B
�fB
�B
�B
�8B
�B
��B
�$B
�sB
��B
��B
�B
�B
��B
��B
��B
��B
��B
�=B
�"B
�=B
�WB
�WB
�WB
��B
�B
�B
�oB
��B
�B
��B
�B
�B
�9B
��B
�%B
�ZB
��B
��B
��B
��B
��B
��B
��B
�fB
�B
�lB
��B
�	B
�>B
��B
��B
�xB
�B
�B
��B
��B
��B
��B
�B
�}B  B  B B 4B 4B OB �B BUB�BB'BAB�BGBaBaBaB{B{B{B�B�B�B�B�B3BB3B3B3BMB3B�B�B�B�BB�B�BBBfB	7B
�BBDBDBxBxB�BB�BBBBjB"B<B�B�B�B�B�B(B\BBB\BBB�B�B�B BBhBNBhBhB�B�BTBoB�B�B�B�B[B,BaBaB�B�BgB�BB9B�B�B+B�B�B�BKBeBeB�B�BB7BBBBBBqB�BCB]BxB�B�B�B�B�B�B�B�B�B/BdB�BB�BVBpB�B�B 'B�B B BB BB \B vB �B �B �B �B!B!�B!�B!�B"hB"�B#:B#�B$B$B$�B$�B%B%�B%�B%�B%�B&�B'mB'�B'�B(>B(XB(�B(�B)*B)_B)_B)�B)�B)�B)�B)�B*B*B*B*KB*eB*�B*B*�B+B+B+�B+�B+�B+�B,�B,�B,�B-B-]B-�B.IB.IB.�B.�B/OB/�B/�B/�B/�B/�B/�B/�B0!B0UB1B1AB1vB1�B2B2B2�B2�B3B3MB33B3�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444BɆB�	BɆB�tBŢB�mB�mB�B��B�3B��B�gB�>B�hB�}B�B�qB�`B�EB��B�oB�\B��B�B��B��B��B�B{JBx�Bo�Ba-B^�BY�BX+BW�BXBa�Bg�Bf�Bc�BWYBM�BC�BA;B?.B7�B0�B)�B�BuB�B�B;B�B�#B�YB��B��B�2B��B|�Bj�B_BMPB:�B.IB($BpByB�BٚB��B��B�B�eB�B��B��B��B}<Bn}Bc:BYKBO\BG�B1AB"NBdB#BB�B	7BGB
�XB
��B
�]B
�B
�B
�hB
چB
ՁB
ЗB
żB
�B
�TB
��B
�FB
�B
��B
�JB
�gB
��B
�B
}�B
xlB
l�B
c:B
^�B
WYB
P�B
H�B
D3B
A�B
=VB
:�B
8�B
4�B
4�B
/�B
+B
(>B
!�B
�B
 B
jB
tB
�B	�cB	��B	�B	��B	�FB	��B	�B	�WB	�B	�$B	�FB	�B	ߤB	ٚB	уB	��B	�}B	��B	��B	�WB	��B	� B	�\B	�xB	��B	��B	�^B	�;B	�B	~�B	~BB	}qB	|�B	|B	{B	zDB	x�B	v�B	r�B	m�B	jB	dtB	]�B	YKB	U�B	T,B	R�B	O\B	MB	J#B	D�B	BAB	<�B	;�B	8�B	4�B	/�B	,B	)�B	(
B	$�B	#�B	#:B	"hB	!�B	�B	!-B	!HB	/B	B	�B	gB	B	gB	�B	�B	B	B	�B	?B	�B��B��B��B��B�GB�B��B�B�WB�QB�mB�`B�NB��B��B�VB�B��B��B��BרB�B��B�[BуBуB�bBЗB��B��B̈́B�JB�~BȴB�B��B�_B�B�mBĜB�3B�-BBB�;B�OB�BB�(B�qB�qB��B��B��B��B�B��B�DB��B�	B��B�RB�B�>B��B��B�lB��B��B��B�B�B��B�VB��B�VB�B��B�B��B�uB�uB��B��B��B�OB��B�UB��B��B��B�B��B��B��B��B��B�EB��B�B��B͹B�BՁB�9B׍B�#B�]B�B�HB�B��B��B�&B��B�fB�KB�B�B�)B�AB�9B�?B��B��B	 �B	�B	�B	�B	�B	�B	fB	<B	NB	B	�B	CB	OB	 B	#TB	$tB	%�B	'8B	+B	+�B	-�B	2aB	5ZB	7�B	:DB	;JB	>BB	@OB	FtB	I�B	KxB	M�B	NpB	Q�B	TFB	U2B	W
B	Y1B	[qB	^�B	a|B	g�B	i�B	mCB	n�B	n�B	p�B	tnB	vFB	z�B	��B	�RB	�^B	��B	��B	�B	�B	�\B	�.B	��B	�MB	�B	�mB	��B	��B	�B	�!B	�BB	��B	�`B	�mB	��B	�QB	��B	��B	�UB	�B	��B	��B	��B	��B	��B	āB	��B	�B	�RB	��B	ʦB	�xB	�<B	�{B	�2B	�9B	��B	��B	�B	�B	�B	�B	� B	�B	��B	�B	�B	��B	�IB	�B	�B	��B	�9B	��B	��B	�8B	�rB	�B	��B	�B	�B
 �B
AB
�B
�B
zB
fB
	�B
�B
�B
�B
�B
�B
B
�B
 �B
!�B
# B
&LB
(�B
+�B
.}B
1�B
1�B
2�B
7�B
<�B
>]B
@ B
A;B
A�B
B[B
D�B
EB
F�B
H1B
J�B
L�B
M�B
NB
N�B
PB
QNB
T�B
XB
ZB
[#B
]�B
^�B
`BB
abB
cB
c�B
d&B
ezB
g8B
g�B
h�B
iB
iyB
j�B
kB
lWB
m�B
n�B
pB
p�B
q�B
q�B
q�B
rGB
r�B
s�B
uB
u�B
utB
v+B
vzB
v�B
v�B
w�B
xlB
|B
~wB
~�B
~�B
�B
�oB
�AB
��B
�+B
�fB
��B
��B
��B
�B
�xB
��B
��B
�"B
�(B
��B
��B
��B
��B
�B
��B
�FB
�{B
�2B
�sB
��B
�7B
�=B
��B
��B
�B
�B
�/B
�IB
�IB
�~B
�dB
�~B
��B
��B
��B
�B
�OB
�5B
�OB
�jB
��B
��B
�-B
�B
��B
��B
�&B
�B
�RB
��B
�KB
�B
�"B
�B
�B
��B
�OB
�OB
��B
��B
�vB
��B
�-B
�MB
��B
�tB
�B
�$B
��B
�B
��B
�*B
��B
��B
�B
��B
�<B
��B
��B
�B
��B
��B
�cB
��B
��B
��B
�'B
�-B
�B
�tB
�zB
ȀB
ȀB
ȀB
��B
�B
�B
ɠB
�=B
��B
�xB
ˬB
�dB
�jB
͟B
͹B
��B
�B
�<B
�<B
�VB
ΊB
ΥB
�(B
�bB
ѷB
ҽB
�uB
�,B
�FB
�{B
��B
�B
�B
�2B
��B
��B
ּB
רB
��B
��B
��B
�+B
��B
�KB
��B
�B
�kB
ڠB
��B
��B
��B
�=B
�WB
�qB
یB
�B
�xB
�B
�dB
�5B
�jB
ޞB
�B
�|B
�B
��B
��B
�@B
�@B
�@B
�tB
�FB
��B
�B
�fB
�B
�B
�8B
�B
��B
�$B
�sB
��B
��B
�B
�B
��B
��B
��B
��B
��B
�=B
�"B
�=B
�WB
�WB
�WB
��B
�B
�B
�oB
��B
�B
��B
�B
�B
�9B
��B
�%B
�ZB
��B
��B
��B
��B
��B
��B
��B
�fB
�B
�lB
��B
�	B
�>B
��B
��B
�xB
�B
�B
��B
��B
��B
��B
�B
�}B  B  B B 4B 4B OB �B BUB�BB'BAB�BGBaBaBaB{B{B{B�B�B�B�B�B3BB3B3B3BMB3B�B�B�B�BB�B�BBBfB	7B
�BBDBDBxBxB�BB�BBBBjB"B<B�B�B�B�B�B(B\BBB\BBB�B�B�B BBhBNBhBhB�B�BTBoB�B�B�B�B[B,BaBaB�B�BgB�BB9B�B�B+B�B�B�BKBeBeB�B�BB7BBBBBBqB�BCB]BxB�B�B�B�B�B�B�B�B�B/BdB�BB�BVBpB�B�B 'B�B B BB BB \B vB �B �B �B �B!B!�B!�B!�B"hB"�B#:B#�B$B$B$�B$�B%B%�B%�B%�B%�B&�B'mB'�B'�B(>B(XB(�B(�B)*B)_B)_B)�B)�B)�B)�B)�B*B*B*B*KB*eB*�B*B*�B+B+B+�B+�B+�B+�B,�B,�B,�B-B-]B-�B.IB.IB.�B.�B/OB/�B/�B/�B/�B/�B/�B/�B0!B0UB1B1AB1vB1�B2B2B2�B2�B3B3MB33B3�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230601065846  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230601065847  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230601065848  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230601065848                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230601065848  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230601065848  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230601070438                      G�O�G�O�G�O�                