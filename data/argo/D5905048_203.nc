CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-23T00:35:25Z creation;2018-01-23T00:35:29Z conversion to V3.1;2019-12-19T07:47:00Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180123003525  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_203                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�F��% 1   @�F��-� @3�PH��dg��q�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|(�@��H@��HAp�A=p�A]p�A}p�A��RA��RA��A��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB�z�BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�C�
C�
C�
C�
C�
C�
C!�
C#�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[�)D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�w�D���D���D�:�D�z�D���D���D�:�D�~D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�S�A�VA�S�A�K�A�A�A�
=A���AŬAş�Aš�A�z�A�`BA�S�A�I�A�G�A�A�A�C�A�C�A�C�A�=qA�=qA�?}A�1'A�$�A�-A�/A�/A��A��A��A��A��`Aĝ�Aė�AēuA�|�A��A��
A�ȴAîAÓuAÏ\AÁA�n�A�VA�?}A�$�A��A�ȴA£�A�5?A�bNA���A���A� �A��A��A�(�A���A�-A�
=A�l�A�bNA�\)A�Q�A�;dA�C�A���A�ƨA�  A�A�ZA�  A��7A�-A�ĜA�oA���A��A�JA��PA��!A�-A�jA��PA���A�ZA�O�A�ȴA�x�A�
=A�;dA�bA�1A��7A�Q�A��\A��A�VA��/A�(�A��!A��A�E�A�A�  A�l�A��A���A�E�A�
=A�$�A��uA�O�A��A��A~��A~JA}x�A|��A{��AyK�AwXAv1AsC�AqK�Ao�hAm�TAl=qAkAkG�Ah�yAg|�Af~�AeO�Ac?}A_��A\n�AX�AW�^AVv�AU�ASƨAP��AOƨAN�uAM�AL�/AJ�AJ�AI33AFȴAD��AC��AB�HAB�+AB�AA��A@r�A@(�A?"�A=�^A<��A;�wA:��A9l�A8VA6�yA5�7A4�/A3C�A2��A2n�A1�A/�7A-�-A,M�A*ZA'C�A%��A$M�A#&�A"v�A"�A!�hA I�A�yA��Al�A��A�A�#A��Av�A�mA��A�\A�
A��A�PA&�A�hA�mA��A/A�Ax�AdZA
��A
=qA	XA5?AJA��AG�A�A��AbNA-AA\)A��A�A��A^5Ax�A �9@�{@��@�r�@���@�@�\)@���@�!@���@�"�@�7@�9@�;d@��@�&�@��;@�V@��@��H@���@�  @ڧ�@�Ĝ@׾w@�v�@�I�@�ȴ@ѡ�@Ѓ@���@�S�@���@·+@͙�@̓u@ʸR@�bN@�9X@� �@�|�@�@��@ě�@�1@¸R@���@�G�@�b@�o@���@���@���@�33@��+@��^@���@��D@�9X@���@�
=@���@�X@��9@�Z@�1'@��w@��!@�~�@�@���@�1'@��m@��;@���@�C�@�"�@�@���@��-@�X@�&�@���@�Z@���@��P@�
=@���@��\@�@�/@��@���@�(�@�(�@��@���@�dZ@��@�=q@���@��7@��h@���@��-@�@���@���@�V@��9@��j@��@�z�@�I�@��@���@�J@��T@���@���@��7@�G�@��@���@��u@�I�@��w@�|�@�33@�"�@�@���@�M�@��@�J@��#@���@���@�x�@�&�@��@�1'@��m@��F@���@��@�\)@�;d@���@�n�@�$�@�J@��@���@��h@���@��/@��j@��@�1'@�ƨ@�l�@�S�@�@�~�@�^5@�=q@�{@��@���@�/@���@���@��@��D@�z�@�j@�1'@���@��@��@�l�@��F@�1@���@�l�@�dZ@�S�@�;d@�
=@���@��+@�n�@�M�@���@���@�hs@��@���@��j@��@��@�z�@�1'@�  @�l�@�+@�ȴ@�^5@�ff@�E�@���@�@�hs@�G�@�7L@��`@���@��j@��D@��D@�z�@�b@���@�|�@�K�@���@��+@�~�@�ff@�^5@�-@��@���@��@�O�@�/@���@���@��9@��@��@�Q�@�9X@��@�  @���@���@�dZ@�S�@�"�@�
=@���@�^5@�-@��@��^@�p�@�?}@�V@�%@��@��@�z�@�1'@���@��@��@��;@��@�S�@�o@���@���@���@��@��H@���@�^5@�5?@��#@���@�x�@�`B@�O�@��@��@���@��@�j@�@��@~�y@~��@~V@~5?@}��@}@}�h@}?}@}`B@}p�@|��@|�j@|�@|��@{�
@zJ@y��@y��@yG�@y7L@x��@xr�@w�@w;d@v�y@v�@v�y@v�+@v@u�@t�@t�/@t�/@t�/@t�@t��@t9X@s��@r=q@q�^@qX@qG�@q7L@q�7@qx�@p��@p�u@p �@o�@o�@nȴ@nv�@nV@nE�@n$�@m@m�@mV@lz�@lj@lZ@k�m@kt�@ko@j��@jM�@i�#@ix�@i%@h��@hbN@h1'@g�@g�@g��@gl�@g;d@g
=@f�R@fv�@fE�@f@e�h@e`B@e/@d��@dz�@c�@cS�@b��@b�\@b-@a��@ax�@a�@`��@`bN@`A�@`A�@` �@`b@_�@_+@^ȴ@^E�@]�T@]�h@]O�@]V@\�@\�@[�F@[��@[33@Z�!@ZM�@Y��@Y��@Yx�@X�`@X�u@Xr�@X1'@W�@W;d@Vȴ@V�+@V$�@U��@UO�@UV@T�@S�
@S�@SdZ@SC�@S"�@R�!@R�\@Rn�@Q��@P��@P�`@P�9@P�@Pr�@P1'@O��@Ol�@N�@NV@N@Mp�@M�@L�/@L9X@Kƨ@K�@KC�@J�H@Jn�@I��@I��@I�7@I7L@I�@H��@HQ�@HA�@G�@G��@G�@G�P@GK�@F��@F�R@Fff@F$�@E@E�h@E`B@D�@DZ@D�@C��@Cƨ@C��@Ct�@B��@B^5@A��@AX@A&�@A%@@��@@�@@1'@?��@?\)@?
=@>�@>�+@>ff@>E�@=�@=��@=�-@=��@=p�@=?}@<��@<Z@<�@;�m@;�@;S�@:�@:��@:n�@:^5@9��@9�^@9��@9��@97L@8��@8Q�@7�@7��@7K�@6�y@6ff@6@5��@5��@5p�@5�@4�/@4�j@4�j@4��@4��@4��@4��@4z�@3��@3�F@3�@3�@3dZ@3o@2�H@2��@2~�@1�#@1�7@1hs@1�@0�`@0��@0�u@0r�@0A�@01'@0  @/��@/�P@/K�@/�@.�y@.�R@.�+@.V@-��@-��@-`B@-�@-V@,�j@,z�@,1@+�F@+�F@+��@+33@+o@*��@*n�@*^5@*^5@*-@)�@)�^@)��@)X@)�@(��@(��@(�9@(�9@(�9@(��@(��@(�@(1'@(  @'�@'��@'��@'|�@'�@&�y@&��@&v�@&{@%��@%`B@%V@$�@$�/@$��@$�@$��@$Z@$1@#�m@#�
@#�
@#��@#dZ@#C�@#33@#"�@"�@"��@"�\@"~�@"=q@!��@!��@!hs@!�@ ��@ ��@ ��@ �@ A�@�@�@l�@�@�y@ȴ@�R@��@ff@5?@�T@�-@/@�/@�@z�@z�@j@9X@�m@��@��@t�@"�@��@�!@�\@n�@-@�@��@�7@X@&�@�`@�9@��@�u@�@Q�@ �@b@�@�w@��@l�@K�@�@�@�R@�R@�+@ff@5?@�T@��@�@p�@?}@�@V@�@�@��@�D@z�@�@��@��@�m@�
@�F@��@dZ@o@@�@�!@n�@=q@-@�@J@�@��@��@��@G�@&�@�@��@��@��@��@Ĝ@�u@bN@A�@1'@ �@  @��@�@��@��@�P@l�@\)@;d@�@��@��@ff@V@{@@�@�@@��@�@`B@O�@?}@�@�j@�@z�@I�@I�@9X@1@�
@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�S�A�VA�S�A�K�A�A�A�
=A���AŬAş�Aš�A�z�A�`BA�S�A�I�A�G�A�A�A�C�A�C�A�C�A�=qA�=qA�?}A�1'A�$�A�-A�/A�/A��A��A��A��A��`Aĝ�Aė�AēuA�|�A��A��
A�ȴAîAÓuAÏ\AÁA�n�A�VA�?}A�$�A��A�ȴA£�A�5?A�bNA���A���A� �A��A��A�(�A���A�-A�
=A�l�A�bNA�\)A�Q�A�;dA�C�A���A�ƨA�  A�A�ZA�  A��7A�-A�ĜA�oA���A��A�JA��PA��!A�-A�jA��PA���A�ZA�O�A�ȴA�x�A�
=A�;dA�bA�1A��7A�Q�A��\A��A�VA��/A�(�A��!A��A�E�A�A�  A�l�A��A���A�E�A�
=A�$�A��uA�O�A��A��A~��A~JA}x�A|��A{��AyK�AwXAv1AsC�AqK�Ao�hAm�TAl=qAkAkG�Ah�yAg|�Af~�AeO�Ac?}A_��A\n�AX�AW�^AVv�AU�ASƨAP��AOƨAN�uAM�AL�/AJ�AJ�AI33AFȴAD��AC��AB�HAB�+AB�AA��A@r�A@(�A?"�A=�^A<��A;�wA:��A9l�A8VA6�yA5�7A4�/A3C�A2��A2n�A1�A/�7A-�-A,M�A*ZA'C�A%��A$M�A#&�A"v�A"�A!�hA I�A�yA��Al�A��A�A�#A��Av�A�mA��A�\A�
A��A�PA&�A�hA�mA��A/A�Ax�AdZA
��A
=qA	XA5?AJA��AG�A�A��AbNA-AA\)A��A�A��A^5Ax�A �9@�{@��@�r�@���@�@�\)@���@�!@���@�"�@�7@�9@�;d@��@�&�@��;@�V@��@��H@���@�  @ڧ�@�Ĝ@׾w@�v�@�I�@�ȴ@ѡ�@Ѓ@���@�S�@���@·+@͙�@̓u@ʸR@�bN@�9X@� �@�|�@�@��@ě�@�1@¸R@���@�G�@�b@�o@���@���@���@�33@��+@��^@���@��D@�9X@���@�
=@���@�X@��9@�Z@�1'@��w@��!@�~�@�@���@�1'@��m@��;@���@�C�@�"�@�@���@��-@�X@�&�@���@�Z@���@��P@�
=@���@��\@�@�/@��@���@�(�@�(�@��@���@�dZ@��@�=q@���@��7@��h@���@��-@�@���@���@�V@��9@��j@��@�z�@�I�@��@���@�J@��T@���@���@��7@�G�@��@���@��u@�I�@��w@�|�@�33@�"�@�@���@�M�@��@�J@��#@���@���@�x�@�&�@��@�1'@��m@��F@���@��@�\)@�;d@���@�n�@�$�@�J@��@���@��h@���@��/@��j@��@�1'@�ƨ@�l�@�S�@�@�~�@�^5@�=q@�{@��@���@�/@���@���@��@��D@�z�@�j@�1'@���@��@��@�l�@��F@�1@���@�l�@�dZ@�S�@�;d@�
=@���@��+@�n�@�M�@���@���@�hs@��@���@��j@��@��@�z�@�1'@�  @�l�@�+@�ȴ@�^5@�ff@�E�@���@�@�hs@�G�@�7L@��`@���@��j@��D@��D@�z�@�b@���@�|�@�K�@���@��+@�~�@�ff@�^5@�-@��@���@��@�O�@�/@���@���@��9@��@��@�Q�@�9X@��@�  @���@���@�dZ@�S�@�"�@�
=@���@�^5@�-@��@��^@�p�@�?}@�V@�%@��@��@�z�@�1'@���@��@��@��;@��@�S�@�o@���@���@���@��@��H@���@�^5@�5?@��#@���@�x�@�`B@�O�@��@��@���@��@�j@�@��@~�y@~��@~V@~5?@}��@}@}�h@}?}@}`B@}p�@|��@|�j@|�@|��@{�
@zJ@y��@y��@yG�@y7L@x��@xr�@w�@w;d@v�y@v�@v�y@v�+@v@u�@t�@t�/@t�/@t�/@t�@t��@t9X@s��@r=q@q�^@qX@qG�@q7L@q�7@qx�@p��@p�u@p �@o�@o�@nȴ@nv�@nV@nE�@n$�@m@m�@mV@lz�@lj@lZ@k�m@kt�@ko@j��@jM�@i�#@ix�@i%@h��@hbN@h1'@g�@g�@g��@gl�@g;d@g
=@f�R@fv�@fE�@f@e�h@e`B@e/@d��@dz�@c�@cS�@b��@b�\@b-@a��@ax�@a�@`��@`bN@`A�@`A�@` �@`b@_�@_+@^ȴ@^E�@]�T@]�h@]O�@]V@\�@\�@[�F@[��@[33@Z�!@ZM�@Y��@Y��@Yx�@X�`@X�u@Xr�@X1'@W�@W;d@Vȴ@V�+@V$�@U��@UO�@UV@T�@S�
@S�@SdZ@SC�@S"�@R�!@R�\@Rn�@Q��@P��@P�`@P�9@P�@Pr�@P1'@O��@Ol�@N�@NV@N@Mp�@M�@L�/@L9X@Kƨ@K�@KC�@J�H@Jn�@I��@I��@I�7@I7L@I�@H��@HQ�@HA�@G�@G��@G�@G�P@GK�@F��@F�R@Fff@F$�@E@E�h@E`B@D�@DZ@D�@C��@Cƨ@C��@Ct�@B��@B^5@A��@AX@A&�@A%@@��@@�@@1'@?��@?\)@?
=@>�@>�+@>ff@>E�@=�@=��@=�-@=��@=p�@=?}@<��@<Z@<�@;�m@;�@;S�@:�@:��@:n�@:^5@9��@9�^@9��@9��@97L@8��@8Q�@7�@7��@7K�@6�y@6ff@6@5��@5��@5p�@5�@4�/@4�j@4�j@4��@4��@4��@4��@4z�@3��@3�F@3�@3�@3dZ@3o@2�H@2��@2~�@1�#@1�7@1hs@1�@0�`@0��@0�u@0r�@0A�@01'@0  @/��@/�P@/K�@/�@.�y@.�R@.�+@.V@-��@-��@-`B@-�@-V@,�j@,z�@,1@+�F@+�F@+��@+33@+o@*��@*n�@*^5@*^5@*-@)�@)�^@)��@)X@)�@(��@(��@(�9@(�9@(�9@(��@(��@(�@(1'@(  @'�@'��@'��@'|�@'�@&�y@&��@&v�@&{@%��@%`B@%V@$�@$�/@$��@$�@$��@$Z@$1@#�m@#�
@#�
@#��@#dZ@#C�@#33@#"�@"�@"��@"�\@"~�@"=q@!��@!��@!hs@!�@ ��@ ��@ ��@ �@ A�@�@�@l�@�@�y@ȴ@�R@��@ff@5?@�T@�-@/@�/@�@z�@z�@j@9X@�m@��@��@t�@"�@��@�!@�\@n�@-@�@��@�7@X@&�@�`@�9@��@�u@�@Q�@ �@b@�@�w@��@l�@K�@�@�@�R@�R@�+@ff@5?@�T@��@�@p�@?}@�@V@�@�@��@�D@z�@�@��@��@�m@�
@�F@��@dZ@o@@�@�!@n�@=q@-@�@J@�@��@��@��@G�@&�@�@��@��@��@��@Ĝ@�u@bN@A�@1'@ �@  @��@�@��@��@�P@l�@\)@;d@�@��@��@ff@V@{@@�@�@@��@�@`B@O�@?}@�@�j@�@z�@I�@I�@9X@1@�
@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B��B��BDBPBDB\B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B!�B�B�B�B#�B!�B �B"�B!�B"�B"�B$�B%�B%�B+B)�B!�B�B��B%�B?}B:^B5?B$�B,B2-BH�BM�BZBW
BP�BE�B/BuBoB1B1B
=BhB1B��B�B��B�BB�B�RB��B�dB�qB�XB�B��B{�Bv�Br�Bl�BaHBB�BN�BL�B;dB!�B+B+B
��B
�NB
�HB
�B
��B
��B
w�B
Q�B
A�B
>wB
F�B
`BB
iyB
]/B
^5B
bNB
K�B
=qB
>wB
A�B
7LB
/B
 �B
JB	��B	��B	�)B	��B	��B	ƨB	�}B	ÖB	�^B	��B	��B	��B	�7B	q�B	N�B	D�B	2-B	>wB	8RB	0!B	'�B	bB	uB	hB	PB	DB	B	B��B�sB�;B�sB�TB�mB�ZB�;B��B��B��BĜBƨB�}B�XB�-B�B��B��B��B��B��B��B�\B� B}�B~�Bp�Be`Bo�Bt�Bs�Bx�B}�Bx�Bl�BhsBt�BhsB`BBbNBk�BffBhsBaHBM�BT�B_;Be`BdZBXBC�B@�BL�BE�BP�B\)BaHBYBT�BR�BQ�BcTBaHBbNBdZBdZB^5B`BB_;BVBM�B\)Be`B_;BXB^5BVBe`Bk�BhsB_;B[#B[#BXBW
BM�B`BBhsBbNBe`BdZB`BBZBffBjBjBcTB`BB]/B^5B]/BT�B^5BbNBffBm�Bo�Bp�Bp�Bk�Bl�Bl�Bt�B�PB�JB�+B�B�DB�hB�hB�VB�{B��B��B��B��B��B��B�B�-B�?B�RB�wB��B��BBŢBĜB��B�B�B�B�
B�TB�NB�NB�B�B��B�B�B��B��B��B��B	B	B	%B	1B	DB	PB	\B	uB	�B	�B	�B	#�B	&�B	'�B	,B	-B	,B	0!B	0!B	49B	:^B	@�B	D�B	H�B	K�B	M�B	Q�B	R�B	R�B	XB	_;B	`BB	`BB	bNB	aHB	`BB	aHB	jB	k�B	n�B	p�B	p�B	t�B	|�B	~�B	� B	�B	�1B	�=B	�PB	�VB	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�'B	�9B	�RB	�XB	�dB	�dB	�^B	��B	B	ÖB	B	B	ÖB	ĜB	ĜB	ÖB	ȴB	ɺB	ɺB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�NB	�BB	�TB	�`B	�`B	�`B	�`B	�ZB	�`B	�mB	�fB	�`B	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
%B
+B
+B
+B
1B
	7B
DB
JB
DB
DB
DB
DB
JB
VB
VB
PB
DB
PB
PB
\B
bB
bB
hB
oB
uB
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
 �B
�B
�B
�B
!�B
"�B
#�B
#�B
$�B
%�B
#�B
#�B
#�B
$�B
#�B
!�B
#�B
$�B
#�B
#�B
"�B
"�B
$�B
#�B
%�B
%�B
#�B
#�B
$�B
#�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
(�B
(�B
)�B
(�B
(�B
)�B
)�B
(�B
)�B
)�B
)�B
'�B
&�B
+B
+B
,B
,B
,B
-B
-B
.B
.B
/B
0!B
0!B
/B
.B
.B
.B
/B
/B
0!B
1'B
1'B
0!B
0!B
1'B
2-B
1'B
1'B
2-B
33B
33B
33B
33B
49B
5?B
49B
33B
49B
49B
6FB
5?B
5?B
5?B
7LB
6FB
5?B
8RB
9XB
9XB
9XB
9XB
:^B
:^B
8RB
9XB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
=qB
=qB
>wB
>wB
=qB
?}B
@�B
@�B
?}B
@�B
@�B
B�B
B�B
B�B
B�B
A�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
E�B
F�B
F�B
G�B
I�B
I�B
I�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
M�B
O�B
O�B
N�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
S�B
T�B
VB
T�B
T�B
T�B
VB
S�B
S�B
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
XB
XB
ZB
YB
ZB
[#B
ZB
ZB
ZB
[#B
\)B
\)B
[#B
\)B
\)B
\)B
]/B
^5B
]/B
]/B
]/B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
_;B
`BB
_;B
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
bNB
bNB
bNB
bNB
bNB
aHB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
bNB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
e`B
e`B
e`B
ffB
gmB
gmB
gmB
ffB
ffB
ffB
gmB
ffB
gmB
hsB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
hsB
hsB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
o�B
p�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
s�B
s�B
s�B
t�B
s�B
t�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B��B��B�B�B��BDBjB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B=B#B"�B!�B!BqBB#�B!�B!B#B"B# B# B%,B&2B&fB+kB*�B#�B�B�B)_BA�B<�B8B)*B/OB5tBJ�BOBZBW?BQNBFtB1vB�B�B
#B
XB�BoB	lB B�-B�yB�B�QB�B��B�<B�.B��B��B�
B��By	Bs�Bm�Bb�BE�BO\BMPB="B$�B
�B�B
�B
��B
� B
��B
āB
�|B
}B
V�B
E�B
BB
IlB
a-B
jeB
_pB
_�B
c�B
OB
@ B
?�B
B[B
88B
0UB
"�B
(B	�BB	��B	ߊB	�{B	�&B	��B	�UB	�gB	��B	�zB	��B	�B	�DB	t�B	S�B	H�B	6FB	?�B	:B	1�B	*eB	�B	�B	B	�B	~B	�B	mB	 �B�B��B�B�tB�
B�,B�'B�TB��B�jBƎB��B�B��B��B��B��B�|B�B��B��B�~B�hB�uB��B�UBs�BiDBq�BvzBu?By�B~�By�BncBj0BuZBjeBb�BdBl�Bg�Bi_Bb�BP�BV�B`'Be�Bd�BY1BF%BB�BN�BG�BRTB\�Ba�BZBVBTFBSuBc�Ba�Bb�Bd�Bd�B_B`�B_�BW?BO�B]Be�B`\BY�B_�BXEBfLBlBiyB`�B]B\�BY�BYKBP�BabBiDBcnBfLBeFBa�B[�BgRBkQBkkBd�Ba|B^�B_;B^OBV�B_VBc:BgRBn/Bp!BqBqBlqBm�BnBv+B�jB��B��B�9B��B��B� B�\B�MB�#B��B��B��B��B��B��B��B��B�	B��B��B� B�-B�%B�mB�bB�SB�EBخB��B�B��B�:B�B��B��B�B�B�B�	B�RB��B	oB	SB	tB	�B	�B	�B	�B	�B	�B	1B	)B	$B	'RB	(>B	,=B	-]B	,�B	0�B	0�B	4�B	:�B	@�B	D�B	H�B	K�B	M�B	RB	S@B	S�B	X_B	_VB	`vB	`�B	b�B	a�B	aB	a�B	j�B	k�B	n�B	p�B	p�B	uB	}"B	HB	�iB	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�)B	��B	�,B	�8B	�*B	�=B	�IB	�oB	��B	�vB	��B	�lB	��B	�B	��B	��B	��B	��B	ðB	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�#B	�"B	�B	�B	�B	�B	� B	�:B	� B	�,B	�@B	�2B	�B	�NB	�B	�nB	�zB	�B	�zB	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	��B	��B	��B	��B	�B	��B	�B	�B	�	B	�	B	�	B	�B	�	B	�B	�+B	�B	�B	�8B	�DB	�B	�B	�B	�HB	�BB	�.B	�(B	�HB
 4B
 OB
;B
'B
GB
GB
GB
MB
MB
SB
gB
mB
gB
YB
mB
mB
gB
tB
zB
zB
zB
�B
	�B
^B
dB
xB
xB
�B
xB
~B
VB
pB
�B
�B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
2B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
B
B
B
 B
"�B
"�B
"�B
!�B
!B
�B
B
IB
!�B
#B
#�B
#�B
$�B
&B
$&B
$B
$&B
%B
$&B
"4B
#�B
$�B
$B
$B
# B
# B
%B
$B
%�B
&B
$&B
$B
%B
$B
&B
%,B
&2B
'8B
'8B
($B
($B
($B
)B
*B
)*B
)*B
*0B
)*B
)B
*0B
*0B
)DB
*0B
*0B
*0B
(>B
'8B
+B
+6B
,=B
,WB
,=B
-)B
-]B
.IB
.cB
/OB
0;B
0UB
/OB
.cB
.cB
.cB
/iB
/OB
0;B
1[B
1AB
0oB
0oB
1[B
2aB
1vB
1vB
2|B
3MB
3hB
3�B
3�B
4nB
5tB
4nB
3�B
4�B
4�B
6zB
5�B
5�B
5�B
7�B
6�B
5�B
8�B
9rB
9�B
9rB
9�B
:xB
:�B
8�B
9�B
<�B
<�B
<�B
<�B
<�B
;�B
;�B
;�B
<�B
=�B
=�B
>�B
>�B
=�B
?�B
@�B
@�B
?�B
@�B
@�B
B�B
B�B
B�B
B�B
A�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
FB
F�B
F�B
G�B
I�B
I�B
I�B
H�B
I�B
IB
IB
I�B
J�B
J�B
K�B
K�B
K�B
MB
MB
MB
MB
K�B
LB
LB
MB
MB
MB
NB
NB
NB
OB
OB
N"B
O�B
O�B
OB
N"B
N"B
N�B
O(B
PB
PB
P.B
PB
QB
R B
S&B
S&B
S&B
S&B
T,B
UB
UB
T�B
UB
UB
T,B
S&B
T,B
UB
VB
U2B
UB
U2B
V9B
TFB
TFB
V9B
VB
V9B
W?B
W?B
XB
XEB
XEB
XEB
XEB
XEB
XEB
X+B
XEB
YKB
YKB
YKB
XEB
XEB
ZQB
YKB
Z7B
[=B
ZQB
ZQB
ZkB
[WB
\CB
\]B
[WB
\]B
\CB
\]B
]IB
^5B
]IB
]IB
]dB
^jB
]IB
]dB
^jB
_pB
_VB
_;B
_VB
_VB
_;B
_pB
^jB
_pB
`\B
_VB
_pB
_pB
_pB
_VB
_VB
`vB
_�B
_pB
`vB
`vB
bhB
bhB
bhB
bhB
bhB
abB
a|B
cnB
cTB
cnB
c�B
c�B
c�B
dtB
dtB
c�B
c�B
c�B
c�B
c�B
b�B
d�B
d�B
d�B
d�B
ezB
e`B
e�B
d�B
dtB
e�B
ezB
e�B
f�B
g�B
g�B
g�B
f�B
f�B
f�B
g�B
f�B
g�B
h�B
i�B
i�B
i�B
i�B
h�B
i�B
i�B
i�B
h�B
h�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
o�B
p�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
s�B
s�B
s�B
t�B
s�B
t�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
w�B
y	B
y	B
y�B
zB
zB
z�B
{B
|1111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111311111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801270034352018012700343520180127003435201806221325162018062213251620180622132516201804050728422018040507284220180405072842  JA  ARFMdecpA19c                                                                20180123093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180123003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180123003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180123003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180123003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180123003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180123003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180123003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180123003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180123003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20180123005533                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180123153524  CV  JULD            G�O�G�O�F�5�                JM  ARSQJMQC2.0                                                                 20180124000000  CF  PSAL_ADJUSTED_QCC  C�  G�O�                JM  ARCAJMQC2.0                                                                 20180126153435  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180126153435  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222842  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042516  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                