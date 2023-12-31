CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-05T21:35:08Z creation;2017-11-05T21:35:12Z conversion to V3.1;2019-12-19T07:57:27Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171105213508  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_176                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�32���1   @�33�`�@;;J#9���dl䎊q�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~y�D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�<�Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @(��@u@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��HB��B��B�z�B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D|)D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}�\D~o\D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�w�DȺ�D���D�7�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D�{D�'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�VA�VA�Q�A�O�A�O�A�M�A�M�A�Q�A�M�A�7LA�  AɅA�bA��mAȋDA�7LA�
=AǙ�A�33A�&�A�$�A�{A�JA�{A�+A�(�A� �A��A�bA�
=A���A��/A�AƬAƕ�A�x�A�`BA�$�A�  Aĺ^A��FA�l�A��A��A��#A��HA�l�A�`BA� �A��HA�r�A�A�33A���A��DA��A��TA�1'A��A� �A�l�A�ƨA�|�A� �A���A��FA���A�ffA�JA�~�A�+A��HA�bNA�I�A��hA��yA��A���A�%A��/A�p�A���A��`A�33A�\)A�%A���A���A�1'A�ZA� �A�  A���A�z�A�%A���A�?}A��A�z�A�mA~��A}��A|z�A{��Az��AzbNAy�PAxM�AwAw&�Av�jAv�DAuG�At�Asl�ArĜAq�Ap��An��Am�wAl�9Aj��Ai��Ah��Ah��Ah=qAgl�AfM�AeK�Ad~�Ad-Ad�Ac�-Ab�`Aa��A`��A`r�A`VA_��A^(�A\��A\E�A\1A[�mAZ�`AY�AX��AX�AWAW?}AV�/AV��AV5?AU��AU;dASXAQ�TAQl�AQO�AQ�AP�AO?}AM"�AK7LAI�TAI+AH�AHI�AG�mAGp�AG�AF�AF1ACp�AA�wAAK�A@��A@  A?�
A?`BA>�jA>Q�A=ƨA=33A<�`A<��A<5?A;��A:I�A9��A9\)A97LA8��A8�\A7�A5p�A4v�A3�7A2�RA2(�A1��A1x�A0��A/p�A.��A.(�A-��A-�hA-|�A,�A+t�A*n�A)�;A(ĜA'�TA'C�A&VA%�A%��A%oA$r�A#
=A!ƨA ��A (�A"�A/Ap�A�jA�DAv�An�A1'A�A��A9XA;dA1'A�#A|�A�AbNA"�A�FA�AZAl�A
��A
�+A	�A	|�A��AA�A=qA �A�FA��AdZAC�A33A��A��A�!AZA�A�A$�A ��@��w@�ff@���@�\)@�@��H@��#@�  @�dZ@�@��\@��@�O�@�j@�R@�h@�b@�hs@�j@�l�@���@�b@�V@�@�(�@�ƨ@��@�^5@��T@�%@��@��@�33@���@���@�v�@Չ7@�  @�M�@Л�@�  @ϝ�@·+@�X@̓u@�A�@��@˕�@�|�@�^5@���@�%@�|�@Ə\@�M�@ź^@�G�@��@�Ĝ@�Z@�+@���@���@�G�@�Q�@�b@��
@�33@�ff@��-@�p�@�7L@��@���@�b@��@��T@�`B@��@���@�bN@�(�@�  @���@�K�@�5?@�"�@��#@��/@�9X@���@�z�@��y@��@��9@�Ĝ@�r�@�1'@���@�t�@��H@�$�@�O�@�Z@���@�ff@��`@�33@��H@���@�M�@�@���@�O�@��`@���@�z�@�Z@�z�@�bN@���@�ƨ@��@���@� �@��@�n�@��R@�ff@���@��@�G�@�;d@���@���@�5?@�M�@��y@��@���@��F@�t�@��
@��@�I�@�bN@��@��P@��@�|�@�33@��@���@�V@��@�A�@�E�@�33@�&�@��@���@���@���@�\)@��\@��`@�dZ@��@��u@�|�@�o@�ȴ@�M�@�@�hs@��@���@��@��@��m@�ƨ@�l�@�+@�
=@���@�V@�{@��@��T@��^@�p�@�V@��@�Ĝ@���@��@~�R@~{@}�T@}�h@}�@|z�@|1@{��@{33@z��@z��@z�!@z��@z��@z�\@z~�@z=q@y��@yX@y%@xĜ@xQ�@w��@v�R@v5?@u�@s��@s��@s"�@r�@qx�@qhs@qX@q%@p��@p��@pb@ol�@n�y@n��@n��@nv�@nff@nff@nff@nE�@m�@m��@m�@m?}@l��@l��@lZ@k��@kt�@j�H@j��@j-@i��@i��@iX@i&�@hĜ@hr�@hQ�@h1'@g��@g\)@g+@f��@f�R@f@eO�@d�@d�/@d�@d��@e?}@e�@e�-@e`B@d��@d�@d��@d��@d�@d�@d�@d��@d�@d��@d�D@d��@dZ@c�@c"�@b~�@a��@`�`@`Ĝ@`bN@`b@`  @_��@_;d@_�@^��@^�y@^E�@]�@]��@]�-@]`B@\�@\z�@\�j@\�j@\��@\�j@\�@\�D@\j@\I�@\(�@\(�@\�@[�
@Z�\@Y��@Y7L@Y%@X�`@XĜ@XbN@Xb@X  @W�@X  @X  @X  @X  @X  @X  @X  @X  @X  @W��@W|�@W��@W�;@W�;@W;d@V��@V��@V��@V��@V��@Vv�@VV@VE�@Vv�@V��@U�@U`B@T��@TI�@T�@Sƨ@S�F@S�F@S��@S��@SdZ@SS�@S"�@R�@R�@R�!@R�@Q��@Q��@Q��@P�@O�P@N�y@M�T@M�@L�@L��@LZ@L9X@L(�@K�m@Kƨ@K��@J�!@J�@I�@JJ@Ihs@IX@I7L@H�u@G|�@F�@F5?@E/@DZ@C�
@C33@B��@Bn�@B~�@C33@Ct�@C�@C��@CC�@B�\@C33@C"�@B=q@A&�@?+@>��@>��@?+@>ȴ@>��@>V@>{@>@=�@=�T@=@=`B@=V@<��@<I�@<I�@<(�@<(�@<�@;�
@;�@;S�@;"�@:��@:�\@:=q@9�@9��@9�^@9��@9�7@9hs@9G�@8��@8��@8��@8�u@8�u@8r�@81'@7�@7��@7�w@7�@7��@7��@7�P@7K�@7+@7+@7+@7�@6��@6ff@5V@4�@4j@3�m@3��@3S�@333@2��@2��@2��@2=q@1x�@1%@0�9@0�@01'@/�@/\)@.��@.ȴ@.ȴ@.��@.E�@.{@-�-@-p�@-?}@,�@,Z@,(�@,1@+��@+�m@+ƨ@+�F@+"�@*�\@*�@)�@)�^@)7L@(bN@(A�@'�;@'��@'��@'�w@'�w@'K�@'�@'
=@&��@&ȴ@&E�@&@%�@%@%�@%O�@%O�@%O�@%`B@%`B@%V@$��@$Z@#�
@#S�@"��@"n�@"^5@"M�@"=q@"�@!�^@!&�@!�@ �`@ ��@ r�@   @��@��@ȴ@��@�+@V@�@��@�@p�@O�@��@�j@Z@(�@�@1@��@ƨ@��@�@S�@��@M�@�@�^@��@��@��@��@��@��@��@�7@X@G�@%@��@��@��@�@1'@  @�@|�@+@�@E�@$�@�@@�h@p�@O�@?}@�@V@�@�D@�@�
@�F@�@S�@C�@C�@C�@"�@�@��@��@�\@~�@^5@M�@=q@J@�@�#@��@x�@X@�@�`@��@Ĝ@�9@r�@ �@b@  @  @�w@�P@�P@l�@�@��@v�@v�@ff@5?@{@@�@�T@��@`B@V@V@V@�@��@j@I�@9X@�@�F@C�@"�@o@@@
��@
�\@
M�@
-@
�@
J@
J@	��@	��@	�@	��@	�7@	x�@	hs@	X@	�@	�@��@Ĝ@��@�u@bN@Q�@  @�@|�@l�@K�@
=@�@�R@��@ff@5?@@�@�T@@��@`B@O�@�@��@�@�D@I�@�@��@�
@�
@ƨ@ƨ@�F@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�VA�VA�Q�A�O�A�O�A�M�A�M�A�Q�A�M�A�7LA�  AɅA�bA��mAȋDA�7LA�
=AǙ�A�33A�&�A�$�A�{A�JA�{A�+A�(�A� �A��A�bA�
=A���A��/A�AƬAƕ�A�x�A�`BA�$�A�  Aĺ^A��FA�l�A��A��A��#A��HA�l�A�`BA� �A��HA�r�A�A�33A���A��DA��A��TA�1'A��A� �A�l�A�ƨA�|�A� �A���A��FA���A�ffA�JA�~�A�+A��HA�bNA�I�A��hA��yA��A���A�%A��/A�p�A���A��`A�33A�\)A�%A���A���A�1'A�ZA� �A�  A���A�z�A�%A���A�?}A��A�z�A�mA~��A}��A|z�A{��Az��AzbNAy�PAxM�AwAw&�Av�jAv�DAuG�At�Asl�ArĜAq�Ap��An��Am�wAl�9Aj��Ai��Ah��Ah��Ah=qAgl�AfM�AeK�Ad~�Ad-Ad�Ac�-Ab�`Aa��A`��A`r�A`VA_��A^(�A\��A\E�A\1A[�mAZ�`AY�AX��AX�AWAW?}AV�/AV��AV5?AU��AU;dASXAQ�TAQl�AQO�AQ�AP�AO?}AM"�AK7LAI�TAI+AH�AHI�AG�mAGp�AG�AF�AF1ACp�AA�wAAK�A@��A@  A?�
A?`BA>�jA>Q�A=ƨA=33A<�`A<��A<5?A;��A:I�A9��A9\)A97LA8��A8�\A7�A5p�A4v�A3�7A2�RA2(�A1��A1x�A0��A/p�A.��A.(�A-��A-�hA-|�A,�A+t�A*n�A)�;A(ĜA'�TA'C�A&VA%�A%��A%oA$r�A#
=A!ƨA ��A (�A"�A/Ap�A�jA�DAv�An�A1'A�A��A9XA;dA1'A�#A|�A�AbNA"�A�FA�AZAl�A
��A
�+A	�A	|�A��AA�A=qA �A�FA��AdZAC�A33A��A��A�!AZA�A�A$�A ��@��w@�ff@���@�\)@�@��H@��#@�  @�dZ@�@��\@��@�O�@�j@�R@�h@�b@�hs@�j@�l�@���@�b@�V@�@�(�@�ƨ@��@�^5@��T@�%@��@��@�33@���@���@�v�@Չ7@�  @�M�@Л�@�  @ϝ�@·+@�X@̓u@�A�@��@˕�@�|�@�^5@���@�%@�|�@Ə\@�M�@ź^@�G�@��@�Ĝ@�Z@�+@���@���@�G�@�Q�@�b@��
@�33@�ff@��-@�p�@�7L@��@���@�b@��@��T@�`B@��@���@�bN@�(�@�  @���@�K�@�5?@�"�@��#@��/@�9X@���@�z�@��y@��@��9@�Ĝ@�r�@�1'@���@�t�@��H@�$�@�O�@�Z@���@�ff@��`@�33@��H@���@�M�@�@���@�O�@��`@���@�z�@�Z@�z�@�bN@���@�ƨ@��@���@� �@��@�n�@��R@�ff@���@��@�G�@�;d@���@���@�5?@�M�@��y@��@���@��F@�t�@��
@��@�I�@�bN@��@��P@��@�|�@�33@��@���@�V@��@�A�@�E�@�33@�&�@��@���@���@���@�\)@��\@��`@�dZ@��@��u@�|�@�o@�ȴ@�M�@�@�hs@��@���@��@��@��m@�ƨ@�l�@�+@�
=@���@�V@�{@��@��T@��^@�p�@�V@��@�Ĝ@���@��@~�R@~{@}�T@}�h@}�@|z�@|1@{��@{33@z��@z��@z�!@z��@z��@z�\@z~�@z=q@y��@yX@y%@xĜ@xQ�@w��@v�R@v5?@u�@s��@s��@s"�@r�@qx�@qhs@qX@q%@p��@p��@pb@ol�@n�y@n��@n��@nv�@nff@nff@nff@nE�@m�@m��@m�@m?}@l��@l��@lZ@k��@kt�@j�H@j��@j-@i��@i��@iX@i&�@hĜ@hr�@hQ�@h1'@g��@g\)@g+@f��@f�R@f@eO�@d�@d�/@d�@d��@e?}@e�@e�-@e`B@d��@d�@d��@d��@d�@d�@d�@d��@d�@d��@d�D@d��@dZ@c�@c"�@b~�@a��@`�`@`Ĝ@`bN@`b@`  @_��@_;d@_�@^��@^�y@^E�@]�@]��@]�-@]`B@\�@\z�@\�j@\�j@\��@\�j@\�@\�D@\j@\I�@\(�@\(�@\�@[�
@Z�\@Y��@Y7L@Y%@X�`@XĜ@XbN@Xb@X  @W�@X  @X  @X  @X  @X  @X  @X  @X  @X  @W��@W|�@W��@W�;@W�;@W;d@V��@V��@V��@V��@V��@Vv�@VV@VE�@Vv�@V��@U�@U`B@T��@TI�@T�@Sƨ@S�F@S�F@S��@S��@SdZ@SS�@S"�@R�@R�@R�!@R�@Q��@Q��@Q��@P�@O�P@N�y@M�T@M�@L�@L��@LZ@L9X@L(�@K�m@Kƨ@K��@J�!@J�@I�@JJ@Ihs@IX@I7L@H�u@G|�@F�@F5?@E/@DZ@C�
@C33@B��@Bn�@B~�@C33@Ct�@C�@C��@CC�@B�\@C33@C"�@B=q@A&�@?+@>��@>��@?+@>ȴ@>��@>V@>{@>@=�@=�T@=@=`B@=V@<��@<I�@<I�@<(�@<(�@<�@;�
@;�@;S�@;"�@:��@:�\@:=q@9�@9��@9�^@9��@9�7@9hs@9G�@8��@8��@8��@8�u@8�u@8r�@81'@7�@7��@7�w@7�@7��@7��@7�P@7K�@7+@7+@7+@7�@6��@6ff@5V@4�@4j@3�m@3��@3S�@333@2��@2��@2��@2=q@1x�@1%@0�9@0�@01'@/�@/\)@.��@.ȴ@.ȴ@.��@.E�@.{@-�-@-p�@-?}@,�@,Z@,(�@,1@+��@+�m@+ƨ@+�F@+"�@*�\@*�@)�@)�^@)7L@(bN@(A�@'�;@'��@'��@'�w@'�w@'K�@'�@'
=@&��@&ȴ@&E�@&@%�@%@%�@%O�@%O�@%O�@%`B@%`B@%V@$��@$Z@#�
@#S�@"��@"n�@"^5@"M�@"=q@"�@!�^@!&�@!�@ �`@ ��@ r�@   @��@��@ȴ@��@�+@V@�@��@�@p�@O�@��@�j@Z@(�@�@1@��@ƨ@��@�@S�@��@M�@�@�^@��@��@��@��@��@��@��@�7@X@G�@%@��@��@��@�@1'@  @�@|�@+@�@E�@$�@�@@�h@p�@O�@?}@�@V@�@�D@�@�
@�F@�@S�@C�@C�@C�@"�@�@��@��@�\@~�@^5@M�@=q@J@�@�#@��@x�@X@�@�`@��@Ĝ@�9@r�@ �@b@  @  @�w@�P@�P@l�@�@��@v�@v�@ff@5?@{@@�@�T@��@`B@V@V@V@�@��@j@I�@9X@�@�F@C�@"�@o@@@
��@
�\@
M�@
-@
�@
J@
J@	��@	��@	�@	��@	�7@	x�@	hs@	X@	�@	�@��@Ĝ@��@�u@bN@Q�@  @�@|�@l�@K�@
=@�@�R@��@ff@5?@@�@�T@@��@`B@O�@�@��@�@�D@I�@�@��@�
@�
@ƨ@ƨ@�F@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�#B�;B�5B�ZB�B��B��B��B��BB
=BPBPB\B\B\BbB�B�B�B �B(�B-B.B/B�B �BB
=B
=BJBB�B�yB�sB�B��BǮB�9B��B��B�VB�VB�=B�+By�B~�B�B|�By�Bw�Bv�Bs�Bo�Bk�BhsB[#BB�B5?B.B'�B!�B�B\BBB
��B
�B
�yB
�;B
�5B
�)B
�B
��B
��B
ȴB
ƨB
B
�jB
�FB
��B
��B
��B
��B
�{B
�\B
�1B
� B
{�B
v�B
r�B
k�B
dZB
`BB
^5B
YB
W
B
N�B
E�B
A�B
=qB
49B
.B
"�B
�B
oB
1B
B
B	��B	��B	��B	�B	�B	�fB	�ZB	�NB	�;B	�#B	�B	��B	��B	��B	ɺB	��B	�jB	�jB	�^B	�LB	�'B	��B	��B	��B	��B	��B	�{B	�oB	�VB	�DB	�B	{�B	q�B	r�B	r�B	o�B	jB	bNB	W
B	M�B	H�B	G�B	F�B	E�B	C�B	@�B	>wB	<jB	5?B	(�B	!�B	%�B	$�B	"�B	"�B	 �B	�B	�B	�B	�B	�B	{B	oB	VB	+B	%B	B	B	B	  B��B�B�B�sB�`B�TB�HB�;B�B��B��B��B��B��B��BȴB��B�wB�jB�LB�9B�3B�B�!B�B�B��B��B��B��B��B�hB�7B�+B�7B�DB�=B�7B�B|�Br�BgmBjBffBgmBe`BbNB^5BYBT�BVBVBR�BQ�BQ�BN�BM�BK�BL�BN�BM�BK�BL�BK�BK�BK�BJ�BI�BH�BE�B?}B=qB:^B9XB7LB9XB8RB7LB8RB8RB5?B33B5?B5?B5?B49B33B2-B.B.B-B)�B.B/B-B/B.B-B0!B0!B.B/B.B,B(�B#�B&�B#�B+B/B.B,B.B/B5?B6FB5?B6FB9XB;dB<jB<jB<jB:^B;dB<jB:^B=qB@�B?}B@�BA�B@�B@�B=qB;dB6FB33B8RB<jB=qB<jB>wB?}BB�BB�BB�BB�BA�BB�BC�BG�BI�BJ�BK�BL�BL�BK�BK�BG�BC�BL�BR�BaHBcTBe`BbNBffBl�Br�Br�Bs�Bt�Bu�Bv�Bu�Bt�Bs�Bp�Bt�Bx�Bx�B|�B|�B{�B{�B{�Bz�Bz�B~�B�B�B�B�PB�{B��B��B��B��B�9BÖBȴBǮBƨB��B�LB�?B�?B�RB�dB�wB��BɺB��B��B�B�;B�HB�TB�ZB�`B�B�B�B�B�B�B�B�B�B�`B�yB	B	B	B	B	  B	B	B	  B��B��B��B��B��B	B	B	B	%B	
=B	DB	JB	VB	uB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	%�B	&�B	)�B	.B	.B	/B	/B	49B	:^B	=qB	>wB	?}B	A�B	C�B	E�B	G�B	H�B	K�B	K�B	K�B	K�B	L�B	L�B	K�B	K�B	N�B	O�B	P�B	Q�B	Q�B	S�B	XB	YB	]/B	aHB	dZB	e`B	jB	l�B	m�B	m�B	o�B	q�B	r�B	s�B	v�B	z�B	{�B	{�B	|�B	}�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�=B	�=B	�JB	�JB	�JB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�FB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�dB	�jB	�qB	�qB	�wB	�wB	�wB	��B	��B	ÖB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	��B	�B	�B	�B	�
B	�
B	�
B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�BB	�BB	�HB	�HB	�HB	�TB	�`B	�fB	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
JB
\B
bB
oB
oB
hB
�B
{B
oB
hB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
$�B
$�B
$�B
$�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
&�B
&�B
&�B
&�B
%�B
$�B
#�B
'�B
(�B
'�B
(�B
(�B
)�B
)�B
,B
,B
+B
+B
-B
.B
/B
/B
/B
.B
/B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
5?B
6FB
6FB
6FB
6FB
6FB
5?B
6FB
8RB
9XB
9XB
8RB
8RB
:^B
:^B
<jB
<jB
<jB
<jB
<jB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
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
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
VB
VB
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
W
B
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
_;B
`BB
`BB
_;B
_;B
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
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B�B��B�B��B�B��B� B�BB̈́B��BοB�sB��B��B��B�;B�,B�B��B�	B�B��BB
=B�BjBvBvB�B�B�B�B�B!HB)yB.cB1'B7�B� B3�B	�B<B�B�BB��B�qB�QB�B��B�XB��B��B�eB�:B��B�B��B}VB�iB��B}�Bz�BxBwLBtnBp�BmBjB^OBFB7�B/�B)yB# B�BhBtBuB
��B
�oB
�QB
�B
�B
��B
��B
�FB
�~B
�RB
�+B
�GB
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
}B
w�B
s�B
l�B
e�B
aB
_B
Y�B
W�B
P}B
G+B
BuB
>�B
5�B
/�B
%,B
B
B

rB
gB
�B	�cB	��B	�B	��B	��B	�RB	��B	�B	��B	�CB	׍B	ҽB	ϑB	�VB	��B	�aB	��B	�B	��B	��B	��B	��B	��B	��B	�CB	�EB	��B	��B	�B	�B	�YB	~B	s3B	sMB	sB	p!B	k�B	dtB	Y�B	PB	JXB	H�B	GEB	F%B	D3B	A;B	?.B	=<B	6�B	+�B	#�B	&�B	%�B	#�B	#TB	!�B	~B	WB	eB	SB	
B	B	@B	\B	�B	�B	�B	mB	�B	 �B�jB�GB��B��B�B�&B��B��B�qBևB��B��B�vB�HB�pB�	B�'B��B��B��B�tB�9B�UB��B��B�B�8B��B�]B��B��B�@B��B�7B�#B��B��B��B�%BBu�BjeBk�Bg�Bh
Be�Bc:B_�BZ�BV�BW?BW
BTFBR�BR�BO�BN�BL�BMjBO(BN<BLJBMBLJBLBLBKDBJ#BIRBF�BA�B?cB<jB;B8�B:xB9rB8RB8�B8�B6FB4nB5�B5�B5�B4�B3�B2�B/OB/5B.cB+�B/ B0;B.�B0B/iB.B0�B0�B.�B/�B.�B,�B*B%�B(�B%�B+�B/�B/B-CB/OB0UB5�B6�B6+B72B9�B;�B<�B<�B<�B;JB<B=<B;B>B@�B@B@�BA�BABA;B>�B<�B7�B4�B8�B<�B=�B="B?.B@BB�BB�BB�BCBB[BC{BDgBHBJ	BKDBLBM6BMBLJBLJBH�BE�BM�BS�Ba�Bc�BezBc�Bg�Bl�Br�BsBtBu?BvFBwfBvzBu�Bt�Bq�BuZBy�By�B}VB}VB|PB|PB|PB{dB{dB.B�;B�AB�SB��B�{B�$B��B�ZB�0B�hB�{B��B�1BǔB�B�XB�B�fB��B��B�wB�UBɆB��B�,B�QB�!B�HB�nB�B��B��B��B��B�B��B��B�GB�UB�B�fB��B	B	[B	GB	gB	B	�B	�B	UB�B�B��B��B�BB	oB	�B	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	!B	#�B	&B	&2B	'RB	*KB	./B	.cB	/�B	/�B	4�B	:�B	=�B	>�B	?�B	A�B	C�B	E�B	G�B	H�B	K�B	K�B	K�B	K�B	MB	MB	LB	L0B	OB	P.B	Q4B	R:B	RTB	T{B	XyB	Y�B	]�B	a�B	d�B	e�B	j�B	l�B	m�B	m�B	o�B	q�B	r�B	tB	wB	{B	|B	|B	}B	~B	~(B	~(B	~BB	�4B	�;B	�AB	�gB	�?B	�tB	�_B	�fB	��B	�rB	��B	�~B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	�B	�"B	�IB	�oB	�aB	�TB	�`B	�fB	�LB	�fB	�LB	�fB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	�B	�	B	�B	��B	�B	�B	�&B	�B	�,B	�9B	�$B	�EB	�1B	�EB	�_B	՛B	�mB	�SB	�B	�?B	�?B	�?B	�EB	�=B	�)B	�CB	�CB	�CB	�CB	�IB	�OB	�\B	�BB	�bB	�|B	�|B	�nB	�zB	�B	�B	�B	�yB	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�"B
 B
;B
UB
AB
GB
GB
�B	�}B	�HB	�cB	�]B	�.B	�.B	�.B	�.B
 4B
 4B
 4B	�.B	�cB
 4B
;B
3B
gB
SB
SB
{B
�B
[B
oB
oB
UB
GB
aB
gB
YB
KB
0B
\B
bB
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
B
�B
�B
 �B
!�B
!�B
!�B
!�B
"�B
#B
#B
$B
$�B
$�B
$�B
%B
%B
%B
'B
'B
'B
'B
&�B
'B
&B
'B
'B
'B
'B
&B
%FB
$tB
(
B
)*B
(>B
)*B
)*B
*0B
*0B
,"B
,=B
+6B
+kB
-]B
./B
/OB
/OB
/OB
.cB
/OB
0UB
1AB
1[B
1AB
2aB
2|B
3hB
3MB
3�B
3�B
5tB
6`B
6`B
6zB
6`B
6zB
5tB
6�B
8�B
9�B
9�B
8�B
8�B
:�B
:�B
<�B
<jB
<�B
<�B
<�B
>�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
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
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
MB
MB
L�B
LB
MB
MB
MB
M�B
M�B
M�B
M�B
N�B
N�B
N�B
OB
OB
OB
N�B
N�B
N�B
OB
PB
PB
Q B
QB
P.B
QB
Q4B
Q4B
R B
R B
R B
R B
S&B
S&B
SB
S&B
S&B
S&B
S@B
S@B
T,B
TB
T,B
T,B
TB
UB
UB
T,B
T,B
U2B
U2B
T�B
UB
UB
U2B
V9B
U2B
V9B
V9B
V9B
V9B
W$B
V9B
W$B
W
B
W$B
W?B
W?B
XEB
Y1B
Y1B
Y1B
YKB
YKB
Y1B
YKB
YKB
YKB
Z7B
Z7B
ZQB
ZQB
[=B
[=B
[=B
\CB
[WB
\]B
\]B
]IB
]IB
]dB
]IB
^jB
^jB
^jB
^jB
]~B
^jB
_VB
_VB
`BB
`\B
_pB
_pB
`vB
aHB
abB
aHB
abB
abB
aHB
abB
a|B
a|B
bhB
bhB
b�B
b�B
cTB
cnB
c�B
c�B
c�B
c�B
c�B
cnB
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
ezB
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�11111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�L�<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711100037432017111000374320171110003743201806221233072018062212330720180622123307201804050428562018040504285620180405042856  JA  ARFMdecpA19c                                                                20171106063507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171105213508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171105213510  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171105213511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171105213511  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171105213511  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171105213511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171105213511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171105213511  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171105213512                      G�O�G�O�G�O�                JA  ARUP                                                                        20171105215439                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171106153457  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171107000000  CF  PSAL_ADJUSTED_QCB���B���G�O�                JM  ARCAJMQC2.0                                                                 20171109153743  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171109153743  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192856  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033307  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                