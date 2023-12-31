CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-23T00:35:24Z creation;2016-07-23T00:35:26Z conversion to V3.1;2019-12-19T08:34:55Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160723003524  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_019                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׽v # 1   @׽v��� @<4Ʌ�oi�dr��D�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   A@  Aa��A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B/��B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D��3D�3D�FfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@��@��HAp�A=p�A_
=A}p�A��A��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)BB'\)B.��B7\)B?\)BG\)BO\)BW\)B_\)Bf��Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B�z�B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Do\D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D�)Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޾D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�~D��D��D�AHD�aH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aǧ�Aǧ�Aǡ�Aǣ�Aǡ�Aǡ�Aǡ�Aǡ�Aǣ�Aǣ�Aǝ�Aǝ�AǛ�AǗ�AǓuAǏ\AǍPAǇ+A�S�A�A�A�G�A�ZA��+A���A�&�A�oA��A��A���A��A�JA��7A���A���A���A��A�bNA��TA���A�n�A�A���A�ƨA�G�A�A�5?A��mA���A�A�A��uA�A��A�|�A�ffA�"�A�bA�bNA�%A��A�v�A���A���A��\A�A��A��jA�JA��FA���A�1'A��+A��DA�jA���AK�A~E�A|�uA{K�AzjAy��AyVAxr�Aw��Av��AvE�At-As�Ar-Apn�Ao�An��Alz�Ajr�Ai�Ah=qAf��Ad��AdA�Ac�AcK�AbbNAa�
AaG�Aa�A`�/A`�DA` �A`bA_l�A^M�A]+A\JA[�-A[oAX��AW�^AW
=AV�AV{AS+AQ\)AP��AO��AN�AM;dAL��AL�DAK33AJ�AI\)AH�RAH^5AG|�AFbNAEƨAEC�AD�9ADjAC�ACXAB�/ABAA�7AA`BAA33A@��A?�;A>r�A<ȴA<v�A<=qA<  A;�A;�FA;�A;;dA:ȴA9�7A9C�A8bA7S�A6�A6��A61A4v�A3�A3"�A2�9A2E�A1K�A0~�A.��A.$�A-oA,jA++A*$�A)��A)O�A(�\A(9XA'�wA&�A%+A#`BA"^5A!��A!"�A -A`BA~�A�A�-A;dA�HA�^AK�A��A5?A`BA5?A��A��A�PA+A"�AAA�DA��A�jA-A
�+A��A�#AVAz�A�AA�#A��AO�A��A �A�-A�PAXA?}A"�Av�A�m@�dZ@�7L@���@��w@�;d@��\@���@�j@�t�@��\@�!@�5?@��@��@��@��@�dZ@�@��@�^@�z�@�@�V@��
@޸R@�M�@�G�@ܼj@�bN@��@�E�@�Ĝ@�K�@֗�@��@Չ7@��`@� �@�
=@���@�~�@�@��T@�hs@�(�@�K�@�n�@͉7@��H@��@Ɂ@�7L@��@���@�\)@�"�@�n�@�x�@�ƨ@�@�5?@��h@��@�33@�M�@��-@�x�@�Ĝ@���@�
=@���@���@��\@��-@�Q�@�@���@��\@��^@��D@�Q�@���@�ȴ@��-@��@�$�@��@���@��D@�r�@�Q�@��@�@�5?@��j@���@�"�@�M�@�@��7@��@�p�@�hs@�X@�r�@��m@��w@��@���@�K�@��\@���@��#@��h@�X@�7L@��@��y@���@�@���@�p�@�/@���@�bN@�1'@��@�  @��F@���@�^5@�`B@���@�bN@� �@��P@��+@��-@��@�z�@� �@�ƨ@���@�dZ@�@���@��\@�M�@�$�@���@���@�j@�I�@�  @��@��@�;d@�ff@�M�@��@��7@�/@���@�I�@�(�@��@�b@�b@�1@�1@�1@�  @���@��@�ƨ@�;d@�n�@�@�&�@�%@��@���@��@�9X@��@�ƨ@��w@��@�S�@���@��7@���@�j@�I�@� �@�1@��;@��w@���@�|�@�t�@�l�@�S�@�;d@���@���@��\@�^5@��T@�`B@�X@�&�@���@�Ĝ@���@��u@��D@�  @;d@~ȴ@}�@}@}�T@~@}�-@}��@}O�@|��@|1@{��@z�\@y�@y��@y�7@y�@w�;@w�P@w|�@w\)@vȴ@vv�@v@u�@u`B@u`B@u?}@u��@w+@x��@xr�@xA�@xA�@x��@y7L@z=q@z��@z��@z^5@zM�@zM�@zM�@zM�@y�@y��@y��@y��@y��@yX@y&�@x�@x  @wl�@v�@vv�@v{@u�T@u?}@t�@t�j@t�D@t9X@s�
@s�F@s��@s��@s��@st�@s"�@r�@r��@r~�@rn�@r^5@rJ@q%@pr�@pb@o��@o|�@o�@n�y@n�+@nE�@m@m�h@mp�@m?}@mV@l�/@l�j@l�j@lz�@l�@k�F@kC�@j��@j^5@i�^@iG�@h��@hĜ@h�@hQ�@h �@g��@f��@f�@f�R@f@e�-@e��@e�h@e�@e`B@e/@d��@d�@c�m@c��@cdZ@cS�@cC�@c@b��@b~�@bn�@b-@a��@a��@a��@a��@`��@`�@_��@^��@^v�@^V@^V@^$�@]�h@]/@\�@\�/@\�/@\��@\�@\�D@\z�@\9X@[��@[�F@[��@[dZ@Z�H@Zn�@Y��@Y�^@Y��@YG�@Y%@X�@XA�@W�;@WK�@Vȴ@V��@V�+@VE�@U�h@U?}@T�@S�@S@R�H@Rn�@R=q@R=q@R-@RJ@Q��@P�9@P1'@Pb@P  @O�@O�;@O�@O+@N�@N��@N�+@Nff@N$�@M��@L�/@L�j@L�D@K�F@Kt�@KC�@Ko@J�!@J^5@J�@I�^@I7L@H��@H1'@G��@G��@G�P@Gl�@GK�@G+@F��@F�y@F�@F{@E/@E�@E�@D�D@Cƨ@C��@CdZ@CdZ@CS�@C33@B��@B�\@B�\@B~�@B~�@B~�@B��@B��@B~�@BM�@B=q@A��@A��@Ax�@AX@A&�@A%@@A�@@ �@@ �@?�@?l�@?
=@>�@>��@>5?@=V@<j@<1@;�
@;C�@;"�@;o@:�H@:��@:~�@:J@9�@9�^@9%@8Ĝ@8��@8��@8��@81'@7�@7�w@7�@7�@7�@7�w@7�@7+@6�@6ȴ@6��@6V@65?@6@5��@5p�@4�@4�j@4�D@41@3�@3S�@3dZ@3S�@3"�@2��@2�\@2n�@2^5@2M�@2=q@2�@2�@2�@2J@1�@1�^@1��@1hs@1G�@1&�@0��@0�9@0r�@0A�@0b@/�;@/�@/|�@/\)@/;d@/;d@.�y@-�T@-�@,�/@,�/@,��@,��@,j@,(�@+��@+�
@+ƨ@+ƨ@+ƨ@+��@+�@+dZ@+"�@+@*��@*M�@)��@)��@)��@)x�@)x�@)hs@)hs@)X@)&�@)%@)%@(�`@(r�@'��@'�w@'+@&�@&�+@&$�@%�@%�T@%��@%@%�-@%��@%`B@%V@$��@$�@$��@$z�@$I�@$1@#�
@#�F@#��@#��@#��@#t�@#dZ@#C�@"�H@"^5@"J@!�^@!��@!��@!x�@!G�@!&�@ �`@ Ĝ@ ��@ �u@�;@|�@�@ȴ@��@ff@�@�h@?}@�@��@��@�j@z�@Z@I�@(�@�@��@��@dZ@C�@@�!@n�@=q@=q@=q@-@�@�@�#@�7@�@bN@|�@�y@$�@�h@�/@�@��@�/@�/@��@�j@�D@�F@t�@S�@C�@33@33@�@��@��@M�@�@��@��@�^@��@�7@hs@G�@7L@7L@&�@�@�@%@��@��@�9@r�@Q�@�;@�P@��@ȴ@�R@V@�@�-@p�@?}@V@V@��@��@�@�D@z�@j@j@j@j@j@Z@I�@(�@�@�m@�
@�F@�@dZ@o@
^5@	��@	��@	&�@�`@Ĝ@Ĝ@�9@r�@  @�@l�@\)@\)@\)@K�@;d@�@V@@��@��@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aǧ�Aǧ�Aǡ�Aǣ�Aǡ�Aǡ�Aǡ�Aǡ�Aǣ�Aǣ�Aǝ�Aǝ�AǛ�AǗ�AǓuAǏ\AǍPAǇ+A�S�A�A�A�G�A�ZA��+A���A�&�A�oA��A��A���A��A�JA��7A���A���A���A��A�bNA��TA���A�n�A�A���A�ƨA�G�A�A�5?A��mA���A�A�A��uA�A��A�|�A�ffA�"�A�bA�bNA�%A��A�v�A���A���A��\A�A��A��jA�JA��FA���A�1'A��+A��DA�jA���AK�A~E�A|�uA{K�AzjAy��AyVAxr�Aw��Av��AvE�At-As�Ar-Apn�Ao�An��Alz�Ajr�Ai�Ah=qAf��Ad��AdA�Ac�AcK�AbbNAa�
AaG�Aa�A`�/A`�DA` �A`bA_l�A^M�A]+A\JA[�-A[oAX��AW�^AW
=AV�AV{AS+AQ\)AP��AO��AN�AM;dAL��AL�DAK33AJ�AI\)AH�RAH^5AG|�AFbNAEƨAEC�AD�9ADjAC�ACXAB�/ABAA�7AA`BAA33A@��A?�;A>r�A<ȴA<v�A<=qA<  A;�A;�FA;�A;;dA:ȴA9�7A9C�A8bA7S�A6�A6��A61A4v�A3�A3"�A2�9A2E�A1K�A0~�A.��A.$�A-oA,jA++A*$�A)��A)O�A(�\A(9XA'�wA&�A%+A#`BA"^5A!��A!"�A -A`BA~�A�A�-A;dA�HA�^AK�A��A5?A`BA5?A��A��A�PA+A"�AAA�DA��A�jA-A
�+A��A�#AVAz�A�AA�#A��AO�A��A �A�-A�PAXA?}A"�Av�A�m@�dZ@�7L@���@��w@�;d@��\@���@�j@�t�@��\@�!@�5?@��@��@��@��@�dZ@�@��@�^@�z�@�@�V@��
@޸R@�M�@�G�@ܼj@�bN@��@�E�@�Ĝ@�K�@֗�@��@Չ7@��`@� �@�
=@���@�~�@�@��T@�hs@�(�@�K�@�n�@͉7@��H@��@Ɂ@�7L@��@���@�\)@�"�@�n�@�x�@�ƨ@�@�5?@��h@��@�33@�M�@��-@�x�@�Ĝ@���@�
=@���@���@��\@��-@�Q�@�@���@��\@��^@��D@�Q�@���@�ȴ@��-@��@�$�@��@���@��D@�r�@�Q�@��@�@�5?@��j@���@�"�@�M�@�@��7@��@�p�@�hs@�X@�r�@��m@��w@��@���@�K�@��\@���@��#@��h@�X@�7L@��@��y@���@�@���@�p�@�/@���@�bN@�1'@��@�  @��F@���@�^5@�`B@���@�bN@� �@��P@��+@��-@��@�z�@� �@�ƨ@���@�dZ@�@���@��\@�M�@�$�@���@���@�j@�I�@�  @��@��@�;d@�ff@�M�@��@��7@�/@���@�I�@�(�@��@�b@�b@�1@�1@�1@�  @���@��@�ƨ@�;d@�n�@�@�&�@�%@��@���@��@�9X@��@�ƨ@��w@��@�S�@���@��7@���@�j@�I�@� �@�1@��;@��w@���@�|�@�t�@�l�@�S�@�;d@���@���@��\@�^5@��T@�`B@�X@�&�@���@�Ĝ@���@��u@��D@�  @;d@~ȴ@}�@}@}�T@~@}�-@}��@}O�@|��@|1@{��@z�\@y�@y��@y�7@y�@w�;@w�P@w|�@w\)@vȴ@vv�@v@u�@u`B@u`B@u?}@u��@w+@x��@xr�@xA�@xA�@x��@y7L@z=q@z��@z��@z^5@zM�@zM�@zM�@zM�@y�@y��@y��@y��@y��@yX@y&�@x�@x  @wl�@v�@vv�@v{@u�T@u?}@t�@t�j@t�D@t9X@s�
@s�F@s��@s��@s��@st�@s"�@r�@r��@r~�@rn�@r^5@rJ@q%@pr�@pb@o��@o|�@o�@n�y@n�+@nE�@m@m�h@mp�@m?}@mV@l�/@l�j@l�j@lz�@l�@k�F@kC�@j��@j^5@i�^@iG�@h��@hĜ@h�@hQ�@h �@g��@f��@f�@f�R@f@e�-@e��@e�h@e�@e`B@e/@d��@d�@c�m@c��@cdZ@cS�@cC�@c@b��@b~�@bn�@b-@a��@a��@a��@a��@`��@`�@_��@^��@^v�@^V@^V@^$�@]�h@]/@\�@\�/@\�/@\��@\�@\�D@\z�@\9X@[��@[�F@[��@[dZ@Z�H@Zn�@Y��@Y�^@Y��@YG�@Y%@X�@XA�@W�;@WK�@Vȴ@V��@V�+@VE�@U�h@U?}@T�@S�@S@R�H@Rn�@R=q@R=q@R-@RJ@Q��@P�9@P1'@Pb@P  @O�@O�;@O�@O+@N�@N��@N�+@Nff@N$�@M��@L�/@L�j@L�D@K�F@Kt�@KC�@Ko@J�!@J^5@J�@I�^@I7L@H��@H1'@G��@G��@G�P@Gl�@GK�@G+@F��@F�y@F�@F{@E/@E�@E�@D�D@Cƨ@C��@CdZ@CdZ@CS�@C33@B��@B�\@B�\@B~�@B~�@B~�@B��@B��@B~�@BM�@B=q@A��@A��@Ax�@AX@A&�@A%@@A�@@ �@@ �@?�@?l�@?
=@>�@>��@>5?@=V@<j@<1@;�
@;C�@;"�@;o@:�H@:��@:~�@:J@9�@9�^@9%@8Ĝ@8��@8��@8��@81'@7�@7�w@7�@7�@7�@7�w@7�@7+@6�@6ȴ@6��@6V@65?@6@5��@5p�@4�@4�j@4�D@41@3�@3S�@3dZ@3S�@3"�@2��@2�\@2n�@2^5@2M�@2=q@2�@2�@2�@2J@1�@1�^@1��@1hs@1G�@1&�@0��@0�9@0r�@0A�@0b@/�;@/�@/|�@/\)@/;d@/;d@.�y@-�T@-�@,�/@,�/@,��@,��@,j@,(�@+��@+�
@+ƨ@+ƨ@+ƨ@+��@+�@+dZ@+"�@+@*��@*M�@)��@)��@)��@)x�@)x�@)hs@)hs@)X@)&�@)%@)%@(�`@(r�@'��@'�w@'+@&�@&�+@&$�@%�@%�T@%��@%@%�-@%��@%`B@%V@$��@$�@$��@$z�@$I�@$1@#�
@#�F@#��@#��@#��@#t�@#dZ@#C�@"�H@"^5@"J@!�^@!��@!��@!x�@!G�@!&�@ �`@ Ĝ@ ��@ �u@�;@|�@�@ȴ@��@ff@�@�h@?}@�@��@��@�j@z�@Z@I�@(�@�@��@��@dZ@C�@@�!@n�@=q@=q@=q@-@�@�@�#@�7@�@bN@|�@�y@$�@�h@�/@�@��@�/@�/@��@�j@�D@�F@t�@S�@C�@33@33@�@��@��@M�@�@��@��@�^@��@�7@hs@G�@7L@7L@&�@�@�@%@��@��@�9@r�@Q�@�;@�P@��@ȴ@�R@V@�@�-@p�@?}@V@V@��@��@�@�D@z�@j@j@j@j@j@Z@I�@(�@�@�m@�
@�F@�@dZ@o@
^5@	��@	��@	&�@�`@Ĝ@Ĝ@�9@r�@  @�@l�@\)@\)@\)@K�@;d@�@V@@��@��@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BQ�BQ�BQ�BR�BR�BS�BR�BR�BR�BR�BT�BT�BVBW
BW
BXBYBZBZBYBZBo�B^5B7LBVBB��B��B�B�B�B�B��BĜB�dB�B��B�DB�B�B�Bx�BbNBQ�BB�B5?B(�B%�B"�B�B�B1B	7BB�B�;B��B�9B�!B��B�JB� Bq�B[#BK�BI�BE�B'�BVB+B
��B
�/B
ɺB
�LB
�B
��B
�{B
�PB
�B
~�B
w�B
s�B
o�B
hsB
bNB
\)B
O�B
D�B
@�B
33B
-B
%�B
�B
1B	��B	��B	�B	�TB	�/B	�#B	�B	��B	��B	ɺB	ǮB	ƨB	ĜB	��B	��B	�wB	�RB	�-B	��B	��B	��B	��B	�PB	�1B	�+B	�B	z�B	n�B	jB	gmB	cTB	YB	VB	S�B	M�B	F�B	B�B	>wB	<jB	8RB	2-B	.B	-B	(�B	&�B	#�B	 �B	�B	�B	�B	�B	�B	{B	\B	JB	%B	B	B	B	B	B	B	  B��B��B��B��B�B�B�B�B�ZB�;B�/B�#B�B�B��B��BĜBB�}B�qB�RB�FB�9B�!B�B�B��B��B��B��B��B�uB�\B�PB�DB�1B�%B�B�Bz�Bx�Bv�Bu�Br�Bo�Bn�BiyBhsBffBe`B`BB]/B\)B[#BXBW
BT�BR�BP�BN�BM�BL�BL�BK�BK�BK�BK�BJ�BJ�BI�BI�BI�BH�BH�BF�BE�B@�B@�B>wB<jB:^B8RB5?B1'B1'B/B,B,B)�B)�B'�B&�B"�B"�B!�B!�B!�B"�B!�B�B �B!�B!�B!�B"�B!�B"�B#�B#�B#�B#�B$�B$�B$�B$�B$�B#�B#�B#�B%�B'�B'�B(�B-B,B+B+B+B-B-B,B-B-B.B.B.B.B/B2-B33B49B49B6FB7LB7LB8RB8RB7LB:^B=qB>wB>wB>wB?}BA�B@�BB�BB�BB�BE�BH�BI�BL�BM�BM�BM�BQ�BQ�BT�B[#B^5B`BBcTBdZBe`Be`Be`Be`Be`BiyBjBk�Bk�Bk�Bl�Bo�Bq�Br�Bs�Bs�Bt�Bw�B|�B|�B� B�B�B�B�B�%B�+B�1B�1B�7B�JB�VB�hB�uB�{B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�3B�?B�dB�wB�wB��BBÖBĜBɺB��B��B��B��B��B�
B�B�B�B�B�B�B�B�B�B�B�#B�5B�TB�sB�B�B�B�B�B��B��B��B��B��B��B��B	B	PB	hB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	&�B	(�B	+B	/B	49B	33B	49B	49B	49B	49B	33B	33B	33B	33B	5?B	9XB	>wB	A�B	B�B	G�B	I�B	J�B	L�B	M�B	M�B	L�B	M�B	P�B	W
B	_;B	bNB	cTB	cTB	cTB	e`B	gmB	jB	k�B	m�B	n�B	r�B	w�B	|�B	�B	�B	�B	�B	�%B	�7B	�PB	�bB	�hB	�oB	�oB	�oB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�FB	�LB	�XB	�XB	�dB	�jB	�wB	�wB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�TB	�TB	�ZB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
PB
VB
\B
\B
bB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
)�B
+B
,B
,B
-B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
1'B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
1'B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
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
8RB
9XB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
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
<jB
<jB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
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
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
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
T�B
T�B
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
W
B
XB
XB
YB
YB
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
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
dZB
e`B
e`B
ffB
ffB
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
iyB
iyB
jB
jB
jB
k�B
k�B
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BQ�BRBQ�BR�BR�BTBR�BSBR�BS&BUBUBV9BW?BW?BXEBYB[WB\�B`vBf�Bw�BeB>�B@B�B;B�JB�AB�B�B�CB��B��B�(B��B�5B�~B��B�B�gB|�Be`BUgBE9B6�B)�B&�B#�B!B�B	B
�BB��B� B�{B��B�hB�LB��B�'BtB\]BL�BKDBIRB+B�B	�B
�JB
ߤB
�dB
�rB
�)B
�IB
��B
��B
�SB
� B
x�B
t�B
p�B
iyB
c�B
^jB
QNB
F%B
B[B
4TB
/ B
(�B
�B
	�B
 OB	��B	�B	�&B	��B	�B	�$B	ѝB	̈́B	�	B	�1B	�+B	�9B	�B	��B	��B	��B	��B	��B	�8B	�&B	�B	�VB	�B	��B	�YB	|�B	o�B	k�B	h�B	eB	Y�B	W
B	U�B	OBB	G�B	C{B	?HB	=�B	9�B	3B	.�B	-�B	)�B	'�B	$�B	!�B	�B	+B	�B	B	_B	B	NB	B	�B	�B	�B	GB	�B	�B	�B	B�cB��B�XB��B�[B�cB��B�=B�B��B��B�BچB�sB��B��B�B��B� B��B�	B�2B�?B��B�OB�B�sB�B��B��B��B��B��B��B�0B�B��B��B�uB{�By�Bw�Bw2BtnBq[Bp!Bj0Bi�Bh�Bf�Ba�B^5B]�B\�BYBYeBV�BTaBRBO�BNpBMBM6BLJBL~BL�BL�BK^BKBJ=BJ#BJXBI�BJ#BI7BF�BA BA;B?B="B;B9�B6FB2|B3MB/�B,�B,�B+6B+6B)�B(XB$ZB$@B"�B"�B#B$tB"�B \B!|B"NB"NB"�B#�B#B#�B$tB$tB$ZB$�B%�B%�B%,B%,B%`B$&B$�B$�B&�B(�B(�B*�B-�B,qB+kB+�B+�B-�B-wB,�B-�B.IB.�B.�B.�B/ B0B2�B3�B4�B4�B7B7�B7�B8�B8�B88B;B>BB>�B>�B?HB@iBA�BAUBCGBC�BC�BF�BI7BJXBMBNBN"BN�BRoBR�BVB[�B^�B`�Bc�Bd�Be�Be�Be�Be�BfBi�Bj�Bk�Bk�Bk�Bm)BpBq�BsBtBtBu�Bx�B}VB}�B�iB�oB�uB��B�mB�YB�zB�fB��B��B��B�B�B��B��B�9B�yB�dB�NB�nB�LB�XB�0B�QB�qB�cB��B�[B��B��B��B��B��B��B��B��B��B�SB��B�)B�0B�BB�hB�MB�$B�+B�EB�KB�KB�KB�KB�1B�KB�7BچBۦB��B��B��B��B��B��B�B��B�B��B��B�B�XB�B��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"B	# B	%,B	'8B	)DB	+kB	/�B	4nB	3�B	4nB	4nB	4nB	4TB	3�B	3�B	3�B	3�B	5�B	9rB	>�B	A�B	B�B	G�B	J	B	K)B	MB	NB	N<B	MB	N"B	Q4B	WsB	_�B	bhB	cnB	c�B	c�B	e�B	g�B	j�B	k�B	m�B	n�B	r�B	wfB	|�B	�GB	�GB	�-B	�B	�B	�B	�PB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	��B	�B	�
B	�B	��B	�*B	�*B	�0B	�6B	�"B	�"B	�IB	�/B	�cB	��B	�|B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�&B	�,B	�MB	�9B	�?B	�sB	�KB	�7B	�QB	�7B	�QB	�QB	�kB	�qB	�jB	�jB	�pB	�pB	�pB	�VB	�vB	�|B	�|B	�|B	�|B	�TB	�B	�B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�	B	�*B	�0B	�6B	�(B	�(B	�BB	�]B
 OB
 iB
�B
aB
MB
gB
SB
B
SB
9B
tB
tB
�B
	lB
	RB
	lB

rB

rB

�B
xB
~B
~B
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B
�B
�B
�B
B
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
!�B
"�B
"�B
$&B
%B
%B
%B
%B
&B
&B
'B
'RB
(sB
*0B
+6B
,"B
,=B
-CB
-CB
-CB
-CB
-CB
.cB
/5B
/OB
/iB
0;B
0UB
1AB
0UB
0oB
1AB
1[B
2GB
2GB
2GB
2GB
2GB
1vB
2aB
3hB
3MB
3MB
3hB
4nB
4nB
4�B
4�B
5tB
5tB
6�B
6�B
7fB
7LB
7fB
7�B
7�B
8�B
8�B
8lB
8RB
8lB
8�B
9rB
8lB
8�B
9�B
9rB
9rB
9rB
9rB
:xB
:�B
:�B
:xB
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
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
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
M�B
NB
NB
NB
NB
M�B
OB
OB
N�B
O(B
O(B
PB
PB
O�B
QB
Q4B
R B
R B
SB
S&B
S&B
S&B
SB
S&B
S�B
T,B
T,B
T,B
T,B
U2B
U2B
UB
U2B
UB
U2B
UB
UB
UB
UB
U2B
V9B
V9B
VSB
VmB
WYB
X_B
X_B
YKB
YeB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
Z7B
ZkB
[=B
[WB
[#B
[=B
[#B
[=B
[WB
[=B
[=B
\]B
\]B
\]B
\]B
]/B
]dB
]IB
]dB
]IB
]IB
]/B
]IB
]IB
]IB
]IB
]dB
]dB
^jB
^jB
^�B
_pB
_pB
`\B
`vB
`�B
`\B
`\B
a|B
a|B
a|B
abB
a|B
b�B
b�B
b�B
bhB
bNB
bhB
bhB
bhB
bhB
bhB
bNB
b�B
bhB
c�B
cnB
c�B
c�B
c�B
c�B
c�B
d�B
ezB
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
j�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<L��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607270033082016072700330820160727003308201806221211242018062212112420180622121124201804050403482018040504034820180405040348  JA  ARFMdecpA19c                                                                20160723093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160723003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160723003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160723003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160723003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160723003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160723003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160723003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160723003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160723003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20160723012042                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160723153649  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160726153308  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160726153308  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190348  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031124  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                