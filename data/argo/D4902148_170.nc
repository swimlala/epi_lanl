CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-04-13T15:36:42Z creation;2019-04-13T15:36:47Z conversion to V3.1;2019-12-18T07:15:46Z update;2022-11-21T05:29:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190413153642  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_170                     2C  DdDNAVIS_A                         0397                            ARGO 011514                     863 @ض �Ӡ�1   @ض!I���@<��dD��*1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@/\)@u@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR�)DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWo\DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�VA�t�A�r�A�p�A�v�A�S�A���A���A�A�A��A��/A���A�ƨA��jA��A���A���A��hA��+A�x�A�r�A�jA�Q�A�(�A���A�A�A�"�A���A��mA�ƨA��\A��A�t�A�VA�?}A��A�VA�  A��A���A��9A��\A�p�A�dZA�ZA�G�A�E�A�9XA�$�A�%A��A���A��wA���A��PA��+A�z�A�z�A�t�A�hsA�O�A�5?A��A���A��A���A�p�A�O�A�A��;A��jA��PA�bNA�O�A�$�A�ĜA��FA��PA�7LA�VA��9A�n�A��A��A��7A�S�A�+A���A��-A�?}A��A��mA���A�x�A�+A�ȴA�I�A���A�+A��9A��A���A�VA�dZA��RA�\)A�|�A��A���A��`A�M�A�A���A�{A�n�A�S�A�"�A�bA\)A|  Az{Aw�TAu�As
=Ar=qAp��An�Am�AljAj��Ai`BAf�jAehsAd�AdĜAd�\AcO�Aa�;Aa7LA_�#A]K�A[�AZ��AY|�AX�jAW��AV�`AV��AU��AU%AR��AP1'AN�AM"�ALĜAK�AJZAI�PAI\)AH�yAHffAH5?AG�AF  ACK�AAp�A@��A@bNA?�wA>�`A=x�A<��A<�9A<�+A<(�A:�A7|�A6�A6��A6�!A6v�A5�;A4�`A4ZA4 �A2��A/\)A.9XA-�7A-XA-"�A,^5A+��A*��A(��A(1'A'A'�A&�uA&�A%��A%��A%?}A$�/A#?}A"�\A"jA"I�A"5?A"$�A"JA!�A!��A!+A �uA =qA (�A �A��A�A�;A��Al�A�A�`AjA�-A��A�A7LA�AVA��AbNA  A��AXA�`An�A��A%A=qAĜAz�A5?A��A�Al�A
�/A
�jA
{A�HA�AhsA��A��A��A��A$�A�-AVAn�AA�TA ��@�J@�K�@�t�@��@�I�@�P@�"�@��@�l�@��@�\@�{@�z�@�"�@�ff@��@��/@�I�@��
@�l�@�!@�{@�-@�X@��@�D@�1@�@㕁@��T@ߝ�@�G�@�C�@�ff@��m@֏\@���@�M�@��/@�b@�C�@���@�&�@˥�@�+@��H@ʸR@�~�@�E�@�@���@�hs@���@���@���@��`@���@ȃ@�33@�7L@�@��@�-@�&�@� �@�t�@��\@�A�@�33@���@�-@���@��/@�(�@��T@�Ĝ@���@��/@��@�S�@�
=@�v�@���@�7L@���@�r�@�1'@��@��w@�|�@���@��@�&�@�dZ@�@�ȴ@�?}@�t�@�ȴ@��@�O�@��@�1'@���@���@��m@��m@��
@���@��@��P@�S�@�+@���@�-@���@��@���@��y@�n�@��@�7L@�Q�@�1'@�  @��P@�
=@��+@�V@�{@��@��@�V@��@��9@��D@�j@�I�@�1@�1@��@���@��@�|�@�|�@�|�@�l�@�\)@�o@��y@���@�ff@��`@�z�@�9X@��
@�dZ@���@��@���@�9X@��@�|�@�;d@��y@��R@�ff@�@��@��T@��T@��@��#@��#@��^@��7@��7@�7L@��@�V@���@��u@��@��D@��D@�A�@���@��@�@��^@��^@��^@��^@��-@���@��7@�X@�7L@���@���@���@�9X@��@�|�@�S�@�"�@���@�{@��^@�X@��@��`@���@��@��u@�r�@�A�@�  @�;@��@��@�w@��@|�@|�@l�@\)@~�y@~E�@}@}?}@}/@|�@|��@|9X@{�F@{�@{dZ@{33@z�@z��@z^5@yX@x1'@w|�@w�@v��@u`B@u/@t�/@t�j@t�D@tj@tI�@tI�@tI�@t(�@s�
@s��@r�H@qX@pĜ@pA�@o|�@nV@n5?@n@mp�@l��@lz�@l9X@k�F@k"�@j~�@i�@i�7@i%@h�9@h1'@h �@g��@g+@f�@f��@f5?@e�-@e�@e�@d�/@d��@d�j@d��@dZ@d�@cƨ@c��@b�@b��@b~�@bn�@bn�@b��@cdZ@cdZ@co@b��@b�!@bM�@a��@b��@b�!@a��@aG�@`��@`��@`Ĝ@`Ĝ@`�9@`�9@`�u@`A�@`1'@_�P@_\)@^��@^5?@]/@\Z@[t�@ZM�@Y�^@Y��@Yx�@YX@Y7L@X��@X��@XQ�@W�w@WK�@Vȴ@V{@UO�@T��@TI�@S��@R�H@Q�#@Qx�@Qx�@QX@QX@QG�@Q7L@Q%@P��@P  @O|�@O+@N�y@N�R@N@M@M�h@Mp�@MV@L�D@LZ@K�F@K�@K33@Ko@J�@J�\@J=q@J-@I��@I��@I�7@IX@I&�@H��@HA�@G�P@G\)@G+@F�@F��@FV@E�-@EV@D�@D�j@D1@Cƨ@C�F@C��@Co@BM�@B-@A��@A�@A�#@A��@Ahs@AX@A7L@@��@@b@?�@?�w@?|�@?�@>�R@>{@=@=�@=O�@=V@<�@<z�@<(�@;�
@;��@;�@;o@:�\@:-@9��@9�7@9�@8�9@8r�@81'@8 �@8  @7�@7�;@7;d@6��@6v�@6V@65?@6@5��@5p�@4��@4�j@4z�@4Z@4�@3ƨ@3S�@2�H@2��@1x�@0bN@/|�@.��@-��@-p�@-`B@-O�@-O�@-O�@-O�@-O�@-O�@-?}@-/@-/@-V@,��@,�@,�/@,��@,�@,z�@,I�@,I�@,9X@,(�@+��@+�F@+C�@*�!@*-@)��@)x�@)X@)%@(�9@(1'@'�;@'�;@'�;@'�;@(  @'�@'�@'�;@'�@'�@'�;@'�@'�@&��@&5?@&@%�@%�@%�T@%��@%��@%�-@%�h@%`B@%?}@%/@$�/@$�@$�@$��@$j@$�@#�m@#�m@#�m@#�m@#�F@#dZ@#33@#@"�@"�@"��@"�!@"�\@"^5@"M�@"�@!�#@!hs@!G�@!�@ ��@ bN@ 1'@  �@   @�w@\)@�y@�@��@ff@$�@@`B@�@V@�@�j@�D@Z@�@�m@�F@��@dZ@C�@�@��@�!@��@�\@~�@n�@n�@n�@=q@=q@��@�#@�#@��@��@��@��@x�@G�@�@��@�@r�@bN@  @�@�w@�@�P@\)@+@
=@�y@�R@�+@ff@5?@$�@@p�@O�@/@�@z�@Z@Z@Z@I�@9X@9X@�
@�@dZ@C�@33@�@��@n�@=q@-@�@�@�#@��@��@�^@x�@%@�u@A�@b@�w@�P@l�@\)@\)@K�@;d@;d@
=@�@��@$�@?}@�@�/@�j@�@��@�D@z�@j@I�@9X@(�@�@1@�m@�
@ƨ@�F@��@��@t�@S�@33@"�@@
��@
J@	�@	��@	��@	�7@	x�@	&�@�9@r�@ �@�@�@|�@\)@;d@�R@��@�+@ff@{@{@{@@��@�-@�@`B@/@��@�@�/@�@j@I�@�@�m@�F@�@dZ@dZ@dZ@dZ@dZ@S�@C�@C�@33@o@�H@��@��@~�@�@J@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�VA�t�A�r�A�p�A�v�A�S�A���A���A�A�A��A��/A���A�ƨA��jA��A���A���A��hA��+A�x�A�r�A�jA�Q�A�(�A���A�A�A�"�A���A��mA�ƨA��\A��A�t�A�VA�?}A��A�VA�  A��A���A��9A��\A�p�A�dZA�ZA�G�A�E�A�9XA�$�A�%A��A���A��wA���A��PA��+A�z�A�z�A�t�A�hsA�O�A�5?A��A���A��A���A�p�A�O�A�A��;A��jA��PA�bNA�O�A�$�A�ĜA��FA��PA�7LA�VA��9A�n�A��A��A��7A�S�A�+A���A��-A�?}A��A��mA���A�x�A�+A�ȴA�I�A���A�+A��9A��A���A�VA�dZA��RA�\)A�|�A��A���A��`A�M�A�A���A�{A�n�A�S�A�"�A�bA\)A|  Az{Aw�TAu�As
=Ar=qAp��An�Am�AljAj��Ai`BAf�jAehsAd�AdĜAd�\AcO�Aa�;Aa7LA_�#A]K�A[�AZ��AY|�AX�jAW��AV�`AV��AU��AU%AR��AP1'AN�AM"�ALĜAK�AJZAI�PAI\)AH�yAHffAH5?AG�AF  ACK�AAp�A@��A@bNA?�wA>�`A=x�A<��A<�9A<�+A<(�A:�A7|�A6�A6��A6�!A6v�A5�;A4�`A4ZA4 �A2��A/\)A.9XA-�7A-XA-"�A,^5A+��A*��A(��A(1'A'A'�A&�uA&�A%��A%��A%?}A$�/A#?}A"�\A"jA"I�A"5?A"$�A"JA!�A!��A!+A �uA =qA (�A �A��A�A�;A��Al�A�A�`AjA�-A��A�A7LA�AVA��AbNA  A��AXA�`An�A��A%A=qAĜAz�A5?A��A�Al�A
�/A
�jA
{A�HA�AhsA��A��A��A��A$�A�-AVAn�AA�TA ��@�J@�K�@�t�@��@�I�@�P@�"�@��@�l�@��@�\@�{@�z�@�"�@�ff@��@��/@�I�@��
@�l�@�!@�{@�-@�X@��@�D@�1@�@㕁@��T@ߝ�@�G�@�C�@�ff@��m@֏\@���@�M�@��/@�b@�C�@���@�&�@˥�@�+@��H@ʸR@�~�@�E�@�@���@�hs@���@���@���@��`@���@ȃ@�33@�7L@�@��@�-@�&�@� �@�t�@��\@�A�@�33@���@�-@���@��/@�(�@��T@�Ĝ@���@��/@��@�S�@�
=@�v�@���@�7L@���@�r�@�1'@��@��w@�|�@���@��@�&�@�dZ@�@�ȴ@�?}@�t�@�ȴ@��@�O�@��@�1'@���@���@��m@��m@��
@���@��@��P@�S�@�+@���@�-@���@��@���@��y@�n�@��@�7L@�Q�@�1'@�  @��P@�
=@��+@�V@�{@��@��@�V@��@��9@��D@�j@�I�@�1@�1@��@���@��@�|�@�|�@�|�@�l�@�\)@�o@��y@���@�ff@��`@�z�@�9X@��
@�dZ@���@��@���@�9X@��@�|�@�;d@��y@��R@�ff@�@��@��T@��T@��@��#@��#@��^@��7@��7@�7L@��@�V@���@��u@��@��D@��D@�A�@���@��@�@��^@��^@��^@��^@��-@���@��7@�X@�7L@���@���@���@�9X@��@�|�@�S�@�"�@���@�{@��^@�X@��@��`@���@��@��u@�r�@�A�@�  @�;@��@��@�w@��@|�@|�@l�@\)@~�y@~E�@}@}?}@}/@|�@|��@|9X@{�F@{�@{dZ@{33@z�@z��@z^5@yX@x1'@w|�@w�@v��@u`B@u/@t�/@t�j@t�D@tj@tI�@tI�@tI�@t(�@s�
@s��@r�H@qX@pĜ@pA�@o|�@nV@n5?@n@mp�@l��@lz�@l9X@k�F@k"�@j~�@i�@i�7@i%@h�9@h1'@h �@g��@g+@f�@f��@f5?@e�-@e�@e�@d�/@d��@d�j@d��@dZ@d�@cƨ@c��@b�@b��@b~�@bn�@bn�@b��@cdZ@cdZ@co@b��@b�!@bM�@a��@b��@b�!@a��@aG�@`��@`��@`Ĝ@`Ĝ@`�9@`�9@`�u@`A�@`1'@_�P@_\)@^��@^5?@]/@\Z@[t�@ZM�@Y�^@Y��@Yx�@YX@Y7L@X��@X��@XQ�@W�w@WK�@Vȴ@V{@UO�@T��@TI�@S��@R�H@Q�#@Qx�@Qx�@QX@QX@QG�@Q7L@Q%@P��@P  @O|�@O+@N�y@N�R@N@M@M�h@Mp�@MV@L�D@LZ@K�F@K�@K33@Ko@J�@J�\@J=q@J-@I��@I��@I�7@IX@I&�@H��@HA�@G�P@G\)@G+@F�@F��@FV@E�-@EV@D�@D�j@D1@Cƨ@C�F@C��@Co@BM�@B-@A��@A�@A�#@A��@Ahs@AX@A7L@@��@@b@?�@?�w@?|�@?�@>�R@>{@=@=�@=O�@=V@<�@<z�@<(�@;�
@;��@;�@;o@:�\@:-@9��@9�7@9�@8�9@8r�@81'@8 �@8  @7�@7�;@7;d@6��@6v�@6V@65?@6@5��@5p�@4��@4�j@4z�@4Z@4�@3ƨ@3S�@2�H@2��@1x�@0bN@/|�@.��@-��@-p�@-`B@-O�@-O�@-O�@-O�@-O�@-O�@-?}@-/@-/@-V@,��@,�@,�/@,��@,�@,z�@,I�@,I�@,9X@,(�@+��@+�F@+C�@*�!@*-@)��@)x�@)X@)%@(�9@(1'@'�;@'�;@'�;@'�;@(  @'�@'�@'�;@'�@'�@'�;@'�@'�@&��@&5?@&@%�@%�@%�T@%��@%��@%�-@%�h@%`B@%?}@%/@$�/@$�@$�@$��@$j@$�@#�m@#�m@#�m@#�m@#�F@#dZ@#33@#@"�@"�@"��@"�!@"�\@"^5@"M�@"�@!�#@!hs@!G�@!�@ ��@ bN@ 1'@  �@   @�w@\)@�y@�@��@ff@$�@@`B@�@V@�@�j@�D@Z@�@�m@�F@��@dZ@C�@�@��@�!@��@�\@~�@n�@n�@n�@=q@=q@��@�#@�#@��@��@��@��@x�@G�@�@��@�@r�@bN@  @�@�w@�@�P@\)@+@
=@�y@�R@�+@ff@5?@$�@@p�@O�@/@�@z�@Z@Z@Z@I�@9X@9X@�
@�@dZ@C�@33@�@��@n�@=q@-@�@�@�#@��@��@�^@x�@%@�u@A�@b@�w@�P@l�@\)@\)@K�@;d@;d@
=@�@��@$�@?}@�@�/@�j@�@��@�D@z�@j@I�@9X@(�@�@1@�m@�
@ƨ@�F@��@��@t�@S�@33@"�@@
��@
J@	�@	��@	��@	�7@	x�@	&�@�9@r�@ �@�@�@|�@\)@;d@�R@��@�+@ff@{@{@{@@��@�-@�@`B@/@��@�@�/@�@j@I�@�@�m@�F@�@dZ@dZ@dZ@dZ@dZ@S�@C�@C�@33@o@�H@��@��@~�@�@J@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��BĜBÖBÖBÖBB�jB�?B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�-B�-B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�VB�\B�\B�bB�\B�\B�VB�VB�JB�DB�7B�B}�B{�Bt�Bq�Bn�BjBe`BcTB^5BVBS�BQ�BP�BL�BC�B:^B49B1'B'�B"�B�B�BhB1B%BBB��B��B�B�`B�B��B�qB�B��B�BiyBYBO�B;dB(�B�BB
��B
�`B
��B
��B
�9B
��B
�=B
�+B
~�B
ffB
VB
E�B
49B
"�B
�B
\B
B	��B	�B	�`B	�/B	��B	ƨB	ÖB	B	�}B	�XB	�!B	�B	��B	��B	�\B	�7B	�B	~�B	z�B	u�B	u�B	r�B	m�B	`BB	R�B	G�B	B�B	?}B	9XB	33B	0!B	.B	+B	'�B	%�B	�B	�B	\B		7B	1B	%B	B��B��B��B��B��B��B�B�)B�B�B��B��B��BǮBB�}B�FB��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�VB�VB�JB�7B�B�B�B�B�B�B�B�B�B� B~�B}�B|�B|�B{�B{�Bz�By�By�Bw�Bv�Bs�Bq�Bk�BgmBbNB^5B]/B\)B[#BZBYBXBVBS�BR�BN�BG�BC�BB�BA�B>wB;dB8RB7LB6FB49B1'B/B.B-B+B(�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B{BuBuBoBoBhBhBbBbB\B\BVBVBVBVBPBPBPBJBJBJBJBJBDBDB
=B	7B	7B	7B	7B1B	7B	7B	7BDBJBJBPBPBVBbBbBbBbBbBhBhBhBhBoBhBhBhBhBbBhBoB{B�B�B�B�B�B�B$�B&�B'�B(�B)�B,B-B2-B5?B5?B5?B5?B:^B;dB<jB>wB?}B@�BA�BA�BB�BB�BB�BD�BF�BH�BM�BN�BM�BR�BYB[#B^5B`BBbNBdZBe`Be`Be`Be`Be`Be`BffBffBgmBgmBiyBjBk�Bq�Bu�Bu�Bw�By�B~�B�B�+B�JB�VB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�'B�'B�'B�'B�-B�?B�LB�^B�qB��BBÖBĜBǮB��B��B�B�/B�BB�TB�fB�B�B�B��B��B��B��B	B	B	B	B	B	B	1B		7B		7B	DB	DB	JB	PB	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	$�B	%�B	&�B	+B	-B	.B	0!B	2-B	33B	49B	5?B	5?B	6FB	9XB	<jB	=qB	=qB	>wB	>wB	?}B	@�B	@�B	@�B	A�B	D�B	I�B	K�B	M�B	M�B	N�B	O�B	P�B	Q�B	R�B	R�B	S�B	T�B	VB	W
B	\)B	aHB	dZB	e`B	ffB	jB	k�B	l�B	m�B	n�B	n�B	o�B	o�B	o�B	o�B	p�B	p�B	s�B	x�B	y�B	z�B	}�B	�B	�B	�B	�+B	�=B	�DB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�?B	�LB	�LB	�RB	�RB	�RB	�XB	�^B	�}B	�}B	B	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�BB	�NB	�TB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
+B
+B
	7B

=B
DB
DB
PB
VB
VB
VB
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
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
/B
1'B
33B
49B
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
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
<jB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
A�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
@�B
A�B
A�B
@�B
@�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
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
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
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
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
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
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
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
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
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
r�B
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
t�B
t�B
u�B
u�B
u�B
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
w�B
w�B
w�B
w�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��BĶB��B��BĜB��B��B�zB��B�hB�MB�MB�hB�hB�MB�MB�MB�MB�hB�hB��B��B��B�[B��B��B�iB�iB�cB�kB�2B�B�:B��B�B��B��B� B�@B�B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�pB�vB�\B�}B��B��B��B��B��B��B��B��B~wB|�Bu?Br-BoBj�Be�Bc�B_BVmBT{BR�BQ�BM�BDMB;0B4�B2B(sB#:B5BkBTB�B�B�B�B��B��B��B�BچB�B��B��B��B�GBk6BZ�BRB=qB+B�BEB
�B
�>B
ΥB
�{B
��B
�B
�)B
�B
��B
iB
X�B
HfB
7LB
$ZB
�B
�B
+B	�DB	��B	�B	��B	�BB	�_B	�B	�aB	�;B	��B	�vB	�)B	��B	�KB	� B	��B	�3B	� B	|B	vzB	wB	t9B	poB	c�B	U2B	IB	CaB	AB	:�B	49B	0�B	.�B	+�B	(�B	'�B	!�B	�B	hB	
#B		7B	EB	�B	 �B��B�jB�B��B�B��B��B�yB�SB՛B��B�.BȚBðB��B��B�sB��B�HB�\B��B��B�7B��B�_B�?B�sB�aB� B��B��B�(B�PB��B��B�{B�aB�GB�aB�AB�uB��B�B��B}B~BB}"B}"B|6B|6B{dBzDBz^Bx�Bw�BuBs�Bm�Bi�BdB_VB]�B\�B[�BZ�BY�BX�BV�BU2BT�BR BIRBDMBCGBB�B@�B<jB9>B7�B7fB5�B2|B0B.�B-�B,=B*eB'�B&�B%�B$�B#�B"�B!|B BB�B�B�BMBB,B�B[B�B�B BhB.B�B�B�B�B�B�B�B�B�B~B�B�B�B�B�B�B
�B
�B
rB
=B	�B
=B
=BDBJB�BB�BVBBB�B�B�B�B�B�B�B�B�B�B�B�B�BB�B&BaBSB�BqB�BjB�B BB%�B'mB(�B)�B*�B,�B.�B2�B5tB5�B5�B6+B:�B;�B=B?B@ B@�BA�BA�BB�BCBCGBESBG�BI�BNVBOvBO(BT,BY�B[�B^�B`�Bb�Bd�BezBe�Be�Be�BezBe�Bf�Bf�Bg�Bg�BjBkBl�Br|BvBvFBxRBz�B�B�mB��B��B��B��B��B�'B�HB�FB�B�8B�8B�$B�>B�DB�KB�=B�WB�OB�;B�[B�[B�AB�[B�vB�|B�tB��B��B�wB��B��B�B�9B�fB�jBѝBخBݘB�B�B��B��B��B�B��B�B�	B��B	 B	AB	MB	GB	GB	mB	fB		RB		lB	xB	^B	~B	�B	�B	�B	B	�B	�B	�B	�B	�B	�B	�B	B	B	 B	�B	 B	 B	!-B	!HB	# B	%B	&2B	'mB	+kB	-wB	.}B	0oB	2|B	3MB	4nB	5tB	5�B	6�B	9�B	<�B	=�B	=�B	>�B	>�B	?�B	@�B	@�B	@�B	A�B	EB	J	B	LB	NB	N"B	O(B	P.B	Q4B	RB	S&B	S&B	T,B	U2B	V9B	W�B	\�B	a�B	d�B	e�B	f�B	j�B	k�B	l�B	m�B	n�B	n�B	o�B	o�B	o�B	o�B	p�B	qB	tTB	y$B	zDB	{0B	~wB	�AB	�AB	�{B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	� B	�,B	�B	�8B	�*B	�B	�0B	�0B	�0B	�B	�WB	�/B	�cB	�UB	�[B	�MB	�MB	�3B	�?B	�fB	��B	��B	��B	��B	��B	�^B	��B	��B	��B	żB	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�0B	�PB	�PB	�\B	�TB	�,B	�9B	�B	�B	�?B	�$B	�_B	�eB	�eB	�qB	�xB	ޞB	��B	�B	�B	��B	��B	��B	�B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�0B	�"B	�"B	�"B	�(B	�.B	�.B
 B
 4B
UB
GB
MB
SB
SB
YB
_B
�B
	�B

rB
^B
xB
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!B
!�B
"B
#B
#B
$B
%B
%B
%B
%�B
&B
&B
'8B
(
B
(
B
)*B
)B
)B
*KB
*KB
+6B
,=B
,=B
,=B
-)B
-]B
.cB
.cB
/�B
1�B
3�B
4�B
6�B
7fB
8RB
8lB
8lB
8lB
8lB
8lB
8lB
8lB
8lB
8RB
8�B
8RB
9XB
9rB
9rB
9�B
9�B
9�B
:xB
:xB
:�B
:�B
:�B
:�B
;�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
@�B
A�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
@�B
A�B
A�B
@�B
@�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
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
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
MB
MB
M�B
N"B
N�B
OB
OB
N�B
O(B
PB
QB
QB
QB
Q B
Q B
R B
R B
R B
S&B
S&B
S&B
S&B
S&B
TB
TB
TB
S�B
UB
UB
UB
T�B
UB
UB
U2B
U2B
VB
VB
VB
VB
V9B
V9B
V9B
V9B
V9B
W$B
W$B
W$B
XEB
XEB
X+B
X+B
YKB
YKB
YKB
YKB
Y1B
Z7B
Z7B
Z7B
ZQB
[#B
[WB
[WB
\CB
\]B
\xB
]IB
]IB
]IB
]/B
]/B
]IB
]IB
]dB
^OB
_pB
_pB
_pB
_pB
_VB
_VB
`vB
`\B
`\B
`vB
abB
abB
abB
a|B
a|B
a�B
b�B
bhB
c�B
c�B
d�B
d�B
dZB
dtB
dtB
dZB
dtB
d�B
e�B
e�B
e�B
f�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
hsB
h�B
i�B
i�B
iyB
i�B
i�B
i�B
iyB
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
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
r�B
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
t�B
t�B
u�B
u�B
u�B
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
w�B
xB
w�B
w�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904240032352019042400323520190424003235202211182138382022111821383820221118213838201904250020192019042500201920190425002019  JA  ARFMdecpA19c                                                                20190414003638  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190413153642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190413153644  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190413153644  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190413153645  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190413153645  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190413153646  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190413153646  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190413153646  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190413153647                      G�O�G�O�G�O�                JA  ARUP                                                                        20190413155537                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190413153130  CV  JULD            G�O�G�O�Fű                JM  ARCAJMQC2.0                                                                 20190423153235  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190423153235  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190424152019  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123838  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                