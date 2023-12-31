CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-03T10:01:07Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220203100107  20220203100107  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٶ�-RW�1   @ٶ��l" @<����m�c�\(�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ Dۼ�D���D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�z�@��H@��Ap�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Do]D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�>D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�D۷�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D鷮D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�~D��GD��G1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�n�A�z�A��DA��\A��DA��7A��PA��DA��PA��DA�r�A�hsA�^5A�ZA�XA�XA�VA�G�A�M�A�S�A�K�A�S�A� �A��TA��RA�hsA�A��mA��TA�ƨA���A�v�A�ZA�A�A�&�A��A�
=A���A��A��A��HA���A���A���A��jA��A��A���A���A���A���A���A�v�A�;dA�A��uA�7LA��/A��+A�JA��TA���A��7A��/A�;dA��RA�33A�z�A�~�A��A��A��A�%A���A�+A�hsA��TA�r�A�jA�
=A�jA�bA���A���A��mA�O�A�M�A�=qA�ȴA�jA��A�x�A���A� �A�?}A�%A~JA{�Ay"�AuG�Aq�TAp�9Ao�PAn��Am��Al��Al~�Ak��Ak��AjM�AiC�Ah�\Ah-AgXAf~�Ae|�Ae33Ad�/Ac�
Ab�!Ab5?Ab  AaC�Aa�Aa�A`Q�A^ĜA]�wA]A[\)AZ��AY��AX��AW�AU��AUAT��AS�AQ%AP-AOAOO�AOoAN��ANv�AM��AM
=AK�mAJ�jAJ1'AI�TAI��AIVAG�
AE��AEdZAD�AC��ACO�AC%AA��A?7LA>�A=�A=��A=&�A<VA;ƨA;ƨA;"�A:��A:bNA9��A8��A7K�A5ƨA5l�A4��A4M�A41A3��A2�A0�HA/�FA/�A.�A.ĜA.�A.1'A-�hA,Q�A+��A*��A)A(�!A'��A'��A'x�A&n�A$�/A#|�A!�TA!�A 5?Ax�AQ�A�A=qA�A;dAA%A9XAC�A�A�RA9XA�hAS�AĜA5?A�A��A`BAC�AVAVA��A(�A��A
=A�FA�/A�;A7LA
�A
�RA
��A
jA
1A	�7A��An�A��A��A�9A�mA�jA-A�A�-A�AȴA�PA
=A ��A  �@��@���@�I�@�9X@�o@��@�9X@�\)@���@�I�@�%@�+@�@��
@�^5@��@� �@�@۶F@���@��@�E�@Չ7@ԛ�@Ӯ@Ѻ^@���@�I�@��@�dZ@�n�@��@�Ĝ@�Z@˥�@�@�=q@��@�r�@�\)@���@�@�/@ě�@ÍP@�\)@��y@��@��`@���@��!@��7@��@�j@��;@�=q@�/@���@�S�@�
=@�ȴ@�=q@�hs@��/@��
@�ȴ@���@�?}@�Q�@��F@�C�@���@�=q@���@��D@���@�dZ@�ff@�hs@��@���@���@�"�@�v�@�@�?}@��;@�|�@�C�@��R@�M�@��#@�p�@��`@�Z@��P@���@���@���@��h@�p�@�X@�&�@��@��@��@�33@��H@�n�@�/@�V@���@��/@���@���@�"�@��y@��!@�n�@�=q@�$�@��@���@�@�@���@�G�@�&�@��/@���@��D@�Z@�1@��@��P@�l�@�\)@�K�@�"�@�ȴ@�v�@�E�@�@��h@�`B@�?}@�V@���@�b@�1@�|�@�+@�o@��!@���@��^@�7L@�V@���@��u@�1'@��
@�C�@�@��@���@�n�@�5?@�{@�@��@��#@�@���@�X@��@��@��/@��@�j@�Q�@��m@��@�|�@�@�ȴ@���@��\@�E�@�$�@�J@�@��@���@�`B@�Ĝ@�j@�(�@�b@���@�ƨ@��@�ȴ@���@���@�M�@��@���@��h@�x�@�O�@�7L@�&�@��@�Z@}�h@|��@|(�@|�@{��@{�
@{ƨ@{ƨ@{ƨ@{�F@{�@{C�@{@z��@yhs@x�@w�;@w��@w�w@w�@w�@w�P@wl�@w\)@w
=@u�@u`B@u�@t�/@t�j@t��@tZ@s�
@sC�@r�@r��@rM�@r=q@r�@rJ@q�@q�^@q�@p��@pQ�@o�@o+@n��@nff@n$�@mO�@m/@m�@mV@l��@l�@l�@lI�@k�m@kS�@j��@i�@i��@i�^@i��@i��@i�7@i�7@ihs@iX@i&�@h�`@hr�@h1'@g�@gK�@f�R@e��@e/@eV@eV@d��@d��@dz�@c�@c@a�^@a7L@`��@`r�@`  @_�w@_��@_l�@_\)@_K�@_+@_�@_�@_
=@^��@^��@^�y@^�@^�@^ȴ@^��@^�+@^v�@^{@]�-@]�@]/@\�@\Z@\9X@\1@[t�@["�@[o@Z�!@Z~�@Z^5@ZM�@ZM�@Z=q@Z-@Y�#@Y�^@Y��@Y�7@Y7L@Y%@X�9@Xr�@X1'@W|�@V��@V��@Vff@V@U�@U��@U�@UO�@T�@T��@TZ@T1@S��@S33@R�H@R��@Q��@Qx�@QG�@Q7L@P�`@P1'@Pb@O�w@O�P@O|�@O;d@N�y@N�R@Nv�@Nv�@Nff@NE�@N$�@N@M@L�@Lj@L�@K��@K33@J��@J^5@JM�@I��@IX@I%@H��@H��@HbN@H  @G\)@G�@F��@F$�@E��@E��@EO�@D��@D�@D�@D��@D��@DZ@C�m@C�@CC�@C"�@B�@B��@B^5@A�@A��@A�7@Ahs@AX@A7L@@��@@bN@@1'@@  @?�@?�@>ȴ@>�R@>V@=�@=�-@=p�@=/@<��@<�/@<z�@<�@;�m@;�F@;S�@:�H@:��@:�!@:��@:�\@:M�@9��@9��@9hs@9hs@9X@9X@9G�@8��@8�9@8A�@7�@7�;@7��@7�@6�@6�R@6��@6E�@5�T@5�h@5O�@5�@4�/@4�@4z�@4j@4Z@4Z@4Z@4Z@49X@4�@3�
@3�@3@2��@2�\@2�\@2M�@1��@1��@1�7@1hs@1hs@1hs@1G�@1&�@1%@0�9@0Q�@0b@/�;@/��@/�P@.ȴ@.@-�-@-p�@-?}@-V@,��@,z�@,j@,j@,Z@,9X@,1@+ƨ@+�@+dZ@+S�@+"�@*��@*��@*^5@*J@)�#@)hs@(��@(Q�@(b@'�@'�w@'�P@'�@&�y@&�@&��@&�+@&ff@&@%@%�-@%O�@%/@%V@$�@$��@$��@$Z@#��@#�
@#ƨ@#�F@#dZ@#33@#33@#"�@#o@#@"�@"�H@"��@"~�@"�@!�@!�^@!��@!hs@!�@ Ĝ@ �@ bN@ Q�@ A�@  �@|�@;d@��@�y@��@��@�+@�+@�+@�+@�+@v�@5?@@�T@�h@/@V@��@�j@��@z�@j@Z@I�@9X@(�@��@�F@��@t�@S�@"�@@��@�\@^5@M�@=q@-@��@�#@�^@��@��@��@x�@&�@��@�9@��@r�@1'@ �@  @��@��@l�@K�@K�@K�@K�@;d@+@�@��@V@{@@�@�-@?}@/@V@��@�j@�D@z�@j@I�@9X@(�@��@�m@�
@ƨ@��@"�@�@~�@M�@=q@�@J@�@J@�@�^@��@X@G�@G�@G�@7L@7L@&�@%@�u@1'@1'@ �@b@  @�;@|�@K�@K�@;d@;d@+@�@�y@��@v�@ff@E�@�T@��@`B@V@�@��@�j@��@�D@z�@j@Z@Z@9X@ƨ@�@33@
�H@
�\@
~�@
-@
J@	��@	hs@	G�@	G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsA�n�A�z�A��DA��\A��DA��7A��PA��DA��PA��DA�r�A�hsA�^5A�ZA�XA�XA�VA�G�A�M�A�S�A�K�A�S�A� �A��TA��RA�hsA�A��mA��TA�ƨA���A�v�A�ZA�A�A�&�A��A�
=A���A��A��A��HA���A���A���A��jA��A��A���A���A���A���A���A�v�A�;dA�A��uA�7LA��/A��+A�JA��TA���A��7A��/A�;dA��RA�33A�z�A�~�A��A��A��A�%A���A�+A�hsA��TA�r�A�jA�
=A�jA�bA���A���A��mA�O�A�M�A�=qA�ȴA�jA��A�x�A���A� �A�?}A�%A~JA{�Ay"�AuG�Aq�TAp�9Ao�PAn��Am��Al��Al~�Ak��Ak��AjM�AiC�Ah�\Ah-AgXAf~�Ae|�Ae33Ad�/Ac�
Ab�!Ab5?Ab  AaC�Aa�Aa�A`Q�A^ĜA]�wA]A[\)AZ��AY��AX��AW�AU��AUAT��AS�AQ%AP-AOAOO�AOoAN��ANv�AM��AM
=AK�mAJ�jAJ1'AI�TAI��AIVAG�
AE��AEdZAD�AC��ACO�AC%AA��A?7LA>�A=�A=��A=&�A<VA;ƨA;ƨA;"�A:��A:bNA9��A8��A7K�A5ƨA5l�A4��A4M�A41A3��A2�A0�HA/�FA/�A.�A.ĜA.�A.1'A-�hA,Q�A+��A*��A)A(�!A'��A'��A'x�A&n�A$�/A#|�A!�TA!�A 5?Ax�AQ�A�A=qA�A;dAA%A9XAC�A�A�RA9XA�hAS�AĜA5?A�A��A`BAC�AVAVA��A(�A��A
=A�FA�/A�;A7LA
�A
�RA
��A
jA
1A	�7A��An�A��A��A�9A�mA�jA-A�A�-A�AȴA�PA
=A ��A  �@��@���@�I�@�9X@�o@��@�9X@�\)@���@�I�@�%@�+@�@��
@�^5@��@� �@�@۶F@���@��@�E�@Չ7@ԛ�@Ӯ@Ѻ^@���@�I�@��@�dZ@�n�@��@�Ĝ@�Z@˥�@�@�=q@��@�r�@�\)@���@�@�/@ě�@ÍP@�\)@��y@��@��`@���@��!@��7@��@�j@��;@�=q@�/@���@�S�@�
=@�ȴ@�=q@�hs@��/@��
@�ȴ@���@�?}@�Q�@��F@�C�@���@�=q@���@��D@���@�dZ@�ff@�hs@��@���@���@�"�@�v�@�@�?}@��;@�|�@�C�@��R@�M�@��#@�p�@��`@�Z@��P@���@���@���@��h@�p�@�X@�&�@��@��@��@�33@��H@�n�@�/@�V@���@��/@���@���@�"�@��y@��!@�n�@�=q@�$�@��@���@�@�@���@�G�@�&�@��/@���@��D@�Z@�1@��@��P@�l�@�\)@�K�@�"�@�ȴ@�v�@�E�@�@��h@�`B@�?}@�V@���@�b@�1@�|�@�+@�o@��!@���@��^@�7L@�V@���@��u@�1'@��
@�C�@�@��@���@�n�@�5?@�{@�@��@��#@�@���@�X@��@��@��/@��@�j@�Q�@��m@��@�|�@�@�ȴ@���@��\@�E�@�$�@�J@�@��@���@�`B@�Ĝ@�j@�(�@�b@���@�ƨ@��@�ȴ@���@���@�M�@��@���@��h@�x�@�O�@�7L@�&�@��@�Z@}�h@|��@|(�@|�@{��@{�
@{ƨ@{ƨ@{ƨ@{�F@{�@{C�@{@z��@yhs@x�@w�;@w��@w�w@w�@w�@w�P@wl�@w\)@w
=@u�@u`B@u�@t�/@t�j@t��@tZ@s�
@sC�@r�@r��@rM�@r=q@r�@rJ@q�@q�^@q�@p��@pQ�@o�@o+@n��@nff@n$�@mO�@m/@m�@mV@l��@l�@l�@lI�@k�m@kS�@j��@i�@i��@i�^@i��@i��@i�7@i�7@ihs@iX@i&�@h�`@hr�@h1'@g�@gK�@f�R@e��@e/@eV@eV@d��@d��@dz�@c�@c@a�^@a7L@`��@`r�@`  @_�w@_��@_l�@_\)@_K�@_+@_�@_�@_
=@^��@^��@^�y@^�@^�@^ȴ@^��@^�+@^v�@^{@]�-@]�@]/@\�@\Z@\9X@\1@[t�@["�@[o@Z�!@Z~�@Z^5@ZM�@ZM�@Z=q@Z-@Y�#@Y�^@Y��@Y�7@Y7L@Y%@X�9@Xr�@X1'@W|�@V��@V��@Vff@V@U�@U��@U�@UO�@T�@T��@TZ@T1@S��@S33@R�H@R��@Q��@Qx�@QG�@Q7L@P�`@P1'@Pb@O�w@O�P@O|�@O;d@N�y@N�R@Nv�@Nv�@Nff@NE�@N$�@N@M@L�@Lj@L�@K��@K33@J��@J^5@JM�@I��@IX@I%@H��@H��@HbN@H  @G\)@G�@F��@F$�@E��@E��@EO�@D��@D�@D�@D��@D��@DZ@C�m@C�@CC�@C"�@B�@B��@B^5@A�@A��@A�7@Ahs@AX@A7L@@��@@bN@@1'@@  @?�@?�@>ȴ@>�R@>V@=�@=�-@=p�@=/@<��@<�/@<z�@<�@;�m@;�F@;S�@:�H@:��@:�!@:��@:�\@:M�@9��@9��@9hs@9hs@9X@9X@9G�@8��@8�9@8A�@7�@7�;@7��@7�@6�@6�R@6��@6E�@5�T@5�h@5O�@5�@4�/@4�@4z�@4j@4Z@4Z@4Z@4Z@49X@4�@3�
@3�@3@2��@2�\@2�\@2M�@1��@1��@1�7@1hs@1hs@1hs@1G�@1&�@1%@0�9@0Q�@0b@/�;@/��@/�P@.ȴ@.@-�-@-p�@-?}@-V@,��@,z�@,j@,j@,Z@,9X@,1@+ƨ@+�@+dZ@+S�@+"�@*��@*��@*^5@*J@)�#@)hs@(��@(Q�@(b@'�@'�w@'�P@'�@&�y@&�@&��@&�+@&ff@&@%@%�-@%O�@%/@%V@$�@$��@$��@$Z@#��@#�
@#ƨ@#�F@#dZ@#33@#33@#"�@#o@#@"�@"�H@"��@"~�@"�@!�@!�^@!��@!hs@!�@ Ĝ@ �@ bN@ Q�@ A�@  �@|�@;d@��@�y@��@��@�+@�+@�+@�+@�+@v�@5?@@�T@�h@/@V@��@�j@��@z�@j@Z@I�@9X@(�@��@�F@��@t�@S�@"�@@��@�\@^5@M�@=q@-@��@�#@�^@��@��@��@x�@&�@��@�9@��@r�@1'@ �@  @��@��@l�@K�@K�@K�@K�@;d@+@�@��@V@{@@�@�-@?}@/@V@��@�j@�D@z�@j@I�@9X@(�@��@�m@�
@ƨ@��@"�@�@~�@M�@=q@�@J@�@J@�@�^@��@X@G�@G�@G�@7L@7L@&�@%@�u@1'@1'@ �@b@  @�;@|�@K�@K�@;d@;d@+@�@�y@��@v�@ff@E�@�T@��@`B@V@�@��@�j@��@�D@z�@j@Z@Z@9X@ƨ@�@33@
�H@
�\@
~�@
-@
J@	��@	hs@	G�@	G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�+B�7B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�qB�jB�dB�^B�XB�LB�?B�9B�9B�9B�?B�9B�9B�3B�-B�-B�-B�'B�'B�!B�!B�B�B�B�B�B�B��B��B��B��B�DB�BiyBH�B0!B49B9XB2-B%�B%B�`B��B��B�Bo�BQ�B@�B9XB,BJB�sB��B��B��B�\B�7B�Bw�BhsB^5BO�B>wB49B'�BB��B��B�`B�#BȴB�^B�B��B��B�By�Bs�Bn�BhsBe`BbNB_;B\)BZBS�BP�BN�BK�BF�BC�B@�B?}B;dB7LB49B49B5?B49B2-B0!B'�B$�B �B�B�B�BbB
=BB��B��B�B�HB�B�
B��B��B��B��B��B��B��B��B��B��B��B��BȴB�}B�jB�dB�LB�3B�!B�B��B��B��B��B��B��B�{B��B��B�uB�hB�PB�B}�Bz�Bx�Bs�Bp�Bo�Bl�BdZBZBVBT�BT�BS�BT�B]/B\)BW
BR�BO�BK�BH�BD�BB�BA�B=qB49B,B"�B�B�B�BhBPBDB+BBB  B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�mB
�fB
�ZB
�NB
�NB
�BB
�BB
�;B
�;B
�5B
�5B
�/B
�#B
�B
�B
�B
�B
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
ȴB
ǮB
ǮB
ÖB
��B
�}B
�}B
�qB
�jB
�dB
�jB
�dB
�^B
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�dB
�dB
�qB
�qB
�wB
�wB
�wB
�}B
��B
��B
��B
B
ÖB
ĜB
ÖB
ƨB
ǮB
ǮB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�
B
�B
�)B
�5B
�BB
�NB
�TB
�TB
�ZB
�mB
�sB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBB+B
=BVBVBoBuB�B�B�B�B$�B&�B&�B+B+B.B0!B2-B5?B7LB;dB@�B@�BB�BB�BB�BC�BE�BH�BK�BL�BO�BQ�BYBZBZB[#B\)BaHBe`BffBhsBk�Bl�Bl�Bo�Bp�Bp�Bp�Br�Bs�Bt�Bv�Bx�Bw�By�B|�B}�B}�B� B� B�B�B�+B�1B�=B�VB�\B�hB�hB�oB��B��B��B��B��B��B��B��B��B�B�B�'B�-B�?B�XB�wB��B��B��BĜBƨBǮBǮBȴBȴBɺB��B��B��B��B��B�B�B�B�#B�)B�;B�TB�`B�`B�mB�B�B�B�B�B�B�B�B��B��B��B��B��B��BBBBB+B	7BDBJBPBVBVB\B�B"�B&�B)�B)�B+B+B,B,B,B,B-B.B/B0!B7LB;dB>wB?}B?}B@�B@�B@�BA�BA�BC�BH�BJ�BK�BL�BM�BM�BN�BP�BS�BT�BW
BYBZBZBZB[#B\)B^5B`BBaHBbNBe`BhsBiyBjBm�Bm�Bn�Bn�Bn�Bn�Bo�Bq�Br�Bu�Bw�B{�B{�B{�B|�B|�B|�B|�B}�B}�B}�B� B�B�B�B�B�+B�=B�JB�PB�PB�PB�PB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�'B�'B�-B�-B�-B�-B�3B�3B�3B�9B�?B�?B�FB�LB�RB�^B�dB�jB�qB�wB�}B�}B��B��BBBÖBĜBŢBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�#B�/B�/B�/B�5B�;B�BB�HB�NB�NB�TB�ZB�ZB�`B�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B��B��B  BBBBBBBBBBBBBB%B%B+B+B1B1B	7B
=B
=B
=B
=BDBDBDBDBDBDBJBJBJBPBVB\B\B\B\BbBhBhBhBhBhBhBhBhBoBuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B"�B#�B#�B#�B#�B$�B$�B$�B%�B&�B%�B&�B&�B&�B'�B'�B'�B(�B(�B(�B)�B(�B)�B)�B)�B)�B)�B)�B+B+B,B,B,B,B-B.B.B.B.B.B.B/B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B1'B1'B2-B2-B33B33B33B49B49B49B49B49B49B49B5?B5?B5?B5?B6FB6FB6FB6FB7LB7LB7LB8RB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B;dB;dB;dB;dB<jB<jB<jB=qB<jB=qB=qB=qB=qB=qB>wB>wB?}B?}B?}B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BB�BC�BD�BD�BE�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BF�BG�BG�BG�BH�BH�BH�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BL�BL�BL�BM�BM�BM�BM�BM�BN�BN�BN�BN�BN�BN�BO�BO�BP�BP�BQ�BQ�BQ�BQ�BR�BS�BS�BS�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�+B�7B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�qB�jB�dB�^B�XB�LB�?B�9B�9B�9B�?B�9B�9B�3B�-B�-B�-B�'B�'B�!B�!B�B�B�B�B�B�B��B��B��B��B�DB�BiyBH�B0!B49B9XB2-B%�B%B�`B��B��B�Bo�BQ�B@�B9XB,BJB�sB��B��B��B�\B�7B�Bw�BhsB^5BO�B>wB49B'�BB��B��B�`B�#BȴB�^B�B��B��B�By�Bs�Bn�BhsBe`BbNB_;B\)BZBS�BP�BN�BK�BF�BC�B@�B?}B;dB7LB49B49B5?B49B2-B0!B'�B$�B �B�B�B�BbB
=BB��B��B�B�HB�B�
B��B��B��B��B��B��B��B��B��B��B��B��BȴB�}B�jB�dB�LB�3B�!B�B��B��B��B��B��B��B�{B��B��B�uB�hB�PB�B}�Bz�Bx�Bs�Bp�Bo�Bl�BdZBZBVBT�BT�BS�BT�B]/B\)BW
BR�BO�BK�BH�BD�BB�BA�B=qB49B,B"�B�B�B�BhBPBDB+BBB  B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�mB
�fB
�ZB
�NB
�NB
�BB
�BB
�;B
�;B
�5B
�5B
�/B
�#B
�B
�B
�B
�B
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
ȴB
ǮB
ǮB
ÖB
��B
�}B
�}B
�qB
�jB
�dB
�jB
�dB
�^B
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�dB
�dB
�qB
�qB
�wB
�wB
�wB
�}B
��B
��B
��B
B
ÖB
ĜB
ÖB
ƨB
ǮB
ǮB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�
B
�B
�)B
�5B
�BB
�NB
�TB
�TB
�ZB
�mB
�sB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBB+B
=BVBVBoBuB�B�B�B�B$�B&�B&�B+B+B.B0!B2-B5?B7LB;dB@�B@�BB�BB�BB�BC�BE�BH�BK�BL�BO�BQ�BYBZBZB[#B\)BaHBe`BffBhsBk�Bl�Bl�Bo�Bp�Bp�Bp�Br�Bs�Bt�Bv�Bx�Bw�By�B|�B}�B}�B� B� B�B�B�+B�1B�=B�VB�\B�hB�hB�oB��B��B��B��B��B��B��B��B��B�B�B�'B�-B�?B�XB�wB��B��B��BĜBƨBǮBǮBȴBȴBɺB��B��B��B��B��B�B�B�B�#B�)B�;B�TB�`B�`B�mB�B�B�B�B�B�B�B�B��B��B��B��B��B��BBBBB+B	7BDBJBPBVBVB\B�B"�B&�B)�B)�B+B+B,B,B,B,B-B.B/B0!B7LB;dB>wB?}B?}B@�B@�B@�BA�BA�BC�BH�BJ�BK�BL�BM�BM�BN�BP�BS�BT�BW
BYBZBZBZB[#B\)B^5B`BBaHBbNBe`BhsBiyBjBm�Bm�Bn�Bn�Bn�Bn�Bo�Bq�Br�Bu�Bw�B{�B{�B{�B|�B|�B|�B|�B}�B}�B}�B� B�B�B�B�B�+B�=B�JB�PB�PB�PB�PB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�'B�'B�-B�-B�-B�-B�3B�3B�3B�9B�?B�?B�FB�LB�RB�^B�dB�jB�qB�wB�}B�}B��B��BBBÖBĜBŢBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�#B�/B�/B�/B�5B�;B�BB�HB�NB�NB�TB�ZB�ZB�`B�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B��B��B  BBBBBBBBBBBBBB%B%B+B+B1B1B	7B
=B
=B
=B
=BDBDBDBDBDBDBJBJBJBPBVB\B\B\B\BbBhBhBhBhBhBhBhBhBoBuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B"�B#�B#�B#�B#�B$�B$�B$�B%�B&�B%�B&�B&�B&�B'�B'�B'�B(�B(�B(�B)�B(�B)�B)�B)�B)�B)�B)�B+B+B,B,B,B,B-B.B.B.B.B.B.B/B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B1'B1'B2-B2-B33B33B33B49B49B49B49B49B49B49B5?B5?B5?B5?B6FB6FB6FB6FB7LB7LB7LB8RB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B;dB;dB;dB;dB<jB<jB<jB=qB<jB=qB=qB=qB=qB=qB>wB>wB?}B?}B?}B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BB�BC�BD�BD�BE�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BF�BG�BG�BG�BH�BH�BH�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BL�BL�BL�BM�BM�BM�BM�BM�BN�BN�BN�BN�BN�BN�BO�BO�BP�BP�BQ�BQ�BQ�BQ�BR�BS�BS�BS�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220203100107                              AO  ARCAADJP                                                                    20220203100107    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220203100107  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220203100107  QCF$                G�O�G�O�G�O�8000            