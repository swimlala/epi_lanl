CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-03T12:43:04Z creation;2023-01-03T12:43:05Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230103124304  20230103132725  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�
XX�1   @�
X���S@.�+J�c�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  BffB��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B~ffB�ffB�ffB���B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  Bș�B˙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C833C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�G@�=q@�=qA�A=�A]�A}�A��\A��\A��\A�\)AΏ\Aޏ\A�\A��\BG�BG�B�B�HB'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�B}�B�
=B�
=B�p�B���B���B���B���B���B��
B���B�p�B���B���B���B���B���Bã�B�=qB�=qBϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C�C�C	��C��C��C��C��C��C��C��C��C��C�RC��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5�C8C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�#�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�+�A�*eA�,�A�-�A�/OA�/OA�3�A�6zA�5tA�1�A�1�A�,qA�1�A�.�A�-�A��A��A���A���A��fA��TA��|A�уAѣ�AѠ\AыDA�R�A�A�LdA�=�A��qA�qA��A���A�H�A��A���A���A��A�E�A�l�A�T,A�=�A�7A��A��kA���A�N<A�W�A��A��8A��mA���A���A�2�A�%FA��A���A���A|H�Axa|Ar�Ap�+An4�Ak�AjOvAi��Ai4Ah�AeVA_RTA]��A[AW�|AW_pAU�WAR��ANffAL�ALQAJ@OAE9XAC��AB��A<�A5�8A35�A2A�A0w�A-�A-� A-�:A-�A*�A)RTA'{JA"��A ��A�FA�A�VAcA	A�MA� A��A�pAd�AѷA�4A��Ag8A�AtTAu�AQ�A�9AQA	A�?A;AخAu%AuA��A|�AY�A�bAH�A �A�AxlAu�A�"A��AcAA�AϫA"�A��AR�A
��A
�uA	֡A	��A	e,A�WAx�A6�A4�A	A�A��A1�A�ZA�sA�A�~A�_Aa�A��A	A,�A/AeA_A u�@��D@��@���@���@�J�@�e�@���@���@��@��e@���@���@� i@��@�m�@�Y@�O@��x@��Z@��3@��K@�p�@��@��P@��@��[@��/@��)@�@���@�GE@�ݘ@���@�!�@���@�M�@��@랄@�r@�@�ں@�^5@�u%@��@�p�@�@�@O@�PH@��@傪@��@�C@���@�D@�0@�a�@�C@��v@�{�@�%F@�~@ܳh@�!�@�J�@��)@�@@�n�@�=q@��@��@ٮ�@�y�@�y>@��d@ס�@׳�@�a@�q@�|�@֑�@�:�@��5@���@Ӯ�@ӗ$@�V@�ѷ@ҵ�@Ҵ9@�a|@��@�'�@Ї�@��@���@�hs@��@�w�@�:*@�!@��@�\)@�!-@̣@�ƨ@ˍP@�%F@ʩ�@��+@�@@�ѷ@��@��@�RT@Ƽj@ơb@Ɓo@���@�Dg@���@Ā�@�	�@��@1@�Xy@��@��@�y�@�u�@�S�@�1�@��8@���@��_@�c�@�:�@���@��@�@��x@�-@���@�4@� i@���@�҉@���@���@��'@��'@��@�r�@�@��@��@�A @�҉@��A@���@�g�@��@��?@�xl@�U2@�GE@��@��Q@��'@��@��?@�?�@��0@�!-@��@�q�@�	@�ƨ@���@��	@��@��@���@�&�@���@�J#@��R@�e@��=@��S@�S�@��@�9X@�˒@��"@�`B@�.I@��@��,@��@�{�@�3�@���@�)_@��|@�ں@���@��D@���@�j@�A @�{�@��@���@�Y�@��@���@�R�@�خ@�`B@�Y@��c@�֡@���@�9X@���@��9@���@���@�e,@�@@��@�r�@�%�@��@��4@�S�@�<6@�*0@��@��@���@��@�c @�%�@���@���@��"@�n/@��@��_@��@�|�@�h
@�U2@�'R@��@��~@�-w@���@�M�@�/�@�M@��@�˒@��0@���@�dZ@�8�@�q@���@��u@�?@�J@��@�RT@�� @�.�@��A@���@�ƨ@��k@�g�@�/@��K@���@���@���@��\@�I�@�@��]@��@��+@��@��'@��@��D@�>B@��@�O@�@���@�s�@�1'@��]@��@��3@��H@���@��P@�o @��@���@���@��Y@��@��@��@���@���@�w�@�/�@���@�ԕ@���@��P@�8@�ѷ@���@�h
@�&�@�  @��T@��d@���@���@�c @��@��D@��@��W@�@�j@�)_@��@���@�Ta@��@��j@��H@�~�@��@���@�|�@�-@��	@��@��@��X@��4@��@���@��q@���@�K�@�֡@���@�:�@�u@���@��h@�iD@�>�@�*0@���@��@�Q@�/�@�	�@�$@�@~C�@}rG@|��@|�@{��@{F�@z�@z~�@z+k@y��@yc�@y8�@y�@x�/@xw�@xM@w��@w�a@w��@wa@v�@v!�@v �@u��@uT�@tPH@tx@s��@s�@sX�@r��@r�}@r�}@r�@r�'@r��@r��@r�h@rq�@q�Z@q��@q�'@q��@qa�@q-w@p��@p�I@p�_@pm�@o��@ox@oH�@o"�@n��@n�2@nߤ@n�,@n��@m��@mzx@l�/@lm�@l'R@k� @k�@j��@i�z@i�n@iF@h�f@h�@i@@h��@hz�@h�@gb�@g�@f��@f}V@fZ�@fOv@e�T@eA @d�@dj@c��@c,�@b��@b��@b{�@b3�@ac@`��@`$@_��@_�4@^�,@^{�@^ �@]�@]�n@]��@]#�@\�@\1'@[�q@[=@Z�]@Zs�@Z_@Y@YB�@Y \@Y�@X��@X�I@XC-@W�m@WC@V6�@U�T@U@@T�U@T��@TC-@S�]@S�&@S��@SW?@R�8@RYK@Q��@P�p@Pe�@PU2@P%�@O��@O�{@O(@N��@Np;@M�@M�@Mx�@M=�@L��@L/�@L!@L�@K�
@KC�@J�@J��@J_@H��@G�m@G�@Fff@E�@EN<@E+�@E�@D�K@D��@D|�@D�@C��@CS�@B�@B�m@B��@Bff@B �@A`B@@��@@ی@@�e@@�@@oi@@q@@l"@@c�@@`�@@�@?�@?�*@?S�@>� @>d�@>$�@=��@=�C@=X@<�K@<ѷ@<��@<�Y@<:�@;� @;U�@;'�@:�c@:��@:H�@:+k@:	@9��@9�#@9��@9c�@9A @8��@8|�@8Q�@8M@7�r@7��@7e�@7F�@6�B@6�@6i�@6e@5�@5�X@5X@4�f@4�9@4tT@47@3��@3A�@2�2@2�@1ϫ@1�z@1��@1��@1�X@1x�@1<6@0�f@0�I@0V�@/��@/��@.��@.�r@._�@.?@-�)@-�=@-hs@,�	@,��@,�e@,q@,$@,x@+�Q@+�$@+dZ@+W?@+J#@+=@+�@*��@*�@*�+@*4@)��@)s�@)\�@)<6@)�@(��@(��@(�4@(j@(2�@(@(�@'��@'�f@'@&��@&M�@%��@%�~@%N<@%*0@%!�@%@%�@$�$@$]d@$$@#�m@#��@#_p@#.I@"�c@"�x@"h
@"H�@!�-@!J�@!�@ ��@ ��@ <�@�r@�6@��@dZ@�@��@�c@��@~�@5?@��@zx@+@�@ی@�)@��@tT@c�@S�@C-@1@�@��@�a@��@,�@�c@�m@kQ@@��@��@��@�~@}�@hs@T�@?}@4@�@�p@��@��@c�@'R@�+@��@��@�[@RT@�]@��@{�@L0@_@��@zx@rG@X@%F@+@%@�|@Ĝ@��@��@�@1'@�@  @��@�a@��@�@\)@9�@S@�}@q�@M�@{@�D@�T@�@�@�"@B�@�@�@�	@��@��@��@~(@_@I�@7�@@��@�4@J#@��@��@��@p;@{@�@�t@x�@^�@L�@B�@4@ \@�@�@�v@�E@�?@�O@��@V�@/�@�@~@�@�m@��@�f@qv@W?@>�@1�@�@
��@
^5@
�@
	@	�@	��@	�@	�M@	x�@	^�@	�@�`@֡111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�+�A�*eA�,�A�-�A�/OA�/OA�3�A�6zA�5tA�1�A�1�A�,qA�1�A�.�A�-�A��A��A���A���A��fA��TA��|A�уAѣ�AѠ\AыDA�R�A�A�LdA�=�A��qA�qA��A���A�H�A��A���A���A��A�E�A�l�A�T,A�=�A�7A��A��kA���A�N<A�W�A��A��8A��mA���A���A�2�A�%FA��A���A���A|H�Axa|Ar�Ap�+An4�Ak�AjOvAi��Ai4Ah�AeVA_RTA]��A[AW�|AW_pAU�WAR��ANffAL�ALQAJ@OAE9XAC��AB��A<�A5�8A35�A2A�A0w�A-�A-� A-�:A-�A*�A)RTA'{JA"��A ��A�FA�A�VAcA	A�MA� A��A�pAd�AѷA�4A��Ag8A�AtTAu�AQ�A�9AQA	A�?A;AخAu%AuA��A|�AY�A�bAH�A �A�AxlAu�A�"A��AcAA�AϫA"�A��AR�A
��A
�uA	֡A	��A	e,A�WAx�A6�A4�A	A�A��A1�A�ZA�sA�A�~A�_Aa�A��A	A,�A/AeA_A u�@��D@��@���@���@�J�@�e�@���@���@��@��e@���@���@� i@��@�m�@�Y@�O@��x@��Z@��3@��K@�p�@��@��P@��@��[@��/@��)@�@���@�GE@�ݘ@���@�!�@���@�M�@��@랄@�r@�@�ں@�^5@�u%@��@�p�@�@�@O@�PH@��@傪@��@�C@���@�D@�0@�a�@�C@��v@�{�@�%F@�~@ܳh@�!�@�J�@��)@�@@�n�@�=q@��@��@ٮ�@�y�@�y>@��d@ס�@׳�@�a@�q@�|�@֑�@�:�@��5@���@Ӯ�@ӗ$@�V@�ѷ@ҵ�@Ҵ9@�a|@��@�'�@Ї�@��@���@�hs@��@�w�@�:*@�!@��@�\)@�!-@̣@�ƨ@ˍP@�%F@ʩ�@��+@�@@�ѷ@��@��@�RT@Ƽj@ơb@Ɓo@���@�Dg@���@Ā�@�	�@��@1@�Xy@��@��@�y�@�u�@�S�@�1�@��8@���@��_@�c�@�:�@���@��@�@��x@�-@���@�4@� i@���@�҉@���@���@��'@��'@��@�r�@�@��@��@�A @�҉@��A@���@�g�@��@��?@�xl@�U2@�GE@��@��Q@��'@��@��?@�?�@��0@�!-@��@�q�@�	@�ƨ@���@��	@��@��@���@�&�@���@�J#@��R@�e@��=@��S@�S�@��@�9X@�˒@��"@�`B@�.I@��@��,@��@�{�@�3�@���@�)_@��|@�ں@���@��D@���@�j@�A @�{�@��@���@�Y�@��@���@�R�@�خ@�`B@�Y@��c@�֡@���@�9X@���@��9@���@���@�e,@�@@��@�r�@�%�@��@��4@�S�@�<6@�*0@��@��@���@��@�c @�%�@���@���@��"@�n/@��@��_@��@�|�@�h
@�U2@�'R@��@��~@�-w@���@�M�@�/�@�M@��@�˒@��0@���@�dZ@�8�@�q@���@��u@�?@�J@��@�RT@�� @�.�@��A@���@�ƨ@��k@�g�@�/@��K@���@���@���@��\@�I�@�@��]@��@��+@��@��'@��@��D@�>B@��@�O@�@���@�s�@�1'@��]@��@��3@��H@���@��P@�o @��@���@���@��Y@��@��@��@���@���@�w�@�/�@���@�ԕ@���@��P@�8@�ѷ@���@�h
@�&�@�  @��T@��d@���@���@�c @��@��D@��@��W@�@�j@�)_@��@���@�Ta@��@��j@��H@�~�@��@���@�|�@�-@��	@��@��@��X@��4@��@���@��q@���@�K�@�֡@���@�:�@�u@���@��h@�iD@�>�@�*0@���@��@�Q@�/�@�	�@�$@�@~C�@}rG@|��@|�@{��@{F�@z�@z~�@z+k@y��@yc�@y8�@y�@x�/@xw�@xM@w��@w�a@w��@wa@v�@v!�@v �@u��@uT�@tPH@tx@s��@s�@sX�@r��@r�}@r�}@r�@r�'@r��@r��@r�h@rq�@q�Z@q��@q�'@q��@qa�@q-w@p��@p�I@p�_@pm�@o��@ox@oH�@o"�@n��@n�2@nߤ@n�,@n��@m��@mzx@l�/@lm�@l'R@k� @k�@j��@i�z@i�n@iF@h�f@h�@i@@h��@hz�@h�@gb�@g�@f��@f}V@fZ�@fOv@e�T@eA @d�@dj@c��@c,�@b��@b��@b{�@b3�@ac@`��@`$@_��@_�4@^�,@^{�@^ �@]�@]�n@]��@]#�@\�@\1'@[�q@[=@Z�]@Zs�@Z_@Y@YB�@Y \@Y�@X��@X�I@XC-@W�m@WC@V6�@U�T@U@@T�U@T��@TC-@S�]@S�&@S��@SW?@R�8@RYK@Q��@P�p@Pe�@PU2@P%�@O��@O�{@O(@N��@Np;@M�@M�@Mx�@M=�@L��@L/�@L!@L�@K�
@KC�@J�@J��@J_@H��@G�m@G�@Fff@E�@EN<@E+�@E�@D�K@D��@D|�@D�@C��@CS�@B�@B�m@B��@Bff@B �@A`B@@��@@ی@@�e@@�@@oi@@q@@l"@@c�@@`�@@�@?�@?�*@?S�@>� @>d�@>$�@=��@=�C@=X@<�K@<ѷ@<��@<�Y@<:�@;� @;U�@;'�@:�c@:��@:H�@:+k@:	@9��@9�#@9��@9c�@9A @8��@8|�@8Q�@8M@7�r@7��@7e�@7F�@6�B@6�@6i�@6e@5�@5�X@5X@4�f@4�9@4tT@47@3��@3A�@2�2@2�@1ϫ@1�z@1��@1��@1�X@1x�@1<6@0�f@0�I@0V�@/��@/��@.��@.�r@._�@.?@-�)@-�=@-hs@,�	@,��@,�e@,q@,$@,x@+�Q@+�$@+dZ@+W?@+J#@+=@+�@*��@*�@*�+@*4@)��@)s�@)\�@)<6@)�@(��@(��@(�4@(j@(2�@(@(�@'��@'�f@'@&��@&M�@%��@%�~@%N<@%*0@%!�@%@%�@$�$@$]d@$$@#�m@#��@#_p@#.I@"�c@"�x@"h
@"H�@!�-@!J�@!�@ ��@ ��@ <�@�r@�6@��@dZ@�@��@�c@��@~�@5?@��@zx@+@�@ی@�)@��@tT@c�@S�@C-@1@�@��@�a@��@,�@�c@�m@kQ@@��@��@��@�~@}�@hs@T�@?}@4@�@�p@��@��@c�@'R@�+@��@��@�[@RT@�]@��@{�@L0@_@��@zx@rG@X@%F@+@%@�|@Ĝ@��@��@�@1'@�@  @��@�a@��@�@\)@9�@S@�}@q�@M�@{@�D@�T@�@�@�"@B�@�@�@�	@��@��@��@~(@_@I�@7�@@��@�4@J#@��@��@��@p;@{@�@�t@x�@^�@L�@B�@4@ \@�@�@�v@�E@�?@�O@��@V�@/�@�@~@�@�m@��@�f@qv@W?@>�@1�@�@
��@
^5@
�@
	@	�@	��@	�@	�M@	x�@	^�@	�@�`@֡111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	)�B	*B	*B	)*B	'�B	'�B	'B	'�B	'�B	'RB	&�B	&�B	&B	&�B	&2B	&B	#�B	!HB	�B	�B	B	�B	CB	�B	?B	_B	B	/B	&�B	N�B	��B
B
�B
?}B
u�B
r-B
qAB
u�B
zB
y�B
}VB
��B
�oB
x�B
v�B
tB
mCB
^�B
T�B
Z�B
C�B
*KB
�B
�B
�B
�B
B
�B
�B	��B	�B	��B	��B	��B	��B	�7B	��B	�B	|�B	xB	hXB	N�B	G�B	@�B	88B	4�B	/OB	 �B	�B	�B	
�B	�B��B�HB	�B��B�jB�MB�B��B�B�"B�B�B�5B�+B�^B�0B�B�_B�B��B	4B	�B	SB	B	�B	"�B	.}B	1vB	>(B	KDB	l=B	�zB	��B	�\B	��B	�&B	�aB	��B	��B	�B	�YB	�YB	�mB	�mB	��B	�sB	�9B	�mB	�mB	�YB	��B	�qB	�]B	�)B	��B	��B	��B	�#B	��B	�)B	��B	�5B	�!B	��B	�VB	�B	��B	��B	��B	��B	�TB	��B	��B	��B	��B	��B	ĶB	ƨB	�1B	�rB	�)B	�B	͹B	��B	��B	�jB	ΊB	οB	��B	��B	̳B	��B	ʦB	�B	ƨB	�?B	�B	�NB	��B	ѝB	бB	�B	�(B	�JB	˒B	�B	ЗB	��B	�aB	�YB	רB	�KB	�B	�B	ٚB	�	B	�WB	�B	�B	ңB	уB	ѝB	��B	�yB	ںB	�EB	֡B	��B	�NB	�bB	�~B	�tB	�ZB	�B	��B	�B	߾B	�xB	�jB	�B	ߤB	�pB	ߤB	�B	�B	�B	޸B	�IB	�5B	߾B	�B	�vB	�OB	�~B	��B	��B	�B	�&B	��B	�B	��B	��B	�B	�cB	�B	�ZB	�B	�B	�'B	�B	�B	�B	�GB	��B	��B	�B	�RB	�fB	�B	��B	��B	�tB	��B	��B	�`B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�zB	��B	�FB	�B	�+B	��B	��B	��B	��B	�B	�FB	��B	��B	�+B	��B	�B	�FB	�B	��B	�FB	�+B	��B	�fB	�2B	��B	��B	��B	�fB	�8B	��B	��B	�2B	��B	��B	�8B	�RB	��B	��B	�6B	��B	��B	�}B
 B
�B
�B
�B
aB
�B
mB
�B
�B
tB
B
�B
�B
�B
�B
+B
�B
�B
B
�B
�B
�B
�B
	RB
	�B
	lB
	lB
	�B

	B

#B

�B

�B

�B
xB
�B
0B
�B
JB
�B
�B
PB
�B
�B
�B
B
�B
BB
vB
�B
B
�B
�B
�B
 B
4B
�B
�B
�B
�B
&B
�B
�B
@B
�B
aB
B
gB
�B
gB
�B
�B
mB
�B
$B
$B

B
�B
�B
B
+B
�B
�B
�B
kB
�B
	B
�B
�B
�B
�B
xB
�B
/B
IB
dB
/B
B
B
B
B
B
�B
�B
5B
jB
B
�B
dB
�B
�B
B
�B
B
 BB
 �B
 �B
 �B
 �B
 �B
!-B
!HB
!HB
 �B
�B
~B
�B
�B
�B
 �B
!B
 �B
!|B
!�B
!�B
!�B
!�B
 �B
 vB
!�B
"�B
"�B
# B
#B
!�B
"NB
"B
!�B
!|B
!�B
!�B
"4B
"�B
#�B
#�B
$tB
%�B
&�B
'B
'8B
'�B
'�B
'�B
'mB
(
B
)DB
)�B
*0B
*eB
*0B
*0B
*�B
*�B
+B
+QB
+�B
+�B
+6B
*�B
+B
+B
*�B
*�B
*KB
+kB
+�B
+�B
+�B
+�B
+�B
,=B
,�B
-)B
-�B
-�B
.IB
.�B
/ B
/5B
/�B
0�B
0�B
0�B
0oB
1[B
1�B
1�B
1�B
1�B
2�B
3MB
3hB
3hB
3�B
4TB
4nB
4�B
5?B
5�B
5�B
6B
6+B
6+B
6`B
6�B
7B
7B
7LB
7�B
7�B
8lB
8�B
8�B
9	B
9XB
9XB
9XB
9	B
8�B
8�B
8lB
8�B
8RB
8�B
8�B
9XB
8�B
:*B
:xB
:DB
:�B
;B
:�B
:�B
:�B
:B
9�B
9$B
8�B
8�B
9�B
:^B
:�B
:�B
:�B
;B
;JB
;dB
<B
="B
=�B
>(B
?.B
?}B
?�B
?�B
?�B
?}B
?�B
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
A;B
A�B
A�B
B[B
BuB
B�B
CaB
C�B
E9B
F?B
FYB
FYB
F%B
F�B
G�B
H�B
H�B
IB
I�B
I�B
J=B
J�B
J�B
J�B
KxB
K^B
KDB
K�B
K�B
KxB
KDB
KxB
KxB
KxB
L0B
LdB
L0B
L�B
L~B
MB
L�B
MB
MB
MB
M6B
M�B
M�B
M�B
M�B
N"B
M�B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
O�B
P�B
P�B
Q B
QhB
QhB
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R B
Q�B
SB
TB
T,B
T{B
UMB
UgB
U�B
V9B
V9B
V�B
V�B
W$B
WYB
WYB
WsB
WsB
W�B
W�B
XyB
X�B
X�B
X�B
Y�B
Z7B
Z�B
[#B
[�B
[�B
[�B
\B
\)B
\CB
\]B
\�B
\�B
]/B
]dB
]~B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_!B
_!B
_VB
_�B
_�B
`'B
`'B
`BB
`vB
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
bhB
c B
cB
c�B
c�B
c�B
c�B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
fB
f2B
fLB
f�B
gB
g8B
h$B
h$B
h
B
h
B
h
B
g�B
h>B
h>B
hXB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jKB
jB
jB
kB
kB
kB
k6B
kkB
kkB
k�B
k�B
k�B
lB
lB
lB
l=B
l=B
lWB
lqB
l�B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nB
nB
nB
ncB
n�B
n�B
oOB
o�B
pB
p;B
pUB
pUB
pUB
p;B
p�B
p�B
p�B
qB
q'B
qvB
q�B
q�B
q�B
rB
rGB
r�B
r�B
sB
sMB
shB
s�B
tB
tB
tB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u�B
u�B
vB
v+B
vB
vFB
v+B
vzB
v�B
v�B
v�B
v�B
wB
wB
wB
wB
wfB
w�B
w�B
x8B
x�B
x�B
x�B
y	B
y	B
y$B
y$B
y>B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
zB
z*B
z^B
zDB
zDB
z�B
{B
z�B
{0B
{B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
|�B
}B
}<B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~wB
~�B
~�B
.B
HB
}B
�B
�B
�B
�B
� B
�B
�4B
�iB
��B
��B
��B
��B
��B
�B
��B
�B
� B
� B
�UB
�oB
��B
��B
��B
�AB
��B
��B
��B
�GB
�{B
��B
��B
��B
��B
��B
�B
�MB
�MB
�MB
�MB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�9B
�9B
�mB
��B
��B
�%B
�%B
�YB
�YB
�YB
��B
�tB
��B
�B
�+B
�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	)�B	*B	*B	)*B	'�B	'�B	'B	'�B	'�B	'RB	&�B	&�B	&B	&�B	&2B	&B	#�B	!HB	�B	�B	B	�B	CB	�B	?B	_B	B	/B	&�B	N�B	��B
B
�B
?}B
u�B
r-B
qAB
u�B
zB
y�B
}VB
��B
�oB
x�B
v�B
tB
mCB
^�B
T�B
Z�B
C�B
*KB
�B
�B
�B
�B
B
�B
�B	��B	�B	��B	��B	��B	��B	�7B	��B	�B	|�B	xB	hXB	N�B	G�B	@�B	88B	4�B	/OB	 �B	�B	�B	
�B	�B��B�HB	�B��B�jB�MB�B��B�B�"B�B�B�5B�+B�^B�0B�B�_B�B��B	4B	�B	SB	B	�B	"�B	.}B	1vB	>(B	KDB	l=B	�zB	��B	�\B	��B	�&B	�aB	��B	��B	�B	�YB	�YB	�mB	�mB	��B	�sB	�9B	�mB	�mB	�YB	��B	�qB	�]B	�)B	��B	��B	��B	�#B	��B	�)B	��B	�5B	�!B	��B	�VB	�B	��B	��B	��B	��B	�TB	��B	��B	��B	��B	��B	ĶB	ƨB	�1B	�rB	�)B	�B	͹B	��B	��B	�jB	ΊB	οB	��B	��B	̳B	��B	ʦB	�B	ƨB	�?B	�B	�NB	��B	ѝB	бB	�B	�(B	�JB	˒B	�B	ЗB	��B	�aB	�YB	רB	�KB	�B	�B	ٚB	�	B	�WB	�B	�B	ңB	уB	ѝB	��B	�yB	ںB	�EB	֡B	��B	�NB	�bB	�~B	�tB	�ZB	�B	��B	�B	߾B	�xB	�jB	�B	ߤB	�pB	ߤB	�B	�B	�B	޸B	�IB	�5B	߾B	�B	�vB	�OB	�~B	��B	��B	�B	�&B	��B	�B	��B	��B	�B	�cB	�B	�ZB	�B	�B	�'B	�B	�B	�B	�GB	��B	��B	�B	�RB	�fB	�B	��B	��B	�tB	��B	��B	�`B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�zB	��B	�FB	�B	�+B	��B	��B	��B	��B	�B	�FB	��B	��B	�+B	��B	�B	�FB	�B	��B	�FB	�+B	��B	�fB	�2B	��B	��B	��B	�fB	�8B	��B	��B	�2B	��B	��B	�8B	�RB	��B	��B	�6B	��B	��B	�}B
 B
�B
�B
�B
aB
�B
mB
�B
�B
tB
B
�B
�B
�B
�B
+B
�B
�B
B
�B
�B
�B
�B
	RB
	�B
	lB
	lB
	�B

	B

#B

�B

�B

�B
xB
�B
0B
�B
JB
�B
�B
PB
�B
�B
�B
B
�B
BB
vB
�B
B
�B
�B
�B
 B
4B
�B
�B
�B
�B
&B
�B
�B
@B
�B
aB
B
gB
�B
gB
�B
�B
mB
�B
$B
$B

B
�B
�B
B
+B
�B
�B
�B
kB
�B
	B
�B
�B
�B
�B
xB
�B
/B
IB
dB
/B
B
B
B
B
B
�B
�B
5B
jB
B
�B
dB
�B
�B
B
�B
B
 BB
 �B
 �B
 �B
 �B
 �B
!-B
!HB
!HB
 �B
�B
~B
�B
�B
�B
 �B
!B
 �B
!|B
!�B
!�B
!�B
!�B
 �B
 vB
!�B
"�B
"�B
# B
#B
!�B
"NB
"B
!�B
!|B
!�B
!�B
"4B
"�B
#�B
#�B
$tB
%�B
&�B
'B
'8B
'�B
'�B
'�B
'mB
(
B
)DB
)�B
*0B
*eB
*0B
*0B
*�B
*�B
+B
+QB
+�B
+�B
+6B
*�B
+B
+B
*�B
*�B
*KB
+kB
+�B
+�B
+�B
+�B
+�B
,=B
,�B
-)B
-�B
-�B
.IB
.�B
/ B
/5B
/�B
0�B
0�B
0�B
0oB
1[B
1�B
1�B
1�B
1�B
2�B
3MB
3hB
3hB
3�B
4TB
4nB
4�B
5?B
5�B
5�B
6B
6+B
6+B
6`B
6�B
7B
7B
7LB
7�B
7�B
8lB
8�B
8�B
9	B
9XB
9XB
9XB
9	B
8�B
8�B
8lB
8�B
8RB
8�B
8�B
9XB
8�B
:*B
:xB
:DB
:�B
;B
:�B
:�B
:�B
:B
9�B
9$B
8�B
8�B
9�B
:^B
:�B
:�B
:�B
;B
;JB
;dB
<B
="B
=�B
>(B
?.B
?}B
?�B
?�B
?�B
?}B
?�B
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
A;B
A�B
A�B
B[B
BuB
B�B
CaB
C�B
E9B
F?B
FYB
FYB
F%B
F�B
G�B
H�B
H�B
IB
I�B
I�B
J=B
J�B
J�B
J�B
KxB
K^B
KDB
K�B
K�B
KxB
KDB
KxB
KxB
KxB
L0B
LdB
L0B
L�B
L~B
MB
L�B
MB
MB
MB
M6B
M�B
M�B
M�B
M�B
N"B
M�B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
O�B
P�B
P�B
Q B
QhB
QhB
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R B
Q�B
SB
TB
T,B
T{B
UMB
UgB
U�B
V9B
V9B
V�B
V�B
W$B
WYB
WYB
WsB
WsB
W�B
W�B
XyB
X�B
X�B
X�B
Y�B
Z7B
Z�B
[#B
[�B
[�B
[�B
\B
\)B
\CB
\]B
\�B
\�B
]/B
]dB
]~B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_!B
_!B
_VB
_�B
_�B
`'B
`'B
`BB
`vB
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
bhB
c B
cB
c�B
c�B
c�B
c�B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
fB
f2B
fLB
f�B
gB
g8B
h$B
h$B
h
B
h
B
h
B
g�B
h>B
h>B
hXB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jKB
jB
jB
kB
kB
kB
k6B
kkB
kkB
k�B
k�B
k�B
lB
lB
lB
l=B
l=B
lWB
lqB
l�B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nB
nB
nB
ncB
n�B
n�B
oOB
o�B
pB
p;B
pUB
pUB
pUB
p;B
p�B
p�B
p�B
qB
q'B
qvB
q�B
q�B
q�B
rB
rGB
r�B
r�B
sB
sMB
shB
s�B
tB
tB
tB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u�B
u�B
vB
v+B
vB
vFB
v+B
vzB
v�B
v�B
v�B
v�B
wB
wB
wB
wB
wfB
w�B
w�B
x8B
x�B
x�B
x�B
y	B
y	B
y$B
y$B
y>B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
zB
z*B
z^B
zDB
zDB
z�B
{B
z�B
{0B
{B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
|�B
}B
}<B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~wB
~�B
~�B
.B
HB
}B
�B
�B
�B
�B
� B
�B
�4B
�iB
��B
��B
��B
��B
��B
�B
��B
�B
� B
� B
�UB
�oB
��B
��B
��B
�AB
��B
��B
��B
�GB
�{B
��B
��B
��B
��B
��B
�B
�MB
�MB
�MB
�MB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�9B
�9B
�mB
��B
��B
�%B
�%B
�YB
�YB
�YB
��B
�tB
��B
�B
�+B
�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230103124238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230103124304  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230103124305  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230103124305                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230103124305  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230103124305  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230103132725                      G�O�G�O�G�O�                