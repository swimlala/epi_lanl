CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:14:47Z creation;2022-06-04T19:14:48Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191447  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��5h��51   @��5�
=q@/hr� Ĝ�d�j~��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�33B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0L�C1��C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @{@tz�@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\A�\)A�\)A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B�=qB���B�p�B�=qB��
B���B���B���B�=qBã�Bǣ�B�p�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C�C�C��C��C	��C��C�RC��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C0�C1��C3�RC5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtz�Dt��Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��
D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A�!�A��A俱A䣣A�A䎿A�}VA�o�A�S�A�#�A�	�A��A��|A��;A��8A��A��>A��ZA��dA��[A��3A��[A��A�BA��zA��A�JA�~�A�t�A�U�A�L0A�(�A���A��^A���A��A�
�A���A�J�Aڣ:A���AƝ~A�R�A���A���A��\A�(XA��A��A�V9A��VA�FA�oA�A�%A�,=A�"�A�p;A�M�A��A�
rA�$A���A��|A�E�A��HA�=�A�TaA�{�A�5?A���A���A��A�gmA�C-A��5A��
A���A�%FA�W
A���A��dA�?}A��qA��pA�3Aw�Aq#�Aj��Ag�6Ae�XAb�LA`e�A^G�A[��AXsAV�<AR�`AQ<�AO��AN�AM  AI�PAE��ADCAAMA?\)A=�|A=Y�A<��A<�oA;n/A9B�A8�A7��A7hsA6�vA6Q�A5�3A5�uA5]dA3��A1-wA/��A.͟A,��A*�FA)�A(�A&�1A%1A$u%A#s�A!4nA��AJ�A�hA�A��A�+A�AA�jA��AGA�MA�AS&A��A�fA�vAYKA�A%A�[A�-A��A��A��A��A�hA�A�9A�+AݘAB�A�]A��A �A&�Al"A�6A*�A�A($A�?A��A!-A�hA^�A�HA;�AR�A�Ao�A;A��AQ�A��A�rAj�A
��A	�A	l"A~(A3�A��A'RA��As�A�AA A��AYAVmA%FA�AkQAW�A�AU2A ��@��)@���@���@��*@�Xy@���@��@�j@�Mj@�_�@�C@�ݘ@���@��h@�O�@�p;@�iD@��@�Xy@���@�X@�?@�4�@�f�@�($@�i�@��@�@�ݘ@�u�@�w2@��@� @��@涮@�@�P@��@��m@�~(@�M�@���@ᧇ@ᝲ@�zx@�Y�@�.I@��@��@��+@��@�@߅@���@��m@�8�@ݚk@�s@�)_@�>B@�O@ڈ�@���@ُ�@ك{@�{J@�@�c�@�s�@�"h@��@�RT@��@�^5@��@�a�@��@�=q@�g�@�҉@�Ta@��@ϓ@�(@Τ�@�S�@�� @�a�@�!�@�Ɇ@�}V@�8�@��@�u@˝�@�f�@��@��@�(@��B@�ȴ@ȿ�@ȉ�@���@�>�@�=q@��@ŝ�@�C@Ļ�@�u%@��@�)_@¹$@�@�zx@�q@�q�@��@���@�g�@���@���@�i�@��@�'�@��h@�(�@�6z@��@��!@��o@�d�@�H�@�b@��T@��[@�F@�q@��[@��@���@�'R@�dZ@��@���@�0U@���@���@��@�8@��v@���@�y>@�ݘ@�.I@��@���@�z@�7�@��@��#@��V@�u�@�U�@�;d@�S@��@���@��\@�YK@��t@��@��?@��@�Ta@���@��V@�b�@�Y@�i�@���@��$@�{J@�#�@��9@���@���@�`B@��@���@�z�@��@�ԕ@���@�G�@�5�@��y@���@�7�@��m@���@�o @�Dg@�ѷ@�}V@�~@��A@��@@�F�@��@��@��I@�tT@�e�@��@�e�@��5@��X@��9@�� @�oi@�)�@��S@�*0@��@���@���@�u%@��@�}�@�V@��@�Z�@�'R@��@�F@���@���@���@�E�@�@���@���@�zx@�$t@��,@��O@��@�=q@��@@�(�@�֡@�Q@��~@�Dg@�+�@��@��@��@���@���@��@���@�;�@���@�rG@�V@�z�@��;@�\)@�4�@���@���@��x@�\�@�4@��@��@�K�@�;@��m@�]d@�@���@�S�@�4@��U@�z@�.�@��#@�x@�.I@��K@�ȴ@���@�V@�~@��@��r@���@��=@�m]@�G�@�/�@���@��@���@�c @�~@��>@���@���@�x�@�\)@�@O@��5@���@�r�@�6@�!@�u@��@��j@��3@���@�Dg@���@��/@���@���@�\�@�Ft@�=q@�3�@�!@�J@��@��@�j�@�IR@�(�@��H@���@��<@��u@�c�@�E�@�5?@�+@�@;d@~�]@~�\@~��@~d�@~4@}�n@}2a@}�@|�@|?�@{�]@{�g@{�q@{�P@{F�@z�y@z�B@z�b@z@�@y��@yԕ@y�S@y7L@x�@x�D@x[�@w�@w�	@w1�@w�@v�X@v�r@u�o@u�X@ue,@u4@t��@t��@t>B@s�
@sX�@s!-@rn�@q@p�v@pS�@o�q@o4�@n��@n��@nC�@m�t@m[W@m�C@m��@mu�@l��@lS�@lb@k��@kC@ja|@i�@i!�@h��@h��@h��@h1@g8@f�<@f�@e8�@du�@c˒@c�w@c/�@b��@a��@`��@`�u@_�*@_S@^�6@^O@]��@]Y�@\�D@[�]@[�a@[�	@[H�@Z͟@Z�1@Z��@Za|@Z4@Y��@Y+�@X�Y@X7�@W�@W��@W�	@WZ�@W�@V��@V=q@U��@U�9@Ue,@T�v@T7�@T �@S��@St�@S8@S�@R�@R��@R͟@RZ�@Q�#@Q��@Q��@QY�@P�	@P��@P�@P��@P��@PFt@O��@O�K@O��@O�	@O9�@N�@M��@M�X@M�@M+@L�I@L�@L��@L�D@L!@Kv`@K>�@J�@J�'@J0U@I�H@IY�@IL�@IO�@IQ�@H֡@HK^@G�&@G�@Gl�@G@O@F��@F�@FOv@E��@E�M@E<6@E;@D��@D�@D`�@DN�@D:�@C�@C��@C�@B҉@B�1@B.�@A�X@Azx@A7L@@�@@Z@@M@?��@?�4@?;d@>�@>e@=e,@<��@<�?@<�o@;�&@;\)@:�m@:��@:R�@:($@9��@9��@9w2@9&�@9;@8��@8r�@8Ft@7��@7J#@6��@6l�@6H�@5�d@5��@4ѷ@4N�@3��@2�@2��@2�r@2u%@2Ta@2�@1��@1+@0�P@0�o@0M@/��@/\)@/S@.�'@.u%@.+k@-�3@-J�@-%@,�@,�K@,�o@+�
@+�q@+Mj@+�@*�s@*�1@*�@)��@)j@(��@(��@(r�@(`�@(D�@("h@'�@'��@'iD@'E9@'"�@&ߤ@&�F@&{�@&C�@%�.@%�d@%�@%��@%s�@%B�@%+@$��@$�)@$�@$K^@#��@#��@#��@#@O@"�c@"�<@"��@";�@!�j@!��@!|@!O�@! \@ ��@ ��@ ��@ y>@ <�@�@�;@�
@��@U�@�@�<@��@z@)�@��@��@|@F@q@�K@�@��@��@h�@K^@4n@	�@�@��@Z�@+@�@@��@�@�h@��@Ov@e@��@�@��@��@|@^�@O�@8�@;@�$@�D@�o@_@<�@(�@�&@�4@X�@9�@o@�<@{@�.@�)@ԕ@�@zx@#�@�@�@�@��@��@�o@bN@M@'R@�@�0@t�@]�@H�@.I@@��@z@-@u@�@��@`B@/@�@��@�@��@�I@j@C-@/�@!@�]@�Q@��@S�@�@�y@�@��@v�@J@�3@�"@5�@@��@�Y@S�@'R@b@�;@��@l�@F�@9�@�@
�@
�@
��@
kQ@
Z�@
:*@
J@	�.@	��@	��@	�@	S&@	Dg@	:�@	&�@�/@�?@�@e�@$@�@��@�	@>�@�c@�L@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A�!�A��A俱A䣣A�A䎿A�}VA�o�A�S�A�#�A�	�A��A��|A��;A��8A��A��>A��ZA��dA��[A��3A��[A��A�BA��zA��A�JA�~�A�t�A�U�A�L0A�(�A���A��^A���A��A�
�A���A�J�Aڣ:A���AƝ~A�R�A���A���A��\A�(XA��A��A�V9A��VA�FA�oA�A�%A�,=A�"�A�p;A�M�A��A�
rA�$A���A��|A�E�A��HA�=�A�TaA�{�A�5?A���A���A��A�gmA�C-A��5A��
A���A�%FA�W
A���A��dA�?}A��qA��pA�3Aw�Aq#�Aj��Ag�6Ae�XAb�LA`e�A^G�A[��AXsAV�<AR�`AQ<�AO��AN�AM  AI�PAE��ADCAAMA?\)A=�|A=Y�A<��A<�oA;n/A9B�A8�A7��A7hsA6�vA6Q�A5�3A5�uA5]dA3��A1-wA/��A.͟A,��A*�FA)�A(�A&�1A%1A$u%A#s�A!4nA��AJ�A�hA�A��A�+A�AA�jA��AGA�MA�AS&A��A�fA�vAYKA�A%A�[A�-A��A��A��A��A�hA�A�9A�+AݘAB�A�]A��A �A&�Al"A�6A*�A�A($A�?A��A!-A�hA^�A�HA;�AR�A�Ao�A;A��AQ�A��A�rAj�A
��A	�A	l"A~(A3�A��A'RA��As�A�AA A��AYAVmA%FA�AkQAW�A�AU2A ��@��)@���@���@��*@�Xy@���@��@�j@�Mj@�_�@�C@�ݘ@���@��h@�O�@�p;@�iD@��@�Xy@���@�X@�?@�4�@�f�@�($@�i�@��@�@�ݘ@�u�@�w2@��@� @��@涮@�@�P@��@��m@�~(@�M�@���@ᧇ@ᝲ@�zx@�Y�@�.I@��@��@��+@��@�@߅@���@��m@�8�@ݚk@�s@�)_@�>B@�O@ڈ�@���@ُ�@ك{@�{J@�@�c�@�s�@�"h@��@�RT@��@�^5@��@�a�@��@�=q@�g�@�҉@�Ta@��@ϓ@�(@Τ�@�S�@�� @�a�@�!�@�Ɇ@�}V@�8�@��@�u@˝�@�f�@��@��@�(@��B@�ȴ@ȿ�@ȉ�@���@�>�@�=q@��@ŝ�@�C@Ļ�@�u%@��@�)_@¹$@�@�zx@�q@�q�@��@���@�g�@���@���@�i�@��@�'�@��h@�(�@�6z@��@��!@��o@�d�@�H�@�b@��T@��[@�F@�q@��[@��@���@�'R@�dZ@��@���@�0U@���@���@��@�8@��v@���@�y>@�ݘ@�.I@��@���@�z@�7�@��@��#@��V@�u�@�U�@�;d@�S@��@���@��\@�YK@��t@��@��?@��@�Ta@���@��V@�b�@�Y@�i�@���@��$@�{J@�#�@��9@���@���@�`B@��@���@�z�@��@�ԕ@���@�G�@�5�@��y@���@�7�@��m@���@�o @�Dg@�ѷ@�}V@�~@��A@��@@�F�@��@��@��I@�tT@�e�@��@�e�@��5@��X@��9@�� @�oi@�)�@��S@�*0@��@���@���@�u%@��@�}�@�V@��@�Z�@�'R@��@�F@���@���@���@�E�@�@���@���@�zx@�$t@��,@��O@��@�=q@��@@�(�@�֡@�Q@��~@�Dg@�+�@��@��@��@���@���@��@���@�;�@���@�rG@�V@�z�@��;@�\)@�4�@���@���@��x@�\�@�4@��@��@�K�@�;@��m@�]d@�@���@�S�@�4@��U@�z@�.�@��#@�x@�.I@��K@�ȴ@���@�V@�~@��@��r@���@��=@�m]@�G�@�/�@���@��@���@�c @�~@��>@���@���@�x�@�\)@�@O@��5@���@�r�@�6@�!@�u@��@��j@��3@���@�Dg@���@��/@���@���@�\�@�Ft@�=q@�3�@�!@�J@��@��@�j�@�IR@�(�@��H@���@��<@��u@�c�@�E�@�5?@�+@�@;d@~�]@~�\@~��@~d�@~4@}�n@}2a@}�@|�@|?�@{�]@{�g@{�q@{�P@{F�@z�y@z�B@z�b@z@�@y��@yԕ@y�S@y7L@x�@x�D@x[�@w�@w�	@w1�@w�@v�X@v�r@u�o@u�X@ue,@u4@t��@t��@t>B@s�
@sX�@s!-@rn�@q@p�v@pS�@o�q@o4�@n��@n��@nC�@m�t@m[W@m�C@m��@mu�@l��@lS�@lb@k��@kC@ja|@i�@i!�@h��@h��@h��@h1@g8@f�<@f�@e8�@du�@c˒@c�w@c/�@b��@a��@`��@`�u@_�*@_S@^�6@^O@]��@]Y�@\�D@[�]@[�a@[�	@[H�@Z͟@Z�1@Z��@Za|@Z4@Y��@Y+�@X�Y@X7�@W�@W��@W�	@WZ�@W�@V��@V=q@U��@U�9@Ue,@T�v@T7�@T �@S��@St�@S8@S�@R�@R��@R͟@RZ�@Q�#@Q��@Q��@QY�@P�	@P��@P�@P��@P��@PFt@O��@O�K@O��@O�	@O9�@N�@M��@M�X@M�@M+@L�I@L�@L��@L�D@L!@Kv`@K>�@J�@J�'@J0U@I�H@IY�@IL�@IO�@IQ�@H֡@HK^@G�&@G�@Gl�@G@O@F��@F�@FOv@E��@E�M@E<6@E;@D��@D�@D`�@DN�@D:�@C�@C��@C�@B҉@B�1@B.�@A�X@Azx@A7L@@�@@Z@@M@?��@?�4@?;d@>�@>e@=e,@<��@<�?@<�o@;�&@;\)@:�m@:��@:R�@:($@9��@9��@9w2@9&�@9;@8��@8r�@8Ft@7��@7J#@6��@6l�@6H�@5�d@5��@4ѷ@4N�@3��@2�@2��@2�r@2u%@2Ta@2�@1��@1+@0�P@0�o@0M@/��@/\)@/S@.�'@.u%@.+k@-�3@-J�@-%@,�@,�K@,�o@+�
@+�q@+Mj@+�@*�s@*�1@*�@)��@)j@(��@(��@(r�@(`�@(D�@("h@'�@'��@'iD@'E9@'"�@&ߤ@&�F@&{�@&C�@%�.@%�d@%�@%��@%s�@%B�@%+@$��@$�)@$�@$K^@#��@#��@#��@#@O@"�c@"�<@"��@";�@!�j@!��@!|@!O�@! \@ ��@ ��@ ��@ y>@ <�@�@�;@�
@��@U�@�@�<@��@z@)�@��@��@|@F@q@�K@�@��@��@h�@K^@4n@	�@�@��@Z�@+@�@@��@�@�h@��@Ov@e@��@�@��@��@|@^�@O�@8�@;@�$@�D@�o@_@<�@(�@�&@�4@X�@9�@o@�<@{@�.@�)@ԕ@�@zx@#�@�@�@�@��@��@�o@bN@M@'R@�@�0@t�@]�@H�@.I@@��@z@-@u@�@��@`B@/@�@��@�@��@�I@j@C-@/�@!@�]@�Q@��@S�@�@�y@�@��@v�@J@�3@�"@5�@@��@�Y@S�@'R@b@�;@��@l�@F�@9�@�@
�@
�@
��@
kQ@
Z�@
:*@
J@	�.@	��@	��@	�@	S&@	Dg@	:�@	&�@�/@�?@�@e�@$@�@��@�	@>�@�c@�L@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B�B�XB�mB��B�LB�zB��B�ZB�B��B��B�,B�B�B�9B��B	 OB		RB	"�B	6FB	;0B	AUB	H�B	[�B	gB	c B	d@B	f2B	c:B	b�B	^jB	V�B	UgB	T{B	UMB	R�B	Q4B	K^B	H�B	=B	A B	>wB	PB	�	B
.�B
��B
�B
��BBX�Bt�Bx�BxRB�B��B��B��B�BB�GBp!Bf�BZ�BO�B^�B�B�,B|Bf�BB�B
��B
�JB
�jB
ňB
�cB
��B
� B
�?B
t�B
jKB
[�B
I�B
:�B
-�B
�B
 B	̈́B	��B	xlB	gB	YeB	I�B	>�B	3B	)DB	)B	�B	�B	�B	�B		�B	MB��B�B�:B�B	AB	
#B	�B	B	#TB	4B	I�B	XyB	X+B	Z�B	c�B	j�B	mwB	k�B	h�B	`�B	SuB	K)B	BB	<�B	>]B	>�B	=�B	:DB	5?B	2�B	+�B	eB	SB�B��B	�B	SB	FB	yB	!�B	�B	'8B	*�B	,�B	-�B	-B	1�B	5�B	4B	7�B	A�B	D�B	D�B	D�B	D�B	FB	F�B	GEB	LJB	Z�B	\�B	[qB	ZkB	[=B	_B	c�B	l�B	yXB	�oB	��B	� B	��B	��B	�KB	�$B	��B	�B	�8B	�kB	�iB	��B	�FB	��B	�(B	�uB	��B	��B	��B	�wB	�6B	�B	� B	�wB	�VB	�qB	�6B	��B	�B	�*B	�fB	�B	�B	�xB	�B	��B	�JB	�jB	�B	��B	�$B	�^B	�VB	��B	�^B	��B	�dB	�PB	�B	�$B	��B	��B	�qB	�B	��B	��B	��B	ÖB	ĜB	�3B	��B	��B	��B	��B	ƎB	�dB	�.B	�oB	� B	�.B	ϫB	ϑB	�bB	ЗB	ѷB	бB	�vB	�B	˒B	��B	�B	�oB	�&B	�FB	ԕB	ԕB	ԯB	��B	��B	��B	�2B	��B	ԯB	��B	ԯB	�gB	��B	ՁB	յB	֡B	�
B	׍B	�_B	ؓB	�_B	��B	�B	��B	ؓB	ؓB	ؓB	�B	�B	�kB	��B	�#B	�#B	�	B	��B	�~B	��B	�]B	�IB	�B	�~B	�~B	�B	�B	�5B	��B	�B	�B	�B	��B	�!B	�!B	�VB	�\B	�bB	�B	�|B	�-B	�B	�4B	��B	�B	��B	�@B	�2B	�B	�2B	�LB	�B	�RB	��B	�mB	�B	�B	��B	�DB	�B	�B	�B	�KB	��B	�B	�B	�B	��B	�wB	�B	��B	��B	�B	�cB	�}B	�}B	��B	�iB	��B	�B	��B	�AB	��B	��B	�|B	�B	�hB	�B	��B	�3B	�hB	�B	��B	��B	��B	��B	��B	��B	��B	�B	�2B	��B	��B	��B	�B	�lB	��B	�lB	�8B	�B	��B	�B	�*B	�^B	��B	��B	�JB	�JB	��B	�jB	�qB	�<B	�VB	�qB	�B	��B	��B	��B	��B	�cB	��B
 B
 OB
 �B
B
B
UB
�B
�B
�B
'B
�B
'B
�B
�B
GB
{B
�B
B
B
�B
�B
�B
�B
YB
�B
�B
_B
�B
zB
_B
�B
	B
	�B
	�B

	B

	B

�B
)B
�B
B
dB
~B
~B
B
B
VB
VB
�B
�B
(B
BB
BB
�B
B
}B
�B
}B
 B
�B
TB
�B
[B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
9B
9B
�B
�B
�B
1B
eB
�B
B
kB
�B
�B
=B
#B
�B
�B
)B
�B
�B
IB
IB
dB
OB
�B
B
�B
 'B
 �B
 �B
!HB
!HB
!�B
"NB
"�B
"�B
#B
#B
"�B
"�B
# B
#B
$B
$ZB
$�B
%`B
%zB
%`B
%`B
%�B
&2B
&2B
&�B
&�B
'�B
'�B
'�B
(>B
(sB
(�B
(�B
(�B
)_B
*0B
*KB
*�B
+B
+kB
+�B
+�B
+�B
+�B
+�B
,"B
,qB
,�B
,qB
,�B
,�B
,�B
,�B
,qB
,qB
,�B
,qB
-CB
-�B
.�B
.�B
/ B
/ B
/OB
/�B
0B
0;B
0B
0!B
0!B
0!B
0B
0;B
0;B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
1�B
2-B
2aB
2�B
2�B
33B
3�B
3�B
4B
4�B
4�B
5B
5B
4�B
5%B
5�B
6zB
6zB
6+B
5�B
6�B
6+B
5�B
6+B
6`B
6�B
7B
7LB
72B
7LB
7�B
8�B
9�B
:B
9�B
:B
9�B
9�B
:DB
:�B
:�B
:xB
:xB
:�B
:�B
:^B
:�B
;B
;B
<B
<B
<�B
<jB
="B
<�B
<jB
;JB
:^B
9rB
8�B
8�B
8lB
8�B
9	B
9rB
9�B
:DB
:�B
:�B
;JB
<B
<jB
=VB
>(B
>(B
=�B
=�B
=VB
=VB
=�B
>BB
>�B
>�B
?cB
@B
@OB
@�B
@�B
AB
A�B
B[B
BAB
BuB
C-B
CaB
C{B
CaB
CaB
CGB
C�B
DB
D�B
D�B
E9B
FB
F�B
F�B
F�B
G�B
GzB
G�B
G�B
H�B
J#B
J�B
J�B
J	B
J#B
J�B
KDB
J�B
LB
L~B
LdB
L�B
MB
M6B
M6B
M�B
N�B
O(B
O(B
OB
OB
OBB
O�B
PB
P}B
P�B
QhB
QhB
Q�B
Q�B
RB
RB
RoB
R�B
R�B
R�B
SB
S@B
S[B
SuB
S�B
S�B
TaB
TB
T{B
T�B
U�B
UgB
U�B
VB
VB
VmB
V�B
V�B
V�B
WsB
W�B
XEB
XEB
X_B
XEB
XyB
XyB
X+B
XEB
Y1B
YKB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
[	B
[qB
[�B
\]B
\�B
\�B
\�B
]IB
]IB
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_VB
`'B
`vB
`vB
`vB
`�B
`�B
`�B
aB
a-B
a-B
aHB
a�B
b4B
bNB
bNB
bhB
b�B
b�B
cB
c:B
c:B
cnB
c�B
dtB
d�B
eB
e,B
e,B
e,B
eFB
eFB
e`B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
gB
g8B
g8B
gRB
gmB
gmB
g�B
g�B
g�B
h
B
h$B
h�B
h�B
h�B
h�B
i*B
i_B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
j�B
kB
kB
kB
kQB
k�B
k�B
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
mCB
m]B
m�B
m�B
nB
nIB
ncB
ncB
n}B
n�B
n�B
n�B
o B
oB
oB
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
p�B
p�B
qB
qB
qB
qAB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
u%B
u%B
u%B
u%B
u%B
uB
u%B
u?B
u�B
u�B
u�B
u�B
utB
uZB
uZB
u�B
vB
v`B
v`B
vzB
vzB
v�B
v�B
w2B
w2B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
xlB
x�B
y$B
y>B
y>B
yrB
y�B
y�B
z*B
zxB
z�B
z�B
{B
{0B
{JB
{JB
{B
{�B
|6B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~BB
~(B
~]B
~]B
~�B
~�B
~�B
B
.B
}B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B�B�XB�mB��B�LB�zB��B�ZB�B��B��B�,B�B�B�9B��B	 OB		RB	"�B	6FB	;0B	AUB	H�B	[�B	gB	c B	d@B	f2B	c:B	b�B	^jB	V�B	UgB	T{B	UMB	R�B	Q4B	K^B	H�B	=B	A B	>wB	PB	�	B
.�B
��B
�B
��BBX�Bt�Bx�BxRB�B��B��B��B�BB�GBp!Bf�BZ�BO�B^�B�B�,B|Bf�BB�B
��B
�JB
�jB
ňB
�cB
��B
� B
�?B
t�B
jKB
[�B
I�B
:�B
-�B
�B
 B	̈́B	��B	xlB	gB	YeB	I�B	>�B	3B	)DB	)B	�B	�B	�B	�B		�B	MB��B�B�:B�B	AB	
#B	�B	B	#TB	4B	I�B	XyB	X+B	Z�B	c�B	j�B	mwB	k�B	h�B	`�B	SuB	K)B	BB	<�B	>]B	>�B	=�B	:DB	5?B	2�B	+�B	eB	SB�B��B	�B	SB	FB	yB	!�B	�B	'8B	*�B	,�B	-�B	-B	1�B	5�B	4B	7�B	A�B	D�B	D�B	D�B	D�B	FB	F�B	GEB	LJB	Z�B	\�B	[qB	ZkB	[=B	_B	c�B	l�B	yXB	�oB	��B	� B	��B	��B	�KB	�$B	��B	�B	�8B	�kB	�iB	��B	�FB	��B	�(B	�uB	��B	��B	��B	�wB	�6B	�B	� B	�wB	�VB	�qB	�6B	��B	�B	�*B	�fB	�B	�B	�xB	�B	��B	�JB	�jB	�B	��B	�$B	�^B	�VB	��B	�^B	��B	�dB	�PB	�B	�$B	��B	��B	�qB	�B	��B	��B	��B	ÖB	ĜB	�3B	��B	��B	��B	��B	ƎB	�dB	�.B	�oB	� B	�.B	ϫB	ϑB	�bB	ЗB	ѷB	бB	�vB	�B	˒B	��B	�B	�oB	�&B	�FB	ԕB	ԕB	ԯB	��B	��B	��B	�2B	��B	ԯB	��B	ԯB	�gB	��B	ՁB	յB	֡B	�
B	׍B	�_B	ؓB	�_B	��B	�B	��B	ؓB	ؓB	ؓB	�B	�B	�kB	��B	�#B	�#B	�	B	��B	�~B	��B	�]B	�IB	�B	�~B	�~B	�B	�B	�5B	��B	�B	�B	�B	��B	�!B	�!B	�VB	�\B	�bB	�B	�|B	�-B	�B	�4B	��B	�B	��B	�@B	�2B	�B	�2B	�LB	�B	�RB	��B	�mB	�B	�B	��B	�DB	�B	�B	�B	�KB	��B	�B	�B	�B	��B	�wB	�B	��B	��B	�B	�cB	�}B	�}B	��B	�iB	��B	�B	��B	�AB	��B	��B	�|B	�B	�hB	�B	��B	�3B	�hB	�B	��B	��B	��B	��B	��B	��B	��B	�B	�2B	��B	��B	��B	�B	�lB	��B	�lB	�8B	�B	��B	�B	�*B	�^B	��B	��B	�JB	�JB	��B	�jB	�qB	�<B	�VB	�qB	�B	��B	��B	��B	��B	�cB	��B
 B
 OB
 �B
B
B
UB
�B
�B
�B
'B
�B
'B
�B
�B
GB
{B
�B
B
B
�B
�B
�B
�B
YB
�B
�B
_B
�B
zB
_B
�B
	B
	�B
	�B

	B

	B

�B
)B
�B
B
dB
~B
~B
B
B
VB
VB
�B
�B
(B
BB
BB
�B
B
}B
�B
}B
 B
�B
TB
�B
[B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
9B
9B
�B
�B
�B
1B
eB
�B
B
kB
�B
�B
=B
#B
�B
�B
)B
�B
�B
IB
IB
dB
OB
�B
B
�B
 'B
 �B
 �B
!HB
!HB
!�B
"NB
"�B
"�B
#B
#B
"�B
"�B
# B
#B
$B
$ZB
$�B
%`B
%zB
%`B
%`B
%�B
&2B
&2B
&�B
&�B
'�B
'�B
'�B
(>B
(sB
(�B
(�B
(�B
)_B
*0B
*KB
*�B
+B
+kB
+�B
+�B
+�B
+�B
+�B
,"B
,qB
,�B
,qB
,�B
,�B
,�B
,�B
,qB
,qB
,�B
,qB
-CB
-�B
.�B
.�B
/ B
/ B
/OB
/�B
0B
0;B
0B
0!B
0!B
0!B
0B
0;B
0;B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
1�B
2-B
2aB
2�B
2�B
33B
3�B
3�B
4B
4�B
4�B
5B
5B
4�B
5%B
5�B
6zB
6zB
6+B
5�B
6�B
6+B
5�B
6+B
6`B
6�B
7B
7LB
72B
7LB
7�B
8�B
9�B
:B
9�B
:B
9�B
9�B
:DB
:�B
:�B
:xB
:xB
:�B
:�B
:^B
:�B
;B
;B
<B
<B
<�B
<jB
="B
<�B
<jB
;JB
:^B
9rB
8�B
8�B
8lB
8�B
9	B
9rB
9�B
:DB
:�B
:�B
;JB
<B
<jB
=VB
>(B
>(B
=�B
=�B
=VB
=VB
=�B
>BB
>�B
>�B
?cB
@B
@OB
@�B
@�B
AB
A�B
B[B
BAB
BuB
C-B
CaB
C{B
CaB
CaB
CGB
C�B
DB
D�B
D�B
E9B
FB
F�B
F�B
F�B
G�B
GzB
G�B
G�B
H�B
J#B
J�B
J�B
J	B
J#B
J�B
KDB
J�B
LB
L~B
LdB
L�B
MB
M6B
M6B
M�B
N�B
O(B
O(B
OB
OB
OBB
O�B
PB
P}B
P�B
QhB
QhB
Q�B
Q�B
RB
RB
RoB
R�B
R�B
R�B
SB
S@B
S[B
SuB
S�B
S�B
TaB
TB
T{B
T�B
U�B
UgB
U�B
VB
VB
VmB
V�B
V�B
V�B
WsB
W�B
XEB
XEB
X_B
XEB
XyB
XyB
X+B
XEB
Y1B
YKB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
[	B
[qB
[�B
\]B
\�B
\�B
\�B
]IB
]IB
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_VB
`'B
`vB
`vB
`vB
`�B
`�B
`�B
aB
a-B
a-B
aHB
a�B
b4B
bNB
bNB
bhB
b�B
b�B
cB
c:B
c:B
cnB
c�B
dtB
d�B
eB
e,B
e,B
e,B
eFB
eFB
e`B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
gB
g8B
g8B
gRB
gmB
gmB
g�B
g�B
g�B
h
B
h$B
h�B
h�B
h�B
h�B
i*B
i_B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
j�B
kB
kB
kB
kQB
k�B
k�B
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
mCB
m]B
m�B
m�B
nB
nIB
ncB
ncB
n}B
n�B
n�B
n�B
o B
oB
oB
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
p�B
p�B
qB
qB
qB
qAB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
u%B
u%B
u%B
u%B
u%B
uB
u%B
u?B
u�B
u�B
u�B
u�B
utB
uZB
uZB
u�B
vB
v`B
v`B
vzB
vzB
v�B
v�B
w2B
w2B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
xlB
x�B
y$B
y>B
y>B
yrB
y�B
y�B
z*B
zxB
z�B
z�B
{B
{0B
{JB
{JB
{B
{�B
|6B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~BB
~(B
~]B
~]B
~�B
~�B
~�B
B
.B
}B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105231  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191447  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191447  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191448                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041455  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041455  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                