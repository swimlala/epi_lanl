CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:29:01Z creation;2022-06-04T19:29:02Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192901  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               gA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @٨TF*1   @٨T�Pg)@+��R�d#I�^1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C33C�fC   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D~��Dy�D��D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�3D�@ Dր D��3D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.{@tz�@�p�@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/�B7�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B��
B���B���B���B���B���B���B���B��
B�
=B���B���B���B�p�B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�B��
B��B��B��B��B��B�p�B���B���C�RC��C��C��C	��C��C��C��C��C��C��C��C��CC�RC��C!��C#��C%��C'��C)��C+��C-��C/�RC1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�CI�CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�DnD�D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��pD�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��pD�:=D�z=DֽpD��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�Z=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�#:A�$A�$@A�%zA�'RA�'A�&�A�%�A�%�A�$A�$@A� �A�A��A��A��A��HA��,A��oA�SA��A��?A҅SA�.AѥA�6�A�ܒAШ�A�bA���A͋�A�'RA�a�A�n/A�f2A��A��A��aA��A���A��A��A��A���A�(A�=A��DA�,qA��A��UA�k�A��ZA�`�A�V�A�@�A�r|A�DgA�GA�%A�O�A���A���A�<6A�_�A�l�A��A��!A��A��{A���A���A��2A���A�2�A��A�LdA�f�A�w�A���A~m�A}�A{L0AxiDAs-�Aq�Akp�Ad<�A[�AV�AR/�AN��AK�AJ1�AGf�AF~(AE�ABa�A@��A>~(A<��A; iA9��A7��A4�/A3�	A34A24nA1cA0��A/l�A-JA+�MA*�^A(�A%�7A#�AA#8�A"�mA!n/A �NA 	A�3Ab�AE�A��A��AW?AxA \A�pA�rA}�Ab�A��A��A��A�A�9A�:A��AR�A?}AA�ARTA��A�A�zA�/A��A�?A�aA��A�fA�A<6A�&A�SAHAXyA�A�$A��A��A�4A�A�"A�}A=�A�AA%FA
�wA	��AĜA6�AC�A3�AoA��AM�A��AW�A�WA�$A�3A:*A��A�4A5?A��A�XA�A��A��Ax�AP�A 1'@��@��U@���@��@���@��@�l"@�>B@��@�=@�~(@���@��T@�/�@��4@�@�_@�{@�M�@��@��y@��@�@�@��@��@靲@궮@�X@�s@陚@��@��@��@�1'@�9X@�S�@��@�c�@��@��v@�^5@�ϫ@��`@�<@�N�@��j@��@���@��@�z@�j@���@�bN@��j@߳�@�u�@�@��@�ی@�u%@�'R@݉7@�:�@��@�z�@ۨX@�>�@�@@�W�@ج�@�i�@�H�@��D@��@��o@ד�@־@ֽ<@�oi@��@��]@���@ӆ�@�[W@��|@�l�@�	@��#@�t�@�Dg@���@а�@�~@Ϗ�@���@�bN@�"h@ͫ�@̵�@��@�E9@�x@�^�@���@��;@��@�S�@�G@œ�@�M�@ò�@��@o@�1@�W?@�ȴ@���@�e�@�#:@��
@���@��@���@��
@��$@�>�@��r@��@�!�@���@�u%@�>B@�u�@��H@���@�-�@��W@� \@���@�1�@��@��t@�F�@��|@��@���@��@���@�@���@�s�@�F�@��x@�@���@��M@���@��.@�w�@�oi@�(�@�ԕ@��@���@�,=@���@�m]@�5�@�ی@��o@�/�@���@��t@�e,@��@��x@�s�@�Ta@�E�@�.�@��@��>@�y�@�W?@��R@�O@���@�O�@�
=@��@��@��|@���@�U2@�!@���@�hs@�
=@���@���@�=q@��~@��@��!@���@�H�@�@��Z@�˒@�s�@�4�@�Y@��@��@��@���@�v�@�3�@�{@�u@��&@��[@�[W@��@��<@���@�A�@�خ@���@�RT@��@���@���@�`�@��@�˒@�k�@�IR@��@�z�@�%�@��H@���@�j�@�<6@��@���@�.�@��k@��@�u%@��@�a�@�H�@�'�@�E9@��@���@�Xy@��3@�Q�@���@��D@�5?@��
@��C@�\)@�F�@�Y@��<@���@�c�@�3�@��D@��a@�p�@�%F@��@��s@���@��\@�Q@�:*@���@���@��@�p�@�J�@���@�|�@�Q�@��]@��'@��~@�\�@�;@�u�@�{@�!�@���@���@��\@��@���@���@�d�@�%�@���@���@��@�a@�@O@��@��j@���@���@���@�xl@�I�@�J@��@���@�g�@�/@� i@�ں@�ȴ@���@���@�R�@��+@�ϫ@���@�H�@�S@��@��/@���@�_@�Ov@�@�@�!@�G@��g@���@���@�}�@�a�@�;@���@�z@� �@�g@y�@~�8@~	@~
�@}��@}#�@|�.@|>B@{��@{�@zp;@zO@y�@x�`@x �@w\)@v�L@v;�@v	@u�S@uV@t�v@tPH@s��@s i@rJ�@q��@p��@o�*@n��@n{@m��@mS&@lbN@k��@kC�@j�c@j��@j~�@jGE@i�)@ij@h֡@h@gRT@f��@e��@e�d@e��@d��@d�?@d��@d�@dy>@c�@cW?@b��@a�@ahs@a%F@`�@_�+@_�@^ں@^�@^;�@]��@]�t@]T�@]&�@\�9@\2�@[�@[S�@[�@Z�@Z�@Zz@Y�@Y��@YA @YA @Y�@X�U@XI�@W��@W�*@We�@W=@V�M@V�A@V;�@V3�@V@U��@U!�@T�$@T��@S�@S�F@Se�@S�@R��@R�m@R�!@R�x@R�+@R	@Q��@Qw2@Qc�@Q�@P��@PA�@P�@O�@O��@O�q@O|�@Os@On/@OW?@O$t@N�@N��@N$�@M�M@MIR@M;@L�)@Lc�@L@K��@Kn/@K@O@K�@J��@J��@JH�@I�M@I7L@I&�@IV@H�@H��@G�W@G��@G&@F�@F҉@F�F@F-@E��@E��@EF@D�5@Dg8@C��@C1�@C�@CS@B�@B�x@BC�@A��@A=�@@�`@@��@@M@?ƨ@?RT@?S@>��@>u@<�5@<��@<V�@;��@;a@;�@:��@:H�@:@9�D@9�@9�#@9ԕ@9�@9@9�=@9�@8Ɇ@8��@8Xy@7�k@7A�@7!-@6�R@5�Z@5��@6u@5��@5u�@4�f@4��@4��@4oi@4K^@3�;@3iD@2�]@2M�@2e@1ԕ@1�7@0�E@0y>@0>B@/�m@/�V@/\)@/�@.�@.Z�@.$�@-��@-x�@- \@,��@+ݘ@+��@+�4@+U�@+P�@+P�@+/�@*�@*H�@*�@)�9@)@)[W@)+�@)-w@)�@(��@(Z@(�@'�;@'ݘ@'�;@'�Q@'��@'�q@'��@'H�@')_@&�@&��@&�}@&c @%��@%��@%hs@%J�@%/@$ѷ@$�I@$%�@#˒@#]�@#&@"�c@"�F@":*@!�3@!�M@!f�@!B�@ �@ w�@ S�@ :�@�[@s@4�@@�@��@�@z@^5@5?@ �@��@��@��@Y�@�@�@�P@�@�`@��@]d@G@�[@�:@j�@H�@)_@��@��@��@�}@�@u%@h
@6�@��@��@�X@��@f�@IR@�@�?@�@c�@"h@�Q@��@.I@�@�H@��@l�@$�@�@��@��@hs@T�@&�@��@��@g8@7�@%�@b@�@�r@��@��@n/@K�@!-@�M@��@��@�A@}V@~�@v�@\�@;�@$�@J@ �@�)@�j@��@�=@��@��@|@x�@`B@Q�@A @7L@%F@;@�5@�@Ɇ@�o@9X@@�F@g�@!-@�b@�A@�A@z@s�@Ta@#:@��@��@��@X@=�@ \@�@�f@ی@��@bN@4n@�@��@�[@v`@_p@F�@�@�@
�@
�R@
��@
��@
kQ@
?@	�)@	�3@	��@	��@	�S@	�@	S&@	 \@	%@ѷ@�@�.@q@K^@!@��@��@U�@E9@!-@Y@͟@��@s�@;�@_@�@�9@�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�#:A�$A�$@A�%zA�'RA�'A�&�A�%�A�%�A�$A�$@A� �A�A��A��A��A��HA��,A��oA�SA��A��?A҅SA�.AѥA�6�A�ܒAШ�A�bA���A͋�A�'RA�a�A�n/A�f2A��A��A��aA��A���A��A��A��A���A�(A�=A��DA�,qA��A��UA�k�A��ZA�`�A�V�A�@�A�r|A�DgA�GA�%A�O�A���A���A�<6A�_�A�l�A��A��!A��A��{A���A���A��2A���A�2�A��A�LdA�f�A�w�A���A~m�A}�A{L0AxiDAs-�Aq�Akp�Ad<�A[�AV�AR/�AN��AK�AJ1�AGf�AF~(AE�ABa�A@��A>~(A<��A; iA9��A7��A4�/A3�	A34A24nA1cA0��A/l�A-JA+�MA*�^A(�A%�7A#�AA#8�A"�mA!n/A �NA 	A�3Ab�AE�A��A��AW?AxA \A�pA�rA}�Ab�A��A��A��A�A�9A�:A��AR�A?}AA�ARTA��A�A�zA�/A��A�?A�aA��A�fA�A<6A�&A�SAHAXyA�A�$A��A��A�4A�A�"A�}A=�A�AA%FA
�wA	��AĜA6�AC�A3�AoA��AM�A��AW�A�WA�$A�3A:*A��A�4A5?A��A�XA�A��A��Ax�AP�A 1'@��@��U@���@��@���@��@�l"@�>B@��@�=@�~(@���@��T@�/�@��4@�@�_@�{@�M�@��@��y@��@�@�@��@��@靲@궮@�X@�s@陚@��@��@��@�1'@�9X@�S�@��@�c�@��@��v@�^5@�ϫ@��`@�<@�N�@��j@��@���@��@�z@�j@���@�bN@��j@߳�@�u�@�@��@�ی@�u%@�'R@݉7@�:�@��@�z�@ۨX@�>�@�@@�W�@ج�@�i�@�H�@��D@��@��o@ד�@־@ֽ<@�oi@��@��]@���@ӆ�@�[W@��|@�l�@�	@��#@�t�@�Dg@���@а�@�~@Ϗ�@���@�bN@�"h@ͫ�@̵�@��@�E9@�x@�^�@���@��;@��@�S�@�G@œ�@�M�@ò�@��@o@�1@�W?@�ȴ@���@�e�@�#:@��
@���@��@���@��
@��$@�>�@��r@��@�!�@���@�u%@�>B@�u�@��H@���@�-�@��W@� \@���@�1�@��@��t@�F�@��|@��@���@��@���@�@���@�s�@�F�@��x@�@���@��M@���@��.@�w�@�oi@�(�@�ԕ@��@���@�,=@���@�m]@�5�@�ی@��o@�/�@���@��t@�e,@��@��x@�s�@�Ta@�E�@�.�@��@��>@�y�@�W?@��R@�O@���@�O�@�
=@��@��@��|@���@�U2@�!@���@�hs@�
=@���@���@�=q@��~@��@��!@���@�H�@�@��Z@�˒@�s�@�4�@�Y@��@��@��@���@�v�@�3�@�{@�u@��&@��[@�[W@��@��<@���@�A�@�خ@���@�RT@��@���@���@�`�@��@�˒@�k�@�IR@��@�z�@�%�@��H@���@�j�@�<6@��@���@�.�@��k@��@�u%@��@�a�@�H�@�'�@�E9@��@���@�Xy@��3@�Q�@���@��D@�5?@��
@��C@�\)@�F�@�Y@��<@���@�c�@�3�@��D@��a@�p�@�%F@��@��s@���@��\@�Q@�:*@���@���@��@�p�@�J�@���@�|�@�Q�@��]@��'@��~@�\�@�;@�u�@�{@�!�@���@���@��\@��@���@���@�d�@�%�@���@���@��@�a@�@O@��@��j@���@���@���@�xl@�I�@�J@��@���@�g�@�/@� i@�ں@�ȴ@���@���@�R�@��+@�ϫ@���@�H�@�S@��@��/@���@�_@�Ov@�@�@�!@�G@��g@���@���@�}�@�a�@�;@���@�z@� �@�g@y�@~�8@~	@~
�@}��@}#�@|�.@|>B@{��@{�@zp;@zO@y�@x�`@x �@w\)@v�L@v;�@v	@u�S@uV@t�v@tPH@s��@s i@rJ�@q��@p��@o�*@n��@n{@m��@mS&@lbN@k��@kC�@j�c@j��@j~�@jGE@i�)@ij@h֡@h@gRT@f��@e��@e�d@e��@d��@d�?@d��@d�@dy>@c�@cW?@b��@a�@ahs@a%F@`�@_�+@_�@^ں@^�@^;�@]��@]�t@]T�@]&�@\�9@\2�@[�@[S�@[�@Z�@Z�@Zz@Y�@Y��@YA @YA @Y�@X�U@XI�@W��@W�*@We�@W=@V�M@V�A@V;�@V3�@V@U��@U!�@T�$@T��@S�@S�F@Se�@S�@R��@R�m@R�!@R�x@R�+@R	@Q��@Qw2@Qc�@Q�@P��@PA�@P�@O�@O��@O�q@O|�@Os@On/@OW?@O$t@N�@N��@N$�@M�M@MIR@M;@L�)@Lc�@L@K��@Kn/@K@O@K�@J��@J��@JH�@I�M@I7L@I&�@IV@H�@H��@G�W@G��@G&@F�@F҉@F�F@F-@E��@E��@EF@D�5@Dg8@C��@C1�@C�@CS@B�@B�x@BC�@A��@A=�@@�`@@��@@M@?ƨ@?RT@?S@>��@>u@<�5@<��@<V�@;��@;a@;�@:��@:H�@:@9�D@9�@9�#@9ԕ@9�@9@9�=@9�@8Ɇ@8��@8Xy@7�k@7A�@7!-@6�R@5�Z@5��@6u@5��@5u�@4�f@4��@4��@4oi@4K^@3�;@3iD@2�]@2M�@2e@1ԕ@1�7@0�E@0y>@0>B@/�m@/�V@/\)@/�@.�@.Z�@.$�@-��@-x�@- \@,��@+ݘ@+��@+�4@+U�@+P�@+P�@+/�@*�@*H�@*�@)�9@)@)[W@)+�@)-w@)�@(��@(Z@(�@'�;@'ݘ@'�;@'�Q@'��@'�q@'��@'H�@')_@&�@&��@&�}@&c @%��@%��@%hs@%J�@%/@$ѷ@$�I@$%�@#˒@#]�@#&@"�c@"�F@":*@!�3@!�M@!f�@!B�@ �@ w�@ S�@ :�@�[@s@4�@@�@��@�@z@^5@5?@ �@��@��@��@Y�@�@�@�P@�@�`@��@]d@G@�[@�:@j�@H�@)_@��@��@��@�}@�@u%@h
@6�@��@��@�X@��@f�@IR@�@�?@�@c�@"h@�Q@��@.I@�@�H@��@l�@$�@�@��@��@hs@T�@&�@��@��@g8@7�@%�@b@�@�r@��@��@n/@K�@!-@�M@��@��@�A@}V@~�@v�@\�@;�@$�@J@ �@�)@�j@��@�=@��@��@|@x�@`B@Q�@A @7L@%F@;@�5@�@Ɇ@�o@9X@@�F@g�@!-@�b@�A@�A@z@s�@Ta@#:@��@��@��@X@=�@ \@�@�f@ی@��@bN@4n@�@��@�[@v`@_p@F�@�@�@
�@
�R@
��@
��@
kQ@
?@	�)@	�3@	��@	��@	�S@	�@	S&@	 \@	%@ѷ@�@�.@q@K^@!@��@��@U�@E9@!-@Y@͟@��@s�@;�@_@�@�9@�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	-B	�B	�B	 B	8B	o�B

�B
M6BH�B��B�#B��B�4B�&B�dB��B�<BnIBh�BSuBPbBO�BK�BdtB��B��B��B�IB��B�aBԕB�NB�;B�vB�B�3B�cB�fBʌB��B�0B̈́B��B�'B��B��Bf�BC-B2aB&�BB
��B
��B
�lB
�zB
�ZB
��B
�EB
��B
~wB
tB
jB
bNB
ZQB
F�B
.B
(
B
�B
SB	��B	��B	�B	�EB	[	B	B�B	1'B	%�B	qB	
B	PB	
rB	B	uB��B�2B�B�B�MB�B�B	^B	
B	dB	�B	!bB	'�B	+kB	#nB	VB	QB	
�B	�B	UB	 �B	�B	�B	{B	�B	3B	�B	�B	9B	I�B	TB	U�B	[�B	k�B	m]B	n�B	r|B	v�B	xB	{�B	|B	{�B	��B	�HB	�B	��B	��B	��B	�B	��B	�wB	��B	��B	��B	�&B	�dB	��B	�IB	�xB	��B	�]B	��B	�-B	��B	��B	��B	�gB	�=B	�B	�QB	�B	�OB	�QB	خB	�
B	��B	ʦB	ȀB	ˬB	��B	�6B	̈́B	҉B	�&B	�@B	�B	�B	ںB	�QB	ևB	�B	��B	�B	�mB	�&B	�vB	��B	��B	бB	��B	�6B	̈́B	�}B	ҽB	՛B	ԕB	֡B	�B	�@B	�B	�PB	��B	��B	�B
3B
�B

�B
	�B
�B
_B
�B
?B
�B
�B	��B	�B	��B	�KB	��B	�B	��B	�B	�B	�"B	��B
'B
�B
�B
3B
B
mB
�B
MB
�B
�B
 B
 �B	�B	�B	�6B	�qB	�"B	�(B
 B
 B
oB
�B
AB
'B
AB
AB
[B
�B
AB
�B
'B
�B
 4B	��B	�B	��B	�}B	��B
 �B
 �B
 4B
 B	��B	�wB	�]B	�"B	�dB	�rB	�$B	�^B	�B	�"B	��B	�6B	�jB	�"B	��B	�B	��B	�PB	�<B	�6B	��B	��B	�(B	��B	��B	��B	��B	��B	�HB	�.B	��B	��B	��B	��B	��B	��B	�]B	�]B	�HB	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
oB
oB
oB
UB
�B
�B
�B
�B
uB
'B
B
B
MB
�B
B
B
�B
�B
�B
9B
�B
�B
B
%B
?B
tB
+B
EB
�B
�B
fB
�B
�B
fB
�B
�B

XB

=B

rB

�B
)B

�B

=B

#B

XB

�B
�B
�B
B
�B
dB
B
0B
�B
�B
^B
�B
dB
vB
(B
�B
HB
�B
�B
�B
�B
�B
�B
�B
NB
hB
�B
4B
�B
oB
B
B
B
�B
B
�B
�B
 B
�B
&B
@B
[B
[B
@B
[B
{B
�B
�B
�B
�B
B
MB
�B
�B
�B
B
mB
�B
�B
�B

B

B
$B
YB
?B
�B
�B
+B
�B
B
�B
�B
KB
eB
�B
B
�B
�B
�B
�B
�B
IB
/B
!B
 �B
"�B
"�B
"�B
"�B
#�B
$ZB
$�B
%B
%�B
%�B
&LB
&2B
&�B
'B
'B
'�B
'�B
'�B
'mB
'�B
(�B
(�B
(�B
)yB
)�B
)�B
)�B
*eB
*�B
*�B
*�B
*�B
+B
*�B
*eB
)�B
(�B
(�B
(XB
(�B
)�B
*�B
*B
*KB
*eB
*�B
+B
+QB
+kB
,qB
,�B
-)B
-]B
-�B
./B
.cB
/B
/ B
/ B
.�B
.�B
/B
/iB
/�B
/�B
/�B
0UB
0�B
1'B
1vB
1�B
2B
2B
2�B
3�B
3�B
4B
4�B
5B
5%B
4�B
5B
4�B
4�B
4�B
5?B
5�B
5�B
6FB
6+B
6+B
6B
72B
7B
7LB
7�B
8B
88B
8�B
9$B
8�B
8�B
9rB
9�B
9�B
:*B
:xB
:�B
;B
;0B
;B
;�B
<PB
<�B
<�B
<�B
=<B
=�B
=�B
>B
>wB
>�B
?HB
?cB
@ B
@�B
A;B
A�B
A�B
A�B
B�B
CGB
C�B
C�B
C�B
D3B
DMB
D�B
D�B
EB
EmB
FB
F�B
G�B
G�B
G�B
G�B
HB
HB
HB
H1B
H�B
H�B
H�B
IRB
I7B
I7B
IRB
I�B
JXB
JrB
JrB
J�B
J�B
KB
K)B
K)B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
MB
MjB
M�B
NB
M�B
N"B
N<B
N�B
N�B
N�B
N�B
N�B
OB
OvB
O�B
O�B
OvB
OvB
O�B
PB
P.B
P�B
P�B
Q B
QNB
QNB
QhB
QhB
QhB
QNB
Q�B
R B
R B
R B
RTB
R�B
SB
S&B
S@B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T,B
TFB
T�B
T�B
T�B
UB
UB
UB
UB
U2B
UgB
U�B
U�B
VB
VB
VB
V�B
W
B
V�B
V�B
V�B
W$B
W�B
W�B
XEB
X_B
X_B
X�B
X�B
Y1B
YB
YeB
YeB
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
\B
\B
\)B
\xB
\�B
]IB
]/B
]dB
^B
^�B
^�B
_B
_�B
_�B
`B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
aHB
aHB
a�B
a�B
abB
aHB
a-B
a-B
a|B
b�B
c:B
c�B
c�B
c�B
cnB
c�B
dB
dB
dB
d�B
dtB
d&B
d@B
dtB
d�B
d�B
d�B
eB
d�B
d�B
eB
ezB
e�B
e�B
gB
f�B
f�B
f�B
f�B
gB
gRB
gB
gmB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
i�B
j0B
j�B
j�B
kB
k6B
kQB
k�B
k�B
lWB
lqB
lqB
l�B
l�B
mB
m)B
m�B
m�B
m�B
m�B
ncB
n}B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
o�B
pB
p!B
p!B
p;B
p;B
pUB
p�B
p�B
qB
qAB
q'B
q'B
q'B
q'B
q[B
q�B
r-B
rGB
raB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
shB
shB
s�B
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
u%B
utB
u�B
vB
vFB
v`B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
xB
x8B
xRB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
zDB
z^B
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{�B
|B
|jB
|�B
}B
}B
}B
}B
}"B
}VB
}qB
}�B
}�B
~(B
~(B
~]B
~BB
~]B
~wB
~�B
~�B
B
.B
�B
�B
� B
�B
�4B
�iB
�OB
��B
��B
��B
��B
� B
�;B
��B
��B
��B
��B
��B
��B
�'B
�uB
�[B
��B
��B
��B
�B
�B
�GB
�aB
�B
�B
�B
�MB
�3B
��B
��B
��B
�9B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	-B	�B	�B	 B	8B	o�B

�B
M6BH�B��B�#B��B�4B�&B�dB��B�<BnIBh�BSuBPbBO�BK�BdtB��B��B��B�IB��B�aBԕB�NB�;B�vB�B�3B�cB�fBʌB��B�0B̈́B��B�'B��B��Bf�BC-B2aB&�BB
��B
��B
�lB
�zB
�ZB
��B
�EB
��B
~wB
tB
jB
bNB
ZQB
F�B
.B
(
B
�B
SB	��B	��B	�B	�EB	[	B	B�B	1'B	%�B	qB	
B	PB	
rB	B	uB��B�2B�B�B�MB�B�B	^B	
B	dB	�B	!bB	'�B	+kB	#nB	VB	QB	
�B	�B	UB	 �B	�B	�B	{B	�B	3B	�B	�B	9B	I�B	TB	U�B	[�B	k�B	m]B	n�B	r|B	v�B	xB	{�B	|B	{�B	��B	�HB	�B	��B	��B	��B	�B	��B	�wB	��B	��B	��B	�&B	�dB	��B	�IB	�xB	��B	�]B	��B	�-B	��B	��B	��B	�gB	�=B	�B	�QB	�B	�OB	�QB	خB	�
B	��B	ʦB	ȀB	ˬB	��B	�6B	̈́B	҉B	�&B	�@B	�B	�B	ںB	�QB	ևB	�B	��B	�B	�mB	�&B	�vB	��B	��B	бB	��B	�6B	̈́B	�}B	ҽB	՛B	ԕB	֡B	�B	�@B	�B	�PB	��B	��B	�B
3B
�B

�B
	�B
�B
_B
�B
?B
�B
�B	��B	�B	��B	�KB	��B	�B	��B	�B	�B	�"B	��B
'B
�B
�B
3B
B
mB
�B
MB
�B
�B
 B
 �B	�B	�B	�6B	�qB	�"B	�(B
 B
 B
oB
�B
AB
'B
AB
AB
[B
�B
AB
�B
'B
�B
 4B	��B	�B	��B	�}B	��B
 �B
 �B
 4B
 B	��B	�wB	�]B	�"B	�dB	�rB	�$B	�^B	�B	�"B	��B	�6B	�jB	�"B	��B	�B	��B	�PB	�<B	�6B	��B	��B	�(B	��B	��B	��B	��B	��B	�HB	�.B	��B	��B	��B	��B	��B	��B	�]B	�]B	�HB	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
oB
oB
oB
UB
�B
�B
�B
�B
uB
'B
B
B
MB
�B
B
B
�B
�B
�B
9B
�B
�B
B
%B
?B
tB
+B
EB
�B
�B
fB
�B
�B
fB
�B
�B

XB

=B

rB

�B
)B

�B

=B

#B

XB

�B
�B
�B
B
�B
dB
B
0B
�B
�B
^B
�B
dB
vB
(B
�B
HB
�B
�B
�B
�B
�B
�B
�B
NB
hB
�B
4B
�B
oB
B
B
B
�B
B
�B
�B
 B
�B
&B
@B
[B
[B
@B
[B
{B
�B
�B
�B
�B
B
MB
�B
�B
�B
B
mB
�B
�B
�B

B

B
$B
YB
?B
�B
�B
+B
�B
B
�B
�B
KB
eB
�B
B
�B
�B
�B
�B
�B
IB
/B
!B
 �B
"�B
"�B
"�B
"�B
#�B
$ZB
$�B
%B
%�B
%�B
&LB
&2B
&�B
'B
'B
'�B
'�B
'�B
'mB
'�B
(�B
(�B
(�B
)yB
)�B
)�B
)�B
*eB
*�B
*�B
*�B
*�B
+B
*�B
*eB
)�B
(�B
(�B
(XB
(�B
)�B
*�B
*B
*KB
*eB
*�B
+B
+QB
+kB
,qB
,�B
-)B
-]B
-�B
./B
.cB
/B
/ B
/ B
.�B
.�B
/B
/iB
/�B
/�B
/�B
0UB
0�B
1'B
1vB
1�B
2B
2B
2�B
3�B
3�B
4B
4�B
5B
5%B
4�B
5B
4�B
4�B
4�B
5?B
5�B
5�B
6FB
6+B
6+B
6B
72B
7B
7LB
7�B
8B
88B
8�B
9$B
8�B
8�B
9rB
9�B
9�B
:*B
:xB
:�B
;B
;0B
;B
;�B
<PB
<�B
<�B
<�B
=<B
=�B
=�B
>B
>wB
>�B
?HB
?cB
@ B
@�B
A;B
A�B
A�B
A�B
B�B
CGB
C�B
C�B
C�B
D3B
DMB
D�B
D�B
EB
EmB
FB
F�B
G�B
G�B
G�B
G�B
HB
HB
HB
H1B
H�B
H�B
H�B
IRB
I7B
I7B
IRB
I�B
JXB
JrB
JrB
J�B
J�B
KB
K)B
K)B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
MB
MjB
M�B
NB
M�B
N"B
N<B
N�B
N�B
N�B
N�B
N�B
OB
OvB
O�B
O�B
OvB
OvB
O�B
PB
P.B
P�B
P�B
Q B
QNB
QNB
QhB
QhB
QhB
QNB
Q�B
R B
R B
R B
RTB
R�B
SB
S&B
S@B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T,B
TFB
T�B
T�B
T�B
UB
UB
UB
UB
U2B
UgB
U�B
U�B
VB
VB
VB
V�B
W
B
V�B
V�B
V�B
W$B
W�B
W�B
XEB
X_B
X_B
X�B
X�B
Y1B
YB
YeB
YeB
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
\B
\B
\)B
\xB
\�B
]IB
]/B
]dB
^B
^�B
^�B
_B
_�B
_�B
`B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
aHB
aHB
a�B
a�B
abB
aHB
a-B
a-B
a|B
b�B
c:B
c�B
c�B
c�B
cnB
c�B
dB
dB
dB
d�B
dtB
d&B
d@B
dtB
d�B
d�B
d�B
eB
d�B
d�B
eB
ezB
e�B
e�B
gB
f�B
f�B
f�B
f�B
gB
gRB
gB
gmB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
i�B
j0B
j�B
j�B
kB
k6B
kQB
k�B
k�B
lWB
lqB
lqB
l�B
l�B
mB
m)B
m�B
m�B
m�B
m�B
ncB
n}B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
o�B
pB
p!B
p!B
p;B
p;B
pUB
p�B
p�B
qB
qAB
q'B
q'B
q'B
q'B
q[B
q�B
r-B
rGB
raB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
shB
shB
s�B
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
u%B
utB
u�B
vB
vFB
v`B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
xB
x8B
xRB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
zDB
z^B
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{�B
|B
|jB
|�B
}B
}B
}B
}B
}"B
}VB
}qB
}�B
}�B
~(B
~(B
~]B
~BB
~]B
~wB
~�B
~�B
B
.B
�B
�B
� B
�B
�4B
�iB
�OB
��B
��B
��B
��B
� B
�;B
��B
��B
��B
��B
��B
��B
�'B
�uB
�[B
��B
��B
��B
�B
�B
�GB
�aB
�B
�B
�B
�MB
�3B
��B
��B
��B
�9B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105249  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192901  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192902  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192902                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042909  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042909  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                