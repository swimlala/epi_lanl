CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:23:45Z creation;2022-06-04T17:23:45Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172345  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ص��˪1   @ص�f���@,�fffff�d:=p��
1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B�ffB���B�  B�33B�ffB�  B�  B�33B�ffB�  B�  B�33B�33B�ffB�  B�  B�  B�  B�  B�  B�33B�33B�33C   C  C  C  C  C
  C  C�C��C  C�fC  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<�C=�fC?�fCA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@n{@�
>@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\B�HBG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B��
B�
=B��
B�
=B�=qB���B��
B�
=B���B���B��
B�
=Bǣ�Bˣ�B��
B��
B�
=Bۣ�Bߣ�B��B��B��B��B��
B��
B��
B���C��C��C��C��C	��C��C�C��C��C�RC��C��C��C�RC��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7�C9�C;�C=�RC?�RCA�RCC�RCE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�D]t{D]�{D^t{D^�{D_nD_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��pD�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��
D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AϬ�AϮ}AϮ�AϮ�Aϱ�AϵAϵAϴAϳ�Aϵ?AϴAϳhAϲ�AϲaAϳhAϳhAϱ�Aϰ�AϭwAϬAϫ�AϪeAϨ�Aϯ�Aϲ-AϮ�Aϯ�Aϴ�AϢ�Aϒ�A�y�A�e�A�]/A�Z�A�YA�VA�UgA�U2A�RTA�I�A�ޞA�E�AˋA��/A���A�v+A�y�A�N�AǓ�A�\�A��A�OA�%A��;A��|A���A� �A��A��=A��*A��A�m�A��A�e,A��0A��DA���A��WA��QA��'A�h
A���A�@�A�u�A�A��A�!�A�;�A���A�(XA�|A�>�A�D�A�XEA���A�:^A|��Av��At2aArxAp2aAk`BAh.�Af5?AbVmAZY�AV˒AR�uAO(AJ�jAH��AC��AAT�A?�A= �A:n/A9�xA9<�A8�QA84A7'RA6�A65�A5-�A4[�A4-�A3��A2�A2�A0�oA/�A.kQA-J#A-�A,X�A+��A+&�A*;dA)7LA)+�A(�}A'�]A'�HA'�=A'}VA'TaA&^�A%�]A%��A%��A$�A$�A#�)A#��A"�TA"CA!l"A ��A~�A;A�bA�[A��A�xA7�A�A��A�[AL�A�AS�A��AoiA�"A�AIRA�>A~�A0�A�dA]dA��A�1APHA��A��A!A��A�oAg8A[�ADgA&�Av�A5�AߤAu�A9XA�A&�AA��A�rAV�A@A��A=�A�TA��A�AW?A��AzAA
��A
��A
]dA	�"A	GEA�FA
�AYKA��A�A��AJ�A	A�gA{�AOA�NA�PAC�A  A� AGEA0�A�AR�A
=A �A �fA F�@���@��@�H@��g@�O@���@�j�@���@�^�@��@��@��^@�&@���@���@��@�W�@�
�@�@� \@��@�@�A�@��3@�1@���@��f@�;�@���@�Y�@�z@�f�@��@��d@�{�@��@�e,@�h@���@���@�Ov@�e,@��@�)�@�*@�v`@�+�@���@�]d@��@ߊ	@���@ݴ�@ݪ�@݁@�4�@�	l@���@�ߤ@ܾ@�a|@�Q�@ګ6@�1�@��@��]@��@ټ@ق�@�8@�^5@�� @ל�@֞�@�V@�Ov@�6�@��@՜@�o @�Z�@��@Ԛ�@���@�^�@��	@��@ѥ�@ѐ�@х@�\)@���@�|�@Ͻ�@��]@��@�3�@��z@�E9@̓u@ˉ7@���@ʄ�@�4n@�u@���@ɜ@�b�@���@�,=@�x�@��@Ɛ.@�J�@��}@�K�@�)_@��/@�C�@Ü@�]�@�*0@��@��/@o@�.�@���@���@�|@�j�@�B�@���@�GE@��@���@��:@��@���@���@�/�@��+@��@���@��z@�($@�@���@�ϫ@��n@�\)@��Y@�($@���@�o�@��p@��A@�,=@��@���@�iD@��K@���@�Q�@�/�@��T@�dZ@��@��@�^5@���@���@�k�@�Mj@�<6@��@��}@���@��_@�V�@��@���@��h@�N<@��v@��e@�R�@�+k@���@�h
@��C@�O�@�6z@�(@��@���@�Ta@�@��S@�o@���@��b@���@��@�RT@�A @��@���@��@���@�Ta@�	�@���@�ƨ@���@��:@�O@���@�i�@�M@�J@���@�l"@��H@��V@���@�g�@�_p@�W?@�6z@��@���@�{�@�GE@���@���@�\)@�@��)@�xl@�&�@��@�`B@��@�ȴ@�|�@�&�@�خ@��	@��@��Y@�I�@��@��>@�}�@�6z@�͟@���@���@�u%@�;�@�{@��g@���@�K�@��y@���@�Q@�'R@���@��@�ϫ@���@�J�@��@���@���@�;�@�ϫ@��n@�|@�@��2@�h�@�  @�ϫ@��C@�n/@�X@�Mj@�E9@�5�@�%F@���@��@��Q@��:@�O�@�@���@�%�@��q@��@���@�C-@�@���@���@���@���@���@�!�@���@��*@�c@�J#@�0�@��@�B[@�_@��#@�s@�%F@�
=@��M@���@��X@���@�Z@��^@���@�%F@��@�Ɇ@�O@�J@�	�@���@��@���@�\�@���@��e@���@�C�@���@���@���@�c@�j�@�;d@��E@���@��@��@���@�k�@�1�@�ی@�Ɇ@���@���@�C-@��@~��@};@|N�@|,=@{Z�@z�H@zL0@yk�@y*0@yV@x��@x�@w��@w�@w@v��@u��@uA @tH@s8@s�@s@r��@r��@r��@r��@r�b@r��@r^5@rOv@q��@p�@py>@oݘ@o��@oK�@o�@n��@n��@m�t@m%@l�@lK^@k�[@kJ#@j��@j�L@j@�@i��@i��@i*0@h�4@h@g�*@gC@f�+@f$�@f!�@f�@e�j@e��@e:�@d�[@c�@cH�@b��@b��@b?@a�n@`ی@`��@`Q�@_�@_�*@_��@_a@_9�@^͟@^	@]�'@]8�@] \@\�|@\|�@[�[@Z_�@ZO@Y|@Y�@X�j@X~@Wv`@V� @V�@U��@U\�@T�	@T�@TXy@S��@S�	@S
=@R��@RJ�@Q��@Q��@Q��@Q��@Q�^@Q�n@PPH@P�@O��@O��@O{J@OW?@N��@N�@Nn�@NR�@NC�@N3�@M�@Lѷ@Lb@K��@K�@K�@Kn/@K1�@J�@J�@J5?@J0U@J �@Ic@H�u@HD�@G˒@G�{@GC�@GY@G�@F�@F��@FM�@E��@E��@Eo @EG�@E7L@EV@D��@C�@C]�@CA�@C&@C�@B��@BC�@B�@A�@A�h@AL�@@��@?��@?|�@>�y@>�]@>��@>q�@>Ta@>5?@=�o@=��@=�@<��@<�@;�P@;�@9�@95�@9�@8�|@8e�@7��@7�:@6��@6�@6;�@5��@5��@5!�@4ѷ@4�@4�@4��@4z�@4]d@4>B@42�@4�@41@3�&@3�[@3W?@2��@2��@2B[@1�#@1c@17L@0�@0�@0G@/��@/=@/�@.�B@.��@.v�@.�@-�"@-/@,�@,�E@,�@,�O@,y>@,e�@+��@+�6@+�@*ȴ@*��@*��@*�@+S@*҉@*�@*q�@*!�@)�j@)��@)8�@)&�@)%F@(�@(y>@'�@'iD@'!-@'�@'C@'�@&�@&�,@&�@&c @&O@&�@&�@%�@%�S@%f�@%%@$�@$u�@$M@#��@#{J@#qv@#�@"ȴ@"�\@":*@"	@!�)@!��@!��@! \@ �v@ ��@ 1@�@�&@خ@�a@�k@n/@1�@�8@�@�y@�@�@�L@��@&�@�@IR@%@�@��@g8@M@˒@e�@1�@�@�M@v�@J�@L0@�@��@�C@m]@+�@�@w�@K^@ �@�@��@�	@8@Y@�@�B@��@�R@�A@L0@�@@�z@c�@?}@-w@�@�@�W@�*@s@;d@$t@�H@q�@E�@�j@��@��@c@O�@�@�@�@r�@g8@`�@U2@x@�g@]�@�@��@l�@Z�@@�@�@�@�z@��@Dg@	l@��@�@w�@c�@c�@j@c�@:�@*�@'R@7@b@x@��@K�@�@
�@
��@
�@
l�@
Ov@
1�@
3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AϬ�AϮ}AϮ�AϮ�Aϱ�AϵAϵAϴAϳ�Aϵ?AϴAϳhAϲ�AϲaAϳhAϳhAϱ�Aϰ�AϭwAϬAϫ�AϪeAϨ�Aϯ�Aϲ-AϮ�Aϯ�Aϴ�AϢ�Aϒ�A�y�A�e�A�]/A�Z�A�YA�VA�UgA�U2A�RTA�I�A�ޞA�E�AˋA��/A���A�v+A�y�A�N�AǓ�A�\�A��A�OA�%A��;A��|A���A� �A��A��=A��*A��A�m�A��A�e,A��0A��DA���A��WA��QA��'A�h
A���A�@�A�u�A�A��A�!�A�;�A���A�(XA�|A�>�A�D�A�XEA���A�:^A|��Av��At2aArxAp2aAk`BAh.�Af5?AbVmAZY�AV˒AR�uAO(AJ�jAH��AC��AAT�A?�A= �A:n/A9�xA9<�A8�QA84A7'RA6�A65�A5-�A4[�A4-�A3��A2�A2�A0�oA/�A.kQA-J#A-�A,X�A+��A+&�A*;dA)7LA)+�A(�}A'�]A'�HA'�=A'}VA'TaA&^�A%�]A%��A%��A$�A$�A#�)A#��A"�TA"CA!l"A ��A~�A;A�bA�[A��A�xA7�A�A��A�[AL�A�AS�A��AoiA�"A�AIRA�>A~�A0�A�dA]dA��A�1APHA��A��A!A��A�oAg8A[�ADgA&�Av�A5�AߤAu�A9XA�A&�AA��A�rAV�A@A��A=�A�TA��A�AW?A��AzAA
��A
��A
]dA	�"A	GEA�FA
�AYKA��A�A��AJ�A	A�gA{�AOA�NA�PAC�A  A� AGEA0�A�AR�A
=A �A �fA F�@���@��@�H@��g@�O@���@�j�@���@�^�@��@��@��^@�&@���@���@��@�W�@�
�@�@� \@��@�@�A�@��3@�1@���@��f@�;�@���@�Y�@�z@�f�@��@��d@�{�@��@�e,@�h@���@���@�Ov@�e,@��@�)�@�*@�v`@�+�@���@�]d@��@ߊ	@���@ݴ�@ݪ�@݁@�4�@�	l@���@�ߤ@ܾ@�a|@�Q�@ګ6@�1�@��@��]@��@ټ@ق�@�8@�^5@�� @ל�@֞�@�V@�Ov@�6�@��@՜@�o @�Z�@��@Ԛ�@���@�^�@��	@��@ѥ�@ѐ�@х@�\)@���@�|�@Ͻ�@��]@��@�3�@��z@�E9@̓u@ˉ7@���@ʄ�@�4n@�u@���@ɜ@�b�@���@�,=@�x�@��@Ɛ.@�J�@��}@�K�@�)_@��/@�C�@Ü@�]�@�*0@��@��/@o@�.�@���@���@�|@�j�@�B�@���@�GE@��@���@��:@��@���@���@�/�@��+@��@���@��z@�($@�@���@�ϫ@��n@�\)@��Y@�($@���@�o�@��p@��A@�,=@��@���@�iD@��K@���@�Q�@�/�@��T@�dZ@��@��@�^5@���@���@�k�@�Mj@�<6@��@��}@���@��_@�V�@��@���@��h@�N<@��v@��e@�R�@�+k@���@�h
@��C@�O�@�6z@�(@��@���@�Ta@�@��S@�o@���@��b@���@��@�RT@�A @��@���@��@���@�Ta@�	�@���@�ƨ@���@��:@�O@���@�i�@�M@�J@���@�l"@��H@��V@���@�g�@�_p@�W?@�6z@��@���@�{�@�GE@���@���@�\)@�@��)@�xl@�&�@��@�`B@��@�ȴ@�|�@�&�@�خ@��	@��@��Y@�I�@��@��>@�}�@�6z@�͟@���@���@�u%@�;�@�{@��g@���@�K�@��y@���@�Q@�'R@���@��@�ϫ@���@�J�@��@���@���@�;�@�ϫ@��n@�|@�@��2@�h�@�  @�ϫ@��C@�n/@�X@�Mj@�E9@�5�@�%F@���@��@��Q@��:@�O�@�@���@�%�@��q@��@���@�C-@�@���@���@���@���@���@�!�@���@��*@�c@�J#@�0�@��@�B[@�_@��#@�s@�%F@�
=@��M@���@��X@���@�Z@��^@���@�%F@��@�Ɇ@�O@�J@�	�@���@��@���@�\�@���@��e@���@�C�@���@���@���@�c@�j�@�;d@��E@���@��@��@���@�k�@�1�@�ی@�Ɇ@���@���@�C-@��@~��@};@|N�@|,=@{Z�@z�H@zL0@yk�@y*0@yV@x��@x�@w��@w�@w@v��@u��@uA @tH@s8@s�@s@r��@r��@r��@r��@r�b@r��@r^5@rOv@q��@p�@py>@oݘ@o��@oK�@o�@n��@n��@m�t@m%@l�@lK^@k�[@kJ#@j��@j�L@j@�@i��@i��@i*0@h�4@h@g�*@gC@f�+@f$�@f!�@f�@e�j@e��@e:�@d�[@c�@cH�@b��@b��@b?@a�n@`ی@`��@`Q�@_�@_�*@_��@_a@_9�@^͟@^	@]�'@]8�@] \@\�|@\|�@[�[@Z_�@ZO@Y|@Y�@X�j@X~@Wv`@V� @V�@U��@U\�@T�	@T�@TXy@S��@S�	@S
=@R��@RJ�@Q��@Q��@Q��@Q��@Q�^@Q�n@PPH@P�@O��@O��@O{J@OW?@N��@N�@Nn�@NR�@NC�@N3�@M�@Lѷ@Lb@K��@K�@K�@Kn/@K1�@J�@J�@J5?@J0U@J �@Ic@H�u@HD�@G˒@G�{@GC�@GY@G�@F�@F��@FM�@E��@E��@Eo @EG�@E7L@EV@D��@C�@C]�@CA�@C&@C�@B��@BC�@B�@A�@A�h@AL�@@��@?��@?|�@>�y@>�]@>��@>q�@>Ta@>5?@=�o@=��@=�@<��@<�@;�P@;�@9�@95�@9�@8�|@8e�@7��@7�:@6��@6�@6;�@5��@5��@5!�@4ѷ@4�@4�@4��@4z�@4]d@4>B@42�@4�@41@3�&@3�[@3W?@2��@2��@2B[@1�#@1c@17L@0�@0�@0G@/��@/=@/�@.�B@.��@.v�@.�@-�"@-/@,�@,�E@,�@,�O@,y>@,e�@+��@+�6@+�@*ȴ@*��@*��@*�@+S@*҉@*�@*q�@*!�@)�j@)��@)8�@)&�@)%F@(�@(y>@'�@'iD@'!-@'�@'C@'�@&�@&�,@&�@&c @&O@&�@&�@%�@%�S@%f�@%%@$�@$u�@$M@#��@#{J@#qv@#�@"ȴ@"�\@":*@"	@!�)@!��@!��@! \@ �v@ ��@ 1@�@�&@خ@�a@�k@n/@1�@�8@�@�y@�@�@�L@��@&�@�@IR@%@�@��@g8@M@˒@e�@1�@�@�M@v�@J�@L0@�@��@�C@m]@+�@�@w�@K^@ �@�@��@�	@8@Y@�@�B@��@�R@�A@L0@�@@�z@c�@?}@-w@�@�@�W@�*@s@;d@$t@�H@q�@E�@�j@��@��@c@O�@�@�@�@r�@g8@`�@U2@x@�g@]�@�@��@l�@Z�@@�@�@�@�z@��@Dg@	l@��@�@w�@c�@c�@j@c�@:�@*�@'R@7@b@x@��@K�@�@
�@
��@
�@
l�@
Ov@
1�@
3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B./B.IB.IB.}B.B./B./B.cB./B.IB.}B.�B.�B.}B.cB.}B.}B-�B-�B-�B-�B-�B-wB-�B-�B-�B-]B-wB-wB,�B,�B,"B-B-CB-wB-�B-�B-�B-�B-�B3Bw�B��B	!B	FYB	PbB	P.B	S�B	e�B	�CB
�B
WsB
hXB
��B��B�iB�OB�B��B��B��B��B�B��B��B��B��B��B�B�_B�&B�\BQhB<�B�B
��B
��B
��B
wB
iDB
_;B
O�B
A�B
5ZB
!bB	��B	ҽB	��B	�B	��B	��B	y�B	h�B	^OB	OvB	N"B	H�B	D�B	A�B	3�B	'RB	 vB	B	�B	"�B	$@B	%�B	'�B	+�B	3B	:�B	@4B	C{B	_B	uB	|6B	��B	�rB	�B	��B	�mB	��B	��B	�B	��B	�B	�XB	��B	�"B	�PB	уB	�qB	ޞB	��B	�bB	�B	�B	��B	��B
SB
�B
�B
/B
5B
!bB
&2B
(XB
%,B
&2B
+kB
4TB
5�B
1�B
2B
2aB
3B
4�B
8B
:�B
;JB
<�B
<jB
>�B
?�B
?�B
A B
AoB
A�B
B'B
BAB
@�B
?�B
A�B
BAB
C�B
C�B
DMB
B�B
A�B
AoB
A B
@OB
@4B
AUB
?�B
?.B
>]B
=VB
;dB
9�B
;dB
=VB
>wB
@ B
B�B
BAB
@�B
@�B
@ B
?�B
?}B
?.B
>�B
>�B
>�B
>�B
>�B
>(B
=VB
<6B
;B
9�B
8B
5%B
3�B
2-B
2�B
2aB
2|B
1[B
1AB
1'B
1AB
0�B
0oB
0UB
0�B
0�B
/iB
/OB
.IB
-�B
,�B
+�B
+B
+6B
)�B
)*B
(sB
(�B
&�B
%�B
%�B
$&B
#�B
"�B
"4B
!B
 BB
 B
�B
�B
�B
�B
jB
5B
dB
�B
	B
kB
�B
B
�B
EB
$B
B
�B
aB
uB
�B
hB
�B
�B
�B
�B
DB
^B
B
<B
"B
�B
jB
B

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

rB

rB
	�B
	B
�B
�B
�B
fB
1B
EB
�B
�B
�B
SB
SB
B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
SB
9B
B
9B
�B
�B
�B
�B
B
SB
�B
B
�B
aB
�B
�B
{B
�B
B
�B
B
�B
�B
mB
�B
�B
?B
%B
B
%B
B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
	lB
	B
	B
	B
	�B
	�B
	�B
	�B
	�B

	B
	�B

	B

�B

�B

�B

	B

=B

#B
	lB
�B
�B
�B
�B
	�B

=B
	�B

=B

�B

�B
�B
dB
JB
~B
~B
VB
�B
�B
�B
PB
B
B
6B
jB
PB
B
�B
B
6B
�B
JB
0B
�B
�B
�B
�B
B
jB
�B
^B
B

�B
)B
�B
�B
�B
�B
JB
�B
6B
�B
"B
�B
�B
�B
.B
HB
�B
 B
NB
hB
�B
�B
NB
NB
NB
:B
B
�B
�B
�B
�B
uB
�B
FB
�B
�B
�B
�B
�B
MB
gB
�B
B
SB
�B
�B
�B
YB
sB
�B
�B
eB
�B
1B
�B
�B
�B
1B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
WB
�B
�B
kB
�B
�B
kB
�B
�B
�B
�B
#B
�B
�B
�B
B
CB
�B
�B
�B
OB
pB
�B
 BB
 �B
!B
 �B
 �B
 �B
 �B
 �B
"�B
$B
$&B
$�B
%FB
%zB
%�B
&fB
&2B
'RB
'8B
'mB
'�B
($B
'�B
(>B
)B
)_B
)�B
*B
*eB
*�B
+B
*�B
+QB
+�B
,B
,=B
,�B
,�B
-B
-)B
-CB
-B
-B
-)B
-wB
-B
.B
.�B
.}B
/�B
/�B
/�B
0;B
0!B
0�B
0�B
1[B
1vB
1[B
2-B
2aB
2GB
2�B
2�B
2�B
2�B
2�B
3B
33B
33B
3hB
3�B
3�B
4nB
4nB
4nB
4�B
4�B
33B
33B
2-B
1�B
3�B
2�B
1�B
1AB
0�B
0�B
0�B
0�B
1AB
1B
1AB
1AB
1�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
5B
5%B
5%B
5%B
5B
5�B
6`B
6`B
7B
7B
7�B
7�B
7�B
7�B
88B
8B
88B
8lB
9�B
9�B
:^B
:^B
:�B
;0B
:�B
;0B
;B
;�B
;�B
<B
<�B
=qB
=VB
=<B
="B
=VB
=B
="B
>B
>]B
>�B
>wB
>�B
?B
?�B
?�B
@ B
@�B
@�B
@�B
AB
A B
A�B
C{B
DMB
EmB
FYB
F�B
GB
G_B
IB
H�B
IRB
IlB
I�B
I�B
I�B
J�B
JXB
J�B
K^B
K�B
KxB
K)B
J�B
J�B
J	B
I�B
I7B
IB
IB
IB
IB
IRB
J�B
J#B
I�B
I�B
J	B
JXB
JXB
J�B
KDB
K^B
K�B
KxB
K^B
K�B
L�B
L�B
M6B
MB
MB
M6B
MB
M�B
N�B
O�B
O�B
P.B
O�B
OB
O�B
PbB
P}B
P}B
P�B
P�B
P�B
P�B
Q B
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
RB
RB
RB
RoB
R�B
R�B
R�B
R�B
R�B
SuB
S�B
S�B
T,B
TB
T,B
TFB
TaB
TaB
T�B
T�B
UB
UMB
U�B
VB
VmB
WYB
W�B
W�B
W�B
X_B
X�B
YeB
Z�B
[#B
[	B
[�B
\]B
\�B
\�B
]/B
]B
]/B
]B
]dB
^B
^OB
_B
_;B
_�B
`\B
`�B
`�B
`�B
a�B
a�B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
c�B
d&B
c�B
c�B
c�B
c�B
c�B
cnB
c�B
c�B
dZB
e,B
eB
e,B
d�B
d@B
d�B
e�B
f2B
fLB
fLB
f�B
gRB
gRB
g8B
gB
gB
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
h
B
h$B
hXB
hsB
h�B
h�B
iDB
i�B
i�B
i�B
i�B
jB
j0B
jB
j0B
jB
jeB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kQB
kkB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
nIB
nIB
n}B
o B
o�B
pUB
p;B
p�B
qvB
q�B
q�B
q�B
q�B
r-B
r|B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
uB
t�B
t�B
tnB
t�B
t�B
t�B
t�B
u%B
u?B
u�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
x8B
x�B
y�B
y�B
zB
z�B
z^B
z�B
{0B
{JB
{B
z�B
{JB
{�B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|6B
|PB
|�B
|6B
|B
{�B
|6B
|6B
|PB
|PB
|�B
|�B
}VB
~B
~�B
~�B
B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�;B
�UB
��B
�UB
��B
��B
��B
�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B./B.IB.IB.}B.B./B./B.cB./B.IB.}B.�B.�B.}B.cB.}B.}B-�B-�B-�B-�B-�B-wB-�B-�B-�B-]B-wB-wB,�B,�B,"B-B-CB-wB-�B-�B-�B-�B-�B3Bw�B��B	!B	FYB	PbB	P.B	S�B	e�B	�CB
�B
WsB
hXB
��B��B�iB�OB�B��B��B��B��B�B��B��B��B��B��B�B�_B�&B�\BQhB<�B�B
��B
��B
��B
wB
iDB
_;B
O�B
A�B
5ZB
!bB	��B	ҽB	��B	�B	��B	��B	y�B	h�B	^OB	OvB	N"B	H�B	D�B	A�B	3�B	'RB	 vB	B	�B	"�B	$@B	%�B	'�B	+�B	3B	:�B	@4B	C{B	_B	uB	|6B	��B	�rB	�B	��B	�mB	��B	��B	�B	��B	�B	�XB	��B	�"B	�PB	уB	�qB	ޞB	��B	�bB	�B	�B	��B	��B
SB
�B
�B
/B
5B
!bB
&2B
(XB
%,B
&2B
+kB
4TB
5�B
1�B
2B
2aB
3B
4�B
8B
:�B
;JB
<�B
<jB
>�B
?�B
?�B
A B
AoB
A�B
B'B
BAB
@�B
?�B
A�B
BAB
C�B
C�B
DMB
B�B
A�B
AoB
A B
@OB
@4B
AUB
?�B
?.B
>]B
=VB
;dB
9�B
;dB
=VB
>wB
@ B
B�B
BAB
@�B
@�B
@ B
?�B
?}B
?.B
>�B
>�B
>�B
>�B
>�B
>(B
=VB
<6B
;B
9�B
8B
5%B
3�B
2-B
2�B
2aB
2|B
1[B
1AB
1'B
1AB
0�B
0oB
0UB
0�B
0�B
/iB
/OB
.IB
-�B
,�B
+�B
+B
+6B
)�B
)*B
(sB
(�B
&�B
%�B
%�B
$&B
#�B
"�B
"4B
!B
 BB
 B
�B
�B
�B
�B
jB
5B
dB
�B
	B
kB
�B
B
�B
EB
$B
B
�B
aB
uB
�B
hB
�B
�B
�B
�B
DB
^B
B
<B
"B
�B
jB
B

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

rB

rB
	�B
	B
�B
�B
�B
fB
1B
EB
�B
�B
�B
SB
SB
B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
SB
9B
B
9B
�B
�B
�B
�B
B
SB
�B
B
�B
aB
�B
�B
{B
�B
B
�B
B
�B
�B
mB
�B
�B
?B
%B
B
%B
B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
	lB
	B
	B
	B
	�B
	�B
	�B
	�B
	�B

	B
	�B

	B

�B

�B

�B

	B

=B

#B
	lB
�B
�B
�B
�B
	�B

=B
	�B

=B

�B

�B
�B
dB
JB
~B
~B
VB
�B
�B
�B
PB
B
B
6B
jB
PB
B
�B
B
6B
�B
JB
0B
�B
�B
�B
�B
B
jB
�B
^B
B

�B
)B
�B
�B
�B
�B
JB
�B
6B
�B
"B
�B
�B
�B
.B
HB
�B
 B
NB
hB
�B
�B
NB
NB
NB
:B
B
�B
�B
�B
�B
uB
�B
FB
�B
�B
�B
�B
�B
MB
gB
�B
B
SB
�B
�B
�B
YB
sB
�B
�B
eB
�B
1B
�B
�B
�B
1B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
WB
�B
�B
kB
�B
�B
kB
�B
�B
�B
�B
#B
�B
�B
�B
B
CB
�B
�B
�B
OB
pB
�B
 BB
 �B
!B
 �B
 �B
 �B
 �B
 �B
"�B
$B
$&B
$�B
%FB
%zB
%�B
&fB
&2B
'RB
'8B
'mB
'�B
($B
'�B
(>B
)B
)_B
)�B
*B
*eB
*�B
+B
*�B
+QB
+�B
,B
,=B
,�B
,�B
-B
-)B
-CB
-B
-B
-)B
-wB
-B
.B
.�B
.}B
/�B
/�B
/�B
0;B
0!B
0�B
0�B
1[B
1vB
1[B
2-B
2aB
2GB
2�B
2�B
2�B
2�B
2�B
3B
33B
33B
3hB
3�B
3�B
4nB
4nB
4nB
4�B
4�B
33B
33B
2-B
1�B
3�B
2�B
1�B
1AB
0�B
0�B
0�B
0�B
1AB
1B
1AB
1AB
1�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
5B
5%B
5%B
5%B
5B
5�B
6`B
6`B
7B
7B
7�B
7�B
7�B
7�B
88B
8B
88B
8lB
9�B
9�B
:^B
:^B
:�B
;0B
:�B
;0B
;B
;�B
;�B
<B
<�B
=qB
=VB
=<B
="B
=VB
=B
="B
>B
>]B
>�B
>wB
>�B
?B
?�B
?�B
@ B
@�B
@�B
@�B
AB
A B
A�B
C{B
DMB
EmB
FYB
F�B
GB
G_B
IB
H�B
IRB
IlB
I�B
I�B
I�B
J�B
JXB
J�B
K^B
K�B
KxB
K)B
J�B
J�B
J	B
I�B
I7B
IB
IB
IB
IB
IRB
J�B
J#B
I�B
I�B
J	B
JXB
JXB
J�B
KDB
K^B
K�B
KxB
K^B
K�B
L�B
L�B
M6B
MB
MB
M6B
MB
M�B
N�B
O�B
O�B
P.B
O�B
OB
O�B
PbB
P}B
P}B
P�B
P�B
P�B
P�B
Q B
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
RB
RB
RB
RoB
R�B
R�B
R�B
R�B
R�B
SuB
S�B
S�B
T,B
TB
T,B
TFB
TaB
TaB
T�B
T�B
UB
UMB
U�B
VB
VmB
WYB
W�B
W�B
W�B
X_B
X�B
YeB
Z�B
[#B
[	B
[�B
\]B
\�B
\�B
]/B
]B
]/B
]B
]dB
^B
^OB
_B
_;B
_�B
`\B
`�B
`�B
`�B
a�B
a�B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
c�B
d&B
c�B
c�B
c�B
c�B
c�B
cnB
c�B
c�B
dZB
e,B
eB
e,B
d�B
d@B
d�B
e�B
f2B
fLB
fLB
f�B
gRB
gRB
g8B
gB
gB
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
h
B
h$B
hXB
hsB
h�B
h�B
iDB
i�B
i�B
i�B
i�B
jB
j0B
jB
j0B
jB
jeB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kQB
kkB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
nIB
nIB
n}B
o B
o�B
pUB
p;B
p�B
qvB
q�B
q�B
q�B
q�B
r-B
r|B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
uB
t�B
t�B
tnB
t�B
t�B
t�B
t�B
u%B
u?B
u�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
x8B
x�B
y�B
y�B
zB
z�B
z^B
z�B
{0B
{JB
{B
z�B
{JB
{�B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|6B
|PB
|�B
|6B
|B
{�B
|6B
|6B
|PB
|PB
|�B
|�B
}VB
~B
~�B
~�B
B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�;B
�UB
��B
�UB
��B
��B
��B
�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104844  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172345  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172345  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172345                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022352  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022352  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                