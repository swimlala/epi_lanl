CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-04T00:41:42Z creation;2022-12-04T00:41:43Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221204004142  20221204010021  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��B�\)1   @���i�7@;n��O�;�dI�^1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  @���AffA@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C�fC  C�C �C"  C#�fC&  C'�fC)�fC,  C.  C0  C2  C4  C6  C7�fC:  C<  C=�fC@  CB�CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��3C�  C��C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C�  C�  C��C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  D fD � D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DLy�DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|y�D}  D}�fD~  D~� D  D� D�  D�@ D�� D��3D�  D�@ D�|�D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D��3D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�C3D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�<�DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D׼�D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D�|�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D���D�@ D߀ D�� D�  D�<�D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
>@�=q@�
>A�A=�A]�A}�A��\A�\)A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�Bw�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��
B��
Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C�C��C	��C��C��C��C��C��C��C��C�RC��C�C�C!��C#�RC%��C'�RC)�RC+��C-��C/��C1��C3��C5��C7�RC9��C;��C=�RC?��CA�CC��CE��CG��CI��CK�RCM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm�Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��)C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C��)C���C��)C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C��)C���C���C���C���C��)C���C���C���C��)C���C���C���C���C���C��)C��)C���C���C���C���C���C���C���C���C��)C���C���C���C��)C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C��)C���C���C���C���C���C���C��)C���C���C��)C���C���C���C���D t{D �{Dt{D�DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dz�D�{D	t{D	�{D
z�D
��Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*��D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8nD8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI��DJt{DJ�{DKt{DK�{DLnDL�{DMt{DM�{DNt{DN�DOt{DO�{DPt{DP�{DQt{DQ�{DRz�DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^��D_t{D_�{D`t{D`�{DanDa�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dfz�Df��Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|nD|�{D}z�D}�{D~t{D~�{Dt{D�{D�:=D�z=D��pD��=D�:=D�w
D��=D��=D�=pD�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��pD�=pD�}pD��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��pD�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��pD��=D�:=D�z=D��pD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��
D�7
D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��pD��pD�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=DŽpD��pD�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��
D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�=pD�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�7
D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׷
D��=D�:=D�z=DؽpD��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�7
D�w
Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��
D�:=D�z=Dߺ=D��=D�7
D�z=D�pD��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��
D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�
D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��pD��=D�:=D�z=D�=D��=D�:=D�z=D�pD��=D�7
D�z=D�=D��=D�:=D�}pD�=D��=D�:=D�z=D�=D��=D�:=D�z=D�pD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��6A��pA���A���A���A��A�ΥA��A��A���A��A��TA�уA�ѷA�уA��&A�҉A���A���A���A���A���A��A��yA���A��?A��KA���A�ںA�ٴA�רA��gA��mA��aA��A�ɆA�ǮA�ƨA���A¿�Aº*A¸RA¯�A�n/A� 'A��A��8A�0UA��A�n�A��eA���A�l�A�c�A��+A��A��A�P}A��A��yA�m)A�P�A���A���A�m�A���A���A��GA�1'A��A�V�A�+�A���A�qvA��eA�<A�,=A�zDA�XA��MA�[WA�,qA���A�F?A�G�A�d�A��uA��GA��7A��dA�;�A���A��ZA�HA�(A�FtA�YA�_A�oA��A�7�A��A��4A�c A�dZA�	A���A�kQA��5A�*eA���A�q�A�HA~��A|)_Ax`�Au8As�As�Apr�AoߤAo�oAoK�Ao
=An�An
=AmAkA�Ai�AgݘAf��Ae?Ac#:AbԕAb��AbB�Ab~Aa�@A`ߤA_�	A^��AZ��AW�hAUѷAU�ATMASs�AR&�AQ`�APZ�AN�AL�PAK��AJ{�AJ%AH��AHA�AGd�AF�AF�AF�NAF��AF�AE�+AEg�AE�AB��A@�jA@�@A?��A>�XA<�|A;��A9uA8VA7��A7�A7�tA7($A6��A5�
A3~�A2�<A1�>A0�SA/�6A/�A.��A-�vA,��A+�XA+	lA*�A)�oA(��A(�kA(��A(��A'�A'*0A&�A$��A#��A#eA"��A ��A /�A�PA�;A��A9XA��Au�AS�A�#A�5A�aA�AAzxA`BAM�A�[Ap;A��A�_A�A~A�}Aa|A-wAA<6A�MA��AMAc�A�4A@�A$tA�A�A�A�A��A��A��A�A��ADgA�A��A4�A �IA "h@�Ĝ@�J�@�s@�� @�@�@���@��X@�@�^5@��@��@�(@�6@�-@�P@��@컙@�tT@��@�?�@��@�2�@�P@��@��@�O�@��@� i@�ߤ@��3@݆�@�N<@��@�w�@��y@�%�@׏�@��@�w�@���@�A�@�q�@ӱ[@�;d@ҕ@с�@��H@б�@ІY@�+k@��@��g@�J�@ʤ�@Ț@�p�@��@�<6@�@��@�<�@���@��o@�j�@�q�@��@�u�@�#:@�G@�-w@�{�@�!�@���@���@���@���@�j�@�e�@�[W@�Y@�(�@�a|@���@�m]@�/@��5@��<@��I@���@�	�@���@�a@��D@��r@�rG@� i@��@���@��@�x�@� i@��Y@���@�8@�j@�+@�h�@�S&@�y>@��@��;@���@��@���@�R�@���@��@��F@�L0@��@���@��@�@�	l@�V@��@�!-@�q@���@���@��z@�Q@�($@�@��W@���@�c�@� \@���@��j@�'R@�m]@�7L@��@�C-@��*@�33@���@��H@�|@��:@���@��&@���@���@�U�@�C@�H�@�"h@���@��@���@�;d@��1@���@�a�@���@��X@�~(@�L0@�(�@�ݘ@���@�e,@���@���@�xl@�V�@�~@�M@��@��]@��@�ԕ@���@�x�@�o�@�p�@�rG@�v`@��@��f@�4�@�^5@�Q�@�1�@��@��@��@��9@���@�>B@�.�@�1�@�.�@�$�@� �@��}@���@���@���@��{@�t�@�\)@�7L@�@���@��@�ff@�h�@�bN@�I�@�O@�  @���@���@��@@��7@�qv@�\�@�7L@���@�ں@��@�Ov@o@~�\@~z@~��@~J@}�N@}`B@|l"@{�	@{�@zM�@y��@y�7@xz�@w��@w�4@w@va|@u�Z@uV@s��@rR�@q��@q\�@q�@p�p@pK^@o�k@o�f@o��@o�f@ov`@oqv@oRT@oA�@o;d@o9�@o=@o+@n�]@n�A@m�X@l9X@k�@j�}@i�#@iT�@iJ�@i	l@hɆ@h�_@hbN@h<�@h�@giD@f��@f�F@f�A@f_�@f�@e�@e��@e�@e\�@ef�@ezx@e��@e�'@erG@d�@d�`@d��@c1�@b�x@b\�@b�@a�N@ak�@a/@`�@`�@`4n@_��@_8@^��@^J�@]��@]��@]��@]=�@\�E@\�U@\��@\��@\V�@\�@\@[H�@Z� @ZJ�@Y�@Y��@YQ�@Y�@X��@X��@X�@W_p@WC@V��@V�'@V��@V� @VH�@V3�@V($@V@U��@U��@U�@Uj@U�@T��@Tz�@TU2@T'R@Sݘ@SE9@R�+@R($@Q��@Q0�@P�[@P�@PXy@PC-@P<�@PG@O˒@O�*@O��@Os@O\)@OF�@O$t@Nߤ@N�'@N�\@Nff@N�@MX@L�|@L��@K�@K�:@K9�@J�H@J�A@I��@I�9@Is�@I5�@H�v@H�$@Hr�@H%�@Hb@G�]@G�@G��@G�$@G]�@G�@F�@F�x@F�@EY�@E \@D��@D:�@C�@C��@C��@Co�@C1�@C"�@Co@B��@B��@Bn�@A�~@A0�@A	l@@�v@@Ĝ@@��@@�@@w�@@m�@@_@@4n@?ƨ@?�k@?l�@?Z�@?Z�@?_p@?_p@?E9@?�@?@>�@>�H@>��@=��@=rG@<�f@<�@<��@<�o@<[�@<  @;�k@;=@:�"@:�@:�1@:~�@:H�@:�@9��@9�@9�)@9�9@9��@9rG@9Vm@9:�@9�@9(�@9IR@90�@8N�@8u�@8z�@8�@7H�@7C�@7P�@71�@7$t@7,�@7.I@79�@76z@74�@6��@6n�@6?@6�@5��@5c�@4�@4y>@4?�@3�Q@3�@@3"�@2��@2��@2� @2u%@2M�@2O@1�.@1�@1�=@10�@0�|@0Ɇ@0��@0��@0��@0�_@0�.@0�o@0m�@0D�@07�@0!@07@0�@/��@/�;@/�m@/�@/�@/�f@/Mj@.�@.H�@-�T@-Vm@,�@,�@,�@,/�@+��@+Mj@+�@+@*�"@*�2@*�s@*��@*�6@*kQ@*_�@*5?@)�T@)�H@)�@)L�@)�@(�|@(��@((�@'��@'8@&��@&L0@%�z@%0�@$�5@$�j@$�z@$�u@$g8@#�K@#\)@#S@"�M@"�M@"�c@"�@"W�@"5?@" �@!�H@!w2@!	l@ �E@ w�@ `�@ I�@ K^@ H@ <�@��@�P@;d@�,@��@~�@�@��@��@Q�@&�@�P@�U@�D@r�@~@�@�@�&@ݘ@�}@ƨ@�0@�V@�@��@v�@u@��@�@��@��@��@��@��@q@`�@6@�@��@��@RT@�@v�@��@��@:�@+@;@��@�[@�u@%�@��@E9@�@�@�r@}V@h
@H�@C�@8�@	@�>@��@�h@^�@F@<6@�@�@��@�`@ѷ@�u@�@U2@  @��@��@{J@l�@]�@Mj@9�@ i@~�@_@!�@�@u�@7�@%�@�@ƨ@�$@�:@�{@{J@Z�@E9@�@
�"@
�y@
�B@
��@
E�@	�@	��@	��@	��@	�h@	|@	s�@	hs@	Y�@	Dg@	 \@��@D�@%�@�@�A@�@�@��@��@!-@��@�y@ߤ@�s@�<@p;@kQ@n�@q�@\�@:*@;�@=q@?@$�@ϫ@��@S&@!�@�|@�|@�P@�@�$@Ft@>B@?�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��6A��pA���A���A���A��A�ΥA��A��A���A��A��TA�уA�ѷA�уA��&A�҉A���A���A���A���A���A��A��yA���A��?A��KA���A�ںA�ٴA�רA��gA��mA��aA��A�ɆA�ǮA�ƨA���A¿�Aº*A¸RA¯�A�n/A� 'A��A��8A�0UA��A�n�A��eA���A�l�A�c�A��+A��A��A�P}A��A��yA�m)A�P�A���A���A�m�A���A���A��GA�1'A��A�V�A�+�A���A�qvA��eA�<A�,=A�zDA�XA��MA�[WA�,qA���A�F?A�G�A�d�A��uA��GA��7A��dA�;�A���A��ZA�HA�(A�FtA�YA�_A�oA��A�7�A��A��4A�c A�dZA�	A���A�kQA��5A�*eA���A�q�A�HA~��A|)_Ax`�Au8As�As�Apr�AoߤAo�oAoK�Ao
=An�An
=AmAkA�Ai�AgݘAf��Ae?Ac#:AbԕAb��AbB�Ab~Aa�@A`ߤA_�	A^��AZ��AW�hAUѷAU�ATMASs�AR&�AQ`�APZ�AN�AL�PAK��AJ{�AJ%AH��AHA�AGd�AF�AF�AF�NAF��AF�AE�+AEg�AE�AB��A@�jA@�@A?��A>�XA<�|A;��A9uA8VA7��A7�A7�tA7($A6��A5�
A3~�A2�<A1�>A0�SA/�6A/�A.��A-�vA,��A+�XA+	lA*�A)�oA(��A(�kA(��A(��A'�A'*0A&�A$��A#��A#eA"��A ��A /�A�PA�;A��A9XA��Au�AS�A�#A�5A�aA�AAzxA`BAM�A�[Ap;A��A�_A�A~A�}Aa|A-wAA<6A�MA��AMAc�A�4A@�A$tA�A�A�A�A��A��A��A�A��ADgA�A��A4�A �IA "h@�Ĝ@�J�@�s@�� @�@�@���@��X@�@�^5@��@��@�(@�6@�-@�P@��@컙@�tT@��@�?�@��@�2�@�P@��@��@�O�@��@� i@�ߤ@��3@݆�@�N<@��@�w�@��y@�%�@׏�@��@�w�@���@�A�@�q�@ӱ[@�;d@ҕ@с�@��H@б�@ІY@�+k@��@��g@�J�@ʤ�@Ț@�p�@��@�<6@�@��@�<�@���@��o@�j�@�q�@��@�u�@�#:@�G@�-w@�{�@�!�@���@���@���@���@�j�@�e�@�[W@�Y@�(�@�a|@���@�m]@�/@��5@��<@��I@���@�	�@���@�a@��D@��r@�rG@� i@��@���@��@�x�@� i@��Y@���@�8@�j@�+@�h�@�S&@�y>@��@��;@���@��@���@�R�@���@��@��F@�L0@��@���@��@�@�	l@�V@��@�!-@�q@���@���@��z@�Q@�($@�@��W@���@�c�@� \@���@��j@�'R@�m]@�7L@��@�C-@��*@�33@���@��H@�|@��:@���@��&@���@���@�U�@�C@�H�@�"h@���@��@���@�;d@��1@���@�a�@���@��X@�~(@�L0@�(�@�ݘ@���@�e,@���@���@�xl@�V�@�~@�M@��@��]@��@�ԕ@���@�x�@�o�@�p�@�rG@�v`@��@��f@�4�@�^5@�Q�@�1�@��@��@��@��9@���@�>B@�.�@�1�@�.�@�$�@� �@��}@���@���@���@��{@�t�@�\)@�7L@�@���@��@�ff@�h�@�bN@�I�@�O@�  @���@���@��@@��7@�qv@�\�@�7L@���@�ں@��@�Ov@o@~�\@~z@~��@~J@}�N@}`B@|l"@{�	@{�@zM�@y��@y�7@xz�@w��@w�4@w@va|@u�Z@uV@s��@rR�@q��@q\�@q�@p�p@pK^@o�k@o�f@o��@o�f@ov`@oqv@oRT@oA�@o;d@o9�@o=@o+@n�]@n�A@m�X@l9X@k�@j�}@i�#@iT�@iJ�@i	l@hɆ@h�_@hbN@h<�@h�@giD@f��@f�F@f�A@f_�@f�@e�@e��@e�@e\�@ef�@ezx@e��@e�'@erG@d�@d�`@d��@c1�@b�x@b\�@b�@a�N@ak�@a/@`�@`�@`4n@_��@_8@^��@^J�@]��@]��@]��@]=�@\�E@\�U@\��@\��@\V�@\�@\@[H�@Z� @ZJ�@Y�@Y��@YQ�@Y�@X��@X��@X�@W_p@WC@V��@V�'@V��@V� @VH�@V3�@V($@V@U��@U��@U�@Uj@U�@T��@Tz�@TU2@T'R@Sݘ@SE9@R�+@R($@Q��@Q0�@P�[@P�@PXy@PC-@P<�@PG@O˒@O�*@O��@Os@O\)@OF�@O$t@Nߤ@N�'@N�\@Nff@N�@MX@L�|@L��@K�@K�:@K9�@J�H@J�A@I��@I�9@Is�@I5�@H�v@H�$@Hr�@H%�@Hb@G�]@G�@G��@G�$@G]�@G�@F�@F�x@F�@EY�@E \@D��@D:�@C�@C��@C��@Co�@C1�@C"�@Co@B��@B��@Bn�@A�~@A0�@A	l@@�v@@Ĝ@@��@@�@@w�@@m�@@_@@4n@?ƨ@?�k@?l�@?Z�@?Z�@?_p@?_p@?E9@?�@?@>�@>�H@>��@=��@=rG@<�f@<�@<��@<�o@<[�@<  @;�k@;=@:�"@:�@:�1@:~�@:H�@:�@9��@9�@9�)@9�9@9��@9rG@9Vm@9:�@9�@9(�@9IR@90�@8N�@8u�@8z�@8�@7H�@7C�@7P�@71�@7$t@7,�@7.I@79�@76z@74�@6��@6n�@6?@6�@5��@5c�@4�@4y>@4?�@3�Q@3�@@3"�@2��@2��@2� @2u%@2M�@2O@1�.@1�@1�=@10�@0�|@0Ɇ@0��@0��@0��@0�_@0�.@0�o@0m�@0D�@07�@0!@07@0�@/��@/�;@/�m@/�@/�@/�f@/Mj@.�@.H�@-�T@-Vm@,�@,�@,�@,/�@+��@+Mj@+�@+@*�"@*�2@*�s@*��@*�6@*kQ@*_�@*5?@)�T@)�H@)�@)L�@)�@(�|@(��@((�@'��@'8@&��@&L0@%�z@%0�@$�5@$�j@$�z@$�u@$g8@#�K@#\)@#S@"�M@"�M@"�c@"�@"W�@"5?@" �@!�H@!w2@!	l@ �E@ w�@ `�@ I�@ K^@ H@ <�@��@�P@;d@�,@��@~�@�@��@��@Q�@&�@�P@�U@�D@r�@~@�@�@�&@ݘ@�}@ƨ@�0@�V@�@��@v�@u@��@�@��@��@��@��@��@q@`�@6@�@��@��@RT@�@v�@��@��@:�@+@;@��@�[@�u@%�@��@E9@�@�@�r@}V@h
@H�@C�@8�@	@�>@��@�h@^�@F@<6@�@�@��@�`@ѷ@�u@�@U2@  @��@��@{J@l�@]�@Mj@9�@ i@~�@_@!�@�@u�@7�@%�@�@ƨ@�$@�:@�{@{J@Z�@E9@�@
�"@
�y@
�B@
��@
E�@	�@	��@	��@	��@	�h@	|@	s�@	hs@	Y�@	Dg@	 \@��@D�@%�@�@�A@�@�@��@��@!-@��@�y@ߤ@�s@�<@p;@kQ@n�@q�@\�@:*@;�@=q@?@$�@ϫ@��@S&@!�@�|@�|@�P@�@�$@Ft@>B@?�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B�xB��B��B�]B��B��B��B��B��B��B��B�B��B��B��B�=B��B�B�B��B��B�#B��B��B�WB�qB�WB�WB��B�WB�=B��B��B�+B��B��B��Bg8BQ�B(XBzB��B�BԯB��B�4B��B�cB��B�[B��B�B��BmwB_�BQhBH1B@OB?B)DB�B�BpB��B�!B�
B�DB��B�JB�B��B�iBw�Bn�B_!BCB1�B'�B B�B�BB��B�qB̈́B��B��B�\B{�Bq�BmwBhsBc�BRBCB;�B8�B1�B+�B �BEB�BdB�B
�lB
�BB
��B
ȀB
�SB
�B
��B
��B
��B
��B
��B
�B
��B
��B
�B
��B
y>B
tB
g�B
e�B
e`B
c�B
b�B
_�B
ZB
S�B
NB
="B
)yB
�B
pB
B
B
�B
hB
JB
B	�}B	�B	�B	�;B	��B	�B	�B	�\B	��B	�VB	�!B	��B	�kB	�EB	�uB	ϫB	�OB	�BB	�B	��B	��B	�mB	��B	�yB	�B	�9B	�MB	��B	��B	�(B	�_B	�'B	HB	zDB	v�B	s�B	q[B	m�B	i*B	d�B	a�B	]IB	[�B	X�B	WYB	WYB	V�B	UMB	QhB	NVB	L�B	E�B	CaB	B'B	@OB	="B	<�B	:�B	:B	8lB	3hB	1'B	0�B	/�B	-�B	,�B	,"B	(sB	'�B	 \B	�B	xB	�B	
B	
B	&B	�B	�B	B	bB	�B	~B	xB	B	{B	  B�B��B��B�wB�]B�BB�BB�BB��B�"B�B�B�B�dB�FB��B�B�B��B�B��B�B��B�IB�OB�cB�B�]B��B�qB�qB�WB�WB�B�B�B��B�B�B�B�B��B�B�B�KB�"B�=B��B��B�B�B�B��B��B�5B�B�B�UB�'B�[B��B�-B�B�TB�9B�B�TB��B�`B��B��B��B	�B	SB	�B	B	�B	�B	�B	�B	�B	mB	�B	�B	"B	'mB	)�B	+�B	,�B	-)B	-�B	-�B	.cB	.}B	.cB	.IB	.}B	5�B	7�B	9>B	9rB	:DB	;B	;B	;JB	>�B	EB	E�B	GB	J�B	M�B	PB	RoB	S�B	T,B	W�B	XyB	YB	ZB	Y�B	YB	Y1B	[	B	\]B	_�B	b�B	d@B	e�B	g�B	k�B	o B	p�B	rB	s�B	t�B	u�B	v�B	y�B	~�B	�B	�B	�B	� B	�B	�aB	��B	�B	��B	�B	��B	�}B	�4B	��B	��B	�aB	�aB	��B	�gB	��B	��B	�B	��B	�BB	�4B	�@B	��B	�kB	��B	��B	�5B	��B	�5B	�hB	��B	�>B	��B	�6B	��B	��B	�OB	�B	̳B	�<B	�B	�:B	�gB	׍B	�EB	�eB	�7B	ܒB	��B	�B	�XB	�B	�B	�B	� B	�B	��B	�oB	�B	�aB	�B	��B	��B	��B	�TB	��B	�fB	�<B
aB
�B
B
B
EB
1B
�B

rB

�B

�B

�B

�B
�B
dB
�B
PB
�B
�B
VB
B
B
�B
{B
SB
�B
�B
�B
1B
�B
qB
)B
�B
�B
OB
B
pB
 vB
!�B
#TB
$�B
'B
*B
+�B
-�B
/OB
1�B
2�B
3�B
72B
:DB
;�B
=�B
?cB
A;B
D�B
F�B
F�B
HB
I�B
J�B
MPB
QB
UMB
W
B
X�B
Y�B
Z�B
\�B
_!B
_;B
_VB
_;B
_pB
_�B
_�B
_�B
`BB
`\B
`BB
`vB
a-B
a�B
d�B
i�B
j�B
oOB
sB
uB
u%B
u�B
v`B
v�B
wB
wfB
w�B
y�B
{0B
|B
|PB
|�B
~(B
cB
�B
� B
�iB
��B
�;B
��B
�'B
��B
�%B
��B
��B
��B
�=B
��B
�xB
�B
�"B
�\B
�bB
��B
�&B
�,B
��B
��B
�B
�1B
��B
�QB
�WB
�xB
��B
�B
�IB
��B
�jB
�OB
��B
�NB
��B
�B
�B
��B
�2B
��B
�B
�_B
��B
�"B
�qB
�B
��B
��B
��B
��B
�B
�OB
��B
�!B
��B
�'B
��B
��B
��B
��B
�9B
��B
�zB
��B
�rB
��B
�jB
�<B
��B
��B
��B
��B
�cB
��B
�4B
��B
��B
��B
� B
��B
�AB
�uB
�-B
ÖB
�gB
ƨB
�_B
�fB
�#B
��B
ˬB
�JB
��B
�"B
�B
�(B
�vB
�bB
бB
�NB
�B
�:B
�TB
ңB
��B
�&B
ӏB
�,B
ԯB
��B
�9B
��B
�+B
ٚB
�QB
�#B
�qB
��B
�)B
ܬB
ܬB
��B
�B
�dB
�B
ߤB
�B
�\B
�B
��B
��B
�-B
�bB
�|B
�B
�B
��B
�:B
�B
�B
��B
�B
�B
��B
�ZB
�tB
�B
�tB
�,B
�B
�B
�
B
�B
�B
�B
��B
�_B
�B
�B
�B
�QB
��B
��B
�WB
�B
��B
��B
��B
��B
�]B
��B
��B
�B
�cB
�B
��B
�B
�B
�5B
�OB
�B
�[B
�'B
�AB
�AB
�vB
�vB
�B
�[B
�[B
�AB
�aB
�B
��B
�B
�B
�B
��B
�B
��B
�+B
�`B
�LB
��B
��B
��B
�B
�8B
�lB
��B
��B
�>B
��B
�DB
�xB
��B
��B
��B
��B
��B
��B
��B
�dB
�dB
��B
��B
��B
�B
�B
�B
�B
��B
��B
��B
�"B
�]B
��B
��B B iB �BB�BuB�B�B�B�B-BGBaB�B�BB�B�BBmB�B�B%B�BzBB	B	7B	�B
�BBDBDB^BxB�BB�B�B�B�B�BpB�B�BBB�BbBbBB4BNBNB4BNB�B B�B@B�B�B{B�BBgB�B�BBSBSB�B
B$B$B$B?B?B?BsBEB�B�B�B7B#B=BWBWB�B�B�B�BBCB�B�B/B�B5BBVB�B B B 'B 'B �B!bB"B"NB#B#:B#TB#nB#�B#�B#�B#�B#�B$@B$�B$�B%B%,B%,B%`B%zB%�B%�B%�B&B&2B&fB&�B'B'RB'�B'�B'�B'�B'�B(
B(�B)_B*�B+6B+kB,B,B,qB,�B,�B,�B,�B,�B-)B-CB-�B-�B-�B-�B.B.�B/5B/�B/�B/iB/�B/�B/�B/�B/�B/�B/�B0�B1[B1[B1�B1�B1�B2B2B2-B2�B3B33B33B3MB3hB3�B3�B3�B3�B3�B4B4B4B3�B4B4�B4�B5?B5�B5�B5�B5�B5tB6FB6�B6�B6�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B��B��B��B��B��B��B��B��B��B��B�xB��B��B�]B��B��B��B��B��B��B��B�B��B��B��B�=B��B�B�B��B��B�#B��B��B�WB�qB�WB�WB��B�WB�=B��B��B�+B��B��B��Bg8BQ�B(XBzB��B�BԯB��B�4B��B�cB��B�[B��B�B��BmwB_�BQhBH1B@OB?B)DB�B�BpB��B�!B�
B�DB��B�JB�B��B�iBw�Bn�B_!BCB1�B'�B B�B�BB��B�qB̈́B��B��B�\B{�Bq�BmwBhsBc�BRBCB;�B8�B1�B+�B �BEB�BdB�B
�lB
�BB
��B
ȀB
�SB
�B
��B
��B
��B
��B
��B
�B
��B
��B
�B
��B
y>B
tB
g�B
e�B
e`B
c�B
b�B
_�B
ZB
S�B
NB
="B
)yB
�B
pB
B
B
�B
hB
JB
B	�}B	�B	�B	�;B	��B	�B	�B	�\B	��B	�VB	�!B	��B	�kB	�EB	�uB	ϫB	�OB	�BB	�B	��B	��B	�mB	��B	�yB	�B	�9B	�MB	��B	��B	�(B	�_B	�'B	HB	zDB	v�B	s�B	q[B	m�B	i*B	d�B	a�B	]IB	[�B	X�B	WYB	WYB	V�B	UMB	QhB	NVB	L�B	E�B	CaB	B'B	@OB	="B	<�B	:�B	:B	8lB	3hB	1'B	0�B	/�B	-�B	,�B	,"B	(sB	'�B	 \B	�B	xB	�B	
B	
B	&B	�B	�B	B	bB	�B	~B	xB	B	{B	  B�B��B��B�wB�]B�BB�BB�BB��B�"B�B�B�B�dB�FB��B�B�B��B�B��B�B��B�IB�OB�cB�B�]B��B�qB�qB�WB�WB�B�B�B��B�B�B�B�B��B�B�B�KB�"B�=B��B��B�B�B�B��B��B�5B�B�B�UB�'B�[B��B�-B�B�TB�9B�B�TB��B�`B��B��B��B	�B	SB	�B	B	�B	�B	�B	�B	�B	mB	�B	�B	"B	'mB	)�B	+�B	,�B	-)B	-�B	-�B	.cB	.}B	.cB	.IB	.}B	5�B	7�B	9>B	9rB	:DB	;B	;B	;JB	>�B	EB	E�B	GB	J�B	M�B	PB	RoB	S�B	T,B	W�B	XyB	YB	ZB	Y�B	YB	Y1B	[	B	\]B	_�B	b�B	d@B	e�B	g�B	k�B	o B	p�B	rB	s�B	t�B	u�B	v�B	y�B	~�B	�B	�B	�B	� B	�B	�aB	��B	�B	��B	�B	��B	�}B	�4B	��B	��B	�aB	�aB	��B	�gB	��B	��B	�B	��B	�BB	�4B	�@B	��B	�kB	��B	��B	�5B	��B	�5B	�hB	��B	�>B	��B	�6B	��B	��B	�OB	�B	̳B	�<B	�B	�:B	�gB	׍B	�EB	�eB	�7B	ܒB	��B	�B	�XB	�B	�B	�B	� B	�B	��B	�oB	�B	�aB	�B	��B	��B	��B	�TB	��B	�fB	�<B
aB
�B
B
B
EB
1B
�B

rB

�B

�B

�B

�B
�B
dB
�B
PB
�B
�B
VB
B
B
�B
{B
SB
�B
�B
�B
1B
�B
qB
)B
�B
�B
OB
B
pB
 vB
!�B
#TB
$�B
'B
*B
+�B
-�B
/OB
1�B
2�B
3�B
72B
:DB
;�B
=�B
?cB
A;B
D�B
F�B
F�B
HB
I�B
J�B
MPB
QB
UMB
W
B
X�B
Y�B
Z�B
\�B
_!B
_;B
_VB
_;B
_pB
_�B
_�B
_�B
`BB
`\B
`BB
`vB
a-B
a�B
d�B
i�B
j�B
oOB
sB
uB
u%B
u�B
v`B
v�B
wB
wfB
w�B
y�B
{0B
|B
|PB
|�B
~(B
cB
�B
� B
�iB
��B
�;B
��B
�'B
��B
�%B
��B
��B
��B
�=B
��B
�xB
�B
�"B
�\B
�bB
��B
�&B
�,B
��B
��B
�B
�1B
��B
�QB
�WB
�xB
��B
�B
�IB
��B
�jB
�OB
��B
�NB
��B
�B
�B
��B
�2B
��B
�B
�_B
��B
�"B
�qB
�B
��B
��B
��B
��B
�B
�OB
��B
�!B
��B
�'B
��B
��B
��B
��B
�9B
��B
�zB
��B
�rB
��B
�jB
�<B
��B
��B
��B
��B
�cB
��B
�4B
��B
��B
��B
� B
��B
�AB
�uB
�-B
ÖB
�gB
ƨB
�_B
�fB
�#B
��B
ˬB
�JB
��B
�"B
�B
�(B
�vB
�bB
бB
�NB
�B
�:B
�TB
ңB
��B
�&B
ӏB
�,B
ԯB
��B
�9B
��B
�+B
ٚB
�QB
�#B
�qB
��B
�)B
ܬB
ܬB
��B
�B
�dB
�B
ߤB
�B
�\B
�B
��B
��B
�-B
�bB
�|B
�B
�B
��B
�:B
�B
�B
��B
�B
�B
��B
�ZB
�tB
�B
�tB
�,B
�B
�B
�
B
�B
�B
�B
��B
�_B
�B
�B
�B
�QB
��B
��B
�WB
�B
��B
��B
��B
��B
�]B
��B
��B
�B
�cB
�B
��B
�B
�B
�5B
�OB
�B
�[B
�'B
�AB
�AB
�vB
�vB
�B
�[B
�[B
�AB
�aB
�B
��B
�B
�B
�B
��B
�B
��B
�+B
�`B
�LB
��B
��B
��B
�B
�8B
�lB
��B
��B
�>B
��B
�DB
�xB
��B
��B
��B
��B
��B
��B
��B
�dB
�dB
��B
��B
��B
�B
�B
�B
�B
��B
��B
��B
�"B
�]B
��B
��B B iB �BB�BuB�B�B�B�B-BGBaB�B�BB�B�BBmB�B�B%B�BzBB	B	7B	�B
�BBDBDB^BxB�BB�B�B�B�B�BpB�B�BBB�BbBbBB4BNBNB4BNB�B B�B@B�B�B{B�BBgB�B�BBSBSB�B
B$B$B$B?B?B?BsBEB�B�B�B7B#B=BWBWB�B�B�B�BBCB�B�B/B�B5BBVB�B B B 'B 'B �B!bB"B"NB#B#:B#TB#nB#�B#�B#�B#�B#�B$@B$�B$�B%B%,B%,B%`B%zB%�B%�B%�B&B&2B&fB&�B'B'RB'�B'�B'�B'�B'�B(
B(�B)_B*�B+6B+kB,B,B,qB,�B,�B,�B,�B,�B-)B-CB-�B-�B-�B-�B.B.�B/5B/�B/�B/iB/�B/�B/�B/�B/�B/�B/�B0�B1[B1[B1�B1�B1�B2B2B2-B2�B3B33B33B3MB3hB3�B3�B3�B3�B3�B4B4B4B3�B4B4�B4�B5?B5�B5�B5�B5�B5tB6FB6�B6�B6�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221204004132  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221204004142  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221204004143  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221204004143                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221204004143  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221204004143  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221204010021                      G�O�G�O�G�O�                