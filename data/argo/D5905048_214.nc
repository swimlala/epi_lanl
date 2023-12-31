CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-26T00:37:47Z creation;2018-02-26T00:37:51Z conversion to V3.1;2019-12-19T07:44:20Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180226003747  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_214                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�N�I��1   @�N����@3T���?�dA��s�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @'�@tz�@�=q@�=qA�A=�A[�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�B�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC�RCE�RCG��CI��CK��CM��CO��CQ��CS��CU��CW�CY�C[�C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}qD��=D��qD�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�7
D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��mA��mA��`A��`A��`A��;A��;A�A�n�A�33A�{A���A��A��yA��HA���A�ƨAǗ�A�JAƴ9AƝ�AƏ\A�v�A�+A��A��A�oA�A���A��A��yA��`A���A���A���A�ƨAżjAŬAŋDA�VA�7LA�"�A��AăA�A�A�;dA��A��TA�x�A�5?Aº^A+A�O�A�?}A�ĜA�t�A�&�A���A��-A�x�A�ZA��
A��A���A�VA�n�A�+A��A���A�"�A��A�33A�A�A��A�?}A��7A�p�A��7A�33A��RA�M�A�l�A�`BA��A��
A���A�z�A�M�A���A��TA��jA�(�A�x�A���A�Q�A��FA���A��A��A�9XA��A��A�$�A�oA�"�A���A��A�bA��/A�JA��\A�{A���A��\A{?}Av~�Aq�An��Al��Ak��AkS�Aj�uAi��Agt�Adn�Abr�Aa��Aa&�A^��AY�wAV~�ATARz�AQ��AQK�AM��AL(�AK�-AKXAH�AE/AC�;AAoA@5?A?t�A>~�A>(�A=��A=VA<v�A;?}A9�A9K�A6{A3+A3�A2�A3A3�-A3�
A3&�A1�PA1��A1�A1�wA0(�A-hsA)��A)�A)O�A%l�A!��A�TAA�A5?AhsA�+A �A�FA�AbAoA��AAJA7LAJAG�A��A�A"�A�HA��A�9A�DA`BA�!A �A�A	�mA	
=A�A��A��A�A�+A �@�ƨ@��-@���@��@�(�@�o@�V@��h@��@�@�O�@�(�@���@��@��@�J@�p�@��/@���@���@�j@��@�C�@�ȴ@�=q@�n�@���@�1@���@�I�@��
@��H@�{@��@ְ!@�z�@ѡ�@У�@�l�@��y@��T@�ff@Ͳ-@��/@�I�@˅@�1@�z�@���@�E�@̣�@̴9@�Ĝ@̃@�Z@���@�X@�7L@�|�@���@�&�@�@ʏ\@ʰ!@���@ɡ�@�`B@ÍP@å�@�j@���@��@�S�@��@��;@���@��-@��^@���@��9@�ȴ@��@��j@�1'@�I�@���@�\)@�p�@�t�@��#@�G�@��@�Ĝ@�I�@�Q�@��9@��@��7@��h@�X@�dZ@��@�V@��9@��@��+@��-@��@�G�@��/@�bN@��
@�K�@�33@���@��\@�E�@��@�J@�@�7L@��@��@�r�@�A�@��@��@��m@�dZ@��;@�(�@� �@� �@�1@���@��F@�|�@���@��@�X@�p�@���@��7@�X@���@��@���@�Ĝ@�(�@��w@��;@�ƨ@�C�@��y@��y@��@�~�@�E�@�E�@�$�@��#@��^@���@���@�x�@�p�@��@�/@�Ĝ@�r�@�9X@�(�@� �@�  @���@�|�@��@��!@�n�@��T@��@�@��^@�hs@�V@��`@���@�%@���@��j@�A�@���@���@��P@�S�@��@��@���@���@�n�@�E�@�-@�J@���@�O�@��j@��u@�r�@�bN@�I�@�(�@���@���@�t�@�+@��@��@���@�"�@�"�@�
=@���@��@���@���@���@��h@��7@�G�@��j@�Q�@� �@���@���@�l�@�K�@�+@�"�@�o@��y@��R@��\@�E�@�5?@�J@��@���@��7@�G�@��@��@�  @��@�S�@�C�@��@���@�$�@�J@�-@��@���@�7L@��`@�z�@��m@���@��
@���@���@���@�K�@��@���@�^5@�M�@��#@��@���@�bN@��@�  @�Z@��u@��
@��@�\)@�C�@�33@�"�@�o@�
=@��H@�n�@��@���@��h@�hs@���@��@�j@�Q�@�(�@��@���@�;d@�33@�33@��@��@���@�M�@�E�@�M�@�$�@�@���@�x�@�?}@��@���@��@��u@�r�@��@�@~��@~v�@~5?@}�h@}`B@}�@|9X@z�@z~�@z=q@y�^@yX@y&�@y�@y�@y�@y�@y%@x�`@xbN@xb@wK�@vȴ@u��@v@u��@u?}@u�@uV@t��@t�j@s�@s"�@sC�@r��@q�#@pĜ@o|�@o�@nv�@n5?@m�T@m�-@m�h@m`B@l��@lZ@l�@k�m@kS�@ko@j�@jn�@i��@i�^@i��@h��@h �@g�;@g��@g��@g;d@f��@fV@fE�@f@f@e��@e�h@d�@dI�@c�m@c��@co@bn�@bM�@b=q@b=q@b-@a�#@ahs@`��@`�u@_�@_;d@^�R@^V@^{@]�T@]V@\9X@[�@["�@[@Z��@Z=q@Y��@YX@X��@XĜ@X�@W��@W|�@W;d@V�R@V��@V��@V��@V��@V�+@VE�@V$�@V@U�T@U�h@UV@T�/@T�@TI�@TI�@T(�@T�@Sƨ@St�@SS�@S33@S"�@S"�@S"�@So@R~�@Q��@Q�@Q�#@Q��@Q��@Qx�@QG�@Q�@P��@PA�@O�;@O�P@OK�@N��@N�+@N5?@M��@M`B@M?}@M�@L��@L�/@L�D@L�D@LZ@L(�@K�m@Kt�@K"�@K@J��@J�\@I��@I�^@Ihs@Hr�@G��@Gl�@F��@E�@E�@D�/@D�j@D�j@D�j@D�@D�@Dz�@DI�@D�@C�@Co@B��@B=q@A�@A��@A�7@AX@A%@@��@@r�@@  @?�P@?
=@>�R@>E�@>5?@>{@=�@<��@<�/@<�/@<��@<�j@<��@<j@<I�@<I�@<9X@<(�@<1@:��@:^5@:�@9��@9x�@8��@8b@7��@7�P@7�P@7|�@7l�@7\)@7�@6v�@65?@5�-@5�h@5?}@4�@4�@4�j@4�D@4j@4I�@4(�@3�
@3�@3dZ@3o@2-@2J@1��@1G�@1�@1%@0��@0  @/�;@/�@/l�@/\)@/\)@/K�@/
=@.�@.��@.V@.@-�-@-��@-O�@,��@,�@,�/@,��@,�@,�D@,(�@+�F@+S�@+"�@*��@*~�@*~�@*n�@*=q@)�^@)X@)7L@(��@(Ĝ@(��@(1'@'�@'�P@'|�@'\)@'+@&�y@&�R@&�R@&v�@&@%@%p�@$��@$�/@$�/@$��@$�j@$��@$z�@$I�@#�
@#S�@#@"�@"�H@"~�@"-@"�@!�#@!�^@!��@!x�@!7L@ �`@ ��@  �@�w@��@|�@l�@\)@K�@K�@K�@K�@K�@�@�+@V@�@@�@?}@V@�@z�@I�@9X@(�@�@�m@�F@��@�@t�@dZ@S�@33@o@�H@�!@~�@n�@^5@^5@=q@�@��@hs@X@7L@%@��@�u@r�@bN@ �@��@�P@l�@\)@l�@\)@
=@��@E�@�T@��@�-@��@?}@�/@��@Z@Z@Z@9X@(�@(�@�@��@�F@�@33@o@�\@~�@n�@^5@M�@-@�@��@��@�@�^@&�@�u@�u@�u@�u@�u@�u@�u@�u@��@�u@�u@�u@��@�@bN@A�@�@l�@;d@+@�@ȴ@��@ff@E�@5?@$�@{@�T@��@�-@��@p�@/@�@�/@�D@z�@(�@�m@ƨ@�F@��@C�@33@"�@o@
�@
��@
~�@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��mA��mA��`A��`A��`A��;A��;A�A�n�A�33A�{A���A��A��yA��HA���A�ƨAǗ�A�JAƴ9AƝ�AƏ\A�v�A�+A��A��A�oA�A���A��A��yA��`A���A���A���A�ƨAżjAŬAŋDA�VA�7LA�"�A��AăA�A�A�;dA��A��TA�x�A�5?Aº^A+A�O�A�?}A�ĜA�t�A�&�A���A��-A�x�A�ZA��
A��A���A�VA�n�A�+A��A���A�"�A��A�33A�A�A��A�?}A��7A�p�A��7A�33A��RA�M�A�l�A�`BA��A��
A���A�z�A�M�A���A��TA��jA�(�A�x�A���A�Q�A��FA���A��A��A�9XA��A��A�$�A�oA�"�A���A��A�bA��/A�JA��\A�{A���A��\A{?}Av~�Aq�An��Al��Ak��AkS�Aj�uAi��Agt�Adn�Abr�Aa��Aa&�A^��AY�wAV~�ATARz�AQ��AQK�AM��AL(�AK�-AKXAH�AE/AC�;AAoA@5?A?t�A>~�A>(�A=��A=VA<v�A;?}A9�A9K�A6{A3+A3�A2�A3A3�-A3�
A3&�A1�PA1��A1�A1�wA0(�A-hsA)��G�O�G�O�A%l�A!��A�TAA�A5?AhsA�+A �A�FA�AbAoA��AAJA7LAJAG�A��A�A"�A�HA��A�9A�DA`BA�!A �A�A	�mA	
=A�A��A��A�A�+A �@�ƨ@��-@���@��@�(�@�o@�V@��h@��@�@�O�@�(�@���@��@��@�J@�p�@��/@���@���@�j@��@�C�@�ȴ@�=q@�n�@���@�1@���@�I�@��
@��H@�{@��@ְ!@�z�@ѡ�@У�@�l�@��y@��T@�ff@Ͳ-@��/@�I�@˅@�1@�z�@���@�E�@̣�@̴9@�Ĝ@̃@�Z@���@�X@�7L@�|�@���@�&�@�@ʏ\@ʰ!@���@ɡ�@�`B@ÍP@å�@�j@���@��@�S�@��@��;@���@��-@��^@���@��9@�ȴ@��@��j@�1'@�I�@���@�\)@�p�@�t�@��#@�G�@��@�Ĝ@�I�@�Q�@��9@��@��7@��h@�X@�dZ@��@�V@��9@��@��+@��-@��@�G�@��/@�bN@��
@�K�@�33@���@��\@�E�@��@�J@�@�7L@��@��@�r�@�A�@��@��@��m@�dZ@��;@�(�@� �@� �@�1@���@��F@�|�@���@��@�X@�p�@���@��7@�X@���@��@���@�Ĝ@�(�@��w@��;@�ƨ@�C�@��y@��y@��@�~�@�E�@�E�@�$�@��#@��^@���@���@�x�@�p�@��@�/@�Ĝ@�r�@�9X@�(�@� �@�  @���@�|�@��@��!@�n�@��T@��@�@��^@�hs@�V@��`@���@�%@���@��j@�A�@���@���@��P@�S�@��@��@���@���@�n�@�E�@�-@�J@���@�O�@��j@��u@�r�@�bN@�I�@�(�@���@���@�t�@�+@��@��@���@�"�@�"�@�
=@���@��@���@���@���@��h@��7@�G�@��j@�Q�@� �@���@���@�l�@�K�@�+@�"�@�o@��y@��R@��\@�E�@�5?@�J@��@���@��7@�G�@��@��@�  @��@�S�@�C�@��@���@�$�@�J@�-@��@���@�7L@��`@�z�@��m@���@��
@���@���@���@�K�@��@���@�^5@�M�@��#@��@���@�bN@��@�  @�Z@��u@��
@��@�\)@�C�@�33@�"�@�o@�
=@��H@�n�@��@���@��h@�hs@���@��@�j@�Q�@�(�@��@���@�;d@�33@�33@��@��@���@�M�@�E�@�M�@�$�@�@���@�x�@�?}@��@���@��@��u@�r�@��@�@~��@~v�@~5?@}�h@}`B@}�@|9X@z�@z~�@z=q@y�^@yX@y&�@y�@y�@y�@y�@y%@x�`@xbN@xb@wK�@vȴ@u��@v@u��@u?}@u�@uV@t��@t�j@s�@s"�@sC�@r��@q�#@pĜ@o|�@o�@nv�@n5?@m�T@m�-@m�h@m`B@l��@lZ@l�@k�m@kS�@ko@j�@jn�@i��@i�^@i��@h��@h �@g�;@g��@g��@g;d@f��@fV@fE�@f@f@e��@e�h@d�@dI�@c�m@c��@co@bn�@bM�@b=q@b=q@b-@a�#@ahs@`��@`�u@_�@_;d@^�R@^V@^{@]�T@]V@\9X@[�@["�@[@Z��@Z=q@Y��@YX@X��@XĜ@X�@W��@W|�@W;d@V�R@V��@V��@V��@V��@V�+@VE�@V$�@V@U�T@U�h@UV@T�/@T�@TI�@TI�@T(�@T�@Sƨ@St�@SS�@S33@S"�@S"�@S"�@So@R~�@Q��@Q�@Q�#@Q��@Q��@Qx�@QG�@Q�@P��@PA�@O�;@O�P@OK�@N��@N�+@N5?@M��@M`B@M?}@M�@L��@L�/@L�D@L�D@LZ@L(�@K�m@Kt�@K"�@K@J��@J�\@I��@I�^@Ihs@Hr�@G��@Gl�@F��@E�@E�@D�/@D�j@D�j@D�j@D�@D�@Dz�@DI�@D�@C�@Co@B��@B=q@A�@A��@A�7@AX@A%@@��@@r�@@  @?�P@?
=@>�R@>E�@>5?@>{@=�@<��@<�/@<�/@<��@<�j@<��@<j@<I�@<I�@<9X@<(�@<1@:��@:^5@:�@9��@9x�@8��@8b@7��@7�P@7�P@7|�@7l�@7\)@7�@6v�@65?@5�-@5�h@5?}@4�@4�@4�j@4�D@4j@4I�@4(�@3�
@3�@3dZ@3o@2-@2J@1��@1G�@1�@1%@0��@0  @/�;@/�@/l�@/\)@/\)@/K�@/
=@.�@.��@.V@.@-�-@-��@-O�@,��@,�@,�/@,��@,�@,�D@,(�@+�F@+S�@+"�@*��@*~�@*~�@*n�@*=q@)�^@)X@)7L@(��@(Ĝ@(��@(1'@'�@'�P@'|�@'\)@'+@&�y@&�R@&�R@&v�@&@%@%p�@$��@$�/@$�/@$��@$�j@$��@$z�@$I�@#�
@#S�@#@"�@"�H@"~�@"-@"�@!�#@!�^@!��@!x�@!7L@ �`@ ��@  �@�w@��@|�@l�@\)@K�@K�@K�@K�@K�@�@�+@V@�@@�@?}@V@�@z�@I�@9X@(�@�@�m@�F@��@�@t�@dZ@S�@33@o@�H@�!@~�@n�@^5@^5@=q@�@��@hs@X@7L@%@��@�u@r�@bN@ �@��@�P@l�@\)@l�@\)@
=@��@E�@�T@��@�-@��@?}@�/@��@Z@Z@Z@9X@(�@(�@�@��@�F@�@33@o@�\@~�@n�@^5@M�@-@�@��@��@�@�^@&�@�u@�u@�u@�u@�u@�u@�u@�u@��@�u@�u@�u@��@�@bN@A�@�@l�@;d@+@�@ȴ@��@ff@E�@5?@$�@{@�T@��@�-@��@p�@/@�@�/@�D@z�@(�@�m@ƨ@�F@��@C�@33@"�@o@
�@
��@
~�@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B5?B5?B5?B5?B6FB5?B5?B5?B49B33B1'B33BB�BN�BS�BVB[#B^5BbNBffBk�B�B�-BÖBǮBǮBŢB��B��B��B�
B�B�)B�;B�BB�HB�mB�mB�mB�B�B�B��B��B��B��BPB)�BE�BI�BK�BN�BZB[#Be`BdZBdZB`BB_;BcTBe`Be`BaHBE�B)�B�B!�B �B!�BuB+B�B6FB0!B-B%�BbB!�B�B(�B2-B,B2-B-B�B�B�#B�B��B��B�
BŢB�LB��B[#BdZBv�BW
BYB8RBbB{B/B)�B�B�B
��B
�B
�5B
��B
��B
��B
�\B
^5B
M�B
33B	��B	�B	�wB	��B	��B	��B	��B	��B	�uB	�B	m�B	L�B	S�B	T�B	P�B	/B	JB��B��B��B	B��B�B�`B�B�sB��B��B�B��B��B�B��B�B�B�?B�dBB�wB��B�B��B�#B�/B�NB��B��B��B�sB��B		7B	B�ZB��B�3BÖBÖB�JBz�B�VB��B��B��B�uB�VB�hB�VB�B{�Bs�Bk�Be`B]/BE�BR�BffBe`BcTB\)Bk�Bl�BiyBcTBT�BXBZBT�BH�BN�BN�BB�BO�B9XB33B:^BN�BN�BXB^5BZBR�BT�BN�BB�B=qBL�BS�BZBS�BG�BQ�B^5B^5BVBYBffBgmBaHBgmBjBn�B� Bu�Bo�Bp�B|�B� B{�Bu�Bm�Bl�Be`Bp�Bt�B}�B� B�oB�hB�VB�{B��B��B�BĜBÖB�^BɺB��B��B��B��B�B��B��BĜB�B�TB�B�sB�`B�B�wBɺB�B�/B��BɺB��B�#B�`B�B�B�B�sB�HB�#B�B�B�B�B��B�TB�)B�B�B�B��B�B��B��B��B	B	B	B	B��B�B	B	B��B	B	PB	{B	�B	�B	�B	�B	�B	&�B	&�B	&�B	+B	/B	49B	49B	1'B	6FB	:^B	>wB	C�B	H�B	L�B	Q�B	Q�B	[#B	_;B	^5B	^5B	^5B	^5B	aHB	bNB	_;B	bNB	dZB	l�B	v�B	w�B	x�B	}�B	� B	|�B	w�B	y�B	}�B	�B	�B	�B	�B	�1B	�1B	�1B	�7B	�PB	�JB	�PB	�bB	�oB	�oB	�{B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�B	�'B	�9B	�RB	�^B	�XB	�RB	�RB	�XB	��B	��B	B	ÖB	B	ŢB	ŢB	ƨB	ȴB	��B	ɺB	ȴB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�)B	�BB	�HB	�;B	�5B	�)B	�/B	�NB	�TB	�ZB	�TB	�NB	�TB	�NB	�ZB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B	��B
B
B	��B	��B	��B
  B
B
B

=B
	7B
B
%B
	7B
	7B

=B
DB
DB
JB
DB
	7B
DB
PB
\B
VB
JB
\B
hB
hB
bB
\B
\B
VB
oB
uB
oB
bB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
#�B
"�B
!�B
#�B
#�B
#�B
!�B
�B
!�B
#�B
 �B
�B
�B
�B
!�B
 �B
"�B
"�B
#�B
#�B
"�B
!�B
"�B
#�B
$�B
"�B
$�B
%�B
#�B
#�B
&�B
%�B
#�B
%�B
(�B
(�B
(�B
&�B
&�B
(�B
+B
)�B
)�B
)�B
'�B
&�B
'�B
(�B
)�B
)�B
)�B
-B
.B
.B
-B
,B
+B
+B
+B
+B
+B
-B
.B
.B
.B
+B
,B
/B
0!B
2-B
1'B
1'B
2-B
0!B
2-B
49B
33B
1'B
49B
5?B
49B
7LB
8RB
8RB
8RB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
8RB
8RB
8RB
:^B
9XB
9XB
8RB
8RB
:^B
:^B
;dB
;dB
:^B
9XB
8RB
8RB
;dB
<jB
;dB
;dB
;dB
:^B
:^B
:^B
8RB
:^B
;dB
;dB
<jB
;dB
<jB
<jB
=qB
?}B
?}B
?}B
?}B
>wB
@�B
?}B
?}B
>wB
>wB
?}B
@�B
?}B
?}B
>wB
?}B
>wB
<jB
>wB
@�B
@�B
>wB
C�B
C�B
F�B
H�B
G�B
G�B
G�B
F�B
F�B
E�B
D�B
D�B
F�B
F�B
G�B
H�B
I�B
H�B
H�B
H�B
G�B
G�B
H�B
H�B
J�B
J�B
L�B
L�B
J�B
K�B
N�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
M�B
L�B
I�B
L�B
O�B
N�B
N�B
N�B
N�B
P�B
R�B
R�B
R�B
R�B
Q�B
P�B
O�B
Q�B
P�B
S�B
R�B
R�B
T�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
T�B
S�B
Q�B
VB
VB
T�B
W
B
W
B
VB
VB
XB
YB
XB
YB
ZB
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
\)B
\)B
[#B
[#B
ZB
ZB
[#B
\)B
\)B
\)B
^5B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
bNB
cTB
cTB
cTB
bNB
bNB
aHB
aHB
aHB
bNB
dZB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
cTB
e`B
ffB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
e`B
dZB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
gmB
iyB
jB
jB
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
jB
jB
jB
k�B
k�B
k�B
jB
jB
jB
k�B
l�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
m�B
n�B
n�B
m�B
l�B
l�B
m�B
m�B
o�B
o�B
n�B
m�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
p�B
p�B
q�B
p�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
r�B
q�B
p�B
r�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
s�B
t�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
x�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
y�B
y�B
z�B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B5ZB5ZB5ZB5ZB6`B5ZB5?B5?B4TB3MB1�B4BCBOBTBV9B[=B^jBb�Bf�Bl=B�AB�|BðB��B�B�?B�B�B�2B�$B�7B�]B�;B�vB�B�B�B�B�B��B�B�?B�B�2B��B"B*BE�BJ#BLdBO�BZ�B\Be�Bd�Bd�Ba-B`BdBe�Bf2BbNBH�B-�B/B%B"�B#�B�B
rB �B72B1vB.�B(�B�B#TB$B)�B4TB.�B3MB.�BCB�tB��B�GBЗB�BרB�EB��B��Bb�Bh
Bx�BZQBZ�B;�B�BsB/�B*�B�B�BoB
�B
��B
�mB
�gB
�B
��B
d@B
R B
9	B
 OB	�B	ĜB	��B	�nB	��B	�$B	�\B	��B	��B	p�B	P�B	VB	VSB	RTB	3B	B��B	 �B��B	-B�}B�UB�B�vB�B�VB�NB��B��B�B�"B�>B��B��B�`B��B�3B��BуB�-B�'B��B�dB�4B�B�$B��B�0B��B		B	�B�B��B��B�SG�O�G�O�BHB��B��B��B�xB��B��B� B�(B�YB}qBu?Bm)Bf�B_!BIRBT{BgmBfLBdtB]�Bk�Bl�Bi�BdBV�BY1B[#BV9BJ�BP.BPbBD�BP�B<6B6`B<�BP.BPHBX�B^�BZ�BS�BU�BO�BD�B?�BN"BT�BZ�BUBI�BSB^�B_BW�BZQBf�Bg�BbNBg�BkBo B��Bw2BqABq�B}�B��B|�Bv�BoiBnIBgRBqvBu�B~�B��B�TB�B��B��B�B��B��B�BðB��BɠB��B�B��BҽB�BԕB�B�?B��B�B�eB�B��B�eB�B��B�B�/B�B�)B��B�=B��B�!B�|B��B��B�NBܒB�WB��B��B�B��B��BݲB�B�7B��B��B�B�%B��B��B	�B	�B	MB	�B�DB�B	�B	�B�B	�B	�B	�B	�B	B	B	/B	 BB	'B	'B	'RB	+QB	/iB	4nB	4�B	1�B	6�B	:�B	>�B	C�B	H�B	MB	R B	R:B	[	B	_;B	^jB	^jB	^�B	^�B	a|B	b�B	_�B	cB	d�B	l�B	v�B	xB	y	B	}�B	� B	}VB	x�B	z^B	~]B	�3B	�mB	��B	�uB	�KB	�fB	��B	��B	�PB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�&B	�4B	�B	�,B	�FB	�B	�'B	��B	��B	�vB	�TB	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�1B	��B	�B	�B	� B	�@B	�TB	�FB	�9B	�SB	�YB	�)B	�BB	�HB	�pB	ބB	ܬB	ݲB	�B	�B	�tB	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�B	��B	��B	�B	�+B	�9B	��B	��B	�0B	�6B	�XB	�DB	�JB	�^B	�B
;B
AB
MB
;B
UB
UB	�cB
UB
;B	�wB	��B	�BB
 OB
UB
3B

=B
	7B
�B
tB
	RB
	lB

rB
^B
xB
~B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
�B
B
B
!B
�B
!B
#�B
# B
"B
$B
$B
$B
!�B
5B
!�B
#�B
!B
B
5B
/B
!�B
!B
#B
"�B
$B
$B
"�B
"B
#B
$B
%B
# B
%B
&B
$&B
$&B
'B
&B
$@B
&2B
)*B
)B
)*B
'8B
'8B
)*B
+6B
*0B
*B
*0B
(>B
'8B
(>B
)DB
*KB
*KB
*KB
-B
./B
./B
-CB
,WB
+6B
+QB
+QB
+QB
+kB
-]B
./B
.IB
.IB
+�B
,qB
/iB
0UB
2GB
1vB
1[B
2GB
0�B
2aB
4nB
3�B
1vB
4nB
5tB
4�B
7fB
8lB
8RB
8lB
7�B
7fB
7�B
7fB
7�B
6zB
6�B
8�B
8�B
8�B
:xB
9�B
9�B
8�B
8�B
:xB
:xB
;B
;B
:xB
9�B
8�B
8�B
;B
<�B
;�B
;B
;�B
:�B
:�B
:�B
8�B
:�B
;�B
;�B
<�B
;�B
<�B
<�B
=�B
?�B
?�B
?�B
?�B
>�B
@�B
?�B
?�B
>�B
>�B
?�B
@�B
?�B
?�B
>�B
?�B
>�B
<�B
>�B
@�B
@�B
>�B
C�B
C�B
F�B
H�B
G�B
G�B
G�B
F�B
F�B
E�B
D�B
D�B
F�B
F�B
G�B
IB
I�B
H�B
H�B
H�B
G�B
G�B
H�B
H�B
J�B
KB
L�B
MB
KB
K�B
N�B
N�B
N�B
N�B
OB
NB
N�B
N�B
N�B
NB
MB
J=B
MB
PB
O(B
OB
OBB
OBB
QB
SB
SB
SB
SB
R B
QB
PB
R B
Q4B
TB
S&B
S&B
UB
T,B
T,B
U2B
U2B
UB
T,B
TB
U2B
TFB
R:B
V9B
V9B
UMB
W?B
W?B
VSB
VSB
XEB
YKB
XEB
Y1B
Z7B
YKB
YKB
YKB
YKB
YKB
YKB
ZQB
ZQB
ZQB
ZQB
[=B
\CB
\CB
[=B
[WB
ZkB
ZQB
[WB
\]B
\]B
\]B
^OB
]dB
\]B
\xB
]dB
^jB
^OB
^jB
^jB
]~B
^jB
_pB
`\B
`vB
_pB
_pB
`vB
`\B
_pB
_pB
`vB
`vB
`\B
bhB
cnB
cnB
cTB
b�B
bhB
a|B
a�B
a�B
bhB
dZB
c�B
b�B
c�B
dtB
d�B
d�B
d�B
d�B
dtB
c�B
d�B
c�B
e�B
f�B
g�B
gmB
g�B
gmB
h�B
g�B
g�B
g�B
e�B
d�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
g�B
i�B
jB
j�B
i�B
i�B
i�B
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
j�B
j�B
j�B
k�B
l�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
m�B
n�B
n�B
m�B
l�B
l�B
m�B
m�B
o�B
o�B
n�B
m�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
p�B
p�B
q�B
p�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
r�B
q�B
p�B
r�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
tB
t�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
w�B
xB
xB
xB
xB
xB
w�B
xB
x�B
w�B
y	B
y�B
y�B
y�B
y	B
y�B
z�B
z�B
y�B
zB
{B
{�B
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803010034252018030100342520180301003425201806221326482018062213264820180622132648201804050730352018040507303520180405073035  JA  ARFMdecpA19c                                                                20180226093527  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180226003747  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180226003749  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180226003750  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180226003751  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180226003751  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180226003751  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180226003751  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180226003751  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180226003751  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180226003751  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180226003751                      G�O�G�O�G�O�                JA  ARUP                                                                        20180226005657                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180226154617  CV  JULD            G�O�G�O�F�w�                JM  ARCAJMQC2.0                                                                 20180228153425  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180228153425  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223035  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042648  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                