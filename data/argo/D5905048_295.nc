CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-26T00:35:41Z creation;2018-10-26T00:35:47Z conversion to V3.1;2019-12-19T07:25:40Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181026003541  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              'A   JA  I2_0577_295                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؋��m 1   @؋� @4[���A�dd�J�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  BffB   B'33B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@�=q@�=qA�A=�A]�A}�A��\A�\)A��\A��\AΏ\Aޏ\A�\A��\BG�BG�B�BG�B&z�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B��
B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=qD�z=D��=D��=D�:=D�z=D��qD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�=qD�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=qD��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�"�A��A��A��A��A��A� �A��A��A��A��A�{A�oA�oA�{A�{A�{A��mA�O�A���A��A�-A�O�A�bNAuA�A��A�ZA���A��
A��HA�E�A���A���A�E�A���A�C�A�A�5?A�VA�1'A�|�A�O�A��
A�l�A�VA�M�A�"�A��;A�33A��A�"�A�-A��uA�r�A�;dA��A��mA���A��jA��TA��A�;dA���A���A�"�A� �A�JA��HA���A��A��^A��
A��^A�5?A�p�A�dZA��yA�ZA���A�dZA�oA���A~$�A|��A{l�Ax�+Aw�TAw33Avz�AsO�Ar{Ap$�AoK�AmO�Al��Ak�Aj��Aj�Ai�#AhE�Af�DAd�A`r�A_��A_oA\�AZAX�AW%ATZARbAP{AO�hAO�ANffAL��AKC�AI�AI+AG��AFffAD��AC�mACAB{AA&�A@n�A?��A?�A=�A<�DA<�A;\)A:��A9��A8v�A7t�A6ZA3��A0�/A0bA/G�A-�A*v�A'ƨA&1A$�jA$�A#p�A"��A!��A 1'A��AjAAĜAI�A�^AJA��A�
A$�A�wA��A��AVA1'Ap�AM�A/A
��A
9XA
JA	ƨA	&�AĜA��A��Ar�At�AjA�A��AoA��A ��A 1@��@�I�@�"�@�5?@�-@�Q�@�ff@�hs@��`@��
@���@���@�V@�@�r�@�r�@� �@�t�@�p�@�1'@���@�9@��@�o@�{@ܓu@�\)@ڧ�@��@�X@�Q�@���@թ�@ԓu@��@��#@�hs@�b@�"�@ҧ�@���@�~�@��/@��;@�n�@�7L@�r�@���@�C�@Ǖ�@��
@�b@���@���@�1@�I�@���@�=q@ʗ�@�bN@��@���@�K�@��@�9X@ȴ9@��/@Ȭ@�Z@�1@ǝ�@��y@�^5@�hs@���@î@�n�@�7L@�?}@�x�@��@�j@�33@�O�@�Z@�dZ@�=q@�@���@��j@���@�Z@���@��@���@�n�@�n�@���@��7@��@�O�@�%@�j@��F@���@�v�@�@��@���@��7@�7L@�I�@�+@���@���@�=q@��@�A�@��!@��
@�l�@�C�@�33@�+@��@��@���@���@��R@���@�5?@���@��@�@�`B@��`@���@�r�@�9X@�1@��F@�t�@�+@�o@���@�v�@�5?@���@�p�@�%@��j@���@��D@��@�Q�@�(�@�b@���@�\)@���@��\@�5?@�-@��@�@��T@���@�x�@�/@���@���@�1'@��@�ƨ@��@�+@��H@���@��+@�5?@���@��h@�G�@�V@���@��@��9@��u@�Q�@�(�@��@�\)@��@��H@��R@�v�@�M�@�5?@��#@���@��-@��7@�/@���@���@�9X@���@�S�@��@��R@���@��R@�^5@�$�@�J@��@��#@��h@�?}@���@���@��u@�(�@��;@�|�@�o@��!@�^5@�=q@��@���@���@���@�X@�%@���@�Z@�1'@��;@�\)@�@�ȴ@���@�v�@�=q@�{@��@�@�hs@���@�z�@��@�A�@��u@��@�r�@�bN@�1'@��m@�t�@�l�@�;d@�@��!@�n�@�{@���@���@�x�@��@��/@��9@���@��u@�j@���@�t�@�+@�
=@���@�M�@�@���@�x�@��@�Ĝ@�z�@�b@�@�@~ff@}�@}�@}p�@}�@}�@}p�@}`B@}�@|��@|z�@{��@{�F@{@z�!@z�@y�^@y�7@y&�@x�9@xA�@x �@w�@w�@wK�@v�y@v��@vv�@v$�@u��@t�/@t�D@s�m@sdZ@rn�@rJ@q��@q��@q�@q��@q��@q�@q��@q�^@q��@qX@p��@p��@pb@o�@o�P@o
=@n�+@n{@m�h@m�@l��@lz�@l(�@l1@k�m@kt�@ko@j��@j^5@j-@j�@i�@i&�@hĜ@h��@hQ�@g�@gl�@g|�@gl�@g�@fȴ@f��@fv�@fE�@e�T@e�h@e?}@d��@d�j@dz�@d�@d��@dj@d�@b�@b�@a��@a&�@`��@`A�@_�;@_�P@_;d@_\)@_;d@^�R@^v�@^5?@^@]p�@\�/@\�D@\�D@\I�@[��@[33@Zn�@ZJ@Y�7@XĜ@X �@W�@W|�@W+@V��@VE�@V{@U�@U�-@Up�@U/@T��@T��@Tj@T9X@T(�@T�@S��@R-@Q��@Q��@Q�^@Q��@Qx�@QX@Q�@P�`@PĜ@Pr�@PQ�@P  @O�w@O��@OK�@O+@N�y@N�@Nȴ@NV@M@Lj@K�
@K��@K�@KdZ@K33@J��@J�!@J�!@J��@J�\@J�\@J^5@I��@Ihs@I7L@I&�@H�`@H�@HA�@G�@G��@G�@G|�@G\)@G+@G
=@G
=@F��@F�R@F��@F�+@Fff@FE�@F{@F@E�T@E`B@D��@Dj@DI�@DI�@D(�@D�@C�m@C�@Ct�@Co@B��@B~�@B=q@A�@A�^@A��@A�7@Ax�@AX@A7L@@��@@bN@@A�@@b@?��@>�y@>5?@>@=�@=�-@=��@=`B@=?}@<��@<��@<�@<�/@<��@<��@<1@;�m@;��@;C�@;"�@;@:�H@:�!@:^5@:�@9�^@9hs@97L@8��@8Ĝ@8��@8�@8Q�@8  @7��@7�@7K�@7
=@6ȴ@6�R@6v�@65?@6{@6@6@5��@5�h@5�@5p�@5`B@5/@4�/@4�@3�m@3��@3o@2�H@2�!@2^5@2=q@2-@2J@2J@1��@1�@1��@17L@0��@0Ĝ@0�9@0��@0bN@01'@0b@0  @/�;@/�@/l�@/K�@/;d@/+@.ȴ@.v�@.V@.@-��@-�@-`B@-/@,�@,�j@,�D@,9X@+��@+ƨ@+"�@*�@*�H@*��@*�\@*M�@*�@*J@)�#@)�^@)�7@)X@)7L@(�`@(��@(Q�@'�@'�P@'\)@'�@&�y@&�R@&v�@&@%��@%�h@%O�@$��@$�@$��@$�D@$�D@$�D@$z�@$I�@$(�@$�@#�m@#�
@#�@#dZ@#S�@#33@"�H@"�!@"��@"n�@"�@!�^@!x�@!�@ ��@ �`@ �u@ bN@ b@�;@�;@�w@|�@;d@
=@�y@�y@�@�R@��@ff@ff@E�@@��@@�h@p�@O�@/@�@��@�@�/@�@�D@j@9X@�m@�@S�@@�H@�!@��@��@�\@~�@^5@M�@=q@�@�^@G�@7L@�`@�u@ �@�w@K�@�y@�y@�@��@��@�+@ff@5?@{@@�@�@�T@��@@��@/@��@�@Z@��@ƨ@��@��@S�@33@@�!@�!@��@��@��@~�@n�@^5@-@��@�#@��@�^@��@hs@&�@%@��@�`@Ĝ@�9@��@�u@r�@Q�@A�@b@  @�;@|�@+@
=@ȴ@��@��@E�@$�@@�T@��@@@��@�@p�@�@�@p�@V@�@��@��@�D@z�@Z@9X@�@1@�m@�
@ƨ@�F@�F@��@�@dZ@C�@33@33@o@"�1111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�"�A��A��A��A��A��A� �A��A��A��A��A�{A�oA�oA�{A�{A�{A��mA�O�A���A��A�-A�O�A�bNAuA�A��A�ZA���A��
A��HA�E�A���A���A�E�A���A�C�A�A�5?A�VA�1'A�|�A�O�A��
A�l�A�VA�M�A�"�A��;A�33A��A�"�A�-A��uA�r�A�;dA��A��mA���A��jA��TA��A�;dA���A���A�"�A� �G�O�G�O�A���A��A��^A��
A��^A�5?A�p�A�dZA��yA�ZA���A�dZA�oA���A~$�A|��A{l�Ax�+Aw�TAw33Avz�AsO�Ar{Ap$�AoK�AmO�Al��Ak�Aj��Aj�Ai�#AhE�Af�DAd�A`r�A_��A_oA\�AZAX�AW%ATZARbAP{AO�hAO�ANffAL��AKC�AI�AI+AG��AFffAD��AC�mACAB{AA&�A@n�A?��A?�A=�A<�DA<�A;\)A:��A9��A8v�A7t�A6ZA3��A0�/A0bA/G�A-�A*v�A'ƨA&1A$�jA$�A#p�A"��A!��A 1'A��AjAAĜAI�A�^AJA��A�
A$�A�wA��A��AVA1'Ap�AM�A/A
��A
9XA
JA	ƨA	&�AĜA��A��Ar�At�AjA�A��AoA��A ��A 1@��@�I�@�"�@�5?@�-@�Q�@�ff@�hs@��`@��
@���@���@�V@�@�r�@�r�@� �@�t�@�p�@�1'@���@�9@��@�o@�{@ܓu@�\)@ڧ�@��@�X@�Q�@���@թ�@ԓu@��@��#@�hs@�b@�"�@ҧ�@���@�~�@��/@��;@�n�@�7L@�r�@���@�C�@Ǖ�@��
@�b@���@���@�1@�I�@���@�=q@ʗ�@�bN@��@���@�K�@��@�9X@ȴ9@��/@Ȭ@�Z@�1@ǝ�@��y@�^5@�hs@���@î@�n�@�7L@�?}@�x�@��@�j@�33@�O�@�Z@�dZ@�=q@�@���@��j@���@�Z@���@��@���@�n�@�n�@���@��7@��@�O�@�%@�j@��F@���@�v�@�@��@���@��7@�7L@�I�@�+@���@���@�=q@��@�A�@��!@��
@�l�@�C�@�33@�+@��@��@���@���@��R@���@�5?@���@��@�@�`B@��`@���@�r�@�9X@�1@��F@�t�@�+@�o@���@�v�@�5?@���@�p�@�%@��j@���@��D@��@�Q�@�(�@�b@���@�\)@���@��\@�5?@�-@��@�@��T@���@�x�@�/@���@���@�1'@��@�ƨ@��@�+@��H@���@��+@�5?@���@��h@�G�@�V@���@��@��9@��u@�Q�@�(�@��@�\)@��@��H@��R@�v�@�M�@�5?@��#@���@��-@��7@�/@���@���@�9X@���@�S�@��@��R@���@��R@�^5@�$�@�J@��@��#@��h@�?}@���@���@��u@�(�@��;@�|�@�o@��!@�^5@�=q@��@���@���@���@�X@�%@���@�Z@�1'@��;@�\)@�@�ȴ@���@�v�@�=q@�{@��@�@�hs@���@�z�@��@�A�@��u@��@�r�@�bN@�1'@��m@�t�@�l�@�;d@�@��!@�n�@�{@���@���@�x�@��@��/@��9@���@��u@�j@���@�t�@�+@�
=@���@�M�@�@���@�x�@��@�Ĝ@�z�@�b@�@�@~ff@}�@}�@}p�@}�@}�@}p�@}`B@}�@|��@|z�@{��@{�F@{@z�!@z�@y�^@y�7@y&�@x�9@xA�@x �@w�@w�@wK�@v�y@v��@vv�@v$�@u��@t�/@t�D@s�m@sdZ@rn�@rJ@q��@q��@q�@q��@q��@q�@q��@q�^@q��@qX@p��@p��@pb@o�@o�P@o
=@n�+@n{@m�h@m�@l��@lz�@l(�@l1@k�m@kt�@ko@j��@j^5@j-@j�@i�@i&�@hĜ@h��@hQ�@g�@gl�@g|�@gl�@g�@fȴ@f��@fv�@fE�@e�T@e�h@e?}@d��@d�j@dz�@d�@d��@dj@d�@b�@b�@a��@a&�@`��@`A�@_�;@_�P@_;d@_\)@_;d@^�R@^v�@^5?@^@]p�@\�/@\�D@\�D@\I�@[��@[33@Zn�@ZJ@Y�7@XĜ@X �@W�@W|�@W+@V��@VE�@V{@U�@U�-@Up�@U/@T��@T��@Tj@T9X@T(�@T�@S��@R-@Q��@Q��@Q�^@Q��@Qx�@QX@Q�@P�`@PĜ@Pr�@PQ�@P  @O�w@O��@OK�@O+@N�y@N�@Nȴ@NV@M@Lj@K�
@K��@K�@KdZ@K33@J��@J�!@J�!@J��@J�\@J�\@J^5@I��@Ihs@I7L@I&�@H�`@H�@HA�@G�@G��@G�@G|�@G\)@G+@G
=@G
=@F��@F�R@F��@F�+@Fff@FE�@F{@F@E�T@E`B@D��@Dj@DI�@DI�@D(�@D�@C�m@C�@Ct�@Co@B��@B~�@B=q@A�@A�^@A��@A�7@Ax�@AX@A7L@@��@@bN@@A�@@b@?��@>�y@>5?@>@=�@=�-@=��@=`B@=?}@<��@<��@<�@<�/@<��@<��@<1@;�m@;��@;C�@;"�@;@:�H@:�!@:^5@:�@9�^@9hs@97L@8��@8Ĝ@8��@8�@8Q�@8  @7��@7�@7K�@7
=@6ȴ@6�R@6v�@65?@6{@6@6@5��@5�h@5�@5p�@5`B@5/@4�/@4�@3�m@3��@3o@2�H@2�!@2^5@2=q@2-@2J@2J@1��@1�@1��@17L@0��@0Ĝ@0�9@0��@0bN@01'@0b@0  @/�;@/�@/l�@/K�@/;d@/+@.ȴ@.v�@.V@.@-��@-�@-`B@-/@,�@,�j@,�D@,9X@+��@+ƨ@+"�@*�@*�H@*��@*�\@*M�@*�@*J@)�#@)�^@)�7@)X@)7L@(�`@(��@(Q�@'�@'�P@'\)@'�@&�y@&�R@&v�@&@%��@%�h@%O�@$��@$�@$��@$�D@$�D@$�D@$z�@$I�@$(�@$�@#�m@#�
@#�@#dZ@#S�@#33@"�H@"�!@"��@"n�@"�@!�^@!x�@!�@ ��@ �`@ �u@ bN@ b@�;@�;@�w@|�@;d@
=@�y@�y@�@�R@��@ff@ff@E�@@��@@�h@p�@O�@/@�@��@�@�/@�@�D@j@9X@�m@�@S�@@�H@�!@��@��@�\@~�@^5@M�@=q@�@�^@G�@7L@�`@�u@ �@�w@K�@�y@�y@�@��@��@�+@ff@5?@{@@�@�@�T@��@@��@/@��@�@Z@��@ƨ@��@��@S�@33@@�!@�!@��@��@��@~�@n�@^5@-@��@�#@��@�^@��@hs@&�@%@��@�`@Ĝ@�9@��@�u@r�@Q�@A�@b@  @�;@|�@+@
=@ȴ@��@��@E�@$�@@�T@��@@@��@�@p�@�@�@p�@V@�@��@��@�D@z�@Z@9X@�@1@�m@�
@ƨ@�F@�F@��@�@dZ@C�@33@33@o@"�1111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
��B
��B
��B
��B
��B!�B_;B�qB��B�NBDB	7B�B��B�sB�B�BB1BBBJB��B��BJB��B\B%B�B�fB�BȴB�B��B��B�B�B}�B}�BhsBK�B�B
��B
�mB
�mB
��B
�B
�%B
�{B
�+B
w�B
ffB
�7B
ƨB
�3B
��B
��B
��B
��B
�PB
_;B
�B
2-B
9XB
5?B
0!B
)�B
�B
B
  B
	7B	�B	��B	��B	�B	��B	��B	B	�wB	�-B	�3B	�9B	��B	��B	��B	�bB	|�B	hsB	@�B	ZB	S�B	5?B	�B	 �B	�B	1B��B	B	JB	
=B	  B�B�B�`B�B�B�B��B�
B��B��BȴB��BƨBƨB�jB�9B��B�XB�LB�B��B��B��B�VB� B��B��B}�Bq�BiyBt�B{�B�B� B~�Bl�BffBO�BbNBn�Be`BiyBdZBP�BT�BB�BR�Bp�Bo�BcTBT�BffBhsBcTBe`Bs�Bu�Bx�Bu�Bm�Bn�Be`Bm�Bm�BgmBffBn�Bk�B`BBT�BP�BO�BT�BK�B[#B]/BffB^5BYB`BBe`B^5BT�BD�BI�BjBm�Bm�BiyBdZB\)B]/B`BBYBe`BgmBe`BbNBgmBiyBp�Bu�Bm�BjBp�Bs�B� B�+B� Bv�Bw�Bw�Bk�BdZBaHBffBbNBk�Bo�Br�Bq�Bz�B� B�B�B�B�1B�JB�uB��B��B�hB��B�B�B�LB��B��B�#B�B�B�B�B�
B�
B�
B�B�
B�B�B�fB�B�B�B�fB�TB�B�B�B��B��B��B��B��B��B��B��B	  B	%B	DB	bB	uB	oB	oB	hB	uB	�B	�B	%�B	.B	.B	/B	1'B	/B	2-B	A�B	B�B	?}B	=qB	:^B	6FB	,B	B�B	H�B	K�B	L�B	L�B	L�B	M�B	O�B	O�B	N�B	N�B	T�B	\)B	\)B	]/B	`BB	e`B	gmB	jB	l�B	m�B	o�B	r�B	u�B	t�B	u�B	w�B	x�B	{�B	~�B	�B	�%B	�+B	�1B	�%B	�1B	�7B	�1B	�DB	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�?B	�RB	�dB	�dB	�^B	�jB	�dB	�qB	�jB	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	ƨB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�)B	�#B	�#B	�)B	�5B	�HB	�BB	�NB	�TB	�TB	�HB	�NB	�HB	�ZB	�`B	�`B	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
  B
  B
B
B
B	��B
B
B
B
  B
B
B
B
B
B
B
B
%B
1B

=B
DB
PB
PB
PB
JB
PB
PB
PB
PB
\B
bB
hB
uB
oB
oB
uB
�B
�B
�B
{B
{B
�B
�B
{B
uB
oB
�B
{B
�B
{B
�B
�B
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
�B
�B
�B
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
�B
 �B
 �B
!�B
 �B
�B
�B
"�B
!�B
 �B
#�B
&�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
,B
+B
)�B
(�B
&�B
&�B
(�B
)�B
,B
,B
+B
/B
.B
0!B
0!B
.B
0!B
0!B
1'B
/B
0!B
1'B
2-B
1'B
0!B
.B
/B
0!B
1'B
1'B
2-B
49B
5?B
5?B
49B
5?B
6FB
7LB
6FB
6FB
6FB
7LB
7LB
6FB
7LB
8RB
7LB
5?B
2-B
8RB
:^B
;dB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
;dB
9XB
7LB
7LB
:^B
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
?}B
?}B
>wB
=qB
>wB
?}B
@�B
@�B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
A�B
A�B
D�B
D�B
E�B
E�B
D�B
D�B
C�B
D�B
C�B
C�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
F�B
F�B
E�B
D�B
F�B
F�B
E�B
D�B
F�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
L�B
L�B
K�B
K�B
J�B
I�B
K�B
K�B
J�B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
L�B
M�B
M�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
N�B
N�B
O�B
P�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
N�B
O�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
R�B
Q�B
R�B
S�B
T�B
W
B
VB
VB
W
B
W
B
XB
XB
W
B
W
B
XB
XB
XB
VB
W
B
XB
XB
W
B
YB
YB
YB
YB
ZB
ZB
YB
ZB
ZB
YB
\)B
]/B
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
\)B
\)B
\)B
\)B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
`BB
`BB
`BB
`BB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
bNB
cTB
dZB
cTB
bNB
dZB
dZB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
e`B
e`B
e`B
ffB
gmB
gmB
ffB
ffB
gmB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
gmB
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
hsB
hsB
hsB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
jB
jB
jB
l�B
k�B
k�B
k�B
k�B
l�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
n�B
m�B
m�B
o�B
n�B
o�B
p�B
q�B
q�B
p�B
p�B
q�B
p�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
v�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
�B
�|B
��B
��B
�B-]BgRB��B��B�zBJB
�B�zB��B�B$ByBzB
�B+BB�B��B��B�B��B�B�B�B��B�B�PB��BөB��B�|B��B�AB�BkQBOB"�B �B
�B
��B
֡B
�oB
�#B
�B
��B
y�B
h>B
��G�O�G�O�B
��B
��B
��B
��B
�vB
c�B
#�B
5?B
:�B
6�B
1[B
+B
!bB
B
'B

�B	�vB	��B	��B	�B	�bB	ΥB	ĶB	��B	�TB	�B	�%B	�DB	��B	�B	��B	�B	k�B	D�B	[WB	UMB	8lB	 BB	#B	yB	xB��B	-B	B	)B	UB��B�B�8B��B�=B��B��B�+B�@B�:B�	B��B��BǮB�B��B�AB��B�RB��B��B�yB��B��B�B��B�1B�UBu?Bl�Bv�B}�B�B� B�Bn�Bh�BR�Bc�Bo�Bf�BjKBe�BSuBW$BFtBT�Bq'Bp;Bd�BWYBg�Bi�BeBgBtnBvzBy$Bv`Bn�Bo�BgBn�Bn�Bh�Bg�Bo5Bl=Ba�BV�BRoBQ4BU�BM�B[�B^Bf�B_�BZkB`�Be�B_pBW
BG�BLBj�Bm�Bm�Bi�Be,B]�B^OBabBZ�Be�Bh>BfLBc�BhXBj0Bq[BvzBn�Bk�BqvBtTB�B��B��Bw�Bx�Bx�Bm)Be�Bb�BgRBc�BlqBpUBs3Br-Bz�B� B�'B�-B�SB�KB�B�@B�!B��B��B�B�6B��B��B�)B��B�#B�eB�BچB�B��BרB��BٚB�+B��B��B�B��B��B�B�mB�B�6B�oB�B�?B�?B�*B�6B�PB�JB�PB��B	 �B	YB	�B	�B	�B	�B	�B	B	,B	?B	B	&LB	./B	.IB	/�B	1�B	/�B	3B	A�B	B�B	@B	>BB	;B	7�B	-�B	B�B	H�B	K�B	L�B	MB	MB	NB	PB	O�B	OB	OBB	UMB	\]B	\xB	]�B	`�B	e�B	g�B	j�B	l�B	m�B	o�B	r�B	u�B	u%B	v+B	x8B	y>B	|PB	HB	�AB	�YB	�EB	�fB	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�&B	�2B	�2B	�RB	�KB	�]B	�]B	�}B	��B	�nB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�#B	�0B	�B	�PB	�PB	�B	�:B	�&B	�+B	�EB	�SB	�YB	�KB	�1B	�1B	�KB	�eB	�QB	�qB	�kB	چB	�xB	یB	یB	ܒB	ބB	�|B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	� B	� B	�B	��B	��B	��B	��B	�B	�B	�*B	�$B	�>B	�B	�<B	�6B	�B	�6B	�VB	�BB
 OB
UB
oB
[B
3B
MB
MB
AB
 �B
 iB
[B
GB
[B	�}B
UB
[B
oB
 iB
oB
oB
uB
aB
gB
{B
mB
tB
KB

=B
^B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
 �B
�B
B
 B
�B
 �B
 �B
!�B
 �B
!B
 B
#B
!�B
!B
$B
'B
'B
&B
'B
'B
'B
'B
'8B
'B
'B
'B
)*B
)*B
,B
+6B
*0B
)DB
'mB
'RB
)DB
*KB
,"B
,=B
+QB
/OB
.IB
0;B
0UB
.cB
0UB
0UB
1[B
/iB
0oB
1[B
2GB
1[B
0oB
.cB
/�B
0oB
1vB
1�B
2|B
4�B
5tB
5tB
4nB
5tB
6zB
7�B
6zB
6zB
6zB
7�B
7�B
6zB
7�B
8lB
7�B
5�B
2�B
8�B
:^B
;B
:�B
:xB
:�B
:�B
:xB
:xB
:�B
;�B
:�B
:�B
;�B
;B
;�B
;�B
<�B
;�B
9�B
7�B
7�B
:�B
=�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
?�B
?�B
>�B
=�B
>�B
?�B
@�B
@�B
?�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
A�B
A�B
D�B
D�B
E�B
E�B
D�B
D�B
C�B
D�B
C�B
C�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
F�B
F�B
E�B
D�B
F�B
F�B
E�B
EB
F�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
L�B
L�B
K�B
K�B
J�B
J	B
K�B
K�B
KB
MB
MB
L�B
K�B
K�B
K�B
K�B
K�B
MB
K�B
MB
NB
NB
MB
MB
NB
NB
NB
M�B
OB
O�B
OB
OB
PB
Q B
PB
PB
PB
Q B
P�B
QB
Q B
OB
PB
N<B
PB
Q4B
R B
R B
R B
S&B
TB
T,B
TB
TB
S&B
R:B
S&B
T,B
UB
W$B
V9B
V9B
W?B
W?B
X+B
XEB
W?B
W?B
XEB
XEB
XEB
VSB
W?B
XEB
XEB
WYB
YKB
YKB
YKB
YKB
ZQB
ZQB
YKB
ZQB
ZQB
YeB
\]B
]/B
\]B
\CB
\CB
]dB
]dB
]IB
]IB
]dB
]dB
]dB
\]B
\]B
\]B
\xB
_;B
_pB
_pB
_VB
_VB
_VB
^jB
`vB
`vB
`vB
`vB
b�B
cnB
cnB
cnB
cTB
cnB
b�B
b�B
c�B
c�B
c�B
bhB
c�B
d�B
cnB
b�B
d�B
d�B
c�B
c�B
c�B
d�B
d�B
e�B
f�B
e�B
e�B
e�B
f�B
gmB
g�B
f�B
f�B
g�B
h�B
h�B
h�B
g�B
h�B
h�B
h�B
h�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
h�B
h�B
h�B
j�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
j�B
j�B
j�B
l�B
k�B
k�B
k�B
k�B
l�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
n�B
m�B
m�B
o�B
n�B
o�B
p�B
q�B
q�B
p�B
p�B
q�B
p�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
v�B
w�B
w�B
w�B
v�B
u�B
v�B
xB
w�B
w�B
w�B
xB
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
y�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�m<7�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810300037312018103000373120181030003731201810300200242018103002002420181030020024201810310029092018103100290920181031002909  JA  ARFMdecpA19c                                                                20181026093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181026003541  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181026003544  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181026003545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181026003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181026003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181026003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20181026003546  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20181026003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181026003546  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20181026003546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181026003547                      G�O�G�O�G�O�                JA  ARUP                                                                        20181026005727                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181026153825  CV  JULD            G�O�G�O�F�]�                JM  ARSQJMQC2.0                                                                 20181027000000  CF  PSAL_ADJUSTED_QCC  C  G�O�                JM  ARSQJMQC2.0                                                                 20181027000000  CF  TEMP_ADJUSTED_QCC  C  G�O�                JM  ARCAJMQC2.0                                                                 20181029153731  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181029153731  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181029170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181030152909  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                