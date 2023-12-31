CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-07-24T00:41:24Z creation;2017-07-24T00:41:34Z conversion to V3.1;2019-12-19T08:01:48Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20170724004124  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_142                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @����
 1   @����@3�"h	ԕ�d�a@N�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM�CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�RCg��Ci��Ck��Cm��Co��Cq�RCs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP��DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��qD��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A��A�{A�{A�{A�{A��A�{A�{A��A��A�{A�oA�VAٶFA�^5A԰!A�z�A�l�A�XA�^5A�VA�K�A�+A�
=AîAº^A��A�M�A��A��/A�|�A�
=A�bNA�VA��hA�t�A��A�-A���A�(�A�=qA�VA��A���A���A�~�A��-A�A�9XA�{A�bNA���A��PA�x�A�JA��A�ĜA�oA�v�A�&�A�|�A��A��A���A�l�A��A���A��jA��A�`BA�%A�9XA�&�A��A��\A���A�5?A�XA��;A�(�A��#A�O�A��A��wA��+A��9A�z�A�O�A�p�A�VA��!A��A�O�A�$�A�1A��A�&�A�33A�jA��#A�A�bNA33A}�TA|-Az�Aw�FAu�#At�+As�wAs`BAs"�Ar�\Aq�;AqXAp�!Ao�#An{Am%Al~�Ak�PAj�HAi��Ai�Ah^5Af�\AdVAb�/A`�A\�A["�AYoAV�RAS�AP5?AM�AL�ALVAK�7AJȴAI��AG�;AE��AD(�AC;dAB�+AAt�A@�+A?�A=�PA<�/A;�A:z�A9��A9��A9t�A8r�A6 �A5`BA3�A2��A1��A17LA0{A.��A-��A-oA,M�A+�A+�7A*�uA)��A(��A'ƨA&�A&�\A&�A%t�A$�\A"v�A!|�A!S�A!"�A �AhsAK�AAr�A�wAVA�wAffAp�A��A��Al�AK�AG�A�Az�A��A`BA�A��A�mA�AdZA�yA"�A�jA?}AQ�A33A��A$�A��A�AȴAl�@���@�$�@�1'@�C�@��@�/@���@���@�{@�@�5?@��/@���@��@�K�@�!@���@�@�Ĝ@�1'@�5?@�j@㝲@�E�@��@�(�@�{@��#@��/@���@�ƨ@���@ָR@�&�@�
=@�9X@�;d@�v�@�hs@��@�`B@�b@�
=@�ȴ@Ɵ�@�M�@���@��`@î@�ȴ@��T@�`B@��@��/@�j@�ƨ@���@�=q@�hs@��/@�Ĝ@�z�@�(�@��
@��@�\)@��@�/@��@� �@���@�;d@��y@���@�{@���@���@�33@���@�E�@�hs@��/@�1@��@�
=@��R@�ff@�E�@�$�@��T@��7@�p�@�O�@�&�@��@��`@�I�@�b@��w@�l�@�dZ@�dZ@�\)@�\)@�S�@�K�@�
=@���@�-@��7@�`B@�?}@��@���@��D@�I�@� �@���@�\)@��@��@���@�~�@���@��^@��h@�hs@�G�@��@��@��9@�I�@���@�ƨ@�S�@�
=@���@��@�ȴ@���@�n�@���@��T@���@��h@�x�@�hs@�X@�/@��@��`@�Ĝ@��9@���@��D@�j@�A�@� �@��@�ƨ@��@��@�|�@��@��R@��\@�~�@�v�@�M�@��@��7@�7L@�&�@�&�@�/@��@��`@���@�j@���@��@��@�t�@�33@���@��y@�~�@�E�@�5?@�-@�{@���@��^@�`B@��@���@���@�z�@�(�@��@�o@��@��\@�v�@�M�@�5?@�-@�@���@�7L@�z�@�1'@���@��F@�dZ@�+@��H@�ff@�J@���@�`B@��@��j@���@���@��u@�9X@��m@���@�o@��H@�ff@�{@��@��#@���@�@���@�X@�V@��`@�Ĝ@��@�r�@�Z@�A�@�b@��;@��@�+@���@�~�@�n�@�M�@�@���@��-@�x�@�%@���@��@�z�@�r�@�I�@��m@���@��P@�|�@�dZ@���@���@���@���@��\@�~�@�~�@�^5@�M�@�=q@�J@�@��-@��h@�G�@��@���@��/@��9@���@�z�@�Q�@�b@��@�|�@�dZ@�K�@�"�@��@���@��+@�V@��@�@���@�p�@�`B@�&�@��@��/@��j@���@�r�@�I�@�9X@��@��@��@\)@~��@~{@}�-@|��@|��@|Z@|1@{ƨ@{S�@z�!@y��@yx�@yG�@y%@x�`@x��@x�u@x �@wK�@v�R@vff@u�h@u�@t�/@tj@t�@s�m@s�
@s��@sC�@s@r�H@r�!@r^5@r�@q�^@qG�@p�u@p �@o�@oK�@nv�@m�T@mp�@l�@l9X@k��@ko@j��@j��@j~�@jn�@jn�@j-@ix�@i&�@h�`@h��@hbN@hb@g��@g|�@f��@f5?@e��@e�h@ep�@e`B@e/@d�j@d(�@c��@c��@cdZ@b��@b~�@b-@a��@aG�@a&�@`�`@`�@_��@_+@^ȴ@^�+@^5?@]��@\�@\�D@\I�@\�@[ƨ@[C�@Z�@Z��@Z��@Z~�@Z�@YX@X�9@XA�@W�w@WK�@V��@V��@Vv�@Vff@VE�@V{@U�@T�@T�/@T��@T�D@TI�@Sƨ@S�@SS�@R�H@R��@Rn�@R=q@R�@Q�#@Q��@Q�@P�9@P��@PQ�@Pb@O�@O�w@O�P@O\)@OK�@O;d@O;d@O
=@Nff@M�@M�-@M�@MO�@MV@L�/@L�j@L�D@L(�@K��@K�m@Kƨ@K�@Kt�@KS�@K33@J��@J~�@Jn�@J^5@J=q@I��@I�#@I��@I7L@H�9@Hr�@HA�@Hb@G�@G�w@G|�@G
=@F�R@F��@F�+@F�+@F�+@Fv�@F5?@E�T@E@E�-@E�-@E�h@E`B@E�@D�j@DZ@D(�@C�m@Cƨ@C�@C33@B��@Bn�@B^5@B-@B�@B�@B�@A��@A�@A�@A��@A�^@A��@A7L@@��@@�9@@�@@�@@Q�@@1'@@ �@@ �@@b@?�@?�w@?K�@>��@>�@>�R@>�+@>ff@>5?@=�T@=�@=/@<��@<I�@;�
@;��@;t�@;"�@:��@:n�@9��@9��@9��@8��@8r�@8A�@8b@7��@7l�@7�@6�+@6ff@6$�@5�T@5��@5/@4��@3�F@3t�@3dZ@3C�@3@2��@2=q@2�@1��@1x�@1X@17L@0�`@0�9@0�@0A�@/��@/|�@/K�@/
=@.�R@.E�@-��@-�h@-V@,�j@,�D@,Z@,�@+��@+"�@*��@*^5@*-@)�@)��@)X@)%@(�`@(�9@(bN@(b@'�;@'�w@'\)@'�@'�@&ȴ@&��@&�+@&V@&5?@&{@%�T@%�-@%p�@%/@$�/@$�@$�D@$�@#�
@#��@#�@#C�@#@"��@"�\@"=q@"J@!�#@!��@!��@!��@!��@!�^@!x�@!&�@!%@ ��@ r�@ b@�;@��@�w@�w@��@l�@��@�R@V@5?@$�@�@@�@/@�@�@��@�@�D@9X@�m@t�@33@o@@�@��@~�@n�@=q@�@�@J@�^@X@G�@G�@&�@�@%@%@%@��@��@�9@�u@A�@ �@ �@b@�@|�@\)@K�@;d@��@�R@��@��@��@��@��@v�@V@$�@�T@��@@�h@`B@�@V@��@��@�/@�D@I�@9X@9X@9X@9X@��@��@S�@C�@@��@�\@~�@M�@=q@-@-@-@-@-@�@��@hs@G�@%@�`@Ĝ@�9@�u@r�@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A��A�{A�{A�{A�{A��A�{A�{A��A��A�{A�oA�VAٶFA�^5A԰!A�z�A�l�A�XA�^5A�VA�K�A�+A�
=AîAº^A��A�M�A��A��/A�|�A�
=A�bNA�VA��hA�t�A��A�-A���A�(�A�=qA�VA��A���A���A�~�A��-A�A�9XA�{A�bNA���A��PA�x�A�JA��A�ĜA�oA�v�A�&�A�|�A��A��A���A�l�A��A���A��jA��A�`BA�%A�9XA�&�A��A��\A���A�5?A�XA��;A�(�A��#A�O�A��A��wA��+A��9A�z�A�O�A�p�A�VA��!A��A�O�A�$�A�1A��A�&�A�33A�jA��#A�A�bNA33A}�TA|-Az�Aw�FAu�#At�+As�wAs`BAs"�Ar�\Aq�;AqXAp�!Ao�#An{Am%Al~�Ak�PAj�HAi��Ai�Ah^5Af�\AdVAb�/A`�A\�A["�AYoAV�RAS�AP5?AM�AL�ALVAK�7AJȴAI��AG�;AE��AD(�AC;dAB�+AAt�A@�+A?�A=�PA<�/A;�A:z�A9��A9��A9t�A8r�A6 �A5`BA3�A2��A1��A17LA0{A.��A-��A-oA,M�A+�A+�7A*�uA)��A(��A'ƨA&�A&�\A&�A%t�A$�\A"v�A!|�A!S�A!"�A �AhsAK�AAr�A�wAVA�wAffAp�A��A��Al�AK�AG�A�Az�A��A`BA�A��A�mA�AdZA�yA"�A�jA?}AQ�A33A��A$�A��A�AȴAl�@���@�$�@�1'@�C�@��@�/@���@���@�{@�@�5?@��/@���@��@�K�@�!@���@�@�Ĝ@�1'@�5?@�j@㝲@�E�@��@�(�@�{@��#@��/@���@�ƨ@���@ָR@�&�@�
=@�9X@�;d@�v�@�hs@��@�`B@�b@�
=@�ȴ@Ɵ�@�M�@���@��`@î@�ȴ@��T@�`B@��@��/@�j@�ƨ@���@�=q@�hs@��/@�Ĝ@�z�@�(�@��
@��@�\)@��@�/@��@� �@���@�;d@��y@���@�{@���@���@�33@���@�E�@�hs@��/@�1@��@�
=@��R@�ff@�E�@�$�@��T@��7@�p�@�O�@�&�@��@��`@�I�@�b@��w@�l�@�dZ@�dZ@�\)@�\)@�S�@�K�@�
=@���@�-@��7@�`B@�?}@��@���@��D@�I�@� �@���@�\)@��@��@���@�~�@���@��^@��h@�hs@�G�@��@��@��9@�I�@���@�ƨ@�S�@�
=@���@��@�ȴ@���@�n�@���@��T@���@��h@�x�@�hs@�X@�/@��@��`@�Ĝ@��9@���@��D@�j@�A�@� �@��@�ƨ@��@��@�|�@��@��R@��\@�~�@�v�@�M�@��@��7@�7L@�&�@�&�@�/@��@��`@���@�j@���@��@��@�t�@�33@���@��y@�~�@�E�@�5?@�-@�{@���@��^@�`B@��@���@���@�z�@�(�@��@�o@��@��\@�v�@�M�@�5?@�-@�@���@�7L@�z�@�1'@���@��F@�dZ@�+@��H@�ff@�J@���@�`B@��@��j@���@���@��u@�9X@��m@���@�o@��H@�ff@�{@��@��#@���@�@���@�X@�V@��`@�Ĝ@��@�r�@�Z@�A�@�b@��;@��@�+@���@�~�@�n�@�M�@�@���@��-@�x�@�%@���@��@�z�@�r�@�I�@��m@���@��P@�|�@�dZ@���@���@���@���@��\@�~�@�~�@�^5@�M�@�=q@�J@�@��-@��h@�G�@��@���@��/@��9@���@�z�@�Q�@�b@��@�|�@�dZ@�K�@�"�@��@���@��+@�V@��@�@���@�p�@�`B@�&�@��@��/@��j@���@�r�@�I�@�9X@��@��@��@\)@~��@~{@}�-@|��@|��@|Z@|1@{ƨ@{S�@z�!@y��@yx�@yG�@y%@x�`@x��@x�u@x �@wK�@v�R@vff@u�h@u�@t�/@tj@t�@s�m@s�
@s��@sC�@s@r�H@r�!@r^5@r�@q�^@qG�@p�u@p �@o�@oK�@nv�@m�T@mp�@l�@l9X@k��@ko@j��@j��@j~�@jn�@jn�@j-@ix�@i&�@h�`@h��@hbN@hb@g��@g|�@f��@f5?@e��@e�h@ep�@e`B@e/@d�j@d(�@c��@c��@cdZ@b��@b~�@b-@a��@aG�@a&�@`�`@`�@_��@_+@^ȴ@^�+@^5?@]��@\�@\�D@\I�@\�@[ƨ@[C�@Z�@Z��@Z��@Z~�@Z�@YX@X�9@XA�@W�w@WK�@V��@V��@Vv�@Vff@VE�@V{@U�@T�@T�/@T��@T�D@TI�@Sƨ@S�@SS�@R�H@R��@Rn�@R=q@R�@Q�#@Q��@Q�@P�9@P��@PQ�@Pb@O�@O�w@O�P@O\)@OK�@O;d@O;d@O
=@Nff@M�@M�-@M�@MO�@MV@L�/@L�j@L�D@L(�@K��@K�m@Kƨ@K�@Kt�@KS�@K33@J��@J~�@Jn�@J^5@J=q@I��@I�#@I��@I7L@H�9@Hr�@HA�@Hb@G�@G�w@G|�@G
=@F�R@F��@F�+@F�+@F�+@Fv�@F5?@E�T@E@E�-@E�-@E�h@E`B@E�@D�j@DZ@D(�@C�m@Cƨ@C�@C33@B��@Bn�@B^5@B-@B�@B�@B�@A��@A�@A�@A��@A�^@A��@A7L@@��@@�9@@�@@�@@Q�@@1'@@ �@@ �@@b@?�@?�w@?K�@>��@>�@>�R@>�+@>ff@>5?@=�T@=�@=/@<��@<I�@;�
@;��@;t�@;"�@:��@:n�@9��@9��@9��@8��@8r�@8A�@8b@7��@7l�@7�@6�+@6ff@6$�@5�T@5��@5/@4��@3�F@3t�@3dZ@3C�@3@2��@2=q@2�@1��@1x�@1X@17L@0�`@0�9@0�@0A�@/��@/|�@/K�@/
=@.�R@.E�@-��@-�h@-V@,�j@,�D@,Z@,�@+��@+"�@*��@*^5@*-@)�@)��@)X@)%@(�`@(�9@(bN@(b@'�;@'�w@'\)@'�@'�@&ȴ@&��@&�+@&V@&5?@&{@%�T@%�-@%p�@%/@$�/@$�@$�D@$�@#�
@#��@#�@#C�@#@"��@"�\@"=q@"J@!�#@!��@!��@!��@!��@!�^@!x�@!&�@!%@ ��@ r�@ b@�;@��@�w@�w@��@l�@��@�R@V@5?@$�@�@@�@/@�@�@��@�@�D@9X@�m@t�@33@o@@�@��@~�@n�@=q@�@�@J@�^@X@G�@G�@&�@�@%@%@%@��@��@�9@�u@A�@ �@ �@b@�@|�@\)@K�@;d@��@�R@��@��@��@��@��@v�@V@$�@�T@��@@�h@`B@�@V@��@��@�/@�D@I�@9X@9X@9X@9X@��@��@S�@C�@@��@�\@~�@M�@=q@-@-@-@-@-@�@��@hs@G�@%@�`@Ĝ@�9@�u@r�@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B!�B �B �B �B �B �B �B �B �B �B �B �B �B �B#�B)�B;dB9XB%�BJB�B)�BC�Bv�B�B�B�B�7B�1B�%B�hB��B��B�hB�hB�VB��B��B��B��B��B�wB��B��BŢBǮBǮB�^B�XB�3B��B�uB�{B�JB�\B�{B�oB��B��B��B�{B�=B�Bk�Bt�BdZBP�BA�B:^B5?B+BuB��B�`B�NB�yB�TB�HB�jBy�BP�B33B�BB
��B
�B
�ZB
��B
�^B
�B
�B
�B
�qB
ǮB
ƨB
ŢB
ÖB
�wB
�'B
��B
��B
�oB
�+B
� B
v�B
n�B
_;B
YB
L�B
E�B
A�B
>wB
<jB
9XB
49B
0!B
+B
&�B
�B
{B
bB
DB
%B
  B	��B	��B	�sB	��B	ÖB	�B	�JB	v�B	e`B	P�B	49B	#�B	�B	�B	�B	�B	oB	JB	B��B�B�B�sB�`B�HB�;B�B��B��B��BɺBǮBƨBĜB�wB�jB�^B�RB�RB�^B�RB�?B�!B�B�B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�hB�bB�\B�JB�7B�7B�7B�=B�7B�1B�B~�B}�B{�B{�B{�B~�B�{B��B��B��B�B�B��B��B��B��B��B�VB�B�B� B~�B~�B{�Bx�Bw�Br�Bl�Bk�BjBk�Bl�Bl�Bl�Bk�BiyBhsBhsBgmBiyBjBo�Bw�B� B� B�B�DB�PB�PB�VB�\B�VB�PB�PB�B�7B�oB��B��B��B��B��B��B�uB�oB�{B�hB�\B�JB�DB�DB�DB�JB�PB�oB��B��B��B��B��B��B��B��B�B�9B�RB�^B�jB�wB��BȴB��B��B��B��B��B��B�B�B�B�#B�5B�BB�`B�B�B�B��B��B��B	B	B	%B	1B	PB	hB	{B	�B	�B	�B	�B	�B	�B	'�B	+B	.B	2-B	2-B	33B	33B	33B	49B	49B	6FB	9XB	=qB	C�B	D�B	E�B	F�B	J�B	L�B	N�B	O�B	R�B	XB	\)B	_;B	`BB	cTB	gmB	jB	k�B	l�B	n�B	o�B	p�B	r�B	v�B	y�B	z�B	� B	�B	�B	�B	�+B	�7B	�DB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�9B	�RB	�^B	�dB	�dB	�dB	�jB	�qB	�wB	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
DB
DB
DB
DB
DB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
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
6FB
6FB
7LB
7LB
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
:^B
;dB
;dB
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
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
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
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
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
O�B
O�B
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
S�B
S�B
S�B
S�B
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
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
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
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B!�B �B �B �B �B �B �B �B �B �B �B!B!bB"�B(XB1�BCGB>BB+�B.B!bB-�BG�Bx�B�MB�B��B��B��B��B�oB��B��B��B�uB��B��B��B��B�B��B�B�BB�oB�zB�BɆB�B�JB��B��B�gB�$B�pB�HB��B�@B�EB��B�hB�9B�~B�BoBxBhsBT�BC�B<PB7B-�BgB��B�fB�B�B�B�2B�aB}�BT{B6�B5B�B
��B
�B
�B
�2B
�PB
�OB
�=B
��B
��B
�B
�+B
�tB
żB
��B
�B
��B
��B
�FB
�7B
��B
y>B
qAB
bB
[=B
NpB
F�B
B'B
?B
="B
:DB
5%B
1'B
,WB
(�B
�B
gB
�B
dB
zB
;B	�dB	�B	�B	�gB	��B	�[B	��B	y�B	h�B	UMB	7�B	&�B	B	�B	�B	�B	FB	�B	�B��B�B��B��B��B�B�|B�KB՛B�NB̘B�rBȚB�fB�EB��B�BB�B�rB��B�B�B��B�'B�B��B��B�KB�
B�fB�nB�B�7B�B��B�$B��B��B��B�B��B��B��B�DB��B�rB�xB�)B��B��B�OB~�B|�B|6B|B.B�B��B��B��B��B��B�=B��B�B�bB��B�bB��B��B��B�B�B|�By�By�Bt�Bm�Bl�BkkBlqBm]BmCBmwBl�Bj�Bi�Bi�Bh�Bj�Bk�Bp;Bx8B��B��B��B��B��B�"B�\B�bB�\B�(B��B�B��B�@B�EB�=B��B�dB�dB�SB�aB��B�B��B�bB�B��B��B��B��B�<B�uB�QB�]B�)B�B�'B�ZB��B��B��B��B��B��B��B��B��B��B�pBѝB�B�oB҉B�aB�mB�yBچB��B��B�-B�fB�B� B�AB�?B��B�qB	�B	�B	tB	�B	�B	�B	�B	�B	�B	�B	�B		B	 BB	(>B	+kB	.cB	2GB	2GB	3hB	3hB	3MB	4nB	4�B	6�B	9�B	=�B	C�B	D�B	E�B	GB	KB	MB	O(B	PHB	S[B	X_B	\xB	_�B	`�B	c�B	g�B	j�B	k�B	l�B	n�B	o�B	p�B	sB	wB	z*B	{JB	�OB	�GB	�MB	�mB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�&B	�B	�B	�B	�8B	�XB	�QB	�CB	�/B	�IB	�cB	�oB	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�0B	�B	�"B	�(B	�.B	�NB	ӏB	�sB	�_B	�eB	�WB	�WB	�]B	�dB	�~B	ޞB	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�B	�B	�	B	�	B	��B	�B	�*B	�0B	�"B	�"B	�(B	�BB	�.B	�.B
 OB
 4B
oB
uB
{B
mB
SB
mB
YB
zB
_B
zB
�B
	�B
xB
xB
xB
xB
�B
�B
jB
jB
�B
�B
vB
�B
vB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
B
�B
B
�B
�B
B
 'B
 'B
 �B
 �B
!�B
!�B
!�B
!�B
"B
#:B
$&B
$&B
$&B
%B
%�B
&2B
&B
%�B
%�B
&B
'B
'B
'B
'B
'B
'B
(>B
(>B
(>B
)*B
)*B
)_B
*eB
*KB
+QB
+QB
,qB
,WB
,WB
,=B
-CB
-)B
-)B
-CB
-CB
-CB
.IB
.IB
.IB
.IB
.IB
/iB
/5B
/iB
/�B
0UB
0UB
0UB
0UB
0UB
0oB
1vB
1AB
1[B
1vB
2|B
2|B
3hB
3hB
4nB
4nB
4nB
4�B
5�B
5�B
6zB
6zB
6�B
6�B
7�B
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;B
;B
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
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IB
J	B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
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
K�B
LB
K�B
L�B
MB
MB
MB
MB
L�B
MB
NB
N"B
N"B
OB
OB
OB
OB
O(B
O�B
P.B
PB
PB
P.B
Q4B
QB
QB
Q4B
R B
R B
R:B
S&B
S&B
S&B
TB
TFB
T,B
TaB
V9B
V9B
V9B
VB
V9B
VB
W?B
W?B
WYB
XEB
XEB
XEB
XEB
XEB
XEB
X_B
Z7B
ZQB
ZQB
Z7B
ZkB
[qB
[WB
[qB
[=B
\]B
\]B
\]B
\xB
]~B
^jB
^jB
^OB
^OB
^jB
_pB
_pB
_pB
_pB
`vB
`\B
`vB
a|B
a|B
a|B
abB
a|B
b�B
b�B
b�B
b�B
b�B
bhB
bhB
c�B
c�B
c�B
c�B
d�B
d�B
dtB
d�B
d�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
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
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707280036372017072800363720170728003637201806221316412018062213164120180622131641201804050718452018040507184520180405071845  JA  ARFMdecpA19c                                                                20170724093524  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170724004124  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170724004127  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170724004129  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170724004131  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170724004131  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170724004133  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170724004133  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170724004134  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170724004134                      G�O�G�O�G�O�                JA  ARUP                                                                        20170724052307                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170724153638  CV  JULD            G�O�G�O�F�ǯ                JM  ARCAJMQC2.0                                                                 20170727153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170727153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221845  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041641  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                