CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-29T00:35:25Z creation;2016-07-29T00:35:28Z conversion to V3.1;2019-12-19T08:34:26Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160729003525  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_021                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׾�%*� 1   @׾�β@�@<1���.�dn�x���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D���D�<�D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{@n{@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\)A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B��
B���B���B���B���B���Bã�Bǣ�B��
Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5�C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{DfnDf�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�7
D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��
D�7
D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=qD��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A͸RA͝�Ả7A�z�A�S�A�5?A�&�A��A�oA�%A���A��yA���A�ĜAʲ-Aʡ�A�$�A���A���A�(�A��A���A�G�A�(�A���A���A��HA�(�A�=qA��;A�;dA�%A� �A��mA��jA��DA�z�A��mA��A�bA�5?A�1'A�dZA���A��A���A�&�A���A��PA��A��wA�ffA�9XA��hA��;A�Q�A�ĜA���A�E�A���A���A�\)A�$�A���A��#A�oA�ffA���A�v�A���A�S�A���A�C�A�r�A���A�
=A���A�?}A�`BA���A��uA��;A�1'A�E�A�
A~�jA}XA{?}Ay��Ax�Aw+Av�Au�As�
As33Ar1AqC�Ap��An��Am�Am��Al~�Ai��Ag��Af�HAfJAeAd��AchsAb��Aa�#Aa�A`��A`I�A^�yA]�TA\ĜA[AY�AYK�AX�HAXJAV��AV(�AUhsAU%ATM�AS7LAR�uARM�AQ��AP=qAN1AMx�AM"�AL��AJ��AJ�AH�`AG&�AE"�AD$�AB�/AA`BA@$�A>VA=K�A=oA<�yA<�RA;�A:�A9��A9�hA8�A7�A6�\A5�-A5�A4��A4jA4JA2��A1�A0^5A/x�A.�RA.A�A-��A,�9A,=qA,1A+�TA+�A)�wA(�!A(1'A'�A';dA&ZA%%A$��A$ȴA$��A$��A$(�A#
=A"VA!�;A!G�A   A��AO�AM�AdZA��A�TA~�A(�A�A;dA�A��An�AE�Al�A�hAv�A��A��AdZA&�A�/A�+A�
AVA
�\A	�A	A	l�A	XA	7LA	"�A�A�AO�AK�A��AbA�AO�A�;A�A E�@���@��m@��R@���@�hs@�ƨ@��@�-@��;@�t�@�ȴ@���@�bN@���@�@�Ĝ@�A�@��@�7L@��@�F@�33@�M�@�@�z�@�
=@�S�@��@؋D@�dZ@�
=@֏\@�@��@�ƨ@�$�@�&�@Гu@υ@θR@��T@��/@�Q�@˅@�n�@�{@�bN@�n�@ũ�@�%@ě�@�r�@� �@öF@�@�hs@�ȴ@��@��9@��9@��@�z�@�j@�Q�@���@�l�@���@��+@�@�z�@���@�G�@� �@�dZ@��@�ȴ@��\@�5?@��h@��@���@��@�^5@�p�@�Ĝ@�9X@�  @�l�@�"�@�ȴ@�v�@�{@��^@�x�@��@�A�@���@�\)@�
=@���@�ȴ@�^5@��^@�&�@�I�@��
@��P@�dZ@�C�@�
=@��@���@��!@�ff@�p�@��@�(�@��w@�+@���@�^5@��-@�Ĝ@�I�@�(�@��m@���@�S�@��\@�@�hs@��@�1@�K�@���@�5?@���@�7L@��@���@��D@���@�dZ@�$�@�p�@�`B@�?}@��/@���@�A�@�b@��m@�l�@��!@�{@�hs@�/@�&�@�&�@���@��@�z�@�I�@��@�\)@�o@�ȴ@��+@�~�@�v�@�^5@��@�@�p�@�?}@��@�%@���@���@��@�1'@��F@��H@�ȴ@��!@�V@��@��-@�p�@�/@���@�9X@��@��;@��
@�ƨ@��w@��F@��@��F@��@�t�@�33@�
=@�ȴ@���@�^5@�E�@�$�@��@��T@�X@�7L@�&�@���@�1'@�w@�@+@~��@~{@}�@}/@|��@|�/@|z�@{��@{t�@{33@z-@y��@y�7@y7L@xA�@w�@wK�@vȴ@v�+@u@u��@u��@u��@v@u�@u�h@u?}@t��@t��@tj@t9X@s�
@sC�@so@so@r�@r��@r��@r��@r��@r��@r�\@rM�@q�^@qX@qX@qX@qG�@p��@p��@p�9@p��@p�u@o��@ol�@o��@o|�@o\)@o;d@n�y@n��@pbN@p1'@p�`@qhs@q��@q��@r��@s"�@r�@r�H@r�@r��@r��@r=q@q%@n�@nff@nff@n��@n�y@oK�@o|�@n��@n5?@kƨ@i7L@hbN@j�@j-@i��@i��@i7L@h�9@fV@d��@d�/@dj@dj@d��@e@dj@c��@c"�@c@b�@c"�@d1@dj@d��@eO�@e`B@ep�@e�h@e?}@e�@e/@e?}@e/@eV@d��@d�j@dI�@c��@c��@cC�@c33@c"�@b�H@b��@b~�@a�@a�^@a��@aX@a�@`Ĝ@`A�@_�w@_\)@^�@^��@^v�@]��@]?}@\��@\I�@\1@\1@[�F@Z��@ZM�@Y7L@X1'@W�;@W�w@W|�@WK�@V��@Vv�@VV@Vv�@V$�@U�@U��@U�-@U��@U��@T�D@S��@S@R�@So@S@R��@Rn�@Q�#@QG�@P�@P1'@Pb@Pb@Pb@Pb@Pb@P  @O�@O��@O;d@N��@M��@M�@L��@L�D@LI�@L9X@L1@K�
@K�F@Kt�@J�@J~�@I��@Ihs@H��@H�9@H�@HA�@H  @G�;@G�@G��@Gl�@F�y@F�@F�@F��@F�+@FV@FE�@F5?@E�@E��@E��@E�h@E�h@EO�@D��@D��@D9X@Cƨ@C�F@C�@CC�@B�H@B��@Bn�@B�@BJ@BJ@A�@A�^@A��@A�7@AX@A�@@��@@Ĝ@@�u@@�@@bN@@Q�@@A�@@b@?�;@?�w@?��@?|�@?l�@?�@>�+@>@=@=p�@=/@<�D@<1@;��@;33@:��@:M�@:J@9��@9hs@97L@8Ĝ@8r�@81'@7�;@7|�@6�@6�R@6�R@6��@6V@6@5��@5�h@5�@4�/@4Z@4Z@4I�@4(�@41@3�F@3�@3C�@2�@2��@2��@2^5@2M�@2=q@2-@2�@1��@1��@0�9@/�;@/��@/�@/�@/�P@/�P@/�P@/�P@/�P@/K�@/�@/�@/
=@.��@.��@.��@.��@.�y@.ȴ@.��@.ff@.$�@-�@-@-�@-`B@-O�@-?}@-/@,�/@,�D@,z�@,j@,I�@+�
@+dZ@*��@*~�@*^5@*^5@*M�@*-@*-@*-@*J@)�@)�#@)�^@)�^@)�^@)�^@)�^@)x�@(��@(�9@(�u@(b@'��@'�w@'�@'K�@&�@&��@&v�@&ff@&@%@%�-@%�h@%`B@%?}@$�@$�/@$�j@$z�@$I�@$I�@$(�@$�@#�m@#��@#@"~�@!�^@!&�@ �9@ �@ bN@ A�@   @�@�P@l�@\)@+@�@�@
=@�@�+@@p�@`B@O�@/@�@�/@��@z�@9X@(�@�@�@�m@��@@-@�7@X@G�@7L@&�@�u@r�@Q�@Q�@A�@ �@  @�@�;@��@�@|�@�@�@�R@��@v�@$�@$�@{@@�T@p�@�/@9X@ƨ@��@t�@t�@C�@o@@�@��@��@~�@�@��@G�@%@�u@bN@Q�@A�@1'@�;@\)@
=@�@ȴ@��@V@$�@@��@�-@�h@p�@O�@/@�@V@��@��@��@�@�/@��@��@�j@�j@�j@�j@�@��@�D@9X@1@�m@�
@�
@�
@ƨ@ƨ@�
@��@dZ@"�@o@o@o@
�@
��@
��@
�!@
^5@
�@	�^@	G�@	&�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A͸RA͝�Ả7A�z�A�S�A�5?A�&�A��A�oA�%A���A��yA���A�ĜAʲ-Aʡ�A�$�A���A���A�(�A��A���A�G�A�(�A���A���A��HA�(�A�=qA��;A�;dA�%A� �A��mA��jA��DA�z�A��mA��A�bA�5?A�1'A�dZA���A��A���A�&�A���A��PA��A��wA�ffA�9XA��hA��;A�Q�A�ĜA���A�E�A���A���A�\)A�$�A���A��#A�oA�ffA���A�v�A���A�S�A���A�C�A�r�A���A�
=A���A�?}A�`BA���A��uA��;A�1'A�E�A�
A~�jA}XA{?}Ay��Ax�Aw+Av�Au�As�
As33Ar1AqC�Ap��An��Am�Am��Al~�Ai��Ag��Af�HAfJAeAd��AchsAb��Aa�#Aa�A`��A`I�A^�yA]�TA\ĜA[AY�AYK�AX�HAXJAV��AV(�AUhsAU%ATM�AS7LAR�uARM�AQ��AP=qAN1AMx�AM"�AL��AJ��AJ�AH�`AG&�AE"�AD$�AB�/AA`BA@$�A>VA=K�A=oA<�yA<�RA;�A:�A9��A9�hA8�A7�A6�\A5�-A5�A4��A4jA4JA2��A1�A0^5A/x�A.�RA.A�A-��A,�9A,=qA,1A+�TA+�A)�wA(�!A(1'A'�A';dA&ZA%%A$��A$ȴA$��A$��A$(�A#
=A"VA!�;A!G�A   A��AO�AM�AdZA��A�TA~�A(�A�A;dA�A��An�AE�Al�A�hAv�A��A��AdZA&�A�/A�+A�
AVA
�\A	�A	A	l�A	XA	7LA	"�A�A�AO�AK�A��AbA�AO�A�;A�A E�@���@��m@��R@���@�hs@�ƨ@��@�-@��;@�t�@�ȴ@���@�bN@���@�@�Ĝ@�A�@��@�7L@��@�F@�33@�M�@�@�z�@�
=@�S�@��@؋D@�dZ@�
=@֏\@�@��@�ƨ@�$�@�&�@Гu@υ@θR@��T@��/@�Q�@˅@�n�@�{@�bN@�n�@ũ�@�%@ě�@�r�@� �@öF@�@�hs@�ȴ@��@��9@��9@��@�z�@�j@�Q�@���@�l�@���@��+@�@�z�@���@�G�@� �@�dZ@��@�ȴ@��\@�5?@��h@��@���@��@�^5@�p�@�Ĝ@�9X@�  @�l�@�"�@�ȴ@�v�@�{@��^@�x�@��@�A�@���@�\)@�
=@���@�ȴ@�^5@��^@�&�@�I�@��
@��P@�dZ@�C�@�
=@��@���@��!@�ff@�p�@��@�(�@��w@�+@���@�^5@��-@�Ĝ@�I�@�(�@��m@���@�S�@��\@�@�hs@��@�1@�K�@���@�5?@���@�7L@��@���@��D@���@�dZ@�$�@�p�@�`B@�?}@��/@���@�A�@�b@��m@�l�@��!@�{@�hs@�/@�&�@�&�@���@��@�z�@�I�@��@�\)@�o@�ȴ@��+@�~�@�v�@�^5@��@�@�p�@�?}@��@�%@���@���@��@�1'@��F@��H@�ȴ@��!@�V@��@��-@�p�@�/@���@�9X@��@��;@��
@�ƨ@��w@��F@��@��F@��@�t�@�33@�
=@�ȴ@���@�^5@�E�@�$�@��@��T@�X@�7L@�&�@���@�1'@�w@�@+@~��@~{@}�@}/@|��@|�/@|z�@{��@{t�@{33@z-@y��@y�7@y7L@xA�@w�@wK�@vȴ@v�+@u@u��@u��@u��@v@u�@u�h@u?}@t��@t��@tj@t9X@s�
@sC�@so@so@r�@r��@r��@r��@r��@r��@r�\@rM�@q�^@qX@qX@qX@qG�@p��@p��@p�9@p��@p�u@o��@ol�@o��@o|�@o\)@o;d@n�y@n��@pbN@p1'@p�`@qhs@q��@q��@r��@s"�@r�@r�H@r�@r��@r��@r=q@q%@n�@nff@nff@n��@n�y@oK�@o|�@n��@n5?@kƨ@i7L@hbN@j�@j-@i��@i��@i7L@h�9@fV@d��@d�/@dj@dj@d��@e@dj@c��@c"�@c@b�@c"�@d1@dj@d��@eO�@e`B@ep�@e�h@e?}@e�@e/@e?}@e/@eV@d��@d�j@dI�@c��@c��@cC�@c33@c"�@b�H@b��@b~�@a�@a�^@a��@aX@a�@`Ĝ@`A�@_�w@_\)@^�@^��@^v�@]��@]?}@\��@\I�@\1@\1@[�F@Z��@ZM�@Y7L@X1'@W�;@W�w@W|�@WK�@V��@Vv�@VV@Vv�@V$�@U�@U��@U�-@U��@U��@T�D@S��@S@R�@So@S@R��@Rn�@Q�#@QG�@P�@P1'@Pb@Pb@Pb@Pb@Pb@P  @O�@O��@O;d@N��@M��@M�@L��@L�D@LI�@L9X@L1@K�
@K�F@Kt�@J�@J~�@I��@Ihs@H��@H�9@H�@HA�@H  @G�;@G�@G��@Gl�@F�y@F�@F�@F��@F�+@FV@FE�@F5?@E�@E��@E��@E�h@E�h@EO�@D��@D��@D9X@Cƨ@C�F@C�@CC�@B�H@B��@Bn�@B�@BJ@BJ@A�@A�^@A��@A�7@AX@A�@@��@@Ĝ@@�u@@�@@bN@@Q�@@A�@@b@?�;@?�w@?��@?|�@?l�@?�@>�+@>@=@=p�@=/@<�D@<1@;��@;33@:��@:M�@:J@9��@9hs@97L@8Ĝ@8r�@81'@7�;@7|�@6�@6�R@6�R@6��@6V@6@5��@5�h@5�@4�/@4Z@4Z@4I�@4(�@41@3�F@3�@3C�@2�@2��@2��@2^5@2M�@2=q@2-@2�@1��@1��@0�9@/�;@/��@/�@/�@/�P@/�P@/�P@/�P@/�P@/K�@/�@/�@/
=@.��@.��@.��@.��@.�y@.ȴ@.��@.ff@.$�@-�@-@-�@-`B@-O�@-?}@-/@,�/@,�D@,z�@,j@,I�@+�
@+dZ@*��@*~�@*^5@*^5@*M�@*-@*-@*-@*J@)�@)�#@)�^@)�^@)�^@)�^@)�^@)x�@(��@(�9@(�u@(b@'��@'�w@'�@'K�@&�@&��@&v�@&ff@&@%@%�-@%�h@%`B@%?}@$�@$�/@$�j@$z�@$I�@$I�@$(�@$�@#�m@#��@#@"~�@!�^@!&�@ �9@ �@ bN@ A�@   @�@�P@l�@\)@+@�@�@
=@�@�+@@p�@`B@O�@/@�@�/@��@z�@9X@(�@�@�@�m@��@@-@�7@X@G�@7L@&�@�u@r�@Q�@Q�@A�@ �@  @�@�;@��@�@|�@�@�@�R@��@v�@$�@$�@{@@�T@p�@�/@9X@ƨ@��@t�@t�@C�@o@@�@��@��@~�@�@��@G�@%@�u@bN@Q�@A�@1'@�;@\)@
=@�@ȴ@��@V@$�@@��@�-@�h@p�@O�@/@�@V@��@��@��@�@�/@��@��@�j@�j@�j@�j@�@��@�D@9X@1@�m@�
@�
@�
@ƨ@ƨ@�
@��@dZ@"�@o@o@o@
�@
��@
��@
�!@
^5@
�@	�^@	G�@	&�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B"�B$�B#�B#�B#�B#�B"�B#�B#�B$�B%�B%�B%�B%�B0!BYBk�BL�B-B�B�B��B�B�B�sB�HB�#B�)B�BB�B��B��B��B�wB�?B��B��B�%Bv�BXBH�BB�B/B�BJB��B�B�mB�BB�B�B�B��BÖB�^B�3B�B��B��B��B��B��B�7Bz�Bn�Be`B\)BQ�BH�BF�B<jB0!B#�B�BbB	7B
��B
�B
�B
�BB
��B
ÖB
�!B
��B
��B
�+B
{�B
n�B
dZB
aHB
W
B
J�B
E�B
=qB
6FB
0!B
#�B
�B
�B
bB	��B	�B	�fB	�NB	�B	�
B	��B	��B	ɺB	ŢB	��B	��B	�XB	�9B	�B	��B	��B	��B	�oB	�\B	�%B	�B	}�B	{�B	x�B	t�B	q�B	o�B	l�B	gmB	\)B	YB	W
B	T�B	J�B	E�B	>wB	2-B	"�B	�B	�B	VB	DB	B��B��B��B��B��B�B�B�B�B�sB�ZB�TB�NB�;B�5B�)B�B��B��BȴBŢBÖB��B�wB�dB�^B�XB�LB�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�DB�%B�B�B}�B|�Bv�Bu�Bt�Bs�Bq�Bp�Bp�Bn�Bl�Bk�B`BB]/B[#BZBYBXBVBS�BQ�BM�BG�BG�BF�BG�BF�BF�BF�BF�BE�BJ�BK�BH�BC�B@�BB�B@�BB�B=qB7LB49B49B:^B;dB:^B33B+B-B33B1'B0!B-B+B(�B'�B'�B&�B%�B$�B#�B#�B"�B!�B �B"�B!�B"�B$�B#�B#�B#�B$�B$�B&�B%�B%�B&�B&�B'�B'�B'�B(�B(�B(�B,B-B-B.B/B/B/B0!B1'B1'B7LB9XB9XB9XB9XB9XB9XB:^B:^B;dB<jB<jB=qB>wBB�BE�BG�BI�BI�BJ�BJ�BK�BL�BM�BO�BO�BP�BR�BR�BS�BT�BVBVBW
BXBYBYBZB[#B]/B^5B`BBaHBaHBaHBcTBe`BffBjBk�Bl�Bm�Bm�Bn�Bn�Bn�Bo�Bp�Bs�Bv�Bx�By�B{�B}�B~�B�B�B�%B�+B�1B�1B�7B�JB�PB�\B�uB�{B��B��B��B��B��B��B�B�B�B�B�3B�RB�RB�RB�dB�qB�}B��B��BBĜBƨB��B��B��B��B��B��B�B�B�;B�HB�NB�TB�TB�TB�TB�TB�ZB�`B�fB�mB�yB�B�B�B�B��B��B	  B	B	B	B	B	B	%B	1B	DB	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	%�B	'�B	+B	,B	-B	0!B	7LB	8RB	8RB	:^B	>wB	A�B	E�B	G�B	H�B	H�B	H�B	H�B	H�B	H�B	L�B	Q�B	Q�B	S�B	T�B	T�B	T�B	W
B	YB	\)B	_;B	aHB	aHB	cTB	dZB	e`B	gmB	k�B	n�B	p�B	q�B	r�B	t�B	v�B	w�B	z�B	|�B	}�B	}�B	}�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	�B	�%B	�%B	�%B	�%B	�1B	�DB	�DB	�JB	�PB	�PB	�VB	�VB	�\B	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�FB	�RB	�RB	�RB	�XB	�FB	�?B	�jB	�}B	��B	��B	��B	�}B	�qB	�dB	�jB	�wB	�}B	B	ɺB	ɺB	ǮB	ǮB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
1B

=B

=B

=B

=B

=B

=B

=B
DB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
,B
,B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
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
9XB
:^B
;dB
;dB
<jB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
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
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
L�B
L�B
L�B
M�B
M�B
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
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
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
VB
VB
VB
VB
VB
VB
W
B
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
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
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
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
cTB
cTB
cTB
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
ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B$�B%FB$B$B$B$B#B$B$B%,B&2B&�B'�B+6B:�Bd&Bv`BT�B4�B�B'8BUB�}B�cB�KB� B�xB�B�B��B��B��BϑB�UB�lB�4B�yB�rBz�BZ7BJ�BE9B1�B!�B�B�B�B�XB�B��BںBچB�6B�B�6B��B��B��B��B��B�!B�yB�B|�BpBf�B]�BR�BI�BH�B>]B2B%FB�B�B
�B
��B
��B
�CB
�4B
�[B
�tB
��B
��B
��B
�B
}�B
o�B
e�B
c B
XyB
K�B
GB
>wB
7�B
2-B
$�B
�B
�B
�B	�B	��B	�B	�B	�	B	�yB	��B	��B	ʦB	�YB	�uB	�AB	��B	��B	��B	��B	��B	�sB	��B	��B	�+B	�B	~�B	}B	z*B	u�B	rGB	p�B	n�B	i�B	\�B	Y�B	XEB	W$B	K�B	G�B	@�B	4nB	$tB	�B	�B	.B	PB	SB�wB�cB��B�(B�JB�B�cB�B�iB��B�B�&B��B��B�;B��B��B�uB�B��BƎBĜB��B�B��B�B��B�	B��B�B��B��B�kB�_B�@B�B�B�NB��B�-B��B��B��B�9B�4B�6B��B�mB�B�B~wBwfBv`Bu�BtTBrBqABqvBpoBoiBn�Ba-B]�B[�BZ�BY�BX�BW?BU�BT�BN�BH1BH1BGBG�BG+BGzBG�BGzBFBK�BMBJrBFBB[BC{BB�BDMB>�B88B4�B5B;�B<�B<�B4�B+�B-�B49B2-B1[B.B+�B)�B)*B)*B'�B&LB%zB$�B$�B#�B#:B#:B$@B"hB#�B%`B$tB$�B$�B%�B&B'�B&�B&�B'�B'�B(�B(�B(�B)�B)�B*eB-]B-�B-�B.}B/�B/�B/�B1'B2aB2�B8lB9�B9�B9�B9�B9�B9�B:�B:�B;�B=B=VB>�B?�BC�BFtBH1BJ#BJ#BKBKDBL~BM�BN�BP}BP�BQ�BS�BS[BTaBU�BVmBVmBWsBXyBYBYeBZ�B[�B]~B^�B`�Ba|Ba�Ba�Bc�Be�BgBj�Bk�Bl�Bm�Bm�Bn�Bn�Bo Bp!BqvBtTBwLByXBz^B|jB~wB�B��B��B�tB��B��B��B��B��B��B�B��B�2B�+B�=B�5B�-B�&B�DB��B��B��B��B��B��B��B��B��B��B��B��B�'B�GB�9B�EB��B�B�B�6B�TB�2B�yBںBߤB�B�B�B�B�B�B�B��B��B�B�B�B��B��B��B�'B�+B�xB	 4B	;B	oB	{B	SB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	"B	$&B	&2B	&2B	($B	+6B	,"B	-wB	0�B	7fB	8�B	8�B	:�B	>�B	A�B	E�B	G�B	H�B	IB	IB	H�B	H�B	IB	MB	R B	R:B	TFB	U2B	U2B	UMB	WsB	YeB	\xB	_pB	a|B	a�B	c�B	dtB	ezB	g�B	k�B	n�B	p�B	q�B	r�B	t�B	v�B	xB	{0B	}"B	~B	~(B	~(B	~(B	B	B	.B	.B	HB	HB	�GB	�?B	�YB	�YB	�?B	�fB	�xB	�xB	�~B	��B	��B	�VB	�pB	��B	��B	��B	�bB	�@B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�*B	�=B	�qB	��B	��B	�QB	�5B	�3B	�B	�?B	�zB	��B	��B	�XB	�*B	�zB	�B	��B	��B	��B	��B	��B	�OB	��B	��B	��B	��B	�}B	B	�=B	�#B	��B	��B	��B	ǮB	ȚB	��B	��B	��B	�B	�B	�B	�9B	�$B	�$B	�+B	�KB	�QB	�QB	�WB	�qB	�CB	�dB	�OB	�pB	�pB	�pB	�vB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�6B	�PB	�]B	�HB	��B	�B
 B
 B
;B
UB
uB
gB
SB
fB

=B

XB

XB

XB

rB

rB

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#B
#B
#B
#B
#B
#B
$B
$B
$B
$B
$B
$�B
%B
$�B
%B
&B
&B
%�B
&B
&2B
'8B
($B
($B
)*B
)DB
*KB
+QB
,WB
,=B
-]B
./B
.IB
.IB
/OB
/OB
/iB
0UB
0;B
1[B
1vB
2|B
3MB
33B
3hB
3MB
3hB
4nB
4nB
4�B
5tB
5tB
6`B
6zB
6zB
6zB
6`B
6zB
7�B
7�B
7�B
7�B
8�B
8RB
8lB
8lB
8�B
8�B
8�B
9�B
:�B
;B
;�B
<�B
;�B
<�B
<�B
<jB
<�B
<�B
<�B
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
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
LB
MB
M6B
MB
NB
NB
NB
NB
NB
OB
OB
N�B
N�B
OB
O�B
O�B
O�B
PB
P.B
P.B
Q4B
RB
RB
R B
R B
R B
R B
R B
S&B
R�B
SB
R�B
SB
S&B
S[B
TaB
UMB
V9B
VB
VB
V9B
VSB
V9B
V9B
VB
VB
V9B
V9B
VB
VB
VB
V9B
W$B
VSB
W?B
W$B
W?B
W?B
W?B
W$B
XB
XEB
XEB
YeB
ZQB
ZkB
[WB
[=B
\]B
\CB
\]B
\]B
\CB
\CB
\]B
\]B
\]B
\]B
]dB
]~B
^jB
^jB
^jB
^OB
^OB
^jB
^�B
_�B
_pB
_pB
_pB
_pB
`vB
`vB
`vB
`vB
`vB
a|B
a|B
abB
a|B
abB
abB
aHB
abB
bhB
bhB
bhB
bhB
bhB
bNB
bhB
bhB
bhB
bhB
bhB
b�B
b�B
c�B
c�B
cnB
cnB
cnB
cnB
cnB
cnB
c�B
c�B
c�B
dtB
dtB
dtB
d�B
d�B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%|�<0�|<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608020034182016080200341820160802003418201806221211422018062212114220180622121142201804050404072018040504040720180405040407  JA  ARFMdecpA19c                                                                20160729093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160729003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160729003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160729003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160729003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160729003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160729003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160729003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160729003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160729003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20160729012047                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160729153529  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160729153529  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160729153529  CV  LATITUDE        G�O�G�O�A�                JM  ARGQJMQC2.0                                                                 20160729153529  CV  LONGITUDE       G�O�G�O��#w
                JM  ARCAJMQC2.0                                                                 20160801153418  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160801153418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190407  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031142  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                