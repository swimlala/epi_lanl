CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-05T10:01:14Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220305100114  20220305100114  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @پU�l��1   @پVO�`@<�G�{�c����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ D�|�Dּ�D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�G@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\A�\)A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#�RC%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{DznDz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��pD��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�w
Dַ
D��=D�:=D�z=D׺=D��=D�:=D�w
Dغ=D��=D�:=D�z=Dٺ=D��=D�7
D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�w
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA��A�ȴA���A��uA��A�x�A�n�A�ffA�`BA�S�A�I�A�G�A�E�A�A�A�?}A�9XA�1'A�/A�+A�(�A�(�A��A�bA�A��HA�v�A���A�9XA�=qA��A�\)A���A�G�A��mA�?}A���A��A�33A���A��PA���A��A��A�
=A�;dA�E�A�I�A�E�A�/A�1A���A�+A���A�1A��wA�5?A��;A�M�A�K�A�t�A�bA�ƨA�S�A�;dA���A��yA�`BA�{A�jA��A�XA�  A��uA�&�A��A�A��A�A���A���A��A�
=A���A���A�\)A���A��A�A��7A�+A��7A��`A�p�A��A���A���A��DA7LA}+AzQ�Aw;dAuoAsx�An�Al5?Ak
=Ajn�AjE�Ai�AiS�AhbNAe�;Aa�#A]�A\1'A[�AY�;AWt�AV �AT=qAS`BAS7LAR�yARz�AR�AQ��AP��AP�AO��AOAN=qAM��AM/AL��AL��AL��AL�uALQ�AK�#AH�jAG\)AF=qAE+AD�+AD �AC��AB��AAA@ffA?7LA>=qA>  A=��A=|�A<��A<Q�A;�;A;�wA;p�A:�+A9��A8VA7�A5K�A2��A2Q�A2�A1�A1S�A0bNA.��A-�wA-K�A-"�A,��A,n�A,5?A+�A+�7A*��A*��A)��A'�A&�A&�9A%ƨA$�RA$^5A#;dA"Q�A" �A!�hA!hsA!O�A!O�A!+A��An�A\)A�AZA9XAbA�HA��A��AJA?}Az�AE�A�TAK�A�-AO�A�RA33AA��A�!Av�AA�AS�A7LA"�A�`A��A�A
�A	dZA	"�A��A{A�-A�A�+Ax�A��AE�A$�AbAƨAp�A ��A v�A 9X@�S�@�^5@���@�A�@���@�=q@�7L@�S�@�`B@�r�@�@�33@���@���@��#@�9X@�?}@�;d@�5?@�&�@ܓu@� �@ە�@���@���@��;@׍P@��@��@��@�A�@ӥ�@��@ѩ�@�G�@�/@�%@Гu@�j@�1'@υ@��@ͩ�@��@̬@ˮ@�^5@��T@���@�A�@�t�@�G�@ģ�@�S�@�@�v�@�E�@�5?@���@�9X@��R@��u@� �@�|�@��@���@��9@��
@��@�^5@�hs@��@�C�@��!@���@��@�Z@��@���@�+@���@�n�@�5?@�@��@���@���@���@��@�~�@�5?@�{@���@�&�@�1@�K�@���@���@��+@�ff@�E�@��@��@���@��@�hs@���@���@��@�@��H@�E�@���@�V@��D@�1'@�K�@��@���@��9@���@��P@��P@��@��@�@��T@��h@�/@���@�Ĝ@�bN@�l�@���@�V@�-@�@�@��@�?}@�%@���@��j@��D@��@�1@��;@��
@��@�K�@�;d@��y@��+@�@��#@�@��^@�x�@�O�@�&�@��D@�b@��F@�S�@�33@��@��@�ff@��@���@�`B@��@��F@��P@�K�@���@�V@��@��@���@�p�@�7L@��D@���@�\)@�33@�@��R@���@���@�5?@��#@��7@�`B@�/@��@�r�@�  @���@��R@�$�@�J@���@��@��#@���@���@�@���@�p�@�G�@�&�@���@���@�bN@��@�w@\)@;d@�@~��@~ff@~5?@}�T@}�-@}?}@|��@|�D@|�@{dZ@{C�@{o@z~�@y7L@x��@x��@x  @w�P@w\)@w;d@w+@w
=@v�@vȴ@vȴ@vȴ@v��@vE�@uO�@uO�@u?}@u/@t�/@t�@s�m@sƨ@sƨ@s�@s33@rM�@q�7@o�;@ol�@o;d@o
=@n��@nv�@n$�@n{@n{@m�T@m�h@m/@l��@lZ@l1@k�m@kƨ@k��@kdZ@kC�@j�@j�!@jn�@j-@i�@i�^@ix�@h��@hA�@h �@h  @g��@g�w@g�P@g+@f�@f�R@f��@fV@e�@e�-@e`B@d��@dI�@c��@c�
@cƨ@c�F@c�@b�\@b=q@a�^@aX@a%@`�`@`�@`  @_�;@_�w@_��@_;d@_�@^�@^��@^V@^{@]@]�h@]`B@]?}@]/@]�@\��@[�
@[S�@["�@Z�H@Z��@Z�\@Z^5@Y�7@X�9@XbN@XA�@X1'@X1'@X1'@W�;@W��@V�R@V5?@V{@V@V@V@U�T@UO�@T�@TI�@T(�@T�@T�@T1@S�m@S�@S33@So@R�H@R��@Rn�@RM�@R=q@Q�@Q�^@Q�^@Q��@Q&�@P��@PbN@O�;@O�@Ol�@O+@N��@N�@N�@N��@N@MV@L�/@L�j@LZ@K��@K��@K"�@J~�@I��@I�7@IG�@I7L@I%@H��@HA�@Hb@G�;@GK�@F�R@F�+@Fv�@Fv�@Fff@F5?@F@E@E�h@E`B@E/@D��@D�@DZ@D1@C�m@C�m@C�F@C��@C33@B��@B~�@A�#@A&�@@�`@@�9@@r�@@  @?��@?�P@?\)@?;d@?
=@>��@>ff@>5?@>$�@>$�@>$�@>{@=@=/@=�@<�j@<z�@<I�@;��@;��@;S�@;"�@:��@:�\@:^5@9��@9G�@8�u@8Q�@7�@7�@7�@7�P@7\)@7K�@7
=@6�@6�R@6�R@6��@6��@6V@6{@5�T@5��@5`B@5V@4��@4�@3��@3o@2�H@2��@2n�@2J@1�@1�#@1X@1G�@17L@17L@0��@0�9@0Q�@/�w@/�P@/\)@.�@.ff@.5?@.$�@-@-��@-�@,�/@,�@,(�@+�
@+��@+dZ@+dZ@+S�@+S�@+C�@*�@*�\@*^5@)�@)��@)7L@)%@(��@(��@(�@(bN@(A�@( �@(b@(  @'�@'l�@'K�@'+@'�@&��@&�@&�R@&��@&�+@&E�@&{@%��@%�-@%p�@%`B@%`B@%`B@%`B@%O�@%O�@%/@$��@#��@#�F@#�@#o@"�!@"�@!��@!��@!hs@!G�@ �`@ �@ Q�@ b@�w@�P@|�@\)@
=@�y@�@�@�@ȴ@ȴ@ȴ@��@V@5?@{@�T@��@`B@O�@V@�@�D@z�@j@�@�m@ƨ@��@dZ@C�@C�@33@"�@@�@��@��@~�@~�@^5@J@�7@�@%@��@r�@ �@  @�@��@�w@��@l�@l�@K�@�@��@ȴ@��@��@�+@V@{@�@@��@�h@O�@/@��@�/@�j@�D@z�@I�@1@��@t�@t�@t�@S�@"�@o@�H@��@�!@��@^5@=q@-@�@J@�@�7@X@G�@�@�`@��@��@�@Q�@A�@A�@A�@1'@ �@b@ �@b@b@  @�;@�P@K�@�y@��@�+@v�@V@�@@�-@p�@O�@/@�j@I�@(�@1@�m@��@t�@"�@
�H@
��@
�!@
�\@
^5@
-@	�#@	hs@	&�@	�@	�@	%@��@��@�@Q�@�@��@l�@;d@�y@ȴ@ȴ@��@��@ff@E�@5?@�@@�@`B@/@�@��@�@�j@�j@��@�D@j@I�@1@ƨ@�F@��@��@t�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA��A�ȴA���A��uA��A�x�A�n�A�ffA�`BA�S�A�I�A�G�A�E�A�A�A�?}A�9XA�1'A�/A�+A�(�A�(�A��A�bA�A��HA�v�A���A�9XA�=qA��A�\)A���A�G�A��mA�?}A���A��A�33A���A��PA���A��A��A�
=A�;dA�E�A�I�A�E�A�/A�1A���A�+A���A�1A��wA�5?A��;A�M�A�K�A�t�A�bA�ƨA�S�A�;dA���A��yA�`BA�{A�jA��A�XA�  A��uA�&�A��A�A��A�A���A���A��A�
=A���A���A�\)A���A��A�A��7A�+A��7A��`A�p�A��A���A���A��DA7LA}+AzQ�Aw;dAuoAsx�An�Al5?Ak
=Ajn�AjE�Ai�AiS�AhbNAe�;Aa�#A]�A\1'A[�AY�;AWt�AV �AT=qAS`BAS7LAR�yARz�AR�AQ��AP��AP�AO��AOAN=qAM��AM/AL��AL��AL��AL�uALQ�AK�#AH�jAG\)AF=qAE+AD�+AD �AC��AB��AAA@ffA?7LA>=qA>  A=��A=|�A<��A<Q�A;�;A;�wA;p�A:�+A9��A8VA7�A5K�A2��A2Q�A2�A1�A1S�A0bNA.��A-�wA-K�A-"�A,��A,n�A,5?A+�A+�7A*��A*��A)��A'�A&�A&�9A%ƨA$�RA$^5A#;dA"Q�A" �A!�hA!hsA!O�A!O�A!+A��An�A\)A�AZA9XAbA�HA��A��AJA?}Az�AE�A�TAK�A�-AO�A�RA33AA��A�!Av�AA�AS�A7LA"�A�`A��A�A
�A	dZA	"�A��A{A�-A�A�+Ax�A��AE�A$�AbAƨAp�A ��A v�A 9X@�S�@�^5@���@�A�@���@�=q@�7L@�S�@�`B@�r�@�@�33@���@���@��#@�9X@�?}@�;d@�5?@�&�@ܓu@� �@ە�@���@���@��;@׍P@��@��@��@�A�@ӥ�@��@ѩ�@�G�@�/@�%@Гu@�j@�1'@υ@��@ͩ�@��@̬@ˮ@�^5@��T@���@�A�@�t�@�G�@ģ�@�S�@�@�v�@�E�@�5?@���@�9X@��R@��u@� �@�|�@��@���@��9@��
@��@�^5@�hs@��@�C�@��!@���@��@�Z@��@���@�+@���@�n�@�5?@�@��@���@���@���@��@�~�@�5?@�{@���@�&�@�1@�K�@���@���@��+@�ff@�E�@��@��@���@��@�hs@���@���@��@�@��H@�E�@���@�V@��D@�1'@�K�@��@���@��9@���@��P@��P@��@��@�@��T@��h@�/@���@�Ĝ@�bN@�l�@���@�V@�-@�@�@��@�?}@�%@���@��j@��D@��@�1@��;@��
@��@�K�@�;d@��y@��+@�@��#@�@��^@�x�@�O�@�&�@��D@�b@��F@�S�@�33@��@��@�ff@��@���@�`B@��@��F@��P@�K�@���@�V@��@��@���@�p�@�7L@��D@���@�\)@�33@�@��R@���@���@�5?@��#@��7@�`B@�/@��@�r�@�  @���@��R@�$�@�J@���@��@��#@���@���@�@���@�p�@�G�@�&�@���@���@�bN@��@�w@\)@;d@�@~��@~ff@~5?@}�T@}�-@}?}@|��@|�D@|�@{dZ@{C�@{o@z~�@y7L@x��@x��@x  @w�P@w\)@w;d@w+@w
=@v�@vȴ@vȴ@vȴ@v��@vE�@uO�@uO�@u?}@u/@t�/@t�@s�m@sƨ@sƨ@s�@s33@rM�@q�7@o�;@ol�@o;d@o
=@n��@nv�@n$�@n{@n{@m�T@m�h@m/@l��@lZ@l1@k�m@kƨ@k��@kdZ@kC�@j�@j�!@jn�@j-@i�@i�^@ix�@h��@hA�@h �@h  @g��@g�w@g�P@g+@f�@f�R@f��@fV@e�@e�-@e`B@d��@dI�@c��@c�
@cƨ@c�F@c�@b�\@b=q@a�^@aX@a%@`�`@`�@`  @_�;@_�w@_��@_;d@_�@^�@^��@^V@^{@]@]�h@]`B@]?}@]/@]�@\��@[�
@[S�@["�@Z�H@Z��@Z�\@Z^5@Y�7@X�9@XbN@XA�@X1'@X1'@X1'@W�;@W��@V�R@V5?@V{@V@V@V@U�T@UO�@T�@TI�@T(�@T�@T�@T1@S�m@S�@S33@So@R�H@R��@Rn�@RM�@R=q@Q�@Q�^@Q�^@Q��@Q&�@P��@PbN@O�;@O�@Ol�@O+@N��@N�@N�@N��@N@MV@L�/@L�j@LZ@K��@K��@K"�@J~�@I��@I�7@IG�@I7L@I%@H��@HA�@Hb@G�;@GK�@F�R@F�+@Fv�@Fv�@Fff@F5?@F@E@E�h@E`B@E/@D��@D�@DZ@D1@C�m@C�m@C�F@C��@C33@B��@B~�@A�#@A&�@@�`@@�9@@r�@@  @?��@?�P@?\)@?;d@?
=@>��@>ff@>5?@>$�@>$�@>$�@>{@=@=/@=�@<�j@<z�@<I�@;��@;��@;S�@;"�@:��@:�\@:^5@9��@9G�@8�u@8Q�@7�@7�@7�@7�P@7\)@7K�@7
=@6�@6�R@6�R@6��@6��@6V@6{@5�T@5��@5`B@5V@4��@4�@3��@3o@2�H@2��@2n�@2J@1�@1�#@1X@1G�@17L@17L@0��@0�9@0Q�@/�w@/�P@/\)@.�@.ff@.5?@.$�@-@-��@-�@,�/@,�@,(�@+�
@+��@+dZ@+dZ@+S�@+S�@+C�@*�@*�\@*^5@)�@)��@)7L@)%@(��@(��@(�@(bN@(A�@( �@(b@(  @'�@'l�@'K�@'+@'�@&��@&�@&�R@&��@&�+@&E�@&{@%��@%�-@%p�@%`B@%`B@%`B@%`B@%O�@%O�@%/@$��@#��@#�F@#�@#o@"�!@"�@!��@!��@!hs@!G�@ �`@ �@ Q�@ b@�w@�P@|�@\)@
=@�y@�@�@�@ȴ@ȴ@ȴ@��@V@5?@{@�T@��@`B@O�@V@�@�D@z�@j@�@�m@ƨ@��@dZ@C�@C�@33@"�@@�@��@��@~�@~�@^5@J@�7@�@%@��@r�@ �@  @�@��@�w@��@l�@l�@K�@�@��@ȴ@��@��@�+@V@{@�@@��@�h@O�@/@��@�/@�j@�D@z�@I�@1@��@t�@t�@t�@S�@"�@o@�H@��@�!@��@^5@=q@-@�@J@�@�7@X@G�@�@�`@��@��@�@Q�@A�@A�@A�@1'@ �@b@ �@b@b@  @�;@�P@K�@�y@��@�+@v�@V@�@@�-@p�@O�@/@�j@I�@(�@1@�m@��@t�@"�@
�H@
��@
�!@
�\@
^5@
-@	�#@	hs@	&�@	�@	�@	%@��@��@�@Q�@�@��@l�@;d@�y@ȴ@ȴ@��@��@ff@E�@5?@�@@�@`B@/@�@��@�@�j@�j@��@�D@j@I�@1@ƨ@�F@��@��@t�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\)BXBS�BS�BR�BQ�BP�BP�BP�BO�BO�BN�BN�BN�BO�BO�BN�BN�BN�BO�BO�BO�BO�BN�BN�BM�BT�BXBcTBjBu�B�B|�Bv�Br�BiyBdZBcTBaHB_;B]/BXBO�BI�B>wB2-B"�B�BB�B�mB�BB��BǮB�wB�3B��B��B��B��B�\B�7B�Bw�BbNB;dB1'B+B�BoB  B�B�B�fB�;B��BɺB�jB�B��B��B�=B}�Bk�BO�B:^B33B.B%�B�B�BbB%B��B��B�B�ZB��BĜB�LB��B�PB�Bq�BYBC�B:^B5?B49B2-B+B$�B�B1B�B�yB�fB�5B�B��BɺBƨBŢBĜBÖBB�}B�wB�^B�RB�FB�3B�!B�B�B�B�B�B��B��B��B��B��B��B��B�oB�VB�DB�1B�1B�%B�B�B�B�%B�B�B~�B}�B|�Bw�Bs�Bl�BffBe`B[#BYBW
BVBT�BP�BM�BG�BF�BE�BD�BC�BB�BA�B?}B<jB:^B8RB2-B/B-B+B%�B$�B"�B�B�B�B�B�B�B�B�BoB\BPBJBJB
=BDB\B{BoBoBJBDB1B1BBB
��B
��B
�B
�B
�B
�B
�B
�sB
�sB
�sB
�mB
�mB
�mB
�sB
�ZB
�TB
�TB
�HB
�5B
�/B
�#B
�
B
�
B
�B
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
��B
ȴB
ǮB
ŢB
ÖB
B
��B
��B
�}B
��B
�}B
�}B
��B
��B
�}B
�}B
��B
��B
��B
��B
��B
B
��B
B
B
ÖB
ŢB
ŢB
ŢB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ƨB
ǮB
ƨB
ȴB
ȴB
ȴB
��B
ɺB
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
�B
�B
�)B
�5B
�5B
�HB
�TB
�`B
�fB
�mB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BBB1B	7B
=B
=B
=BPBoB�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B%�B%�B(�B,B/B1'B33B7LB;dB>wBB�BJ�BJ�BJ�BJ�BO�BVBW
BYB\)B]/B^5B`BBe`BhsBk�Bl�Bm�Bo�Br�Bs�Bt�Bu�Bv�By�B|�B�B�B�B�%B�+B�1B�=B�PB�hB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�LB�jB�qB��BƨBǮBɺB��B��B��B��B�
B�)B�;B�BB�HB�TB�TB�ZB�sB�B�B�B�B�B��B��B��BB
=B
=BDBJBJBPBPBVBbBhBuB{B�B�B�B�B�B �B!�B!�B"�B$�B$�B&�B&�B)�B,B-B/B33B33B49B7LB=qB>wB@�BC�BD�BF�BF�BG�BG�BH�BI�BI�BI�BI�BK�BO�BP�BP�BQ�BR�BVBW
BXBXBYBYB]/B_;Be`BgmBhsBiyBk�Bl�Bm�Bm�Bm�Bn�Bo�Bp�Br�Bs�Bt�Bu�Bv�Bv�Bw�Bx�By�Bz�B{�B|�B}�B}�B� B�B�B�%B�+B�+B�+B�7B�=B�DB�JB�JB�PB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�-B�-B�-B�9B�FB�RB�RB�XB�XB�^B�^B�qB�wB�}B�}B��B�}B��B��BBŢBƨBǮBƨBǮBǮBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�B�B�#B�#B�/B�5B�5B�5B�;B�BB�BB�NB�TB�ZB�`B�fB�fB�mB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B��B��B  BBBBBBBBBB%B+B+B+B	7B
=B
=BDBJBJBJBJBPBPBVBVBVBVBVB\B\B\B\BhBhBhBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B"�B"�B"�B"�B#�B"�B#�B#�B$�B$�B%�B&�B&�B&�B'�B'�B(�B'�B(�B(�B(�B(�B)�B)�B+B+B+B+B,B,B,B,B-B-B.B.B.B.B.B.B/B/B/B/B/B1'B1'B1'B2-B33B49B49B49B5?B5?B5?B6FB6FB7LB7LB7LB7LB8RB8RB8RB9XB9XB9XB9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�BB�BB�BB�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BH�BI�BI�BJ�BI�BJ�BK�BK�BK�BJ�BK�BL�BK�BL�BL�BL�BL�BM�BM�BM�BM�BN�BM�BN�BO�BN�BO�BO�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BS�BS�BT�BS�BT�BT�BT�BVBVBVBW
BW
BW
BXBXBXBXBYBZBZBZB[#B[#B[#B[#B\)B]/B]/B]/B\)B]/B^5B]/B^5B_;B_;B_;B_;B`BB`BB`BB`BBaHBaHBaHBaHBbNBbNBcTBbNBcTBbNBcTBcTBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBffBffBff44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B\)BXBS�BS�BR�BQ�BP�BP�BP�BO�BO�BN�BN�BN�BO�BO�BN�BN�BN�BO�BO�BO�BO�BN�BN�BM�BT�BXBcTBjBu�B�B|�Bv�Br�BiyBdZBcTBaHB_;B]/BXBO�BI�B>wB2-B"�B�BB�B�mB�BB��BǮB�wB�3B��B��B��B��B�\B�7B�Bw�BbNB;dB1'B+B�BoB  B�B�B�fB�;B��BɺB�jB�B��B��B�=B}�Bk�BO�B:^B33B.B%�B�B�BbB%B��B��B�B�ZB��BĜB�LB��B�PB�Bq�BYBC�B:^B5?B49B2-B+B$�B�B1B�B�yB�fB�5B�B��BɺBƨBŢBĜBÖBB�}B�wB�^B�RB�FB�3B�!B�B�B�B�B�B��B��B��B��B��B��B��B�oB�VB�DB�1B�1B�%B�B�B�B�%B�B�B~�B}�B|�Bw�Bs�Bl�BffBe`B[#BYBW
BVBT�BP�BM�BG�BF�BE�BD�BC�BB�BA�B?}B<jB:^B8RB2-B/B-B+B%�B$�B"�B�B�B�B�B�B�B�B�BoB\BPBJBJB
=BDB\B{BoBoBJBDB1B1BBB
��B
��B
�B
�B
�B
�B
�B
�sB
�sB
�sB
�mB
�mB
�mB
�sB
�ZB
�TB
�TB
�HB
�5B
�/B
�#B
�
B
�
B
�B
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
��B
ȴB
ǮB
ŢB
ÖB
B
��B
��B
�}B
��B
�}B
�}B
��B
��B
�}B
�}B
��B
��B
��B
��B
��B
B
��B
B
B
ÖB
ŢB
ŢB
ŢB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ƨB
ǮB
ƨB
ȴB
ȴB
ȴB
��B
ɺB
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
�B
�B
�)B
�5B
�5B
�HB
�TB
�`B
�fB
�mB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BBB1B	7B
=B
=B
=BPBoB�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B%�B%�B(�B,B/B1'B33B7LB;dB>wBB�BJ�BJ�BJ�BJ�BO�BVBW
BYB\)B]/B^5B`BBe`BhsBk�Bl�Bm�Bo�Br�Bs�Bt�Bu�Bv�By�B|�B�B�B�B�%B�+B�1B�=B�PB�hB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�LB�jB�qB��BƨBǮBɺB��B��B��B��B�
B�)B�;B�BB�HB�TB�TB�ZB�sB�B�B�B�B�B��B��B��BB
=B
=BDBJBJBPBPBVBbBhBuB{B�B�B�B�B�B �B!�B!�B"�B$�B$�B&�B&�B)�B,B-B/B33B33B49B7LB=qB>wB@�BC�BD�BF�BF�BG�BG�BH�BI�BI�BI�BI�BK�BO�BP�BP�BQ�BR�BVBW
BXBXBYBYB]/B_;Be`BgmBhsBiyBk�Bl�Bm�Bm�Bm�Bn�Bo�Bp�Br�Bs�Bt�Bu�Bv�Bv�Bw�Bx�By�Bz�B{�B|�B}�B}�B� B�B�B�%B�+B�+B�+B�7B�=B�DB�JB�JB�PB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�-B�-B�-B�9B�FB�RB�RB�XB�XB�^B�^B�qB�wB�}B�}B��B�}B��B��BBŢBƨBǮBƨBǮBǮBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�B�B�#B�#B�/B�5B�5B�5B�;B�BB�BB�NB�TB�ZB�`B�fB�fB�mB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B��B��B  BBBBBBBBBB%B+B+B+B	7B
=B
=BDBJBJBJBJBPBPBVBVBVBVBVB\B\B\B\BhBhBhBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B"�B"�B"�B"�B#�B"�B#�B#�B$�B$�B%�B&�B&�B&�B'�B'�B(�B'�B(�B(�B(�B(�B)�B)�B+B+B+B+B,B,B,B,B-B-B.B.B.B.B.B.B/B/B/B/B/B1'B1'B1'B2-B33B49B49B49B5?B5?B5?B6FB6FB7LB7LB7LB7LB8RB8RB8RB9XB9XB9XB9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�BB�BB�BB�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BH�BI�BI�BJ�BI�BJ�BK�BK�BK�BJ�BK�BL�BK�BL�BL�BL�BL�BM�BM�BM�BM�BN�BM�BN�BO�BN�BO�BO�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BS�BS�BT�BS�BT�BT�BT�BVBVBVBW
BW
BW
BXBXBXBXBYBZBZBZB[#B[#B[#B[#B\)B]/B]/B]/B\)B]/B^5B]/B^5B_;B_;B_;B_;B`BB`BB`BB`BBaHBaHBaHBaHBbNBbNBcTBbNBcTBbNBcTBcTBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBffBffBff44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220305100114                              AO  ARCAADJP                                                                    20220305100114    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220305100114  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220305100114  QCF$                G�O�G�O�G�O�8000            