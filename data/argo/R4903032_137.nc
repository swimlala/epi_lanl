CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-25T09:02:40Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220325090240  20220325090240  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @��S�r��1   @��Tq�@<�hr�!�c�1&�x�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D3D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�=q@�=qA�A=�A]�A}�A��\A��\A�A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�RC[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pDº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�=pD�s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A��A��RA�ĜA�ĜA�ĜA��wA��wA�ƨA�ȴA�A��wA�A��jA��wA���A�A���A��9A��-A��9A��9A��-A��!A��A��A���A���A���A���A���A���A���A���A�~�A�1A���A��jA�VA�oA���A���A�/A�;dA���A���A��HA�A�v�A�A�33A��A��A�p�A��FA�33A��mA�G�A��A��
A�?}A��A�dZA�5?A�M�A���A���A�`BA��-A�M�A�+A�&�A�
=A��`A��
A��A�+A��#A��hA��A�(�A��A��#A��PA�1'A���A�hsA��A�ĜA� �A�ĜA���A�v�A�ĜA�jA��A��;A��\A��FA��A�(�A�jA���A��jA��
A��A~�DA~{A|�`A|JA{7LAz{Au"�Ar�Aq�mAo;dAlĜAk"�Ai�AihsAi"�Ah��AfQ�Ad�!Ac"�Aa��A`��A_p�A^�9A^bA]|�A\�A\(�AY�hAX-AW��AW��AW;dAVVAUp�ATn�AR5?AP�AOO�AJȴAH��AG/AF��AFI�AEƨAD��AB��AB�AAx�A@��A?p�A>$�A=�A<=qA:ZA9l�A8�/A7�A6��A6-A5�A5oA4��A3�mA2�uA1�^A1&�A0ffA-ƨA,�HA,�\A,ffA,ffA+�A+VA)�;A)S�A)
=A(��A(�+A(�A'��A'`BA&�`A&��A&9XA%�A%G�A$VA#x�A#&�A"�yA"��A"1'A!�A!?}A �HA z�A (�A �A�^A��AffA�A��AƨA��AhsAVA�A7LA��A��AO�A�wA�`AȴA��A�Al�A��A~�AA�A�A�A~�A9XA��AS�AjA�TA�A
��A	t�A1A�A�-AG�A1'AVA ȴA ~�A ZA $�@�dZ@��\@��-@���@��@���@�
=@�v�@��^@�ƨ@��@��@�  @���@��@���@��
@�R@�^5@�$�@�Ĝ@�  @�;d@�@�@��@�1@�t�@��y@�M�@�@���@�(�@߮@�"�@��@���@ۍP@�{@�O�@���@�Z@ו�@�\)@֧�@ղ-@�hs@ԃ@ѩ�@�S�@Ͳ-@��`@�I�@�b@��H@�V@�33@�?}@+@�Ĝ@�1@�|�@�^5@��j@���@�l�@�dZ@�;d@�ff@�O�@�"�@���@�@��@��@��D@���@�33@���@��T@�&�@��@� �@�ƨ@�
=@��!@�ff@��h@�&�@���@��D@��w@���@��R@��\@�E�@�hs@���@�1'@��@��@�x�@��7@��@���@�j@�(�@��@���@��w@��@�S�@��@��@���@�E�@���@��7@���@��@�^5@��@��7@�G�@�&�@��j@� �@���@�S�@�o@��R@�n�@�^5@�V@��@�x�@��@�I�@��@�@���@�$�@��@���@���@�  @�dZ@�+@�ȴ@���@���@��\@��\@��+@��+@��+@���@��@�Ĝ@� �@��F@�dZ@�S�@�K�@�S�@�C�@�o@�@�@���@��H@�v�@��7@��@��`@���@�1'@��@�1@��@��
@��@�;d@�
=@���@���@���@�V@�$�@���@�&�@��@���@��9@��u@�Z@��@�K�@�+@�
=@���@���@�5?@���@��-@���@�p�@���@�r�@�j@�bN@�Q�@�A�@�1'@�w@~ȴ@~E�@~E�@~5?@}��@}?}@}V@|Z@{�F@{t�@z�\@y7L@xĜ@xr�@x �@w�@w�;@w�w@w�P@wK�@w
=@vv�@v{@u�-@t�/@s��@sdZ@so@r��@r-@q��@q&�@p�9@pA�@o��@o�@o|�@oK�@o;d@o�@n�R@nv�@nV@n$�@m�h@mO�@mO�@m/@l�/@lj@k�m@k��@kS�@j��@jn�@jM�@j-@j�@i��@i�^@i��@iG�@i%@h��@h�`@hA�@g�w@g�P@g|�@g|�@gl�@g�@f��@fv�@f$�@e�T@e�h@e/@d�@d�@c�m@cdZ@c33@b�@b��@b�\@b~�@b^5@a�^@a�7@ax�@aX@ahs@ahs@ahs@aX@a%@`��@`��@`��@`Ĝ@`b@^��@]�T@]�-@]�@]p�@]�@]p�@]`B@]O�@]?}@]/@]V@\�@\�j@\��@\�@Z�@Zn�@Z-@Y��@Yhs@X�@X1'@Xb@W�@W
=@V��@VV@V5?@V{@U��@U�h@U�@T��@T��@Tz�@S��@S��@R��@Rn�@Q��@Q%@P�9@Pr�@PQ�@P1'@O�@O|�@N�@Nff@N{@N@N@M�T@M�-@M�-@M�-@M��@M`B@M�@L�@Lj@K��@K��@K"�@J��@J=q@J�@I��@I��@Ix�@H��@H1'@H �@Hb@G�;@G��@G�@F�y@FV@D�/@D9X@D�@C��@C�
@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@C�
@CdZ@B�H@Bn�@B�@A��@A�@A�#@A��@A&�@@r�@@1'@@ �@@ �@?�@?�w@?��@?|�@?K�@>�y@>v�@>E�@>5?@=��@=p�@<��@<��@<j@<(�@;��@;ƨ@;��@;��@;t�@;33@:�!@:-@:�@9�@9�^@9��@9�7@9&�@8��@8�`@8�`@8Ĝ@8bN@8  @7l�@7;d@7�@6��@6�y@6ȴ@6��@6�+@6$�@5��@5�h@5p�@5?}@5V@4��@4�@4�/@4��@4�D@4Z@41@3��@3dZ@3S�@333@2�@2��@2��@2~�@2n�@2^5@1��@1�#@1��@1x�@1%@0��@0bN@01'@0b@0  @/�w@/�@/l�@/�@.�@.��@.v�@.E�@.$�@-@-p�@-/@,�j@,�D@,z�@,Z@,�@,1@+�m@+��@+33@+o@*��@*��@*��@*�\@*M�@)�^@)��@)�7@)x�@)hs@)hs@)X@(�`@(�@(1'@(A�@(A�@(1'@(b@'�@'�w@'��@'�P@'|�@'|�@'l�@&�y@&ff@&E�@&$�@%@%��@%O�@%V@$��@$Z@$9X@$�@#�F@#�@#C�@"�@"~�@!�@!�@!�#@!��@!��@!X@ �`@ �`@ �`@ Ĝ@ �@ A�@ b@��@�@�P@l�@l�@;d@�R@V@�T@@�-@@@��@V@��@��@�D@�D@z�@j@(�@��@ƨ@S�@@��@=q@J@��@�#@��@��@�^@�7@x�@X@Ĝ@�@Q�@�@��@|�@l�@\)@K�@�@��@�@�R@�+@v�@E�@@��@�h@�h@�h@�@�@p�@`B@O�@?}@/@�@��@�@z�@(�@�
@��@�@t�@C�@"�@@�H@��@�\@~�@n�@^5@=q@=q@=q@-@�@�@J@��@�@�#@�#@X@�`@�u@�u@r�@b@�@;d@ȴ@v�@ff@V@5?@$�@$�@{@@�@`B@O�@?}@�@V@�/@�/@��@�j@�j@�j@�D@j@9X@1@1@�
@ƨ@ƨ@�F@�F@��@t�@S�@"�@
��@
~�@
^5@
=q@	�@	�^@	��@	x�@	G�@	7L@	�@	%@	%@	%@��@�`@Ĝ@��@��@��@�u@�@1'@  @��@�@\)@;d@;d@+@�@��@�@�R@��@�+@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A��A��RA�ĜA�ĜA�ĜA��wA��wA�ƨA�ȴA�A��wA�A��jA��wA���A�A���A��9A��-A��9A��9A��-A��!A��A��A���A���A���A���A���A���A���A���A�~�A�1A���A��jA�VA�oA���A���A�/A�;dA���A���A��HA�A�v�A�A�33A��A��A�p�A��FA�33A��mA�G�A��A��
A�?}A��A�dZA�5?A�M�A���A���A�`BA��-A�M�A�+A�&�A�
=A��`A��
A��A�+A��#A��hA��A�(�A��A��#A��PA�1'A���A�hsA��A�ĜA� �A�ĜA���A�v�A�ĜA�jA��A��;A��\A��FA��A�(�A�jA���A��jA��
A��A~�DA~{A|�`A|JA{7LAz{Au"�Ar�Aq�mAo;dAlĜAk"�Ai�AihsAi"�Ah��AfQ�Ad�!Ac"�Aa��A`��A_p�A^�9A^bA]|�A\�A\(�AY�hAX-AW��AW��AW;dAVVAUp�ATn�AR5?AP�AOO�AJȴAH��AG/AF��AFI�AEƨAD��AB��AB�AAx�A@��A?p�A>$�A=�A<=qA:ZA9l�A8�/A7�A6��A6-A5�A5oA4��A3�mA2�uA1�^A1&�A0ffA-ƨA,�HA,�\A,ffA,ffA+�A+VA)�;A)S�A)
=A(��A(�+A(�A'��A'`BA&�`A&��A&9XA%�A%G�A$VA#x�A#&�A"�yA"��A"1'A!�A!?}A �HA z�A (�A �A�^A��AffA�A��AƨA��AhsAVA�A7LA��A��AO�A�wA�`AȴA��A�Al�A��A~�AA�A�A�A~�A9XA��AS�AjA�TA�A
��A	t�A1A�A�-AG�A1'AVA ȴA ~�A ZA $�@�dZ@��\@��-@���@��@���@�
=@�v�@��^@�ƨ@��@��@�  @���@��@���@��
@�R@�^5@�$�@�Ĝ@�  @�;d@�@�@��@�1@�t�@��y@�M�@�@���@�(�@߮@�"�@��@���@ۍP@�{@�O�@���@�Z@ו�@�\)@֧�@ղ-@�hs@ԃ@ѩ�@�S�@Ͳ-@��`@�I�@�b@��H@�V@�33@�?}@+@�Ĝ@�1@�|�@�^5@��j@���@�l�@�dZ@�;d@�ff@�O�@�"�@���@�@��@��@��D@���@�33@���@��T@�&�@��@� �@�ƨ@�
=@��!@�ff@��h@�&�@���@��D@��w@���@��R@��\@�E�@�hs@���@�1'@��@��@�x�@��7@��@���@�j@�(�@��@���@��w@��@�S�@��@��@���@�E�@���@��7@���@��@�^5@��@��7@�G�@�&�@��j@� �@���@�S�@�o@��R@�n�@�^5@�V@��@�x�@��@�I�@��@�@���@�$�@��@���@���@�  @�dZ@�+@�ȴ@���@���@��\@��\@��+@��+@��+@���@��@�Ĝ@� �@��F@�dZ@�S�@�K�@�S�@�C�@�o@�@�@���@��H@�v�@��7@��@��`@���@�1'@��@�1@��@��
@��@�;d@�
=@���@���@���@�V@�$�@���@�&�@��@���@��9@��u@�Z@��@�K�@�+@�
=@���@���@�5?@���@��-@���@�p�@���@�r�@�j@�bN@�Q�@�A�@�1'@�w@~ȴ@~E�@~E�@~5?@}��@}?}@}V@|Z@{�F@{t�@z�\@y7L@xĜ@xr�@x �@w�@w�;@w�w@w�P@wK�@w
=@vv�@v{@u�-@t�/@s��@sdZ@so@r��@r-@q��@q&�@p�9@pA�@o��@o�@o|�@oK�@o;d@o�@n�R@nv�@nV@n$�@m�h@mO�@mO�@m/@l�/@lj@k�m@k��@kS�@j��@jn�@jM�@j-@j�@i��@i�^@i��@iG�@i%@h��@h�`@hA�@g�w@g�P@g|�@g|�@gl�@g�@f��@fv�@f$�@e�T@e�h@e/@d�@d�@c�m@cdZ@c33@b�@b��@b�\@b~�@b^5@a�^@a�7@ax�@aX@ahs@ahs@ahs@aX@a%@`��@`��@`��@`Ĝ@`b@^��@]�T@]�-@]�@]p�@]�@]p�@]`B@]O�@]?}@]/@]V@\�@\�j@\��@\�@Z�@Zn�@Z-@Y��@Yhs@X�@X1'@Xb@W�@W
=@V��@VV@V5?@V{@U��@U�h@U�@T��@T��@Tz�@S��@S��@R��@Rn�@Q��@Q%@P�9@Pr�@PQ�@P1'@O�@O|�@N�@Nff@N{@N@N@M�T@M�-@M�-@M�-@M��@M`B@M�@L�@Lj@K��@K��@K"�@J��@J=q@J�@I��@I��@Ix�@H��@H1'@H �@Hb@G�;@G��@G�@F�y@FV@D�/@D9X@D�@C��@C�
@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@C�
@CdZ@B�H@Bn�@B�@A��@A�@A�#@A��@A&�@@r�@@1'@@ �@@ �@?�@?�w@?��@?|�@?K�@>�y@>v�@>E�@>5?@=��@=p�@<��@<��@<j@<(�@;��@;ƨ@;��@;��@;t�@;33@:�!@:-@:�@9�@9�^@9��@9�7@9&�@8��@8�`@8�`@8Ĝ@8bN@8  @7l�@7;d@7�@6��@6�y@6ȴ@6��@6�+@6$�@5��@5�h@5p�@5?}@5V@4��@4�@4�/@4��@4�D@4Z@41@3��@3dZ@3S�@333@2�@2��@2��@2~�@2n�@2^5@1��@1�#@1��@1x�@1%@0��@0bN@01'@0b@0  @/�w@/�@/l�@/�@.�@.��@.v�@.E�@.$�@-@-p�@-/@,�j@,�D@,z�@,Z@,�@,1@+�m@+��@+33@+o@*��@*��@*��@*�\@*M�@)�^@)��@)�7@)x�@)hs@)hs@)X@(�`@(�@(1'@(A�@(A�@(1'@(b@'�@'�w@'��@'�P@'|�@'|�@'l�@&�y@&ff@&E�@&$�@%@%��@%O�@%V@$��@$Z@$9X@$�@#�F@#�@#C�@"�@"~�@!�@!�@!�#@!��@!��@!X@ �`@ �`@ �`@ Ĝ@ �@ A�@ b@��@�@�P@l�@l�@;d@�R@V@�T@@�-@@@��@V@��@��@�D@�D@z�@j@(�@��@ƨ@S�@@��@=q@J@��@�#@��@��@�^@�7@x�@X@Ĝ@�@Q�@�@��@|�@l�@\)@K�@�@��@�@�R@�+@v�@E�@@��@�h@�h@�h@�@�@p�@`B@O�@?}@/@�@��@�@z�@(�@�
@��@�@t�@C�@"�@@�H@��@�\@~�@n�@^5@=q@=q@=q@-@�@�@J@��@�@�#@�#@X@�`@�u@�u@r�@b@�@;d@ȴ@v�@ff@V@5?@$�@$�@{@@�@`B@O�@?}@�@V@�/@�/@��@�j@�j@�j@�D@j@9X@1@1@�
@ƨ@ƨ@�F@�F@��@t�@S�@"�@
��@
~�@
^5@
=q@	�@	�^@	��@	x�@	G�@	7L@	�@	%@	%@	%@��@�`@Ĝ@��@��@��@�u@�@1'@  @��@�@\)@;d@;d@+@�@��@�@�R@��@�+@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Be`Be`Be`BdZBcTBbNBbNBbNBbNBbNBaHBaHB`BB`BB`BB`BB`BB`BB`BB_;B_;B^5B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B`BB`BB`BB`BB_;B`BBdZBgmBffBffBe`BdZB`BBS�BK�BL�BI�BN�BW
BffBy�B{�B~�Bz�Bp�BiyBe`B[#BR�BI�BC�B=qB9XB.B�BhB\BDB  B��B��B��B�B�B�B�B�NB��B�B��BYB/B�B�BuB	7BB��B��B�B�fB�BɺBÖB�jB�LB�'B��B�B\)BA�B)�B�BB�sB�/B��B��BǮB�}B�RB�B�{B�Bv�BhsBZBN�BG�BD�BA�B=qB5?B+B#�B�B�BoBVB
=B1BB  B��B�B�B�B�B�`B�BB�#B��BȴBB�-B��B��B��B��B��B��B�hB�\B�JB�1B�B~�By�Bu�Bq�Bm�Bk�BgmBe`BbNB^5B\)BZBVBR�BN�BL�BJ�BD�B@�B?}B>wB>wB<jB9XB7LB5?B5?B49B33B2-B1'B0!B/B.B-B+B)�B'�B%�B$�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoB\BJB1BBB
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
�B
�B
�B
�B
�yB
�fB
�`B
�NB
�;B
�5B
�5B
�)B
�)B
�)B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�B
�B
�
B
�
B
�B
�B
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
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�)B
�)B
�#B
�#B
�;B
�HB
�NB
�NB
�NB
�NB
�ZB
�ZB
�ZB
�ZB
�fB
�fB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B  BBBB	7B
=BJBJBJBPBbBhBhBoBuB{B{B�B�B�B�B�B#�B&�B'�B(�B-B.B1'B33B:^B<jB<jB@�BC�BC�BE�BF�BF�BG�BH�BH�BJ�BJ�BJ�BM�BO�BP�BQ�BW
BZBYBZB[#BZB\)B_;BbNBcTBffBl�Bo�Bp�Br�Bs�Bs�Bs�Bx�B|�B�B�7B�PB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�LB�^B�jB�jB�jB�qB��B��B��B��BBŢB��B��B�
B�B�/B�5B�5B�;B�;B�HB�TB�ZB�`B�mB�mB�B�B�B��B��B��B��B��B��BB+B1B	7BDBPBoB�B�B�B�B�B�B�B�B �B!�B!�B"�B$�B'�B(�B)�B+B.B2-B5?B8RB9XB=qBD�BF�BH�BI�BJ�BJ�BK�BL�BM�BN�BQ�BS�BVBZB^5B`BBbNBdZBffBhsBk�Bl�Bn�Bp�Bq�Br�Br�Bs�Bs�Bu�Bv�Bw�Bw�By�Bz�Bz�B{�B|�B~�B�B�B�B�%B�+B�+B�1B�1B�7B�=B�=B�DB�PB�PB�PB�bB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�'B�9B�LB�LB�RB�RB�RB�RB�XB�XB�XB�XB�^B�^B�dB�dB�jB��BBBÖBĜBǮBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�)B�/B�/B�/B�;B�;B�HB�NB�TB�TB�TB�ZB�ZB�ZB�ZB�ZB�`B�fB�fB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBB%B%B+B1B1B1B	7B
=BDBJBJBPBPBVBVBVB\B\BbBhBhBoBoBoBuBuB{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B"�B"�B#�B#�B$�B$�B$�B$�B%�B%�B%�B&�B'�B'�B'�B(�B(�B)�B)�B)�B+B,B,B,B,B,B-B-B.B.B.B/B/B/B/B0!B0!B1'B1'B1'B1'B1'B2-B2-B2-B2-B33B33B33B33B49B49B49B49B49B49B5?B6FB6FB6FB7LB7LB8RB8RB9XB:^B:^B:^B;dB;dB;dB<jB<jB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B@�B@�BA�BA�BA�BA�BB�BB�BB�BC�BC�BD�BD�BD�BD�BD�BD�BE�BF�BF�BF�BF�BF�BG�BG�BG�BG�BH�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BM�BN�BN�BN�BN�BN�BN�BO�BO�BO�BO�BP�BP�BP�BP�BQ�BR�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBYBYBYBYBYBYBZB\)B[#B[#B\)B]/B]/B^5B_;B_;B_;B_;B_;B^5B^5B_;B_;B`BB`BB`BB`BB`BBaHB`BB`BBaHBaHBaHBaHBbNBaHBbNBbNBcTBcTBcTBdZBdZBdZBdZBcTBdZBe`BffBffBffBffBgmBgmBgmBhsBgmBgmBgmBgmBgmBhsBhsBhsBhsBhsBhsBiyBhsBiyBiyBjBjBk�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bm�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   Be`Be`Be`BdZBcTBbNBbNBbNBbNBbNBaHBaHB`BB`BB`BB`BB`BB`BB`BB_;B_;B^5B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B`BB`BB`BB`BB_;B`BBdZBgmBffBffBe`BdZB`BBS�BK�BL�BI�BN�BW
BffBy�B{�B~�Bz�Bp�BiyBe`B[#BR�BI�BC�B=qB9XB.B�BhB\BDB  B��B��B��B�B�B�B�B�NB��B�B��BYB/B�B�BuB	7BB��B��B�B�fB�BɺBÖB�jB�LB�'B��B�B\)BA�B)�B�BB�sB�/B��B��BǮB�}B�RB�B�{B�Bv�BhsBZBN�BG�BD�BA�B=qB5?B+B#�B�B�BoBVB
=B1BB  B��B�B�B�B�B�`B�BB�#B��BȴBB�-B��B��B��B��B��B��B�hB�\B�JB�1B�B~�By�Bu�Bq�Bm�Bk�BgmBe`BbNB^5B\)BZBVBR�BN�BL�BJ�BD�B@�B?}B>wB>wB<jB9XB7LB5?B5?B49B33B2-B1'B0!B/B.B-B+B)�B'�B%�B$�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoB\BJB1BBB
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
�B
�B
�B
�B
�yB
�fB
�`B
�NB
�;B
�5B
�5B
�)B
�)B
�)B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�B
�B
�
B
�
B
�B
�B
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
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�)B
�)B
�#B
�#B
�;B
�HB
�NB
�NB
�NB
�NB
�ZB
�ZB
�ZB
�ZB
�fB
�fB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B  BBBB	7B
=BJBJBJBPBbBhBhBoBuB{B{B�B�B�B�B�B#�B&�B'�B(�B-B.B1'B33B:^B<jB<jB@�BC�BC�BE�BF�BF�BG�BH�BH�BJ�BJ�BJ�BM�BO�BP�BQ�BW
BZBYBZB[#BZB\)B_;BbNBcTBffBl�Bo�Bp�Br�Bs�Bs�Bs�Bx�B|�B�B�7B�PB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�LB�^B�jB�jB�jB�qB��B��B��B��BBŢB��B��B�
B�B�/B�5B�5B�;B�;B�HB�TB�ZB�`B�mB�mB�B�B�B��B��B��B��B��B��BB+B1B	7BDBPBoB�B�B�B�B�B�B�B�B �B!�B!�B"�B$�B'�B(�B)�B+B.B2-B5?B8RB9XB=qBD�BF�BH�BI�BJ�BJ�BK�BL�BM�BN�BQ�BS�BVBZB^5B`BBbNBdZBffBhsBk�Bl�Bn�Bp�Bq�Br�Br�Bs�Bs�Bu�Bv�Bw�Bw�By�Bz�Bz�B{�B|�B~�B�B�B�B�%B�+B�+B�1B�1B�7B�=B�=B�DB�PB�PB�PB�bB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�'B�9B�LB�LB�RB�RB�RB�RB�XB�XB�XB�XB�^B�^B�dB�dB�jB��BBBÖBĜBǮBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�#B�)B�/B�/B�/B�;B�;B�HB�NB�TB�TB�TB�ZB�ZB�ZB�ZB�ZB�`B�fB�fB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBB%B%B+B1B1B1B	7B
=BDBJBJBPBPBVBVBVB\B\BbBhBhBoBoBoBuBuB{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B"�B"�B#�B#�B$�B$�B$�B$�B%�B%�B%�B&�B'�B'�B'�B(�B(�B)�B)�B)�B+B,B,B,B,B,B-B-B.B.B.B/B/B/B/B0!B0!B1'B1'B1'B1'B1'B2-B2-B2-B2-B33B33B33B33B49B49B49B49B49B49B5?B6FB6FB6FB7LB7LB8RB8RB9XB:^B:^B:^B;dB;dB;dB<jB<jB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B@�B@�BA�BA�BA�BA�BB�BB�BB�BC�BC�BD�BD�BD�BD�BD�BD�BE�BF�BF�BF�BF�BF�BG�BG�BG�BG�BH�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BM�BN�BN�BN�BN�BN�BN�BO�BO�BO�BO�BP�BP�BP�BP�BQ�BR�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBYBYBYBYBYBYBZB\)B[#B[#B\)B]/B]/B^5B_;B_;B_;B_;B_;B^5B^5B_;B_;B`BB`BB`BB`BB`BBaHB`BB`BBaHBaHBaHBaHBbNBaHBbNBbNBcTBcTBcTBdZBdZBdZBdZBcTBdZBe`BffBffBffBffBgmBgmBgmBhsBgmBgmBgmBgmBgmBhsBhsBhsBhsBhsBhsBiyBhsBiyBiyBjBjBk�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bm�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220325090240                              AO  ARCAADJP                                                                    20220325090240    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220325090240  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220325090240  QCF$                G�O�G�O�G�O�8000            