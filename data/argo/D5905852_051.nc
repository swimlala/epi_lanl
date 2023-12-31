CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-27T12:38:22Z creation;2020-06-27T12:38:25Z conversion to V3.1;2022-08-02T05:10:49Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ad   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ϥ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200627123822  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA  A30_8420_051                    2C  D   APEX                            8420                            2.11.2                          846 @�$XTb��1   @�$X�ZC�@0q���l��b��;dZ1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A���A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>33C@�CA�fCC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  CjL�Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڃ3D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@<(�@�Q�@���A (�A�
A@��A`��A�  A��A���A�=qA���A��
A�  A�{B {B�B(�B
=B   B'��B033B8ffB?��BG�BO�
BX
=B_��Bg��Bp  Bx(�B�
=B���B���B���B���B�
=B��B��B��HB��B���B��B��B�  B�B�{B�\B�
=B�\B���B���B�#�B�  B��B���B�  B�B�B���B�  B�  B�B���C�qC
=C�C  C	��C�qC  C�qC�qC  C�C�CC�qCC C!�qC$�C%�qC(  C*�C+�qC-��C/�qC2  C4�C5�qC8C:�C<�C>.C@
CA��CC�CE�CG�qCJCL  CN�CO�qCR�CS�qCU��CW�qCY�qC\  C^�C`
=Ca�qCc�qCf�Ch�CjQ�Ck��Cm�3Co��Cr  Cs��Cu�RCw�RCy�qC{�qC~�C�HC�  C�  C���C�HC�C��C��C��C�HC�  C��C��C���C��qC�  C�HC���C�HC���C��)C�  C��C��C��)C���C�HC��C��C���C�  C��C�C��C��C�C��C�HC��C��C��C��C��C��C�HC�C��C���C���C�  C���C���C��C�fC�HC���C�C��C�HC�HC��C��C�HC�  C��qC��qC��qC���C�  C�HC�  C�  C�HC�  C��C���C�  C�HC���C�  C��qC���C�  C��C�HC�  C�C��C�  C��C�HC���C�C��C��qC���C��C�  C��qC��)C��qC���C��C��C���C��qC��qC���C��C���C��qC��C��C��qC��C�fC�HC��C��C��qC�  C��C�  C���C�HC��C��C��D  �D � D �\D��DHD��D�D�HD�D� D�D�D�D��D �D� D �D��D	  D	� D	�\D
\D�D�HD �D� D�\D�HD �D\D  D�HD�D�HD �D� D�\D�HDHD�HD�D��D �D��D  D\DHD�HDHD� D��D� D  D� DHD�HD  D� DHD\D  D� D�\D� D��D ~D!�D!�3D"�D"��D#�D#�3D$ �D$~D$�\D%�HD&  D&~D' �D'�HD(�D(��D(�\D)��D*3D*�HD*��D+��D,�D,�HD-HD-�HD. �D.��D/HD/��D0HD0�HD1�D1��D2HD2��D33D3��D4 �D4��D5  D5� D6 �D6� D6��D7��D8HD8� D8�\D9� D:  D:��D; �D;��D<�D<�HD=HD=��D>�D>��D?HD?� D@ �D@��DA�DA��DA�\DB�HDC�DC��DDHDD��DE �DE~DF  DF� DF�\DG\DHHDH��DIHDI��DJ �DJ� DK  DK�HDL�DL�HDMHDM�HDN�DN��DN��DO\DP  DP~�DP�DQ�HDR{DR��DSHDS�HDT  DT\DU  DU�HDVHDV��DW�DW� DW��DX~�DX�\DY�HDZHDZ\D[  D[�HD\  D\~D]  D]��D^ �D^~�D_  D_� D_�qD`\Da�Da�HDb �Db� Dc �Dc�HDd �Dd~�Dd��De~Df �Df�HDg  Dg\Dh  Dh�HDi�Di�3DjHDj� Dj�\Dk\Dk��Dl��Dm�Dm�HDn  Dn��Do3Do��Do�Dp\Dp��Dq~Dr  Dr��Dr�\Ds~�Dt  Dt��DuHDu��Dv�Dv��Dw �Dw�3Dx3Dx�3Dy�Dy��DzHDz��D{ �D{\D{�\D|\D} �D}��D~  D~� D �D\D� �D�A�D��=D��HD�  D�?�D��RD��RD� �D�@ D��D��\D���D�@�D��RD�� D�  D�?�D��D��RD�HD�@�D�\D���D�  D�?�D�\D��RD� �D�@ D��D�� D� RD�?�D�� D���D� �D�@RD�� D���D���D�@ D��RD��HD� �D�@�D���D���D� �D�@RD���D���D� RD�?\D�� D���D��D�B=D��=D��RD��
D�?�D��D�� D� �D�@RD��RD���D���D�?�D�� D���D�HD�@�D��RD�� D���D�@ D�� D���D� �D�@ D���D���D� RD�@�D��RD��\D��\D�@ D���D���D� RD�@ D��RD��HD�HD�@RD��D�� D�  D�@�D�� D��\D� RD�AHD���D��HD� RD�?�D�� D���D� �D�@�D���D���D��D�A�D��HD��HD� �D�@�D���D��RD�  D�@RD���D��RD���D�@�D��RD���D� �D�@�D�� D��\D�  D�@�D��HD���D� RD�@�D��D��\D���D�@RD�� D���D� RD�?\D��RD���D� �D�?�D���D���D� �D�@�D�\D��RD�HD�@RD��HD��HD���D�@ D���D��HD�  D�?\D���D��RD� RD�AHD���D��RD���D�?\D�\D�� D� �D�@�D���D�� D��
D�@RD��D��
D��\D�@ D��HD��RD���D�@�D���D���D� RD�@RD��D��\D��\D�@RD���D��HD� �D�@�D���D��HD��D�AHD���D���D� RD�?�D�� D��RD�  D�@�D���D���D�HD�@�D�� D���D� �D�@ D��D��RD�  D�@RD��RD��\D� RD�@�D�� D��\D���D�@�D���D���D� RD�@RD��HD���D�HD�?�D�� D���D���D�?�D�� D��\D���D�@ D�\D�� D�HD�@ D�\D���D� �D�?\D�\D�� D� RD�@ D�
D�� D� �D�AHDÀ�Dÿ�D��
D�@ DĀRDĿ�D� �D�@ D��D�� D��\D�?
D�
Dƿ
D���D�@ Dǀ�D���D� �D�@�DȀ Dȿ\D��\D�@ Dɀ�D��RD� �D�@�Dʀ Dʿ�D�  D�?�D�\D˿\D� RD�@�D̀�D��RD�  D�@�D́HD���D� �D�?�D�\D��RD�HD�@RD��D���D� �D�@ D�
D�� D�  D�?�Dр D��RD�HD�@RD��Dҿ\D� RD�AHD�\Dӿ�D�HD�A�DԀRD��RD� �D�?\DՀRD���D� RD�@RDր�Dֿ\D���D�@ D�\D׿
D��\D�@ D؁HD��HD�  D�?\D�
Dپ�D� RD�B=Dڂ=D���D� RD�?\D��D�� D���D�@ D܀RDܿ�D��\D�?
D�
Dݿ\D���D�@ DހRD���D� �D�@�D߀RD߿\D�  D�@�D���D���D� RD�AHD��D���D���D�?�D��D�� D��\D�?\D�RD�� D�  D�@ D�\D�� D���D�?
D�RD���D�  D�?\D�
D��RD��D�@RD��D���D� �D�@�D��D��RD���D�@ D逤D��HD� RD�?\D�RD��RD� RD�@�D�
D�� D� �D�AHD��D��RD�  D�@RD�RD���D� �D�@RD� DD� �D�A�D�RD�
D���D�@ D��RD���D���D�@�D�HD���D� RD�?�D�RD��HD� �D�@ D�
D�D�  D�?\D�D��HD� RD�?�D���D��RD��fD�>�D�
D��\D��\D�@ D���D���D� RD�?�D�
D���D� �D�AHD���D��RD�  D�@�D��HD���D��\D�@ D��RD���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A��A��A��A��A��A��A��A��A��A��A��A�:A�A��A�	A���A���A̧�A̜�A̗�A�'�AȒA��A��A��Aé�A�]�A~A��KA��3A�PHA�x�A���A��(A��yA�[�A���A��vA���A�NpA���A��A���A�E�A���A�A��	A��A�5tA��A�|�A�b�A�FA��
A��nA��{A�}"A�\)A���A�1A� A��A�zA�rA���A�e`A��A�hsA��A��(A��[A�uA��DA�tA�.A��A���A�$�A�A�A�poA�SA�ǮA�0�A�*eA�9�A�h>A��A���A�D�A�ɺA���A��A}v`Aw}VAv�IAvAr�AlK�Af��Ad�$Ad�AbCAa7LA]�AZ�AV�'ATa|AS�FAP�fANk�AL,=AJH�AH�ZAE��AD5�AB� AAs�A<��A8TaA6�6A5A4�>A4;dA22�A0�sA-�A+�2A*YKA(�)A(X�A)�^A)#:A&H�A&�A%A$k�A#��A!��A �A ��A �NA �?A ��A �jA!�A!�AA!o�A یA @A��A��A��A$tA	Ax�A�A(�A�BA��Ac�A!AzxA��AG�A�kA�A�A�A�AخA�6A��A�A%FAqA=qA\�AC�A�A��A%A|�AC�AU�A=qA��APHAMA�rAl�A��A�jA�DA<�A�A
�EA
�.A
p�A
=qA	��A	��A	^�A	@A�_AP�A��A�)Al�A��A�AA�&A��AMjAAeAuA�?Aw�AqA�HA�pAxA~(A]�A'�A��A0�ASA�AɆA~�ATaA�A��Aj�AGA�A �}A (�@�o@�A�@�6�@���@���@�3�@�B�@��@�0�@�PH@��@��@�ѷ@��+@�l�@�N�@��@�ѷ@�k@��@�o�@�ѷ@��@��@��@@�^5@��r@�rG@��@�%F@�*�@��@陚@�4@�:*@��@�O@�ݘ@�h�@��@��@��v@�c�@��T@��@�@�L0@�7@���@�v`@��@�K^@��@�[W@���@���@�6z@�z�@ٷ@�'�@�K^@�c�@ֿ�@�8�@�_@�\)@��@�C�@��@ӻ0@�!�@�D�@я�@�=�@�ȴ@�-@�_@Ϲ�@��@Ξ@�K^@��&@�o�@�q@��K@̥z@�5?@���@���@˞�@�_p@���@�Z@��N@��@���@Ȝx@��j@�Dg@ƍ�@��W@��3@��@ı�@�_@�O@íC@�IR@���@¿�@�{�@�7@���@��'@���@���@�1�@���@��2@���@�H�@�
�@��N@�|�@�Mj@�C@��8@��@��@���@�1'@���@�4@�h
@�e@�Z�@��@�h�@��@�@���@�`B@�C�@��@�~(@��o@�m]@�@��@�]d@��@��@�n/@�Z�@�=@��B@�3�@���@�s@��f@���@�q@��>@�S&@�9�@�0�@���@�K^@��@���@�(�@��@��x@���@�҉@�z�@�;�@���@���@�O@�Y@���@�:*@��@�ԕ@�n/@��@���@���@��d@�$@��.@���@��?@��O@�خ@�a@��@��)@�Z@�Ov@���@�(�@��@���@�l�@���@��o@��>@���@�T�@���@��9@�m�@���@���@�V@�+@�q@��M@���@��@��z@�B�@��6@�N�@��]@��@�Vm@�(�@��@��m@�d�@�7@��.@��T@��@�]�@�@@��@��5@��@��>@��@�z@�?�@�C�@�xl@�I�@��@��N@���@�\)@��@���@�u%@�N�@���@�o�@���@��F@�-�@��0@�|�@�<6@�0�@�q@�֡@�z@�V@�I�@�@���@�� @��"@�O�@��,@��@�p;@��@���@��@��@�ԕ@�@���@�s�@��@���@�-@��.@��@���@�Dg@�ں@��B@��?@���@���@�Ta@�/�@�@�ԕ@���@�{J@�'�@��@��@���@�L0@� �@��@��@��0@��^@��@�N<@�!�@��r@�+k@�@���@���@�A�@���@��!@��b@��1@���@�kQ@�V�@��@���@�Vm@�@��'@���@�E�@��.@��@��6@��w@��X@�j�@�(@��2@�ѷ@���@��}@�d�@��@+@~��@~��@~0U@}�@}��@}[W@|�U@|�@{�F@{�{@{S�@{�@z�m@z�@zM�@z{@y�^@y#�@x�@x��@xQ�@x�@w��@v�c@v��@vJ�@u��@u�~@u�@t��@t'R@s�;@s��@s�:@s]�@s;d@s+@s�@r�@rC�@q��@q�@p�.@o��@n�6@nu%@nkQ@nff@n^5@nJ�@n)�@n �@m��@m��@ma�@m%F@l�j@l��@l*�@k�&@k��@k�	@k�@j�X@jv�@i�@i�=@irG@h�$@g��@g8@f��@f8�@e�M@eJ�@e7L@e@d�5@d��@d/�@c��@c;d@b�"@bں@b��@bC�@a�T@aX@aF@aG�@a!�@`M@_E9@_6z@_/�@_,�@_,�@_,�@_�@_�@^�@^��@^��@^H�@]�@]��@]\�@\��@[�@[�@@[x@[�@Z�@Z�x@Zs�@ZV@ZE�@Z?@Z;�@Y��@YN<@X�`@X��@X�@XK^@XM@W��@W��@W1�@V͟@V��@Vxl@V)�@U��@U5�@T��@T��@T	�@S��@S�k@Sn/@So@R��@R��@R\�@R-@Q�@QVm@P�@P֡@PɆ@P�o@O�@O�q@O~�@O$t@N�6@N.�@M��@M<6@Loi@L�@K��@K�
@J�@Jq�@J\�@J@Ic�@HZ@H$@G�@G�{@GW?@F�c@F� @F�@E�@D�5@D�@D��@D~@C��@C|�@CC�@B�@B�!@B�}@B��@B�A@B
�@A��@A�@Am]@AL�@A+�@@�@@h�@?�}@?��@?l�@?+@?
=@>xl@=��@=��@=G�@<�K@<�D@<c�@<*�@<x@;��@;�@:�H@:ȴ@:�L@:u@9�X@9�X@9}�@9(�@8�@8�O@8I�@7��@7y�@7C@6��@6E�@5�T@5��@5+@4��@4��@41'@3��@3��@3Mj@34�@2�@2��@2	@1��@1o @1a�@1F@1�@0�z@09X@/�@/�V@/iD@/P�@/"�@.�y@.�b@.^5@.?@.($@.�@.@-@-7L@,��@,�.@,!@+�K@+��@+iD@+9�@*��@*��@*�!@)��@)0�@)+@)	l@(��@(�@(��@(m�@(4n@(@'��@'��@'&@&��@&�@&Z�@&&�@%��@%��@%�M@%k�@%Dg@% \@$�@$��@$�Y@$|�@$`�@$6@$$@$	�@$G@$G@#��@#�
@#��@#~�@#>�@#�@"�M@"�@"��@"��@"�@"Ov@"C�@":*@"_@!��@!�d@!x�@!<6@!(�@!+@!V@ ��@ r�@ e�@ I�@ �@��@X�@1�@,�@)_@�8@��@;�@u@��@^�@��@��@>B@b@�+@�;@�6@��@�0@��@�f@o�@Z�@@��@��@z@ff@ff@_�@Q@E�@
�@�'@<6@��@_@�@�	@>�@
=@�@�@ߤ@�<@�@GE@+k@4@ϫ@�@O�@-w@%@֡@y>@/�@�Q@X�@�@�@�y@�@�]@��@Z�@Q@GE@!�@�>@�t@�h@o @O�@�@�E@�@Z@7@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A��A��A��A��A��A��A��A��A��A��A��A�:A�A��A�	A���A���A̧�A̜�A̗�A�'�AȒA��A��A��Aé�A�]�A~A��KA��3A�PHA�x�A���A��(A��yA�[�A���A��vA���A�NpA���A��A���A�E�A���A�A��	A��A�5tA��A�|�A�b�A�FA��
A��nA��{A�}"A�\)A���A�1A� A��A�zA�rA���A�e`A��A�hsA��A��(A��[A�uA��DA�tA�.A��A���A�$�A�A�A�poA�SA�ǮA�0�A�*eA�9�A�h>A��A���A�D�A�ɺA���A��A}v`Aw}VAv�IAvAr�AlK�Af��Ad�$Ad�AbCAa7LA]�AZ�AV�'ATa|AS�FAP�fANk�AL,=AJH�AH�ZAE��AD5�AB� AAs�A<��A8TaA6�6A5A4�>A4;dA22�A0�sA-�A+�2A*YKA(�)A(X�A)�^A)#:A&H�A&�A%A$k�A#��A!��A �A ��A �NA �?A ��A �jA!�A!�AA!o�A یA @A��A��A��A$tA	Ax�A�A(�A�BA��Ac�A!AzxA��AG�A�kA�A�A�A�AخA�6A��A�A%FAqA=qA\�AC�A�A��A%A|�AC�AU�A=qA��APHAMA�rAl�A��A�jA�DA<�A�A
�EA
�.A
p�A
=qA	��A	��A	^�A	@A�_AP�A��A�)Al�A��A�AA�&A��AMjAAeAuA�?Aw�AqA�HA�pAxA~(A]�A'�A��A0�ASA�AɆA~�ATaA�A��Aj�AGA�A �}A (�@�o@�A�@�6�@���@���@�3�@�B�@��@�0�@�PH@��@��@�ѷ@��+@�l�@�N�@��@�ѷ@�k@��@�o�@�ѷ@��@��@��@@�^5@��r@�rG@��@�%F@�*�@��@陚@�4@�:*@��@�O@�ݘ@�h�@��@��@��v@�c�@��T@��@�@�L0@�7@���@�v`@��@�K^@��@�[W@���@���@�6z@�z�@ٷ@�'�@�K^@�c�@ֿ�@�8�@�_@�\)@��@�C�@��@ӻ0@�!�@�D�@я�@�=�@�ȴ@�-@�_@Ϲ�@��@Ξ@�K^@��&@�o�@�q@��K@̥z@�5?@���@���@˞�@�_p@���@�Z@��N@��@���@Ȝx@��j@�Dg@ƍ�@��W@��3@��@ı�@�_@�O@íC@�IR@���@¿�@�{�@�7@���@��'@���@���@�1�@���@��2@���@�H�@�
�@��N@�|�@�Mj@�C@��8@��@��@���@�1'@���@�4@�h
@�e@�Z�@��@�h�@��@�@���@�`B@�C�@��@�~(@��o@�m]@�@��@�]d@��@��@�n/@�Z�@�=@��B@�3�@���@�s@��f@���@�q@��>@�S&@�9�@�0�@���@�K^@��@���@�(�@��@��x@���@�҉@�z�@�;�@���@���@�O@�Y@���@�:*@��@�ԕ@�n/@��@���@���@��d@�$@��.@���@��?@��O@�خ@�a@��@��)@�Z@�Ov@���@�(�@��@���@�l�@���@��o@��>@���@�T�@���@��9@�m�@���@���@�V@�+@�q@��M@���@��@��z@�B�@��6@�N�@��]@��@�Vm@�(�@��@��m@�d�@�7@��.@��T@��@�]�@�@@��@��5@��@��>@��@�z@�?�@�C�@�xl@�I�@��@��N@���@�\)@��@���@�u%@�N�@���@�o�@���@��F@�-�@��0@�|�@�<6@�0�@�q@�֡@�z@�V@�I�@�@���@�� @��"@�O�@��,@��@�p;@��@���@��@��@�ԕ@�@���@�s�@��@���@�-@��.@��@���@�Dg@�ں@��B@��?@���@���@�Ta@�/�@�@�ԕ@���@�{J@�'�@��@��@���@�L0@� �@��@��@��0@��^@��@�N<@�!�@��r@�+k@�@���@���@�A�@���@��!@��b@��1@���@�kQ@�V�@��@���@�Vm@�@��'@���@�E�@��.@��@��6@��w@��X@�j�@�(@��2@�ѷ@���@��}@�d�@��@+@~��@~��@~0U@}�@}��@}[W@|�U@|�@{�F@{�{@{S�@{�@z�m@z�@zM�@z{@y�^@y#�@x�@x��@xQ�@x�@w��@v�c@v��@vJ�@u��@u�~@u�@t��@t'R@s�;@s��@s�:@s]�@s;d@s+@s�@r�@rC�@q��@q�@p�.@o��@n�6@nu%@nkQ@nff@n^5@nJ�@n)�@n �@m��@m��@ma�@m%F@l�j@l��@l*�@k�&@k��@k�	@k�@j�X@jv�@i�@i�=@irG@h�$@g��@g8@f��@f8�@e�M@eJ�@e7L@e@d�5@d��@d/�@c��@c;d@b�"@bں@b��@bC�@a�T@aX@aF@aG�@a!�@`M@_E9@_6z@_/�@_,�@_,�@_,�@_�@_�@^�@^��@^��@^H�@]�@]��@]\�@\��@[�@[�@@[x@[�@Z�@Z�x@Zs�@ZV@ZE�@Z?@Z;�@Y��@YN<@X�`@X��@X�@XK^@XM@W��@W��@W1�@V͟@V��@Vxl@V)�@U��@U5�@T��@T��@T	�@S��@S�k@Sn/@So@R��@R��@R\�@R-@Q�@QVm@P�@P֡@PɆ@P�o@O�@O�q@O~�@O$t@N�6@N.�@M��@M<6@Loi@L�@K��@K�
@J�@Jq�@J\�@J@Ic�@HZ@H$@G�@G�{@GW?@F�c@F� @F�@E�@D�5@D�@D��@D~@C��@C|�@CC�@B�@B�!@B�}@B��@B�A@B
�@A��@A�@Am]@AL�@A+�@@�@@h�@?�}@?��@?l�@?+@?
=@>xl@=��@=��@=G�@<�K@<�D@<c�@<*�@<x@;��@;�@:�H@:ȴ@:�L@:u@9�X@9�X@9}�@9(�@8�@8�O@8I�@7��@7y�@7C@6��@6E�@5�T@5��@5+@4��@4��@41'@3��@3��@3Mj@34�@2�@2��@2	@1��@1o @1a�@1F@1�@0�z@09X@/�@/�V@/iD@/P�@/"�@.�y@.�b@.^5@.?@.($@.�@.@-@-7L@,��@,�.@,!@+�K@+��@+iD@+9�@*��@*��@*�!@)��@)0�@)+@)	l@(��@(�@(��@(m�@(4n@(@'��@'��@'&@&��@&�@&Z�@&&�@%��@%��@%�M@%k�@%Dg@% \@$�@$��@$�Y@$|�@$`�@$6@$$@$	�@$G@$G@#��@#�
@#��@#~�@#>�@#�@"�M@"�@"��@"��@"�@"Ov@"C�@":*@"_@!��@!�d@!x�@!<6@!(�@!+@!V@ ��@ r�@ e�@ I�@ �@��@X�@1�@,�@)_@�8@��@;�@u@��@^�@��@��@>B@b@�+@�;@�6@��@�0@��@�f@o�@Z�@@��@��@z@ff@ff@_�@Q@E�@
�@�'@<6@��@_@�@�	@>�@
=@�@�@ߤ@�<@�@GE@+k@4@ϫ@�@O�@-w@%@֡@y>@/�@�Q@X�@�@�@�y@�@�]@��@Z�@Q@GE@!�@�>@�t@�h@o @O�@�@�E@�@Z@7@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	QB	Q B	P�B	P�B	P�B	P�B	P�B	P�B	Q B	Q4B	Q B	QNB	QB	QB	P�B	P�B	PbB	PB	Q B	Q B	O�B	[qB	ncB	p�B	mCB	n�B	n�B	u�B	��B	� B	��B	��B	�B	��B	�nB	��B
�B
dB
�B
.B
A�B
a|B
u%B
��B
��B
��B
�B
�GB�B%zB.�B?B`�B~BB��B��B��B�,B�sB��B��B��B��B��B��BۦB�oB�%B�]B��BЗB�EB�hBªB��B�TB�DB�gB��B��B��B��Bp�BJ�B#:B
��B
ݲB
�EB
��B
��B
tB
<�B	�nB	�1B	�B	��B	��B	j�B	I�B	A�B	<B	9$B	3hB	.B	%�B	YB	�B��B�aB��B�B�aB�AB�B	 �B	-�B	E�B	C-B	B�B	?cB	L�B	YB	l�B	t�B	r�B	h�B	U�B	CaB	8B	,WB	.�B	MjB	SB	:*B	>B	A�B	6�B	-�B	%�B	1vB	D�B	PHB	Q�B	\)B	jB	�gB	�jB	�nB	��B	�pB	�/B	�BB	�TB	��B	� B	��B	�4B	��B	�HB	�B	��B	�VB	�|B	��B	��B	�bB	�_B	��B	��B	�ZB	�5B	�tB	��B	��B	��B	�fB	�'B	�	B	�B	��B	�MB	�GB	āB	ǮB	��B	�)B	�<B	бB	��B	�:B	уB	��B	ϫB	уB	�.B	��B	��B	��B	� B	�,B	�[B	�B	�yB	�B	��B	רB	�yB	�B	�B	�QB	�CB	�~B	�qB	��B	�/B	�mB	�RB	�B	�B	�nB	��B	��B	�B	�B	�_B	��B	�B	�B	�IB	�B	�B	�-B	�B	�|B	�MB	��B	�UB	�AB	��B	�OB	�0B	��B	�B	�B	�B	�B	�B	��B	�sB	�2B	�,B	�B	�B	�B	�LB	�B	��B	�B	�_B	�XB	�B	�B	�_B	�DB	�yB	��B	��B	�sB	�B	��B	�mB	�B	�mB	�B	�B	�
B	�B	�B	�B	�B	��B	�B	��B	�B	�B	�2B	�B	�B	�B	�B	��B	�LB	��B	�FB	�2B	�LB	�LB	�B	�B	�B	�LB	�8B	�fB	�2B	�B	�B	�B	�B	�8B	��B	�B	�RB	�mB	�>B	�B	�$B	�>B	��B	��B	�>B	�$B	�XB	�B	��B	��B	�B	�B	�6B	�6B	�6B	��B	�0B	�B	��B	�B	�QB	�B	�B	��B	��B	��B	�wB	��B	�wB	�wB	�B	�B	��B	�iB	�B	�iB	�B	�;B	�;B	�B	�vB	�AB	��B	�vB	�vB	�[B	��B	�B	��B	�B	�B	��B	�%B	��B	�nB	�9B	�9B	��B	��B	��B	�FB	��B	�B	��B	�FB	��B	��B	��B	�B	�*B	�B	�$B	�8B	�	B	�rB	��B	��B	�^B	��B	��B	�PB	��B	�VB	�"B	�B	��B	�<B	��B	�B	��B	�B	�<B	�<B	��B	��B	��B	��B	�PB	�B	�qB
 B
 OB
 �B
'B
�B
-B
�B
�B
B
�B
�B
�B
;B
  B	�}B
oB
3B
?B
B
�B
�B
�B
�B
{B
MB
�B
tB
�B
1B
�B
	lB

�B
	�B

	B
)B
�B
�B
�B
�B
�B
xB
�B
0B
VB
bB
NB
 B
}B
B
 B
4B
B
�B
 B
�B
�B
&B
B
�B
uB
�B
gB
�B
B
mB
�B
�B

B
�B
mB
�B
aB
B
$B
KB
�B
kB
7B
	B
�B
�B
]B
]B
IB
dB
IB
5B
�B
VB
;B
pB
 �B
 �B
!|B
!�B
!�B
!�B
"B
"4B
"4B
"�B
"�B
#nB
"�B
# B
#nB
$B
$&B
$B
$@B
$tB
$�B
$�B
$�B
$�B
$ZB
$&B
$&B
$B
$ZB
$�B
$�B
$�B
$�B
%B
%�B
&B
&2B
&fB
&�B
'B
'mB
'RB
'�B
'�B
(sB
)yB
)�B
)�B
*B
)�B
*0B
*B
*KB
+6B
+B
+QB
+QB
,"B
,=B
,"B
+�B
+�B
+�B
+�B
,B
,=B
,�B
-)B
./B
.}B
.B
.IB
.}B
.cB
.�B
/OB
/�B
0B
0�B
2aB
3B
3B
2�B
2�B
1�B
0�B
0�B
1B
1[B
1�B
2�B
3MB
33B
3�B
3�B
4B
49B
49B
4TB
4�B
4�B
4�B
4�B
5%B
5�B
6B
6`B
6zB
6�B
7B
7�B
7fB
7fB
7LB
7LB
7fB
7�B
8B
8B
8�B
9�B
9�B
9�B
9�B
:DB
:DB
:B
:^B
:xB
:�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=VB
=�B
>(B
>]B
>]B
>�B
>�B
>�B
>�B
>wB
>wB
>�B
?B
?cB
@B
?�B
?�B
@4B
A B
@�B
AB
A�B
A�B
A�B
A�B
A�B
BB
BB
BuB
B�B
CB
C-B
C-B
CGB
C{B
C�B
D3B
DB
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
D�B
D�B
D�B
E9B
F%B
FYB
F�B
GEB
G�B
G�B
G�B
H�B
IB
IB
I�B
I�B
I�B
I�B
I�B
J�B
K)B
K)B
KDB
K^B
K^B
K�B
K�B
K�B
K�B
L0B
LJB
LJB
LJB
L~B
L�B
L�B
MjB
N"B
N�B
N�B
N�B
OvB
O�B
O�B
O�B
PB
O�B
P�B
QNB
QNB
QNB
Q�B
RB
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
SB
S[B
S�B
S&B
R�B
RoB
RTB
R:B
R:B
RTB
RoB
RoB
R�B
R�B
SB
S[B
S�B
TB
TB
S�B
S�B
S�B
TB
TFB
T{B
U2B
VB
VmB
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
VmB
V�B
W
B
V�B
V�B
WYB
W�B
W�B
XEB
X�B
Y1B
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
YB
YeB
ZB
ZQB
ZkB
ZkB
Z�B
[	B
[#B
[=B
[�B
[�B
[�B
\]B
\�B
\�B
]B
]�B
]�B
]�B
^B
^B
^B
^5B
^B
^�B
^�B
_VB
_;B
_�B
_�B
_�B
`B
`�B
`�B
aHB
aHB
a�B
a�B
a�B
a�B
bB
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
dB
d@B
d�B
d�B
d�B
eB
e,B
eB
eB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gRB
gRB
gRB
g�B
h$B
hXB
h�B
h�B
h�B
i*B
iyB
i�B
i�B
j0B
jKB
jeB
jB
j�B
kB
kB
kQB
kQB
kkB
kkB
kQB
kkB
k�B
k�B
k�B
lB
l"B
l=B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
m)B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
n}B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p!B
poB
p�B
p�B
q'B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
t9B
t�B
u%B
uZB
vB
vFB
v�B
v�B
wLB
wfB
wfB
wLB
w�B
w�B
xB
xB
w�B
xRB
xlB
x�B
x�B
x�B
y	B
yrB
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{dB
{dB
{JB
{dB
{�B
{�B
|6B
|B
|PB
|jB
|�B
}<B
}qB
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	QB	Q B	P�B	P�B	P�B	P�B	P�B	Q B	QB	Q4B	QB	QhB	Q4B	Q4B	P�B	P�B	P�B	P}B	QhB	R B	S@B	_�B	o�B	s3B	o�B	r�B	pB	w�B	�yB	�B	��B	�cB	ѝB	��B	��B	��B
�B
�B
�B
.�B
BAB
a�B
u�B
��B
�:B
�;B
��B
�B�B&LB0UBA�Bc�B�iB��B�MB�KB��B�B��B�B��B�tB�B'BߊB�B�B�.B��B�NB�B��B�mB��B��B�qB�+B�%B�kB��B�+Bt�BO�B'�B
��B
��B
��B
��B
��B
|PB
C{B	�*B	͟B	� B	��B	��B	q�B	O(B	C�B	=qB	;�B	5�B	2|B	*B	#B	tB�XB��B�B�|B��B�B��B	�B	/�B	I7B	H�B	GEB	AoB	NB	ZkB	m�B	w2B	uB	k�B	X+B	ESB	9�B	,�B	.B	N�B	U�B	:�B	>�B	C�B	88B	0B	'8B	1vB	DMB	PHB	Q�B	\)B	i�B	�3B	��B	�ZB	� B	��B	��B	�HB	��B	��B	��B	�TB	� B	��B	��B	�|B	�-B	�\B	��B	�bB	�pB	��B	�yB	�!B	�nB	��B	��B	��B	��B	�4B	��B	�8B	�AB	�>B	��B	�B	�B	��B	��B	ȚB	�3B	��B	οB	��B	уB	�&B	ңB	бB	��B	��B	�bB	�HB	�bB	�B	҉B	ԯB	ӏB	ևB	��B	�B	�EB	�+B	��B	ٚB	ٚB	�kB	�xB	��B	��B	�#B	�dB	�B	�B	�B	�B	��B	��B	�B	�KB	�eB	��B	�]B	�B	�B	�cB	�oB	�vB	�|B	�B	��B	��B	�3B	�UB	��B	�B	�!B	�B	�B	�"B	�6B	�B	��B	�B	�_B	��B	�B	�zB	�B	��B	��B	�B	�>B	�B	�KB	�B	�B	�yB	��B	��B	��B	�B	�*B	�*B	��B	�>B	��B	�
B	�
B	�B	�mB	�B	��B	�B	�8B	��B	�B	�2B	�B	�8B	��B	�2B	�fB	��B	��B	��B	�B	�2B	�B	�FB	�B	�B	��B	��B	�B	�8B	�B	��B	��B	��B	�B	��B	��B	�B	�mB	�mB	�8B	�B	��B	�B	�B	�$B	�B	�sB	�>B	�XB	�sB	�sB	�B	��B	�B	�0B	�KB	�QB	�kB	�kB	�kB	�B	�B	��B	�0B	��B	�B	�B	�B	�]B	�]B	�)B	��B	�/B	��B	��B	�IB	�cB	� B	�B	��B	�B	��B	�oB	��B	�'B	��B	�vB	�AB	��B	�B	�B	�B	��B	��B	��B	�9B	�B	�?B	��B	�B	��B	��B	�%B	�FB	�B	��B	�B	�`B	�B	�zB	��B	��B	��B	�DB	��B	�xB	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	�}B	��B	��B	�<B	�jB	��B	�B	��B	��B	�B	�(B	��B	��B	��B	�B	�qB
 OB
 �B
B
[B
�B
aB
-B
�B
-B
�B
B
AB
�B
 iB	��B
UB
gB
�B
MB
B
EB
�B
�B
�B
�B
B
�B
�B
fB
	B
	�B

�B
	�B

#B
^B
B
6B
�B
�B
JB
�B
JB
0B
�B
�B
�B
hB
�B
oB
oB
�B
hB
B
TB
�B
�B
[B
FB
�B
�B
�B
�B
�B
9B
�B
B
B
YB
YB

B
�B
aB
B
YB
B
�B
kB
�B
WB
�B
)B
�B
�B
�B
�B
�B
�B
�B
�B
pB
�B
 �B
!B
!�B
!�B
!�B
!�B
"4B
"NB
"hB
"�B
# B
#�B
"�B
#TB
#nB
$B
$@B
$&B
$@B
$�B
$�B
$�B
%B
$�B
$�B
$ZB
$ZB
$&B
$�B
$�B
$�B
$�B
$�B
%B
%�B
&2B
&fB
&�B
&�B
'RB
'�B
'�B
'�B
($B
(�B
)�B
)�B
*B
*0B
*KB
*eB
*�B
*�B
+QB
+6B
+kB
+�B
,qB
,qB
,=B
+�B
+�B
+�B
+�B
,"B
,�B
,�B
-]B
.cB
.�B
.IB
.}B
.�B
.}B
.�B
/iB
/�B
0UB
0�B
2|B
33B
3MB
2�B
2�B
2GB
0�B
0�B
1'B
1vB
2B
2�B
3�B
3hB
3�B
3�B
49B
4TB
4TB
4nB
4�B
4�B
5B
5B
5ZB
6B
6+B
6zB
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
8B
8�B
9�B
9�B
9�B
:B
:^B
:xB
:^B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
=B
=<B
=qB
=�B
>BB
>wB
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?HB
?�B
@4B
@ B
@ B
@�B
AUB
AB
AoB
A�B
A�B
BB
BB
BB
B'B
B'B
B�B
B�B
C-B
CGB
CaB
CaB
C�B
C�B
D3B
D3B
D3B
DB
D�B
D�B
D�B
D�B
D�B
D�B
DgB
D�B
D�B
D�B
D�B
D�B
EmB
F?B
FtB
F�B
G_B
G�B
G�B
HB
H�B
IB
IB
I�B
I�B
I�B
J	B
J#B
J�B
KDB
KDB
K^B
K^B
KxB
K�B
K�B
K�B
K�B
LJB
LdB
L~B
L~B
L�B
L�B
MB
M�B
N<B
N�B
N�B
OB
O�B
O�B
PB
PB
P.B
P.B
P�B
QNB
QhB
Q�B
Q�B
R B
RB
Q�B
RB
RoB
R�B
SB
SB
R�B
SB
S&B
S�B
S�B
S@B
SB
R�B
R�B
R:B
RoB
RoB
R�B
R�B
R�B
SB
S@B
S�B
S�B
T,B
TFB
S�B
S�B
S�B
T,B
TFB
T{B
UB
VB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
V�B
V�B
W
B
W?B
W
B
V�B
W�B
W�B
W�B
X_B
X�B
YKB
YeB
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
[#B
[WB
[qB
[�B
[�B
\B
\�B
\�B
\�B
]IB
]�B
]�B
^B
^B
^B
^5B
^OB
^5B
^�B
^�B
_pB
_VB
_�B
_�B
`B
`BB
`�B
aB
abB
abB
a�B
a�B
a�B
a�B
b4B
b�B
bhB
b�B
b�B
b�B
b�B
c�B
c�B
d@B
dZB
d�B
d�B
eB
e,B
eFB
e,B
e`B
f2B
f�B
f�B
f�B
f�B
f�B
gB
g8B
gmB
gmB
g�B
g�B
h>B
hsB
h�B
h�B
iB
iDB
i�B
i�B
jB
jKB
jeB
jeB
j�B
j�B
kB
k6B
kQB
k6B
kQB
kkB
kkB
k�B
k�B
k�B
lB
l"B
l"B
l"B
lWB
l=B
lqB
l�B
l�B
l�B
mB
m)B
mCB
mwB
m�B
m�B
m�B
nB
m�B
ncB
n�B
n�B
n�B
o B
o B
o�B
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
qvB
q�B
q�B
raB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sB
s3B
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
tnB
t�B
u?B
u�B
v+B
vzB
v�B
wB
wLB
wfB
wfB
wfB
w�B
w�B
xB
x8B
w�B
xlB
x�B
x�B
x�B
x�B
y$B
y�B
y�B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{dB
{dB
{B
{�B
{�B
|PB
|6B
|jB
|�B
|�B
}VB
}�B
}�B
}�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202007090045422020070900454220200709004542202007090201012020070902010120200709020101202207271538472022072715384720220727153847  JA  ARFMdecpA30a                                                                20200627123820  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200627123822  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200627123823  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200627123824  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200627123824  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200627123825  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200627123825                      G�O�G�O�G�O�                JA  ARUP                                                                        20200627125359                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200629000000  CF  PSAL_ADJUSTED_QC@;�@;�G�O�                JM  ARCAJMQC2.0                                                                 20200708154542  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200708154542  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200708170101  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063847  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                