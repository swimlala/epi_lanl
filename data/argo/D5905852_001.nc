CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-29T06:44:47Z creation;2019-05-29T06:44:48Z conversion to V3.1;2022-08-02T05:13:03Z update;     
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
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190529064447  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_001                    2C  D   APEX                            8420                            2.11.2                          846 @ئ�, 1   @ئ},_� @+�����d���"��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @y��@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  BffB   B(  B0  B8  B@ffBH  BP  BX  B_33Bg��Bp  Bx  B�  B�  B�ffB�33B���B�33B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  BЙ�B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�CL�C��C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C633C7�fC:�C<  C=�fC@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @333@p��@��@��Ap�A=��A\��A}p�A���A��HA�z�A��\A���A�ffA�=qA��\BffBz�B��B�\B'(�B/=qB7z�B?��BG=qBOQ�BWz�B^��Bg{Bo\)BwQ�BffB��3B�B��
B��=B���B��3B���B��3B���B�\)B���B��3B���B���B���B���BîBǮB�B�G�BӨ�B�u�Bۊ=Bߞ�B�B��B�B�3B�B���B���B��3C��C�
C޸C�{C	�
C�
C��C)C��C�\C��C��C�
C�{C�{C��C!�)C#޸C%޸C'�)C)�)C+��C-�{C/ٚC1�HC3��C6�C7C9��C;�HC=ǮC?��CA�=CC��CE��CG�\CI�
CK�)CM�{CO�{CQ�\CS�\CU�{CW��CY��C[�{C]�HC_�HCaٚCc�\Ce�{Cg�)Ci�)Ck�)Cm�
Co�\Cq�{Cs�)Cu�
Cw�{Cy�{C{�
C}�
C�{C���C��C��C��=C��\C��C��C��C���C��=C��=C��C��\C���C��C���C��C���C��C��C��=C��C���C��=C��C��C��C���C��C��C��C��C��\C��C��\C��\C���C��=C���C��=C��C��C��C���C��C��C��=C��=C��C���C���C��\C��C���C��C��C��C���C��C��=C��C��C��C��C��\C��=C���C��C���C��C��=C���C��=C��=C��C��=C��=C��\C���C��=C��=C���C���C��C��\C��C��C��=C���C��C��C���C��C��\C���C��=C���C��=C���C��\C��\C��C���C���C���C���C��=C���C��fC��C��C��C���C��C��\C��C���C��C��C��C��=C���C��\C���C��=C���C��=C��D vfD �
Dw�D�fDt{D��Dt{D��Du�D�DvfD�
Dw
D�
DvfD�
DvfD�D	w
D	�D
r�D
��Du�D��Dt{D��Dt{D��Dt{D�{DuD�3Dr�D�{Du�D�fDuD�Du�D��Dr�D�Ds�D�Du�D�fDu�D��Dw
D�
Du�D�{Du�D�{Ds�D�{DvfD�fDuD�
DxRD�
DvfD�
D u�D �D!t{D!�
D"x�D"�{D#t{D#�fD$w
D$�
D%uD%�D&u�D&��D'w
D'��D(s�D(�{D)vfD)�fD*u�D*��D+t{D+��D,vfD,�D-uD-�{D.uD.�fD/u�D/�D0w
D0��D1u�D1�{D2s�D2��D3t{D3�D4uD4��D5w
D5�
D6w�D6��D7u�D7��D8w
D8�fD9s3D9�{D:w
D:��D;u�D;��D<w
D<�fD=w
D=�D>u�D>�fD?vfD?��D@s�D@�DAu�DA�fDBw
DB��DCuDC�{DDu�DD��DEt{DE�DFt{DF��DGuDG�{DHu�DH�fDIuDI��DJvfDJ�fDKvfDK�3DLt{DL��DMz�DM��DNt{DN�DOuDO�fDPt{DP�{DQt{DQ�DRvfDR�DSt{DS�DTvfDT�{DUt{DU�fDVw
DV�fDWu�DW�fDXw
DX��DYt{DY��DZs�DZ�{D[w
D[�
D\u�D\�D]vfD]��D^w
D^�
D_w
D_�D`uD`�DauDa�
DbvfDb�fDcw�Dc�
Ddw
Dd�
Dew�De��Dfu�Df��DguDg��DhvfDh�Dis3Di�{DjuDj�
DkvfDk�fDlxRDl�RDmxRDm��DnvfDn�
DovfDo�
Dpu�Dp��Dqw
Dq��DruDr��Dss�Ds��DtuDt�
DuvfDu�fDvvfDv��DwvfDw�3Dxt{Dx��Dys�Dy�{Dzu�Dz��D{u�D{�
D|x�D|��D}u�D}�fD~vfD~��Du�D�D�:�D�{3D��3D��3D�;�D�z�D���D��3D�<)D�|)D���D���D�:�D�{�D���D���D�:�D�z=D���D���D�:=D�{�D���D��3D�:�D�z=D��=D���D�:�D�z�D��3D���D�:�D�{�D���D���D�;3D�{3D���D��3D�;�D�z�D���D��3D�;�D�|)D���D���D�9�D�z�D���D���D�:�D�{3D��3D��3D�:=D�y�D��3D��3D�:=D�z�D��{D��)D�;3D�z=D���D���D�:�D�y�D���D���D�;�D�{3D���D��=D�:=D�{�D���D��{D�=qD�z�D���D���D�:�D�z�D��3D��3D�;�D�{3D���D��=D�:�D�z=D��=D���D�:�D�{3D���D��=D�:�D�{3D���D���D�;�D�{3D���D���D�:�D�{�D��)D���D�:�D�z�D���D���D�;�D�{�D��)D��{D�<{D�{3D���D���D�:�D�z=D���D���D�;�D�{�D���D��=D�;�D�{�D���D��=D�;�D�{�D��3D��=D�:�D�z�D���D���D�;�D�z�D���D���D�;�D�{�D���D���D�;�D�{3D���D��3D�<)D�{�D��3D���D�;�D�{3D���D��3D�:�D�y�D��=D��=D�9�D�z�D��=D���D�:�D�z�D��3D���D�:�D�{�D���D��=D�:=D�{3D���D��3D�;3D�{�D��3D��3D�:�D�z=D��=D��=D�:=D�z�D���D���D�:�D�{�D���D��=D�:=D�{3D��{D��3D�:=D�y�D���D���D�;3D�{3D���D���D�:�D�z�D���D���D�;3D�z�D��=D���D�:�D�y�D���D��=D�;�D�{3D��3D��)D�<)D�{3D��3D��3D�;�D�{�D���D���D�:�D�{3D���D���D�;3D�{3D���D���D�:=D�{�D��)D���D�:�D�z�D���D���D�:�D�{�D���D��3D�;�D�z�D��=D���D�;�D�|�D���D���D�;�D�{3Dº=D���D�:�D�z�Dù�D���D�:=D�y�Dĺ�D��3D�;�D�|{Dż)D��3D�9�D�z=Dƻ3D��3D�:=D�z=DǺ=D���D�:�D�z�DȻ3D���D�:�D�z�Dɺ�D���D�:�D�z�Dʻ3D���D�;�D�z=D˺�D���D�:�D�y�D̹�D���D�<)D�{3Dͺ�D���D�:�D�z�Dκ=D���D�9�D�{3DϺ�D��=D�:�D�{3Dл�D��)D�;�D�z�Dѹ�D��3D�:�D�z�Dһ�D���D�;3D�z�DӺ�D���D�:�D�z�DԺ�D���D�:=D�{3Dպ�D���D�;�D�z�Dֺ=D���D�:=D�z�D׻3D���D�;3D�z�Dغ�D���D�:�D�{�Dٻ3D���D�;3D�|)Dڼ)D���D�:�D�{3Dۺ�D���D�:�D�z�Dܹ�D���D�:�D�z=Dݺ�D��3D�;�D�z�D޹�D���D�:�D�y�Dߺ=D���D�;3D�{3D�3D���D�;�D�{�D��D���D�:=D�z�D�3D���D�:�D�z=D��D���D�;�D�|)D��D���D�:�D�z�D廅D��3D�:=D�z=D滅D���D�9�D�z�D��D���D�;3D�z�D��D��=D�:�D�{�D��D��3D�:=D�z=D�=D���D�;3D�z�D뺏D���D�:=D�y�D�=D���D�:�D�z�D���D��3D�;�D�{�D��D��)D�:�D�y�DﺏD��3D�:�D�y�D��D���D�;�D�{�D�=D��3D�;�D�z�D��D���D�;3D�z�D�D��3D�;3D�{�D���D���D�:�D�z=D���D��3D�;�D�|)D���D���D�<)D�|)D���D���D�:�D�z�D��HD���D�:=D�z�D���D��=D�;3D�z=D��3D��)D�;3D�z�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aѕ�AѬAѪ�AѰUAѮ�AѯAѯOAѮ}AѭAѭwAѭCAѰUAѴAѵAѸ�AѶAѵAѱ�Aѱ'AѲaAѲ�AѲ�AѲaAѴ9Aѵ�Aѷ�AѸ�A��<A��A� �Aё�A�7�A�\A�m�A�+A��|AϦLAʹ�A�A�Aɿ�A�qA�qAA�y�A���A�d�A�oA�s�A�F�A�m]A�B�A�|�A��A���A��A�g8A�7LA���A�_pA���A��pA�|�A�rA���A��OA��A�%�A�N�A��vA�J�A���A��A�gA��xA���A�ȴA��A��jA�2�A��PA�(�A���A��A�MjA�ѷA���A�(XA�m�A�q�A��A���A�EA}˒AxTaAt_Ai��A`�_A^��A\�AY��AX�AXl"AW�AU2aARv�AOt�AJ�1AD�7AC"hAAA A?�A=��A<�CA9t�A7��A7&�A5l"A4]dA3��A2B[A1xA0v`A0qA.|A.GA,�7A)��A(��A'p�A%~�A&]�A"��A cA�xAjA:*A��A�<AhsA��A/A��A�[Ag�A�pA�AR�A�A��A$�A�TA�A;dA_A�A6zAA��A�
A-wA��AM�A4A��A}VA3�A�A�A�8Ah
A/�A�_A��A��AT�A=A�Ay�APHAp;A)�A	AԕA4�A
�A
e�A
W?A	xAc�AC-A:�A=�AN<AAAe,A�9A8�A\)AC-A6A(�AVA��A��A��A�CA��A��A>BA O@��@�ݘ@���@��
@��@���@���@�&�@�J@��@��+@���@���@��@���@�y�@�(�@��@���@���@�-�@��@���@�Ft@�H�@�-@��@��m@�@@�Q@���@�-w@�1'@��@�p�@�B�@꩓@��@��U@�$@�0�@�!@�x@�@���@��d@�8@�Z�@��@��@�k�@��@���@��W@�Y�@ޫ6@�1'@�w2@�4�@���@ܺ�@ܜx@�Q�@��@�@ۗ�@ے:@�=�@�%@��@�u�@��]@ؔF@�b@��@�*�@կ�@Գh@�~�@�9X@�(�@�)�@�&�@�#:@�@���@ӛ=@�#�@�$t@�)_@��]@�$�@�C@��m@�Mj@�҉@Κ�@�6�@��@ͭC@͢�@̓{@�V@̮}@���@ˠ'@�~�@�T�@ʟ�@�@Ɂ@�/�@�@���@��E@ȼj@�?@��@��A@��N@ǅ@�L�@��y@Ʊ�@ƍ�@��@�F�@�A�@�?}@�1�@��s@�r�@�J�@�"h@Å�@�~(@�PH@��@�o�@��@��@�K^@�{J@�u%@��j@�8@��2@�V�@���@�g�@�j�@�qv@�H�@�C@��@�R�@�J@�X@�@��R@�s�@�Ov@�#:@��j@��1@�?�@�'R@��@��n@�/@�5?@�&�@��}@�A�@��"@��@�kQ@�I�@��@���@�-w@�ں@��h@�c @�-�@�!@� �@�!@�	@��@���@��z@��t@���@���@���@��@�a|@�=q@�$@���@��7@���@�ݘ@���@���@�;d@�ی@� �@�a@��@��@��t@��:@�!�@���@�q�@�4@��H@��*@�dZ@��5@�[�@�($@��D@��t@�f�@��5@���@�w�@�YK@�B[@�	�@��@�ϫ@��d@���@���@��4@�p�@�X�@�,�@���@��?@�?@���@��1@�1�@�7@��@��~@��R@�_@��Z@�rG@�@��c@�Ĝ@���@�m�@�@��"@��1@��@��"@�^�@�	l@��@�u%@�M@��.@���@�خ@���@��:@�|@�IR@���@�&�@�e,@�J�@�&�@��@��@��c@�V�@��@��@���@�s@��v@��b@�z�@�n�@�a|@�K^@�&�@�x@��@���@���@�c@�iD@�K�@�1�@��@��@���@�ی@���@���@���@��}@�r�@�(�@��"@�e,@�1�@��@���@�@�@��#@��'@���@�;d@���@��P@��@���@�҉@��@��<@��O@�s�@�$@��)@��@�~�@�^�@�8@��@�o@��@��y@���@��@��f@���@�3�@��w@���@��@��C@�s@�)_@��@��X@���@�e@���@�_p@��@���@�Q�@�@�	@n/@33@@~��@}�=@|��@|6@{E9@z�x@z@y�C@yS&@x��@xS�@w�F@we�@w�@v�@v��@vQ@u�T@uu�@t�E@tl"@t<�@t�@sƨ@s�$@s�@s_p@s i@rZ�@r�@q�@o��@o�@n�@n8�@m@m�@l�I@lj@l$@l�@lb@l�@k�6@k'�@j��@j��@j�]@jYK@i�@i�Z@j	@j_@j{@i��@i��@h�_@g�@e��@dl"@dI�@d6@d"h@d �@d@dG@c�]@c�r@c�@cO@b��@aԕ@`�@^�b@^a|@^Z�@^V@^GE@^:*@^5?@^#:@]��@]��@]�7@]k�@]A @]%F@\�@\�E@\��@\,=@[�@Z�<@Z��@Z�r@Zs�@ZH�@ZGE@Z$�@ZJ@Y�H@Y-w@X��@X�)@X��@W�]@W�	@V�c@U��@T�E@T!@R�s@RC�@Q��@Q�@Q�n@Q5�@P�.@O1�@N�2@N��@Nz@NC�@N3�@N_@M�@M�C@M��@M�'@Mw2@L�9@K��@K��@Kv`@J�2@J@�@Ia�@H�U@Hy>@Gݘ@G/�@F��@Fs�@F1�@F �@E�)@E�H@E�=@E��@Ezx@EX@E�@D��@D	�@Bd�@BV@BOv@B@�@B0U@B�@B�@A�o@A��@A<6@@�@?˒@?�@>s�@>
�@=��@=�@=zx@=B�@=/@= \@=@@=;@<�K@<��@<��@<Xy@;�@:�A@9�j@9��@9�@9m]@9%@8�Y@8U2@7�A@7�w@7��@7�@@7U�@6��@6��@6
�@5X@5+�@5V@4�@4��@3��@3 i@2�"@2��@2�@2�,@2C�@1/@0�@0��@0~(@0w�@0z�@0j@0x@/�@/�m@/ݘ@/خ@/�*@/X�@/W?@/RT@/K�@/Mj@/9�@/@/�@/�@/�@.��@.��@.��@.��@.��@.�A@.c @.H�@.8�@.3�@.1�@.($@-�)@-��@-��@-T�@-�@,�@,|�@,$@+�P@+4�@*�]@*� @*+k@)�@)�X@)c@)�@(��@(�e@(��@(��@(z�@(c�@(H@'�@'�@'�;@'�6@'~�@&��@&M�@%�>@%��@%IR@%�@$��@$��@$x@#�@#a@#C�@#&@#@#�@"�y@"�X@"�!@"�6@"��@"Q@";�@"$�@!��@!j@!G�@!/@!@!�@ �O@ M@ �@��@��@y�@Mj@��@�X@��@�h@�b@YK@H�@e@4@�@	@�.@��@�j@ϫ@��@�t@��@��@%@Xy@�a@��@��@��@j�@+@��@��@ �@�C@��@L�@ \@�@�)@��@ݘ@��@�;@ݘ@�;@��@�g@�w@�4@\)@A�@9�@@p;@$�@�Z@��@S&@�@��@q@]d@N�@>B@M@ݘ@�K@�@��@��@��@�@�f@�@j�@F�@6z@C@�'@J�@�X@w2@S&@�@:�@�r@�Q@�6@��@y�@dZ@U�@"�@�@�@��@�2@�@�m@��@�<@�@u@hs@�@��@e�@I�@>B@-�@��@��@)_@
�H@
�R@
��@
u%@
d�@
YK@
R�@
?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aѕ�AѬAѪ�AѰUAѮ�AѯAѯOAѮ}AѭAѭwAѭCAѰUAѴAѵAѸ�AѶAѵAѱ�Aѱ'AѲaAѲ�AѲ�AѲaAѴ9Aѵ�Aѷ�AѸ�A��<A��A� �Aё�A�7�A�\A�m�A�+A��|AϦLAʹ�A�A�Aɿ�A�qA�qAA�y�A���A�d�A�oA�s�A�F�A�m]A�B�A�|�A��A���A��A�g8A�7LA���A�_pA���A��pA�|�A�rA���A��OA��A�%�A�N�A��vA�J�A���A��A�gA��xA���A�ȴA��A��jA�2�A��PA�(�A���A��A�MjA�ѷA���A�(XA�m�A�q�A��A���A�EA}˒AxTaAt_Ai��A`�_A^��A\�AY��AX�AXl"AW�AU2aARv�AOt�AJ�1AD�7AC"hAAA A?�A=��A<�CA9t�A7��A7&�A5l"A4]dA3��A2B[A1xA0v`A0qA.|A.GA,�7A)��A(��A'p�A%~�A&]�A"��A cA�xAjA:*A��A�<AhsA��A/A��A�[Ag�A�pA�AR�A�A��A$�A�TA�A;dA_A�A6zAA��A�
A-wA��AM�A4A��A}VA3�A�A�A�8Ah
A/�A�_A��A��AT�A=A�Ay�APHAp;A)�A	AԕA4�A
�A
e�A
W?A	xAc�AC-A:�A=�AN<AAAe,A�9A8�A\)AC-A6A(�AVA��A��A��A�CA��A��A>BA O@��@�ݘ@���@��
@��@���@���@�&�@�J@��@��+@���@���@��@���@�y�@�(�@��@���@���@�-�@��@���@�Ft@�H�@�-@��@��m@�@@�Q@���@�-w@�1'@��@�p�@�B�@꩓@��@��U@�$@�0�@�!@�x@�@���@��d@�8@�Z�@��@��@�k�@��@���@��W@�Y�@ޫ6@�1'@�w2@�4�@���@ܺ�@ܜx@�Q�@��@�@ۗ�@ے:@�=�@�%@��@�u�@��]@ؔF@�b@��@�*�@կ�@Գh@�~�@�9X@�(�@�)�@�&�@�#:@�@���@ӛ=@�#�@�$t@�)_@��]@�$�@�C@��m@�Mj@�҉@Κ�@�6�@��@ͭC@͢�@̓{@�V@̮}@���@ˠ'@�~�@�T�@ʟ�@�@Ɂ@�/�@�@���@��E@ȼj@�?@��@��A@��N@ǅ@�L�@��y@Ʊ�@ƍ�@��@�F�@�A�@�?}@�1�@��s@�r�@�J�@�"h@Å�@�~(@�PH@��@�o�@��@��@�K^@�{J@�u%@��j@�8@��2@�V�@���@�g�@�j�@�qv@�H�@�C@��@�R�@�J@�X@�@��R@�s�@�Ov@�#:@��j@��1@�?�@�'R@��@��n@�/@�5?@�&�@��}@�A�@��"@��@�kQ@�I�@��@���@�-w@�ں@��h@�c @�-�@�!@� �@�!@�	@��@���@��z@��t@���@���@���@��@�a|@�=q@�$@���@��7@���@�ݘ@���@���@�;d@�ی@� �@�a@��@��@��t@��:@�!�@���@�q�@�4@��H@��*@�dZ@��5@�[�@�($@��D@��t@�f�@��5@���@�w�@�YK@�B[@�	�@��@�ϫ@��d@���@���@��4@�p�@�X�@�,�@���@��?@�?@���@��1@�1�@�7@��@��~@��R@�_@��Z@�rG@�@��c@�Ĝ@���@�m�@�@��"@��1@��@��"@�^�@�	l@��@�u%@�M@��.@���@�خ@���@��:@�|@�IR@���@�&�@�e,@�J�@�&�@��@��@��c@�V�@��@��@���@�s@��v@��b@�z�@�n�@�a|@�K^@�&�@�x@��@���@���@�c@�iD@�K�@�1�@��@��@���@�ی@���@���@���@��}@�r�@�(�@��"@�e,@�1�@��@���@�@�@��#@��'@���@�;d@���@��P@��@���@�҉@��@��<@��O@�s�@�$@��)@��@�~�@�^�@�8@��@�o@��@��y@���@��@��f@���@�3�@��w@���@��@��C@�s@�)_@��@��X@���@�e@���@�_p@��@���@�Q�@�@�	@n/@33@@~��@}�=@|��@|6@{E9@z�x@z@y�C@yS&@x��@xS�@w�F@we�@w�@v�@v��@vQ@u�T@uu�@t�E@tl"@t<�@t�@sƨ@s�$@s�@s_p@s i@rZ�@r�@q�@o��@o�@n�@n8�@m@m�@l�I@lj@l$@l�@lb@l�@k�6@k'�@j��@j��@j�]@jYK@i�@i�Z@j	@j_@j{@i��@i��@h�_@g�@e��@dl"@dI�@d6@d"h@d �@d@dG@c�]@c�r@c�@cO@b��@aԕ@`�@^�b@^a|@^Z�@^V@^GE@^:*@^5?@^#:@]��@]��@]�7@]k�@]A @]%F@\�@\�E@\��@\,=@[�@Z�<@Z��@Z�r@Zs�@ZH�@ZGE@Z$�@ZJ@Y�H@Y-w@X��@X�)@X��@W�]@W�	@V�c@U��@T�E@T!@R�s@RC�@Q��@Q�@Q�n@Q5�@P�.@O1�@N�2@N��@Nz@NC�@N3�@N_@M�@M�C@M��@M�'@Mw2@L�9@K��@K��@Kv`@J�2@J@�@Ia�@H�U@Hy>@Gݘ@G/�@F��@Fs�@F1�@F �@E�)@E�H@E�=@E��@Ezx@EX@E�@D��@D	�@Bd�@BV@BOv@B@�@B0U@B�@B�@A�o@A��@A<6@@�@?˒@?�@>s�@>
�@=��@=�@=zx@=B�@=/@= \@=@@=;@<�K@<��@<��@<Xy@;�@:�A@9�j@9��@9�@9m]@9%@8�Y@8U2@7�A@7�w@7��@7�@@7U�@6��@6��@6
�@5X@5+�@5V@4�@4��@3��@3 i@2�"@2��@2�@2�,@2C�@1/@0�@0��@0~(@0w�@0z�@0j@0x@/�@/�m@/ݘ@/خ@/�*@/X�@/W?@/RT@/K�@/Mj@/9�@/@/�@/�@/�@.��@.��@.��@.��@.��@.�A@.c @.H�@.8�@.3�@.1�@.($@-�)@-��@-��@-T�@-�@,�@,|�@,$@+�P@+4�@*�]@*� @*+k@)�@)�X@)c@)�@(��@(�e@(��@(��@(z�@(c�@(H@'�@'�@'�;@'�6@'~�@&��@&M�@%�>@%��@%IR@%�@$��@$��@$x@#�@#a@#C�@#&@#@#�@"�y@"�X@"�!@"�6@"��@"Q@";�@"$�@!��@!j@!G�@!/@!@!�@ �O@ M@ �@��@��@y�@Mj@��@�X@��@�h@�b@YK@H�@e@4@�@	@�.@��@�j@ϫ@��@�t@��@��@%@Xy@�a@��@��@��@j�@+@��@��@ �@�C@��@L�@ \@�@�)@��@ݘ@��@�;@ݘ@�;@��@�g@�w@�4@\)@A�@9�@@p;@$�@�Z@��@S&@�@��@q@]d@N�@>B@M@ݘ@�K@�@��@��@��@�@�f@�@j�@F�@6z@C@�'@J�@�X@w2@S&@�@:�@�r@�Q@�6@��@y�@dZ@U�@"�@�@�@��@�2@�@�m@��@�<@�@u@hs@�@��@e�@I�@>B@-�@��@��@)_@
�H@
�R@
��@
u%@
d�@
YK@
R�@
?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B8lB7�B8RB7�B7B72B72B7B6�B6�B6�B8B8�B9$B9	B:*B:�B9�B:^B;B:�B9�B8�B;dB=�B>�B<�BWYB�	B	A�B	��B	ĜB	�EB
�B
1'B
@�B
H�B
�yB
�B
�BEB$B{�B�9B�dB��B�`B̳B�B�B�B�BjB!-B)�B3�B="BBABL�BNBM�BL�B<jB&�B~B2BPB1BB��B�BݲB�B�hBuZBk�BU�B9	BsB
��B
�VB
��B
��B
�|B
��B
x�B
UgB
3B
�B	�B	�fB	ܬB	�B	�hB	�B	}�B	y�B	tTB	n�B	m�B	mwB	o�B	s�B	u�B	vFB	u�B	o5B	h
B	iB	e,B	e,B	iDB	q�B	o B	jB	hsB	a�B	\�B	T�B	PB	P�B	RB	R�B	i�B	p�B	\�B	U�B	Q�B	M�B	raB	b�B	`�B	ZkB	X�B	W�B	V�B	U�B	VSB	Y�B	^�B	_�B	c�B	d�B	h�B	vFB	�%B	�qB	��B	�$B	�pB	�	B	��B	�"B	�B	��B	�&B	�@B	�@B	�
B	��B	�&B	��B	�B	�%B	��B	�JB	׍B	�B	��B	�>B	�B	�OB	�0B	��B	��B	��B	�vB	�OB	�DB	�B	�
B	�B	�B	�B	��B	�B	��B	یB	��B	�)B	�B	�4B	�vB	�jB	��B	�YB	��B	�:B	҉B	ңB	�oB	� B	�B	ѷB	�hB	�4B	� B	�bB	�HB	��B	�,B	�2B	��B	�mB	�?B	��B	��B	�?B	��B	��B	֡B	�mB	��B	��B	��B	�xB	�xB	�B	�;B	ߊB	��B	��B	�B	��B	�B	�-B	��B	�B	��B	�B	��B	�bB	��B	�B	�hB	�B	�B	��B	�B	ߤB	�;B	ބB	ބB	��B	ބB	�|B	��B	�TB	��B	�B	�B	��B	��B	�8B	�B	�B	�XB	�B	�eB	��B	�6B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�[B	�B	��B	�B	�|B	�aB	�B	�B	�MB	��B	�B	��B	��B	��B	�tB	��B	��B	��B	��B	��B	�LB	��B
 OB
 �B
;B
�B
oB
UB
UB
�B
'B
�B
[B
AB
uB
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
B
B
_B
�B
�B
B
B
�B
�B
�B
�B
�B
	B
	B
�B
�B
	RB
�B
�B
�B
	�B
	�B
	�B
	7B
	�B
	lB
	lB
KB
�B

�B
	�B
	�B

�B
)B

�B
)B
�B
dB
JB
B
B
jB
�B
�B
6B
B
�B
�B
VB
<B
B
"B
�B
�B
�B
B
~B
JB
B
B
�B
�B
�B
pB
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
VB
(B
�B
�B
�B
�B
\B
�B
�B
�B
@B
&B
B
&B
@B
�B
�B
�B
FB
,B
B
aB
�B
�B
gB
�B
�B
�B
B
9B
SB
9B
mB
mB
�B
�B
$B
$B

B
?B
?B
?B
$B
YB
?B
?B
?B
?B
YB
YB
$B
�B
�B
�B
�B
�B
�B
�B
B
QB
�B
qB
�B
�B
]B
�B
�B
/B
~B
�B
B
�B
�B
�B
B
VB
�B
�B
�B
�B
�B
 B
�B
�B
 vB
!HB
"4B
!�B
"NB
"NB
"B
"4B
#TB
#nB
#TB
#TB
#�B
$�B
%,B
%FB
%FB
%FB
%FB
%zB
%zB
%�B
%�B
%�B
%�B
%�B
&B
&2B
&2B
&2B
&LB
&fB
&fB
&�B
&�B
&fB
&�B
'RB
(XB
(>B
(XB
(�B
(�B
)B
)�B
)�B
)yB
*0B
*KB
*0B
*0B
*KB
*0B
*KB
*0B
*B
*KB
*eB
*�B
+B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
+�B
-]B
/ B
/B
/�B
0!B
0B
0!B
/�B
0oB
0�B
0�B
0�B
0�B
1�B
2-B
2-B
2�B
2�B
3�B
4�B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
6�B
7B
7�B
7�B
8B
8B
88B
8�B
9>B
9rB
9�B
9�B
9�B
9�B
:DB
:^B
:xB
:�B
:�B
:xB
:�B
:�B
:�B
:�B
:�B
;0B
;0B
;�B
="B
=qB
=VB
=�B
=�B
>(B
>BB
>(B
>wB
>]B
>wB
>BB
>BB
>�B
>�B
>�B
>�B
?HB
>�B
?B
?B
?B
>�B
?.B
>wB
@ B
@B
AB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
AoB
AUB
A�B
A�B
A�B
C�B
DMB
DgB
DgB
DgB
DgB
D�B
DgB
DMB
D�B
D�B
D�B
D�B
EB
EB
EB
EB
EB
E�B
F?B
F�B
F�B
F�B
F�B
GB
F�B
F�B
F�B
GzB
G�B
G�B
G�B
G�B
H�B
HfB
H�B
IB
I�B
J=B
KxB
K�B
K�B
K�B
L0B
L�B
M�B
O�B
P.B
PbB
P�B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
SB
S�B
S�B
S�B
TFB
T�B
U�B
U�B
U�B
VSB
V�B
V�B
W$B
WsB
W�B
W�B
W�B
W�B
W�B
XB
W�B
X+B
X_B
X�B
Z7B
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZB
ZB
Y�B
ZkB
[	B
[�B
\CB
\xB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
^OB
^�B
^�B
^�B
^�B
_B
_pB
_;B
_�B
_�B
_�B
_�B
`BB
`vB
`�B
`�B
a�B
a�B
a|B
a|B
aHB
b�B
b�B
b�B
b�B
b�B
bNB
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
dtB
dtB
d�B
d�B
d�B
e,B
e,B
e,B
eFB
eFB
e`B
ezB
e�B
ezB
ezB
e�B
e�B
ezB
e�B
e�B
e�B
e�B
fB
f2B
fB
fB
fB
f2B
f�B
ffB
f�B
f�B
f�B
gB
gmB
g�B
g�B
h$B
hXB
h�B
h�B
iB
iB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
jKB
j0B
j0B
jeB
j�B
k�B
k�B
k�B
lWB
lWB
lWB
l�B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
nB
n/B
nB
n/B
ncB
ncB
n}B
o5B
oB
o5B
oOB
oOB
oOB
o�B
o�B
p!B
p;B
pUB
p�B
p�B
q'B
q'B
q'B
q'B
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r|B
s3B
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
uB
u?B
u?B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
wLB
wfB
xB
x8B
x8B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{�B
|B
|jB
|PB
|jB
}�B
}�B
}�B
}�B
}�B
~B
}�B
~B
~BB
~]B
~]B
~]B
~wB
~wB
~wB
~wB
~BB
~BB
~�B
�B
�OB
��B
��B
��B
��B
��B
�B
�oB
�B
�uB
�uB
��B
��B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B88B7�B8RB7�B7B7LB72B7B6�B6�B6�B8B8�B9>B9$B:*B:�B9�B:^B;B:�B9�B8�B;B=�B>�B=BX+B�XB	B�B	��B	�mB	��B
oB
1�B
B'B
L�B
��B
��B
�%B	�BB}�B�sB��B�KB��B��B�BB�5B �B/B vB%B-�B6zB?.BE�BNpBO�BQ4BR:B@B(�B!B�B�B	�B�B�B��B�:B��B�MBw�BoBZB>(B�B
�DB
�B
�|B
�B
�2B
��B
cB
\�B
;B
FB	�B	�B	�TB	�~B	��B	�VB	�B	|�B	v�B	p!B	n�B	o�B	r�B	w�B	zDB	|�B	{�B	q[B	jB	kQB	g�B	gRB	l�B	s�B	pUB	l�B	i�B	b�B	^�B	VSB	P�B	Q�B	TFB	TFB	l=B	s�B	^�B	WYB	TB	N�B	vzB	e,B	a�B	Z�B	Y1B	X_B	WYB	V9B	W?B	Z�B	_!B	`\B	d�B	e�B	i�B	v�B	�EB	��B	��B	��B	��B	�B	�PB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�>B	�;B	�?B	��B	��B	�B	��B	�wB	��B	�9B	�}B	��B	�:B	�TB	�FB	�B	�UB	��B	�
B	�B	��B	��B	�B	�|B	�B	ޞB	��B	�)B	ݘB	��B	�B	��B	�;B	�B	�B	��B	҉B	ҽB	ҽB	ҽB	�TB	�:B	��B	ѝB	�hB	�NB	�4B	�NB	�}B	�FB	�MB	�B	ևB	��B	�B	�+B	�YB	�$B	�
B	��B	ּB	ּB	�+B	ںB	��B	��B	��B	��B	�'B	�!B	�HB	�B	��B	��B	��B	�B	�-B	�HB	�-B	�B	��B	�NB	�B	��B	�TB	�:B	�B	�B	�'B	�B	��B	��B	�VB	�;B	��B	�hB	�B	�B	��B	�eB	�DB	�sB	�B	�B	�
B	��B	�eB	�B	��B	�kB	�QB	�B	�WB	��B	��B	�)B	��B	�TB	��B	�TB	�GB	�-B	�B	��B	�vB	��B	��B	�B	��B	�B	�hB	�B	�?B	��B	��B	�B	��B	��B	��B	�LB	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
GB
B
�B
�B
�B
�B
B
%B
B
B
EB
_B
�B
EB
�B
fB
�B
	lB
�B
�B
�B
	B
	lB
	RB
	B
	RB

	B
	7B
	B
	RB
	�B
	�B

	B
	�B

�B
	�B
	�B
�B
�B
DB
	�B
	�B

�B
^B
B
xB
~B
�B
�B
jB
PB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
\B
<B
�B
B
�B
dB
PB
B
pB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
vB
�B
B
�B
�B
�B
.B
hB
&B
uB
uB
uB
�B
�B
B
B
FB
{B
{B
{B
�B
B
MB
�B
�B
�B
9B
�B
mB
�B
�B
�B
�B

B
$B
YB
YB
YB
sB
YB
YB
YB
�B
sB
sB
sB
�B
�B
�B
�B
+B
EB
�B
�B
�B
B
7B
�B
�B
	B
�B
B
B
xB
�B
/B
�B
5B
OB
OB
�B
�B
B
VB
�B
�B
�B
�B
 B
�B
 BB
 'B
 'B
 �B
!�B
"NB
"4B
"�B
"�B
"hB
"�B
#�B
#�B
#�B
#�B
$ZB
%,B
%`B
%zB
%zB
%`B
%zB
%�B
%�B
%�B
&2B
&B
&B
&2B
&2B
&LB
&LB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'RB
'�B
(�B
(sB
(�B
(�B
)DB
)yB
)�B
)�B
)�B
*B
*eB
*KB
*eB
*eB
*eB
*B
*eB
*KB
*�B
*�B
*�B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
,"B
,=B
.B
/iB
/�B
0B
0UB
0B
0UB
0;B
0�B
0�B
1B
1'B
1vB
2B
2aB
2�B
2�B
33B
49B
4�B
4�B
4�B
5B
5%B
5�B
6FB
6`B
6�B
7fB
7�B
8B
8RB
8lB
8�B
8�B
9XB
9�B
9�B
9�B
:B
:DB
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;B
:�B
;B
;�B
<B
=qB
=�B
=�B
=�B
=�B
>BB
>]B
>BB
>wB
>wB
>�B
>]B
>�B
>�B
>�B
>�B
>�B
?}B
?B
?.B
?.B
?.B
?.B
?cB
>�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B[B
B�B
DMB
DgB
DgB
D�B
D�B
DgB
D�B
D�B
DgB
D�B
D�B
D�B
EB
E9B
ESB
ESB
ESB
EmB
F%B
FYB
F�B
F�B
F�B
F�B
G+B
F�B
G+B
G+B
G�B
G�B
H1B
HB
H1B
H�B
H�B
IB
I�B
J	B
J�B
K�B
LB
LB
L0B
L~B
L�B
N"B
P.B
PbB
P�B
P�B
Q4B
QhB
Q�B
RB
RB
R B
RB
RTB
SuB
S�B
S�B
TB
T�B
U2B
U�B
V9B
V9B
V�B
V�B
W$B
WYB
W�B
W�B
W�B
W�B
XB
XB
X+B
X+B
X_B
X�B
Y1B
ZQB
Y�B
ZB
ZB
ZB
ZQB
ZQB
Z7B
ZkB
Z7B
Z�B
[qB
[�B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]/B
]B
]IB
]dB
]�B
^�B
^�B
^�B
_B
_B
_VB
_�B
_pB
`'B
`B
`'B
`'B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
c B
b�B
b�B
b�B
b�B
b�B
cTB
dB
c�B
c�B
c�B
dB
c�B
dB
dtB
d�B
dtB
d�B
d�B
d�B
eFB
e,B
eFB
e`B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fB
f2B
fLB
f2B
f2B
f2B
ffB
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
h$B
hXB
h�B
h�B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
i�B
jB
jB
jB
jeB
jKB
jeB
jB
j�B
k6B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
nB
n/B
n/B
nIB
nIB
ncB
n�B
n}B
n�B
oOB
oOB
oOB
oiB
o�B
o�B
o�B
p!B
pUB
poB
p�B
p�B
p�B
q[B
qAB
qAB
q[B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
tnB
t�B
u?B
utB
utB
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
w�B
w�B
w�B
w�B
xB
xlB
xlB
x�B
y	B
yXB
y�B
y�B
y�B
y�B
z*B
zDB
z^B
z^B
zxB
z�B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{�B
|PB
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~B
~(B
~B
~BB
~]B
~]B
~]B
~wB
~�B
~�B
~wB
~�B
~]B
~�B
HB
�B
��B
��B
�B
�B
� B
��B
�;B
��B
�AB
��B
��B
��B
��B
��B
��B
��B
�-33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<AV�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902220048002019022200480020190222004800202207271129562022072711295620220727112956202207271532462022072715324620220727153246  JA  ARFMdecpA30a                                                                20190529064055  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190529064447  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190529064447  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190529064448  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190529064448  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190529064448                      G�O�G�O�G�O�                JA  ARUP                                                                        20190529065917                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190219154510  CV  JULD_LOCATION   G�O�G�O�F�5$                JM  ARGQJMQC2.0                                                                 20190219154510  CV  LATITUDE        G�O�G�O�AX1'                JM  ARGQJMQC2.0                                                                 20190219154510  CV  LONGITUDE       G�O�G�O��%��                JM  ARSQJMQC2.0                                                                 20190220000000  CF  PSAL_ADJUSTED_QC@=p�@z�HG�O�                JM  ARCAJMQC2.0                                                                 20190221154800  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190221154800  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727022956  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063246  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                