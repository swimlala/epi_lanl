CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-07T12:38:05Z creation;2020-06-07T12:38:08Z conversion to V3.1;2022-08-02T05:10:55Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200607123805  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               1A   JA  A30_8420_049                    2C  D   APEX                            8420                            2.11.2                          846 @�Q�^o�1   @�R<� @0�b���b��)^�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B�ffB�  B�  B���B�  B�  B�  B�  B�  B�  B˙�B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�ffB�  C   C  C�fC  C  C
  C  C  C� C��C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @3�
@z=q@�  @�ffAffA>=qA^�RA33A�\)A�p�A�33A���AϮA߮A�A��B�
B�B��BB'��B/��B7B?�
BGBO�BWB_��BgBoBw�RB��B��HB��HB��HB��)B���B��HB���B��RB�G�B�B�ǮB��3B���B��
B�ǮB��
B��B��B�\)BϽqB��
B��)B���B��
B���B�qB�ǮB���B��B�G�B��)B���C��C�HC�C�C	�C�fC�3CnC��C�C�C�C��C�C�C��C!�fC#�fC%��C'�C)�C+�C-��C/�RC2�C3�)C5�HC7�C9�fC;�C=�C?�CA�CC�fCE�fCG�CI��CK��CM��CO��CQ��CS��CU��CW�CY��C[��C]�fC_�Ca��Cc�Ce��Cg�fCi�Ck��Cm�fCo�3Cq�RCs�Cu�fCw�Cy��C{�3C}�C�C���C���C���C���C���C���C���C��
C��
C���C���C��RC��
C��
C��
C��
C��RC��
C��{C��3C��
C��
C���C���C��{C��{C���C��3C��3C���C���C��RC��RC��{C��3C���C��)C���C��RC���C���C���C��
C��
C��
C��RC��RC���C��RC��RC���C��
C��RC���C��RC��
C���C��
C���C��
C��3C��{C��RC��
C��RC���C���C���C��3C���C��RC��
C��RC���C���C��
C��3C���C���C��RC��{C���C��{C���C��{C��
C���C��)C���C���C��RC��RC���C��{C��C��3C��
C��
C��{C���C��
C��RC���C��RC���C��{C��
C��3C��
C��RC��3C���C���C��{C���C��
C���C��
C��RC��{C��RC���C���C���C��RC��{C���C���D z�D ��D|)D��D{�D��D{�D�)D|)D��D~D�qDz=D��D}qD��Dz�D��D	|)D	��D
z�D
��Dy�D�=D|�D�D|)D�=Dz�D�D|)D��Dz�D��D|)D��D{�D�)Dz�D��D{�D��Dx�D��Dy�D��D|)D��D|)D�)D~D��D~D�qD{�D��D|)D�=Dy�D��Dz=D�=Dz=D��D |)D ��D!x�D!��D"z�D"��D#|�D#��D${�D$�D%}qD%�=D&y�D&��D'|)D'��D(z�D(��D)|)D)�)D*|)D*��D+|)D+�qD,}qD,�qD-z�D-��D.{�D.��D/{�D/�)D0|)D0�)D1|)D1�)D2|�D2��D3|)D3�qD4~D4��D5{�D5��D6z�D6��D7{�D7�qD8|�D8�D9}qD9��D:|�D:��D;}qD;��D<{�D<�=D=|)D=�)D>z=D>��D?|)D?�)D@|)D@�qDA|)DA�)DB{�DB��DC{�DC�)DD}qDD�)DEz�DE��DF{�DF�)DG|�DG��DHx�DH�=DI{�DI�)DJ|�DJ��DK{�DK�=DL{�DL�)DM{�DM�)DNz=DN��DOz�DO�=DPz�DP��DQ|)DQ��DRz=DR�=DS|)DS��DT|�DT��DU|)DU��DVy�DV��DWz=DW�=DXz=DX��DYy�DY��DZz=DZ��D[|)D[��D\|�D\��D]z�D]��D^z=D^��D_|�D_��D`{�D`��Da|)Da�)Dbz=Db��Dc{�Dc��Dd|)Dd��Dez=De�=Df{�Df��Dgz�Dg�)Dh|)Dh��Diz=Di��Djz=Dj�)Dk|�Dk�qDl|)Dl��Dmz�Dm�=Dnz=Dn��Do{�Do�qDp}qDp�=Dqz�Dq��Drz=Dr�=Ds{�Ds�qDt~�Dt��Dux�Du��Dv|)Dv��Dw~Dw�Dx~Dx�Dy|)Dy��Dz|)Dz��D{z=D{�)D|}qD|�)D}z�D}�=D~{�D~��Dz=D�)D�>D�~D��D��qD�=qD�}�D���D��fD�>�D�~D���D��fD�>fD�~�D��fD��D�<{D�|{D��qD��qD�<�D�|�D��qD��D�>fD�~D���D��D�=�D�~D��D��D�=�D�~�D���D���D�>�D�}�D��{D��qD�=qD�|)D���D��fD�>fD�}qD��qD��
D�>fD�|�D��{D���D�>D�}qD��{D��)D�=D�~fD��D��D�=qD�|�D��D��D�>D�~fD��qD��)D�=D�~�D���D���D�=�D�}�D��qD��D�=D�}qD��qD��D�>fD�~�D���D��\D�?
D�~fD��D��qD�>D�~fD��fD��D�=�D�}qD���D���D�=qD�~D��qD��D�<�D�|)D��qD��D�=D�}D��{D��{D�=�D�~�D���D���D�>fD�~fD��fD��qD�>D�
D���D��D�>fD�
D���D���D�=qD�}qD���D��fD�>D�~D���D��fD�=�D�}D���D���D�=�D�}qD��qD��{D�=qD�~D��qD��qD�>�D�
D��D��qD�>D�~�D��qD���D�>D�}qD���D��qD�>D�~�D��
D���D�>fD�
D��D���D�?\D�~fD���D��D�=qD�}�D���D���D�=qD�}qD��D��D�=D�~fD��D���D�=D�}qD��)D��{D�>fD�~�D���D��fD�>�D�}�D���D��D�>fD�~fD��qD���D�>�D�~�D��fD��fD�>�D�~D��fD��D�>D�}qD��qD���D�=�D�~D��qD���D�>fD�}�D��D���D�=�D�}qD��D��qD�=D�}�D��D���D�=D�~D��D���D�=qD�}qD��{D��)D�<{D�}�D���D��fD�?
D�~D���D��fD�=�D�|�D��D��qD�=D�}D��D��{D�<)D�|�D��qD��D�=D�~fD���D��
D�>D�}D��D��D�=D�}D��qD���D�>D�~D¾D��D�>�D�~fDýqD��D�=qD�~DľfD��qD�>D�~DŽD��D�>�D�~fDƾfD��fD�>D�~fDǽ�D��qD�>�D�~fDȽqD��D�?
D�}�Dɼ{D��D�=�D�~�Dʾ�D��qD�=qD�~D˽�D��D�<�D�|�D̽D��qD�>D�~fD;D��qD�<�D�|{DνD���D�>�D�~�DϽD��D�=D�}DнD���D�>fD�\DѾfD���D�<{D�}DҽD��qD�>D�}qDӾD��\D�?
D�~�DԽ�D���D�>D�}�Dս�D���D�>D�
Dֿ
D��
D�>fD�~�D׽D��)D�=D�~�Dؾ�D��qD�<)D�}DپD���D�=�D�}�DھfD��D�<�D�}�D۽D���D�=D�~Dܿ
D��D�=�D�~fDݾD��D�>fD�~D޽�D��qD�=qD�}D߼�D��qD�=�D�}�D�D��D�>fD�}qD�qD��fD�>fD�}qD�D���D�>D�}�D㾸D��fD�>�D�
D�D��D�=�D�}D��D��D�>D�}D��D��D�=qD�~fD�fD��fD�>fD�}qD�fD��
D�=�D�|�D�qD��qD�<�D�}D�fD��D�=�D�}qD�qD���D�>fD�~fD�D���D�=qD�~D���D��D�=�D�}D��D���D�?
D�~�DﾸD��qD�=qD�~�D�D��
D�?\D�\D�\D��\D�>fD�}D��D��fD�=�D�|�D�D��qD�<{D�|�D���D���D�<)D�|�D��qD��fD�>D�}�D���D���D�=�D�~fD��\D��
D�>�D�~�D��
D��\D�>D�|�D��D��
D�>fD�}qD��fD���D�=D�~fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�jKA�kQA�v`A�|�A�{A�{A�|�AЀiA��A�jA�cTA�S&A�`vA�a�A�WsA�X�A��A�~A�A�	lA�	7A��A���A��"A���A��GA��A��QA�ԕA�ӏA��mA�ԕA��A��EAϻ�AϹ�A���A�7A��EA�oiA�?HA�A A��`A�7A�-�A���A���A��lA���A��_A�}�A���A�zxA�C�A��A���A��A�Z�A�d�A�8�A��A��6A�g�A���A�V�A��A�?A�p�A��A�+6A���A�6�A�L0A��>A�OvA��tA�T�A���A���A�ѷA�A���A��aA�2-A��A��]A�P�A��KA�0UA}/Az�~Av�?Asz�An��Ak?�Ah�Ag~�Ae��AbA]�A\AV�nAN��AJh�AH��AF��AC��A?}�A;��A8-A7�A7{�A5c�A4�A5A5�A4�wA3��A3�bA3k�A2a|A1A0(�A-(A+��A)�AA)>BA)6A)�MA)�A);A'=qA$'RA"[WA!��A!�A!�tAA��A4�A�,A�yA&�A'�A�rA�?A�"AxAMAC�A�A �A��A��AیA
�A��A�FAa|A��A��AX�AffA��AMA�uA��A7�A�^A�AMAa�A�A��A��A�SA33AJ�AqA��A3�A��A�-A}VA*�A
�bA
��A
iDA
oA	�A	v�A	5?A��A_�A$A��A�0A��A`BA�AN�A�'Al�A��A�FAOvA��AC-A�A��AB�A�TASA �A @��*@�b�@���@���@�K�@���@���@���@��@�Ft@��Z@�q@�q�@��@��?@�[W@�j@�*�@�:@�8@�0�@�%F@�@�Z@��@���@��@�~(@��@�Dg@썹@�=q@��g@�RT@���@�D@�H@���@�@�Ft@�p�@��@�{�@�%F@��@�ff@�/�@��T@�{@�2a@��@��@��@�`B@�&@�<@���@�-@��@�+@޳h@�oi@���@�A�@܏\@��d@�^�@�ߤ@څ�@�I�@���@�rG@تe@��}@�<6@��@֧�@�H@՜@�p;@���@��z@ӈf@�҉@��@�@O@ЦL@�K^@ψf@��@δ9@�2�@��@���@�\)@��?@�}V@��a@�g�@��@ʚ@�@ɠ�@�+@�l"@�
�@��W@ǡ�@�e�@�)_@��X@�kQ@���@�c@�C@��U@�	�@ï�@�s@�=@��f@ �@��#@��f@�
=@��4@�N�@���@��2@�a|@�e@�� @��{@�.I@��f@��@�>�@� i@���@��u@�1�@��K@���@�
=@���@�Q�@�$@���@�2a@��@��r@�{@���@���@��U@�_�@�E�@�	@��r@��@��6@���@��@��@���@�{@���@�k�@�Q@��@��q@�o@��@�w�@�a|@�M@�/�@�7�@���@�\�@�/�@��@�ی@��_@�W�@�7�@��@���@�(@�YK@���@��M@��@�R�@��@��@�s@�9�@��|@��}@�,=@��g@��@�/�@���@���@���@��r@��@�qv@�a�@�S&@�.I@���@��O@�u%@�1'@�E�@�O@�G@��N@�Q�@���@��<@���@�w�@�0U@���@���@���@�N<@��@���@�M�@�&�@�  @���@��M@�;d@��M@���@��@���@�]d@��@��@�P�@�"�@��|@��!@�_@�l"@�R�@�e@��@���@�9�@�ѷ@��m@���@�(@���@��@�@�	�@���@��@���@�n/@�O@���@��@�i�@�:*@��@��@���@���@�e,@�+@���@��f@��M@��@���@��e@�V@�7�@�@��9@��C@���@�X@�F�@�C@��@�Z@�&�@�@� �@��>@��}@���@�=@�@���@��r@�l"@�0U@���@��^@�B�@�@@��|@�z@�e@��@���@�<6@��)@���@�Q�@�'R@�<�@��@���@��;@��=@�j@�A @�C@���@���@���@��@�`�@�C-@�4n@�M@��d@���@�o @�RT@�=�@�o@��U@���@�V@�:*@�@�x@�q@���@��@�;�@���@�l�@�RT@��@�֡@��h@��@���@���@�:*@�;@|�@~�c@~C�@~{@}�z@}c@|�E@|��@|M@|!@{��@{x@{U�@z��@z�L@zh
@zTa@z8�@z4@y��@y^�@x��@x��@x��@x>B@w]�@v��@v��@vJ�@u�@u�@uL�@t�/@t]d@t$@s�@s��@so�@s9�@r��@r�@r� @r��@rV@qԕ@qT�@p��@pXy@p�@o�F@n�@n��@n�A@nu@m�@m��@m��@mrG@m�@ll"@l	�@k��@k�[@ko�@j��@j��@jJ@i�>@iG�@h�|@hPH@g��@g�@f�A@e�>@e�X@e��@e�@eVm@e�@d|�@d�@c��@c\)@b�@bJ�@a�@a�'@ac@aA @`�E@`C-@_ƨ@_a@_�@^�m@^v�@^u@]f�@]?}@]�@\�@\�I@\oi@\PH@\@[��@[�@Z��@Z4@Y�3@Yzx@YT�@Y�@XtT@Xb@W�@W��@Wv`@Wb�@WJ#@W�@V��@U�^@U�"@U:�@T��@T��@T��@T��@TS�@S��@S)_@R�@R�@Q�z@Qx�@QX@Q(�@P�.@PA�@P@OdZ@OE9@O"�@N�!@NJ�@M��@M�@L�|@L��@L�@K�}@Ke�@K�@J�s@J��@Jd�@J#:@I�D@I�@I�@IY�@H�)@H�Y@H'R@H�@G�@G��@GK�@F�@F��@F�@E�@E�@E2a@D�@D~(@D@C�
@C�[@C��@Cn/@C(@B�6@Bi�@B\�@B�@B�@A�.@A�=@AN<@AL�@A5�@A�@@�j@@�Y@@C-@@	�@?��@?��@?��@?��@?J#@?�@>�+@>.�@=��@=�9@=��@=o @=O�@=�@<�U@<�@<�_@<h�@;�}@;b�@;F�@;'�@:��@:Ov@9��@9�@9��@9f�@9�@8Ɇ@8��@8l"@8]d@8Xy@8"h@7o�@7U�@74�@7"�@6ں@6�\@6W�@6@�@6�@5��@57L@4�@4e�@4V�@4H@4:�@4-�@4�@4�@3�	@3>�@2�}@2�@1�D@1��@1a�@18�@0�O@0��@09X@/�]@/ݘ@/�@/��@/�P@/~�@/b�@/�@.��@.~�@.�@-��@-��@-S&@-+�@,�E@,�z@,�Y@,]d@,(�@,�@+�}@+�:@+S�@+9�@+�@*�x@)�@)�-@)��@)\�@)(�@)V@(�@(�)@(��@(�@'��@'��@'J#@'"�@&��@&��@&i�@&-@&O@&�@&	@&�@%��@%�@%�"@%j@%4@%�@$��@$��@$�@$l"@$S�@$N�@$(�@#�W@#��@#_p@#W?@#P�@#@O@"�@"�b@"�+@"_�@"_@!��@!��@!Y�@!q@ �[@ �4@ PH@   @ƨ@{J@iD@Mj@1�@��@��@h
@GE@0U@��@��@�X@�"@Y�@��@��@�I@j@�@�m@� @��@a@e�@n/@n/@l�@RT@�@�2@�m@�@��@a|@�@�>@�-@��@hs@�@�@��@q@M@�@��@{J@6z@�h@��@��@�@�9@@�n@c@m]@?}@�@�@�.@bN@/�@x@��@�@��@��@��@��@��@��@j�@,�@�8@�R@_�@#:@��@�X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�jKA�kQA�v`A�|�A�{A�{A�|�AЀiA��A�jA�cTA�S&A�`vA�a�A�WsA�X�A��A�~A�A�	lA�	7A��A���A��"A���A��GA��A��QA�ԕA�ӏA��mA�ԕA��A��EAϻ�AϹ�A���A�7A��EA�oiA�?HA�A A��`A�7A�-�A���A���A��lA���A��_A�}�A���A�zxA�C�A��A���A��A�Z�A�d�A�8�A��A��6A�g�A���A�V�A��A�?A�p�A��A�+6A���A�6�A�L0A��>A�OvA��tA�T�A���A���A�ѷA�A���A��aA�2-A��A��]A�P�A��KA�0UA}/Az�~Av�?Asz�An��Ak?�Ah�Ag~�Ae��AbA]�A\AV�nAN��AJh�AH��AF��AC��A?}�A;��A8-A7�A7{�A5c�A4�A5A5�A4�wA3��A3�bA3k�A2a|A1A0(�A-(A+��A)�AA)>BA)6A)�MA)�A);A'=qA$'RA"[WA!��A!�A!�tAA��A4�A�,A�yA&�A'�A�rA�?A�"AxAMAC�A�A �A��A��AیA
�A��A�FAa|A��A��AX�AffA��AMA�uA��A7�A�^A�AMAa�A�A��A��A�SA33AJ�AqA��A3�A��A�-A}VA*�A
�bA
��A
iDA
oA	�A	v�A	5?A��A_�A$A��A�0A��A`BA�AN�A�'Al�A��A�FAOvA��AC-A�A��AB�A�TASA �A @��*@�b�@���@���@�K�@���@���@���@��@�Ft@��Z@�q@�q�@��@��?@�[W@�j@�*�@�:@�8@�0�@�%F@�@�Z@��@���@��@�~(@��@�Dg@썹@�=q@��g@�RT@���@�D@�H@���@�@�Ft@�p�@��@�{�@�%F@��@�ff@�/�@��T@�{@�2a@��@��@��@�`B@�&@�<@���@�-@��@�+@޳h@�oi@���@�A�@܏\@��d@�^�@�ߤ@څ�@�I�@���@�rG@تe@��}@�<6@��@֧�@�H@՜@�p;@���@��z@ӈf@�҉@��@�@O@ЦL@�K^@ψf@��@δ9@�2�@��@���@�\)@��?@�}V@��a@�g�@��@ʚ@�@ɠ�@�+@�l"@�
�@��W@ǡ�@�e�@�)_@��X@�kQ@���@�c@�C@��U@�	�@ï�@�s@�=@��f@ �@��#@��f@�
=@��4@�N�@���@��2@�a|@�e@�� @��{@�.I@��f@��@�>�@� i@���@��u@�1�@��K@���@�
=@���@�Q�@�$@���@�2a@��@��r@�{@���@���@��U@�_�@�E�@�	@��r@��@��6@���@��@��@���@�{@���@�k�@�Q@��@��q@�o@��@�w�@�a|@�M@�/�@�7�@���@�\�@�/�@��@�ی@��_@�W�@�7�@��@���@�(@�YK@���@��M@��@�R�@��@��@�s@�9�@��|@��}@�,=@��g@��@�/�@���@���@���@��r@��@�qv@�a�@�S&@�.I@���@��O@�u%@�1'@�E�@�O@�G@��N@�Q�@���@��<@���@�w�@�0U@���@���@���@�N<@��@���@�M�@�&�@�  @���@��M@�;d@��M@���@��@���@�]d@��@��@�P�@�"�@��|@��!@�_@�l"@�R�@�e@��@���@�9�@�ѷ@��m@���@�(@���@��@�@�	�@���@��@���@�n/@�O@���@��@�i�@�:*@��@��@���@���@�e,@�+@���@��f@��M@��@���@��e@�V@�7�@�@��9@��C@���@�X@�F�@�C@��@�Z@�&�@�@� �@��>@��}@���@�=@�@���@��r@�l"@�0U@���@��^@�B�@�@@��|@�z@�e@��@���@�<6@��)@���@�Q�@�'R@�<�@��@���@��;@��=@�j@�A @�C@���@���@���@��@�`�@�C-@�4n@�M@��d@���@�o @�RT@�=�@�o@��U@���@�V@�:*@�@�x@�q@���@��@�;�@���@�l�@�RT@��@�֡@��h@��@���@���@�:*@�;@|�@~�c@~C�@~{@}�z@}c@|�E@|��@|M@|!@{��@{x@{U�@z��@z�L@zh
@zTa@z8�@z4@y��@y^�@x��@x��@x��@x>B@w]�@v��@v��@vJ�@u�@u�@uL�@t�/@t]d@t$@s�@s��@so�@s9�@r��@r�@r� @r��@rV@qԕ@qT�@p��@pXy@p�@o�F@n�@n��@n�A@nu@m�@m��@m��@mrG@m�@ll"@l	�@k��@k�[@ko�@j��@j��@jJ@i�>@iG�@h�|@hPH@g��@g�@f�A@e�>@e�X@e��@e�@eVm@e�@d|�@d�@c��@c\)@b�@bJ�@a�@a�'@ac@aA @`�E@`C-@_ƨ@_a@_�@^�m@^v�@^u@]f�@]?}@]�@\�@\�I@\oi@\PH@\@[��@[�@Z��@Z4@Y�3@Yzx@YT�@Y�@XtT@Xb@W�@W��@Wv`@Wb�@WJ#@W�@V��@U�^@U�"@U:�@T��@T��@T��@T��@TS�@S��@S)_@R�@R�@Q�z@Qx�@QX@Q(�@P�.@PA�@P@OdZ@OE9@O"�@N�!@NJ�@M��@M�@L�|@L��@L�@K�}@Ke�@K�@J�s@J��@Jd�@J#:@I�D@I�@I�@IY�@H�)@H�Y@H'R@H�@G�@G��@GK�@F�@F��@F�@E�@E�@E2a@D�@D~(@D@C�
@C�[@C��@Cn/@C(@B�6@Bi�@B\�@B�@B�@A�.@A�=@AN<@AL�@A5�@A�@@�j@@�Y@@C-@@	�@?��@?��@?��@?��@?J#@?�@>�+@>.�@=��@=�9@=��@=o @=O�@=�@<�U@<�@<�_@<h�@;�}@;b�@;F�@;'�@:��@:Ov@9��@9�@9��@9f�@9�@8Ɇ@8��@8l"@8]d@8Xy@8"h@7o�@7U�@74�@7"�@6ں@6�\@6W�@6@�@6�@5��@57L@4�@4e�@4V�@4H@4:�@4-�@4�@4�@3�	@3>�@2�}@2�@1�D@1��@1a�@18�@0�O@0��@09X@/�]@/ݘ@/�@/��@/�P@/~�@/b�@/�@.��@.~�@.�@-��@-��@-S&@-+�@,�E@,�z@,�Y@,]d@,(�@,�@+�}@+�:@+S�@+9�@+�@*�x@)�@)�-@)��@)\�@)(�@)V@(�@(�)@(��@(�@'��@'��@'J#@'"�@&��@&��@&i�@&-@&O@&�@&	@&�@%��@%�@%�"@%j@%4@%�@$��@$��@$�@$l"@$S�@$N�@$(�@#�W@#��@#_p@#W?@#P�@#@O@"�@"�b@"�+@"_�@"_@!��@!��@!Y�@!q@ �[@ �4@ PH@   @ƨ@{J@iD@Mj@1�@��@��@h
@GE@0U@��@��@�X@�"@Y�@��@��@�I@j@�@�m@� @��@a@e�@n/@n/@l�@RT@�@�2@�m@�@��@a|@�@�>@�-@��@hs@�@�@��@q@M@�@��@{J@6z@�h@��@��@�@�9@@�n@c@m]@?}@�@�@�.@bN@/�@x@��@�@��@��@��@��@��@��@j�@,�@�8@�R@_�@#:@��@�X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	bB	HB	�B	�B	�B	�B	�B	�B	�B	�B	�B	pB	pB	<B	"B	VB	�B	�B	pB	VB	<B	6B	~B	0B	^B	
XB		�B	�B	EB	�B		7B	
�B	B	
�B		�B		�B�2B��B��B�B��B��B��B�B	l�B	�B	��B	��B	�;B	�MB	��B	� B	��B
B
.B
#�B
pB
)_B
r-B
��B
�B
�]B
��B�B
�B
ðB
�sB
�B
ބB
�[B
�B
�lB
��B
�B
�gB
�B
�0B
�aB
�[B
tB
_;B
F�B
#�B	�BB	ߊB	�aB	��B	�B	�*B	��B	�uB	��B	|6B	xB	m)B	i�B	q�B	lqB	\)B	K�B	=�B	!�B�B��B��B	B	B	 �B��B��BݲB�bB�B	  B	�B	�B	�B	B	�B	$tB	+�B	/iB	%�B	<B	�B�cB�QB�B	_B	gB	qB	kB	6B��B�BB		7B	+B	 BB	�B	*�B	7�B	HB	@�B	%�B	!�B	(�B	6`B	:�B	J	B	\�B	cnB	i�B	r�B	y>B	�B	��B	��B	�mB	�B	��B	�FB	�B	��B	�eB	�B	�B	�wB	��B	�KB	��B	�
B	��B	�UB	�B	��B	�LB	�XB	��B	��B	�B	��B	��B	�B	��B	�GB	��B	ƎB	�B	��B	ȀB	�B	�=B	ˬB	�VB	�pB	�VB	ΊB	ΥB	�BB	�B	ңB	�B	�[B	�uB	�oB	�TB	ӏB	�FB	�2B	�2B	��B	�MB	��B	ՁB	ԕB	ՁB	��B	�{B	��B	ּB	�2B	�mB	ּB	�mB	�
B	ּB	ևB	�B	�MB	�aB	ՁB	�uB	�aB	�B	�mB	�SB	�B	��B	�mB	֡B	�9B	յB	ևB	��B	�9B	��B	յB	յB	�SB	��B	��B	��B	ּB	ּB	�
B	��B	�
B	��B	��B	��B	��B	�B	�B	�EB	�+B	�yB	�B	�B	�1B	�B	�B	�KB	��B	�B	�kB	��B	�WB	�qB	��B	��B	�]B	�xB	�B	�/B	�IB	ݘB	��B	��B	�5B	ބB	�jB	�5B	޸B	�B	ߤB	ߤB	߾B	�'B	�vB	�BB	�\B	�B	��B	��B	�B	�NB	��B	�B	�B	��B	�B	�B	�B	�B	�ZB	��B	�,B	�`B	�B	�B	��B	�FB	�fB	�B	�B	�XB	��B	�B	�0B	��B	�B	�B	�B	��B	��B	��B	�qB	�6B	�B	��B	�"B	�=B	��B	�B	�B	��B	��B	�B	�]B	�)B	�B	�B	�OB	�B	�B	�iB	�B	�B	�UB	��B	�B	��B	�B	�vB	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�9B	�B	��B	��B	��B	��B	�9B	�3B	�hB	�aB	�aB	��B	�B	��B	��B	�?B	�B	�B	�B	�aB	�aB	�aB	�|B	�MB	�B	�ZB	�fB	��B	��B	�>B	�8B	�8B	�fB	�fB	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 4B
  B
 B
 OB
 �B
oB
B
B
�B
�B
gB
MB
�B
B
�B
�B
?B
�B
_B
�B
�B
�B
�B
�B
KB
fB
�B
�B
	lB

rB

�B

�B
^B
�B
JB
�B
�B
jB
jB
�B
�B
�B
oB
B
�B
$B
�B
�B
B
�B
�B
�B
�B
yB
_B
7B
)B
/B
~B
dB
~B
5B
5B
�B
B
!B
�B
�B
�B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!�B
!�B
!�B
"4B
"NB
"B
"4B
"B
"�B
"�B
"�B
"�B
#B
#�B
$ZB
$�B
%B
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
&2B
%�B
%`B
%zB
%�B
%`B
%`B
(�B
(�B
)�B
)�B
*B
*B
*KB
*B
*�B
*�B
+B
+�B
+�B
+�B
+�B
,�B
-B
-CB
-CB
-wB
-�B
.B
-�B
.B
.cB
.IB
./B
-�B
.}B
/OB
0!B
/�B
/OB
/iB
/�B
/OB
/B
/ B
/�B
0UB
0�B
1�B
1�B
2-B
2-B
2B
2GB
2�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
6�B
7B
6�B
7B
7LB
7fB
7�B
8B
88B
8�B
8�B
8�B
8�B
8�B
8�B
9	B
9�B
9�B
9�B
:*B
:xB
:�B
:�B
:�B
;dB
;dB
;B
;�B
;dB
;B
;B
;�B
<B
<�B
<�B
="B
="B
<�B
;dB
;�B
<B
<�B
=B
<�B
<�B
<�B
=B
=�B
>�B
>�B
>�B
?cB
?�B
?�B
?�B
?�B
@B
?�B
@iB
A B
A B
@�B
A;B
AUB
A�B
B�B
C-B
CaB
C�B
C�B
C�B
DB
DgB
E9B
EB
ESB
E9B
ESB
E�B
FB
FYB
F�B
F�B
GB
G_B
G�B
H�B
H�B
H�B
IB
IRB
IlB
IlB
I�B
J#B
JrB
J�B
K^B
KxB
K�B
K�B
K�B
LJB
L�B
L~B
L�B
L�B
L�B
L�B
L�B
M6B
M�B
M�B
N<B
NVB
NVB
N�B
NpB
N�B
OBB
O�B
O�B
O�B
P�B
P�B
P�B
Q B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
S&B
SB
S�B
TB
T{B
T�B
T�B
UB
U2B
UMB
UgB
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V�B
W?B
W�B
W�B
XB
W�B
XEB
X_B
XyB
X�B
X�B
Y1B
Y1B
YB
Y1B
YeB
Y�B
Y�B
Y�B
ZB
ZB
Y�B
Z7B
ZQB
Z7B
ZQB
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\CB
\CB
\)B
[�B
[�B
[�B
[�B
[�B
\)B
\]B
[�B
[�B
[�B
[�B
\B
\xB
\xB
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^B
^�B
^�B
_;B
_�B
`'B
aB
a|B
abB
a|B
a�B
a�B
a�B
a�B
bB
b4B
b�B
b�B
b�B
c B
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
d@B
d�B
ezB
ezB
e�B
f2B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
h>B
h>B
h�B
h�B
iDB
iDB
i_B
jB
j�B
j�B
j�B
j�B
kB
kB
kkB
kkB
lB
l�B
l�B
l�B
m)B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
n�B
o5B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
p!B
poB
pUB
pUB
pUB
p�B
p�B
p�B
p�B
qAB
q'B
qAB
q'B
qB
p�B
qAB
q[B
qvB
q�B
r-B
r�B
r�B
r�B
sMB
s3B
shB
sMB
shB
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
vFB
v+B
v+B
vB
vB
v+B
v`B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
xB
xB
xRB
x�B
x�B
y	B
y	B
yXB
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
{B
{0B
{B
{�B
{�B
|B
|6B
|6B
|6B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}�B
}�B
~B
~]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	}B	HB	�B	�B	�B	B	�B	�B	�B	�B	�B	pB	�B	pB	VB	�B	�B	�B	�B	pB	VB	PB	�B	JB	xB	
�B		�B	�B	_B	�B		RB	
�B	^B	^B	
�B	�B��B��B��B�yB�IB��B�	B��B	l�B	��B	�KB	��B	��B	�B	��B	�B	�B
�B
hB
%`B
!bB
,�B
v�B
��B
��B
�B
�?B�B
�yB
ȚB
�B
�,B
��B
�kB
͹B
�PB
ʦB
�hB
ƨB
�cB
��B
��B
�SB
x�B
eB
NB
+QB
�B	�:B	�B	��B	��B	�B	�zB	�B	�VB	��B	{�B	pUB	k�B	t�B	q'B	aB	O�B	D�B	)�B	 �B��B��B	uB	FB	�B�$B��B��B�TBڠB	  B	 B	eB	�B	�B	VB	%�B	-]B	1�B	)*B	HB	�B� B�B�B	1B	SB	5B	�B	BB	 �B�.B		�B	-�B	"NB	�B	*eB	8�B	J�B	C�B	&�B	#B	*0B	7LB	:�B	J	B	]B	c�B	i�B	r�B	yXB	�B	�(B	��B	��B	��B	�IB	�2B	�>B	��B	�QB	��B	�IB	�B	�wB	�6B	��B	�>B	�B	��B	�|B	�?B	�B	��B	�0B	��B	��B	��B	�B	��B	� B	��B	�EB	��B	ǔB	�fB	��B	�lB	��B	�JB	οB	��B	ΊB	��B	�(B	�B	��B	ӏB	�uB	�,B	�B	�B	�&B	�,B	��B	��B	յB	յB	�SB	ևB	�B	��B	��B	֡B	��B	ևB	��B	�9B	�
B	�?B	��B	�sB	�YB	�
B	֡B	�B	�MB	�B	��B	��B	�SB	ևB	ևB	�mB	�$B	ּB	��B	֡B	�mB	�
B	�SB	ּB	�9B	�B	�B	ּB	�$B	�
B	�$B	�?B	׍B	׍B	�YB	׍B	��B	�EB	�EB	�+B	�_B	�_B	ؓB	ؓB	��B	�B	ٴB	�eB	��B	ٴB	ٚB	�7B	چB	��B	�#B	��B	��B	�qB	�]B	ܬB	��B	�dB	�dB	ݘB	�5B	ބB	�jB	ޞB	��B	��B	ޞB	�VB	߾B	��B	��B	�B	��B	�-B	��B	��B	��B	�bB	�4B	�hB	�B	�B	��B	�B	�TB	�TB	�:B	� B	�tB	�B	�,B	�B	��B	�zB	��B	��B	�B	�B	�B	�B	�B	�*B	�B	�B	�QB	�B	�WB	��B	�B	�)B	�CB	��B	�B	�=B	�=B	�qB	��B	�]B	��B	��B	�/B	�cB	��B	��B	��B	�B	��B	�B	�OB	�B	��B	�B	�B	�B	��B	�AB	�[B	��B	��B	�B	�B	�B	�-B	�aB	�GB	�B	��B	�GB	�3B	�B	�nB	��B	�B	�?B	�%B	�B	��B	��B	�B	��B	��B	�3B	�B	��B	��B	��B	��B	��B	�aB	�B	�|B	�B	�B	�B	�B	��B	��B	�>B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	�*B	��B	�0B	�B	��B	�"B	��B	��B	�B	��B	�BB
 4B
 OB
 4B
 OB
 �B
B
�B
AB
B
�B
B
�B
�B
B
9B
�B
�B
tB
B
�B
B
�B
�B
B
1B
fB
�B
�B
�B
	�B

�B

�B
B
xB
�B
~B
6B
�B
�B
�B
B
�B
�B
�B
gB
B
sB
+B
_B
�B
KB
7B
1B
B
�B
_B
QB
xB
IB
�B
�B
�B
jB
�B
�B
;B
VB
�B
�B
 BB
 vB
 �B
 �B
 �B
!B
 �B
!B
!B
 �B
!bB
!�B
"4B
"B
"NB
"�B
"hB
"hB
"�B
"�B
"�B
#B
#B
#:B
$B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
&2B
&LB
&�B
&�B
&�B
'B
'B
&fB
&LB
%�B
%�B
%�B
%�B
%`B
(�B
)B
)�B
)�B
*KB
*0B
*eB
*�B
+B
*�B
+B
+�B
,B
+�B
,"B
,�B
-CB
-wB
-]B
-�B
.IB
.IB
.IB
.IB
.�B
.�B
.}B
.IB
.�B
/�B
0�B
0;B
/�B
/�B
/�B
/iB
/OB
/B
/�B
0oB
0�B
2B
2-B
2|B
2|B
2-B
2|B
3B
49B
4�B
4�B
4�B
5B
5%B
5B
7B
7LB
7B
72B
7�B
7�B
7�B
88B
8RB
8�B
8�B
8�B
9	B
9	B
9$B
9$B
9�B
9�B
:B
:^B
:�B
:�B
;B
;B
;�B
;�B
;�B
;�B
;B
;0B
;�B
<6B
<jB
<�B
=B
=<B
=qB
<�B
;�B
;�B
<PB
=B
=<B
=B
="B
="B
=VB
>B
>�B
?B
?B
?�B
@B
@4B
@ B
@4B
@OB
@ B
@�B
AUB
AoB
A B
AUB
AUB
A�B
B�B
CGB
C�B
DB
DB
D3B
DMB
D�B
E�B
E9B
EmB
EmB
E�B
E�B
F?B
F�B
F�B
F�B
GEB
G�B
H1B
H�B
H�B
IB
I7B
I�B
I�B
I�B
I�B
JXB
J�B
J�B
KxB
K�B
K�B
K�B
LJB
LdB
L�B
L�B
L�B
MB
MB
MB
MB
M�B
NB
N"B
NVB
NVB
NpB
N�B
N�B
N�B
O�B
O�B
O�B
PHB
P�B
Q B
QB
QNB
Q�B
Q�B
Q�B
RB
Q�B
RoB
R�B
R�B
S@B
SuB
T,B
TaB
T�B
T�B
T�B
U2B
UMB
UgB
U�B
U�B
U�B
U�B
VB
VSB
VmB
V�B
V�B
V�B
V�B
W$B
WsB
W�B
W�B
X+B
XB
XyB
X�B
X�B
X�B
Y1B
YKB
YKB
Y1B
YeB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZQB
ZQB
ZQB
ZkB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[	B
[=B
[=B
[�B
[�B
[�B
[�B
[�B
\B
\CB
\]B
\]B
\]B
\B
\)B
\B
\B
[�B
\]B
\�B
\B
[�B
[�B
\B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
^OB
^�B
_B
_�B
_�B
`\B
a-B
a|B
a|B
a|B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
d@B
dZB
eB
e�B
e�B
f2B
fLB
f�B
f�B
gB
gRB
g�B
g�B
g�B
hsB
hsB
h�B
iB
iyB
i_B
i�B
jKB
kB
j�B
kB
kB
k6B
kQB
k�B
k�B
lWB
l�B
l�B
mB
m]B
mwB
m�B
mwB
m�B
m�B
m�B
m�B
nB
n/B
nB
n�B
n�B
n�B
oB
o B
oOB
o�B
oiB
o�B
o�B
o�B
o�B
pB
p;B
poB
pUB
poB
poB
p�B
qB
p�B
p�B
q[B
q[B
qvB
qAB
q'B
p�B
q[B
q�B
q�B
q�B
r-B
r�B
r�B
sB
shB
shB
s�B
shB
s�B
s�B
s�B
s�B
s�B
t9B
t�B
t�B
uB
u?B
u�B
u�B
u�B
vB
vFB
v+B
v+B
v+B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
y	B
y$B
y$B
y�B
y�B
zB
zB
y�B
zDB
z�B
z�B
z�B
z�B
{JB
{JB
{�B
{�B
|B
|6B
|6B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}VB
}�B
}�B
~BB
~]33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006190052282020061900522820200619005228202006190201032020061902010320200619020103202207271538322022072715383220220727153832  JA  ARFMdecpA30a                                                                20200607123804  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200607123805  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200607123807  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200607123807  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200607123808  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200607123808  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200607123808                      G�O�G�O�G�O�                JA  ARUP                                                                        20200607125404                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200609000000  CF  PSAL_ADJUSTED_QC@8Q�@~�RG�O�                JM  ARCAJMQC2.0                                                                 20200618155228  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200618155228  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200618170103  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063832  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                