CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-30T09:39:23Z creation;2019-10-30T09:39:25Z conversion to V3.1;2022-08-02T05:11:53Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191030093923  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_027                    2C  D   APEX                            8420                            2.11.2                          846 @���π1   @���k$�@/��҈��cx����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Br��BxffB��B���B�  B�ffB�33B���B�  B�  B���B�  B���B�33B�ffB�  B�  B�  B�  B�33B���B�ffB�  B�  B�  B�  B�  B�33B�ffB�ffB���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @;�@|(�@�Q�@�ffA33A?\)A_
=A�A��
A��A���A��A�  A��A�A���B�B  B  B�HB'�
B/�HB7�
B?��BG��BO�BW�B_�HBh  Br�\BxQ�BffB��B��B�\)B�33B���B��)B��)B�B��fB���B�8RB�ffB�B�  B�
=B��)B�(�Bǳ3B�W
B��)B��B��HB��fB�B��B�L�B�L�B�B��)B��HB��B���C��C�qC�RC�3C	�RC��C�RC�qC�3C�C��C��C��C\C�C�3C!�RC#�RC%��C'��C)�RC+�RC-�3C/�C1��C3��C5�RC7��C9�3C;�C=�C?�3CA��CC��CE��CG�CI�CK��CM��CO�RCQ�qCS�qCVCW�3CY�3C[��C]��C_��Ca�qCc�RCe�3Cg��Ci�RCk�3Cm�RCo�RCq��Cs�RCu��Cw��Cy�3C{�C}��C��C��qC��)C��)C���C��RC��
C��RC���C��)C��)C���C���C��)C��)C�HC�HC��qC���C��qC�  C��qC���C���C���C���C���C���C���C�  C���C���C��)C�  C��qC��)C��)C��C��C���C���C��qC��)C���C��qC�  C��)C��)C�  C���C��qC���C���C��qC�  C���C���C���C���C���C���C���C��)C���C���C��qC���C���C��)C��)C��RC���C�HC�  C���C���C��qC���C��RC���C��RC��RC���C��qC���C���C��qC���C���C��
C��RC��)C��qC���C��qC���C��
C���C�HC�  C��)C��qC�  C��qC���C��RC��RC��
C��RC���C��)C��qC���C���C���C���C�  C��)C���C��)C�  C��)C���C���C���C�  C��C��C��qD }qD �qD~�D �D~D�)D|)D��D~�D�\D~D�D~D�qD\D  D~�D��D	~�D	�)D
|)D
�D~�D�\D~�D��D|�D�D\D��D~D�\D\D��D}qD��D}qD�qD~D��D� D�D|�D�qD|�D�qD~D��D\D�)D|�D�qD~DHD\D�D}qD�)D|)D��D~�D�\D~D�D ~D ��D!\D!�qD"~D#HD#��D#�D${�D$��D%{�D%�)D&|)D&��D'|�D'�qD(\D(�\D)� D* �D*~D*��D+|)D+��D,|�D,��D-~�D-�\D.~�D.�D/|�D/��D0}qD0�)D1|�D1�\D2\D2�D3}qD3�qD4}qD4�qD5~�D6  D6\D6�qD7}qD8  D8}qD8��D9|)D9��D:|)D:��D;\D;�\D<��D<�\D=}qD=��D>~D>�qD?��D@HD@� D@�\DA}qDA�qDB|�DB�qDC~�DC��DD��DE  DE\DF �DF~�DF�DG~DG�\DH|�DH��DI~�DI��DJ|�DJ�qDK}qDK�qDL~DL��DM~DN  DN~�DN��DO�HDP  DP}qDP�DQ~�DQ�qDR|)DR�DS}qDS��DT\DT�DU}qDU�qDV~DV��DW}qDW�DX~DX�DY~�DY�qDZ|)DZ�D[~D[�qD\~D\��D]~D]��D^|)D^�)D_|�D_��D`}qD`�Da~Da��Db�HDb��Dc}qDc�\Dd~�Dd��De\Df  Df~Df��Dg~Dg��Dh\Dh�Di}qDi�qDj|�Dj�qDk}qDk�qDl|�Dl�qDm~Dm�qDn}qDn�qDo|�Do��Dp}qDp�qDq~Dq��Dr� Dr��Ds|)Ds�Dt\Dt�qDu|�Du�)Dv|�Dv�qDw~Dw�\Dx~Dx�Dy~�Dz  Dz\D{  D{�HD{�\D|\D|�D}}qD}��D~|�D~�qD~�D�\D�?
D�~fD��
D���D�?
D��D��
D��D�=�D�~�D���D�  D�?
D�~�D��\D��\D�?
D�~D��fD��
D�>�D�\D��\D��fD�>D�
D���D���D�?
D��D��\D��D�?
D��D��fD���D�?
D�~D��
D��
D�>D�
D��RD��
D�=�D�~D��
D��
D�?
D�~�D���D��
D�>fD�\D��RD��\D�>�D�~�D��fD��D�>D�}�D��
D��
D�>fD�
D��\D��fD�=�D�
D�� D��
D�>fD�
D��fD��qD�>fD�~D���D��fD�?
D�~�D��D���D�?\D�\D��\D��
D�>�D�� D���D��\D�?�D�~D��qD���D�?�D�
D���D��D�?�D�\D��
D�  D�?
D�~D��qD��
D�?�D�� D�� D��
D�>�D�~�D��fD��fD�>fD�~�D��\D��
D�>�D�
D��\D���D�>�D�~fD��
D���D�>�D�~fD��
D��\D�?
D�~�D��\D���D�?
D�~�D��D��
D�?
D�~fD���D��fD�?
D��D�� D�  D�?�D�
D��\D��
D�?
D�~fD���D���D�>fD�
D��\D��
D�?\D��D��RD���D�>�D�
D��RD��
D�>D�
D��\D��D�>�D�\D��
D��
D�?\D�� D��
D��fD�?\D��D���D��fD�>D�\D���D� �D�@RD�
D��\D��
D�?
D�\D�� D� RD�@�D��RD���D���D�>�D�}�D���D��\D�>�D�~fD��D��fD�?
D�~�D��fD���D�?�D��RD���D���D�>�D�~D��D��fD�?
D�
D��\D��\D�>fD�~fD�� D��\D�>fD�
D��
D��
D�@ D��D��\D���D�?\D��RD��RD��\D�>�D�
D��fD���D�?\D��D��
D��\D�?�D�� D��
D���D�>fD�~fD�� D�  D�?\D�
D��\D��
D�?
D��D��
D��
D�?
D�
D��RD��
D�>fD�\D�� D��\D�?
D�~�DĽqD���D�?�Dŀ D���D���D�?
D�
DƾfD��fD�>�D�\Dǿ
D���D�?\D�\Dȿ�D�  D�@ D��Dɿ�D��\D�?
D�~�Dʿ\D���D�?\D��D˿
D��
D�?\D�
D̿\D��
D�?
D��DͿ\D���D�?�D�
Dο\D���D�@ D��DϿ�D���D�>�D�\D���D��\D�?\D��DѾfD���D�?\D��D�� D��
D�>�D��Dӿ\D��D�>�D��DԾ�D���D�>�D�
Dտ�D��
D�>fD��Dֿ\D��\D�?�D�~�D׿\D���D�>�D�}qDؾD���D�>�D�
Dٿ�D��
D�>fD�\Dڿ\D��
D�>�D�
Dۿ
D��fD�>fD�
D��RD�  D�?�D�\Dݿ\D���D�@ Dހ D޿\D��
D�?\D�
D߿�D���D�?
D�~fD�fD���D�?�D� D�� D���D�>�D�~fD�D��fD�?\D�\D�
D��\D�?\D�\D修D��
D�>fD�
D徸D���D�?\D�\D�� D�  D�>�D��D翮D��fD�?
D�~fD辸D��
D�>fD�~fD龸D��fD�>�D�~�D�D��
D�>�D�}qD�D���D�>fD�~�D쿮D���D�>�D�~�D��
D���D�?\D� D��RD���D�=�D�~�D￮D��\D�>�D�~�D�\D��fD�=�D�~�D�D��fD�>fD�~D�D���D�?
D�\D�� D��\D�?�D�D�� D��\D�>�D�~D��qD���D�@ D��D��fD��\D�@�D��D��
D��
D�>fD�~fD��fD���D�?\D�~fD��qD��fD�?\D�� D��
D��D�>�D�~fD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�P}A�(XA߭�A߉�A߀ A�}�A��A߀4A�}�A�z�A�{A�|PA�|A�|A�}�A߁A߃�A߃GA߃{A߁A�}"A�{�A�z�A�{�A�y�A�yrA�sMA�R A�ںAܛqA�~A�<A�`BA�}"AΟ!A��A�	A��]Aǃ{A�;�A���A���A��A��3A���A���A�D�A�ZA��A��A�K^A�R A�!�A���A�m)A���A���A�0�A�	A�]�A�_;A���A��A��A�yrA��zA��OA��$A~��A~A|OAz�Aw��Ar�VAo&�AmK^Aj�$AfDgAd:�A`�)A]�AZ�4AXU�AU�oAP�4AL�RAJ8�AE�DAC"�AB`BA?��A> iA<`BA9�A7ffA5�A3/�A1��A0��A-�	A,�A+�HA*��A(�zA(�{A(A A'��A'bA&&A%xA#�5A#qvA#	A"��A"�A!?�A �A �A ?}A��A~�A�A�/A&�A�AAw�A��A$�A��Ae�A��A|�A��AZ�A��A1A��A"�A��A�A33A_pA�A�VA_�A�A�)A��A��A��Ah
A/�A iA��AS�A�A"hA�tA{JA�A��AϫA�eA��A]�A�rA�A
�A	g8A��A�A��A�9AA�A�$A�DA7�A��A_�A\�AQ�A:*AL0A�WA��A7�A��A��A4A��A��A@A ĜA M�@�W?@��e@�V@�?@���@���@�X@��@�!@�?}@�K^@��q@�Mj@���@�>B@��0@�RT@���@�ƨ@�A�@�%@���@�@�1@�u@�y>@��@�@�u�@��@�c�@��A@�o @�Y@�m�@�k�@�1�@颜@�@��@�'�@��y@濱@�|@��#@�0�@��@��+@�>�@���@�g�@��@�U�@�;@ܷ�@�!@��@�v�@��@�^�@��v@��@�j�@��@�1�@Ջ�@�q@ԑ�@�J@�X@���@ҕ�@��@ѡ�@�&@�_@���@���@ρ@Σ�@�A�@�!@͗$@�33@�kQ@˘�@�8�@ʿ�@��@�w�@�+k@�n/@�<6@Ȟ@ș1@�]d@�;�@�$�@���@ǒ:@�l�@� i@�H�@���@őh@�8@���@���@�z�@��A@�u�@���@®}@�?@��@��A@��>@��j@�ϫ@���@��X@�}�@�v`@�G�@���@��@��0@���@�Z@�7�@�4@��#@��a@�n/@��_@�c @�/�@��@��4@��@�@���@���@�"�@���@�@���@�͟@��j@�+�@���@�G�@���@�^5@�4@���@���@�_�@�
�@���@��~@�e,@�
=@���@�ݘ@�w2@��@��@��L@�oi@�)�@��H@�W?@���@���@��.@�`�@��d@�k�@�O@�;d@�@���@���@��o@���@��~@�[W@�ȴ@�V@�/�@��N@��@�oi@��@�  @���@��t@�^�@��@�O@��k@�W?@�(@��P@���@�S�@�~@�~@��@��@�	@��@��@�@���@�x@�)_@���@��b@�~(@�?@��@��6@��@���@�^�@�0�@�S@��X@���@�7�@��6@�RT@�S&@�A @�)_@�
=@���@���@�C-@��@�u@��@���@�m]@�@@���@�^5@��@��F@��=@�Vm@���@�J�@��@�'�@��@�4n@���@��w@���@���@���@��@�K�@�,�@�@�S@���@���@�u�@� �@��9@�s�@�E9@�1�@��@���@�Z�@�G@��#@��h@�%@��U@�� @�i�@�c @�3�@��@��j@���@�IR@�V@�ں@��@�,=@��]@��0@���@�[W@���@���@�u%@�i�@�>B@��@���@�U�@��@���@��z@�z@�\�@�e@���@�v`@�@O@�@���@��@�}�@�Q�@��	@��B@���@�>B@�%�@��@��
@��-@��q@��C@���@�n/@�-w@�ں@��4@��D@�GE@�@��&@��}@��6@�خ@��@�ƨ@��*@�S�@��@��4@�PH@�1�@��@�1@���@��@�ƨ@���@���@���@�W?@��,@�;�@� �@��@��@�@@�@~6�@}c@|��@|bN@|PH@|Xy@{�q@{\)@z��@z��@z~�@z�@y�@y�C@ys�@y0�@y�@x�I@x>B@x�@w��@w>�@vOv@u`B@t��@t1@s�&@s˒@sqv@rs�@q��@q@pѷ@p�j@p��@pm�@p'R@o��@o!-@nB[@n@m��@m@m�S@m(�@l�@l��@k��@kj�@k)_@j��@j��@i�)@i��@h��@h�Y@h<�@gj�@f�y@f�1@fp;@f$�@e�@eB�@d�@c��@c8@b�H@b�@a��@a\�@`�@_��@_Y@^?@^_@]�@]��@]m]@]B�@\��@\��@\C-@[�6@[t�@[E9@[8@['�@[o@Z��@Z4@Ym]@X��@W�@W�K@We�@W�@V��@V\�@V_@U�@U`B@T��@T�@TS�@S��@Sx@SRT@S�@R�@R��@Q�#@QS&@Q+@P��@P'R@Oƨ@O��@Oqv@OZ�@O33@O@N��@N�+@NQ@M��@L�@L�e@Lq@K�@K��@KRT@K'�@J��@J�A@JM�@J;�@J�@I�.@I��@I��@I�=@IVm@H��@H��@H�o@H2�@G�K@Gb�@F�@F��@F��@Fi�@F^5@FB[@F�@E�H@Ej@E*0@E@D��@D�.@DC-@DM@Cƨ@C��@CX�@C6z@B�]@B_�@B�@A�@A}�@ADg@@�[@@  @?�@?��@?Z�@?33@>�@>�<@>�@>�x@>l�@>Q@>�@=��@= \@<��@<"h@;��@;��@;=@:��@:�L@:��@:H�@9�j@9��@9x�@9(�@8�?@8u�@8A�@8	�@7��@7P�@7$t@6�"@6�<@6R�@6?@5�D@5��@5�@5�@4�/@4֡@4��@4Q�@3��@3.I@2�@2�]@2��@2��@2�\@2�@2s�@1�z@1f�@0��@0��@0�u@0Q�@0%�@/�;@/x@.��@.5?@-�@-�h@-%@,��@,N�@+ƨ@+1�@*�@*��@*W�@*_@)��@)�@)J�@(�5@(�O@(Z@'��@'�K@'e�@'Mj@'/�@&�@&�F@&i�@&�@%@%|@%#�@$��@$��@$PH@$>B@$�@#�W@#�f@#s@#dZ@#"�@"��@"c @"-@!�>@!��@!�n@!��@!�M@!\�@!(�@!�@ ѷ@ �z@ M@ %�@ 	�@��@=@�@S@�"@��@ں@��@��@@�@��@7L@�	@�e@bN@c�@*�@b@��@��@�{@�k@a@�@�r@~�@@�@3�@
�@�)@ԕ@\�@��@��@��@�@�Y@S�@(�@�g@�@�@ �@"h@M@ƨ@iD@4�@��@�<@�b@Q@5?@#:@�@��@k�@&�@�@Ɇ@�9@z�@H@6@	�@�A@�@�Q@��@@O@�@�y@��@�b@��@Q@	@�)@�j@�-@rG@k�@k�@hs@[W@:�@+@ی@��@�@�u@�.@�D@�D@�@tT@`�@M@��@��@v`@K�@H�@C�@A�@;d@+@�@ߤ@͟@�X@�<@�6@s�@d�@^5@Ta@H�@;�@-@	@@@�>@��@�t@��@�"@e,@?}@�`@��@��@��@��@��@q@Q�@9X@~@��@��@��@�f@o�@a@O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�P}A�(XA߭�A߉�A߀ A�}�A��A߀4A�}�A�z�A�{A�|PA�|A�|A�}�A߁A߃�A߃GA߃{A߁A�}"A�{�A�z�A�{�A�y�A�yrA�sMA�R A�ںAܛqA�~A�<A�`BA�}"AΟ!A��A�	A��]Aǃ{A�;�A���A���A��A��3A���A���A�D�A�ZA��A��A�K^A�R A�!�A���A�m)A���A���A�0�A�	A�]�A�_;A���A��A��A�yrA��zA��OA��$A~��A~A|OAz�Aw��Ar�VAo&�AmK^Aj�$AfDgAd:�A`�)A]�AZ�4AXU�AU�oAP�4AL�RAJ8�AE�DAC"�AB`BA?��A> iA<`BA9�A7ffA5�A3/�A1��A0��A-�	A,�A+�HA*��A(�zA(�{A(A A'��A'bA&&A%xA#�5A#qvA#	A"��A"�A!?�A �A �A ?}A��A~�A�A�/A&�A�AAw�A��A$�A��Ae�A��A|�A��AZ�A��A1A��A"�A��A�A33A_pA�A�VA_�A�A�)A��A��A��Ah
A/�A iA��AS�A�A"hA�tA{JA�A��AϫA�eA��A]�A�rA�A
�A	g8A��A�A��A�9AA�A�$A�DA7�A��A_�A\�AQ�A:*AL0A�WA��A7�A��A��A4A��A��A@A ĜA M�@�W?@��e@�V@�?@���@���@�X@��@�!@�?}@�K^@��q@�Mj@���@�>B@��0@�RT@���@�ƨ@�A�@�%@���@�@�1@�u@�y>@��@�@�u�@��@�c�@��A@�o @�Y@�m�@�k�@�1�@颜@�@��@�'�@��y@濱@�|@��#@�0�@��@��+@�>�@���@�g�@��@�U�@�;@ܷ�@�!@��@�v�@��@�^�@��v@��@�j�@��@�1�@Ջ�@�q@ԑ�@�J@�X@���@ҕ�@��@ѡ�@�&@�_@���@���@ρ@Σ�@�A�@�!@͗$@�33@�kQ@˘�@�8�@ʿ�@��@�w�@�+k@�n/@�<6@Ȟ@ș1@�]d@�;�@�$�@���@ǒ:@�l�@� i@�H�@���@őh@�8@���@���@�z�@��A@�u�@���@®}@�?@��@��A@��>@��j@�ϫ@���@��X@�}�@�v`@�G�@���@��@��0@���@�Z@�7�@�4@��#@��a@�n/@��_@�c @�/�@��@��4@��@�@���@���@�"�@���@�@���@�͟@��j@�+�@���@�G�@���@�^5@�4@���@���@�_�@�
�@���@��~@�e,@�
=@���@�ݘ@�w2@��@��@��L@�oi@�)�@��H@�W?@���@���@��.@�`�@��d@�k�@�O@�;d@�@���@���@��o@���@��~@�[W@�ȴ@�V@�/�@��N@��@�oi@��@�  @���@��t@�^�@��@�O@��k@�W?@�(@��P@���@�S�@�~@�~@��@��@�	@��@��@�@���@�x@�)_@���@��b@�~(@�?@��@��6@��@���@�^�@�0�@�S@��X@���@�7�@��6@�RT@�S&@�A @�)_@�
=@���@���@�C-@��@�u@��@���@�m]@�@@���@�^5@��@��F@��=@�Vm@���@�J�@��@�'�@��@�4n@���@��w@���@���@���@��@�K�@�,�@�@�S@���@���@�u�@� �@��9@�s�@�E9@�1�@��@���@�Z�@�G@��#@��h@�%@��U@�� @�i�@�c @�3�@��@��j@���@�IR@�V@�ں@��@�,=@��]@��0@���@�[W@���@���@�u%@�i�@�>B@��@���@�U�@��@���@��z@�z@�\�@�e@���@�v`@�@O@�@���@��@�}�@�Q�@��	@��B@���@�>B@�%�@��@��
@��-@��q@��C@���@�n/@�-w@�ں@��4@��D@�GE@�@��&@��}@��6@�خ@��@�ƨ@��*@�S�@��@��4@�PH@�1�@��@�1@���@��@�ƨ@���@���@���@�W?@��,@�;�@� �@��@��@�@@�@~6�@}c@|��@|bN@|PH@|Xy@{�q@{\)@z��@z��@z~�@z�@y�@y�C@ys�@y0�@y�@x�I@x>B@x�@w��@w>�@vOv@u`B@t��@t1@s�&@s˒@sqv@rs�@q��@q@pѷ@p�j@p��@pm�@p'R@o��@o!-@nB[@n@m��@m@m�S@m(�@l�@l��@k��@kj�@k)_@j��@j��@i�)@i��@h��@h�Y@h<�@gj�@f�y@f�1@fp;@f$�@e�@eB�@d�@c��@c8@b�H@b�@a��@a\�@`�@_��@_Y@^?@^_@]�@]��@]m]@]B�@\��@\��@\C-@[�6@[t�@[E9@[8@['�@[o@Z��@Z4@Ym]@X��@W�@W�K@We�@W�@V��@V\�@V_@U�@U`B@T��@T�@TS�@S��@Sx@SRT@S�@R�@R��@Q�#@QS&@Q+@P��@P'R@Oƨ@O��@Oqv@OZ�@O33@O@N��@N�+@NQ@M��@L�@L�e@Lq@K�@K��@KRT@K'�@J��@J�A@JM�@J;�@J�@I�.@I��@I��@I�=@IVm@H��@H��@H�o@H2�@G�K@Gb�@F�@F��@F��@Fi�@F^5@FB[@F�@E�H@Ej@E*0@E@D��@D�.@DC-@DM@Cƨ@C��@CX�@C6z@B�]@B_�@B�@A�@A}�@ADg@@�[@@  @?�@?��@?Z�@?33@>�@>�<@>�@>�x@>l�@>Q@>�@=��@= \@<��@<"h@;��@;��@;=@:��@:�L@:��@:H�@9�j@9��@9x�@9(�@8�?@8u�@8A�@8	�@7��@7P�@7$t@6�"@6�<@6R�@6?@5�D@5��@5�@5�@4�/@4֡@4��@4Q�@3��@3.I@2�@2�]@2��@2��@2�\@2�@2s�@1�z@1f�@0��@0��@0�u@0Q�@0%�@/�;@/x@.��@.5?@-�@-�h@-%@,��@,N�@+ƨ@+1�@*�@*��@*W�@*_@)��@)�@)J�@(�5@(�O@(Z@'��@'�K@'e�@'Mj@'/�@&�@&�F@&i�@&�@%@%|@%#�@$��@$��@$PH@$>B@$�@#�W@#�f@#s@#dZ@#"�@"��@"c @"-@!�>@!��@!�n@!��@!�M@!\�@!(�@!�@ ѷ@ �z@ M@ %�@ 	�@��@=@�@S@�"@��@ں@��@��@@�@��@7L@�	@�e@bN@c�@*�@b@��@��@�{@�k@a@�@�r@~�@@�@3�@
�@�)@ԕ@\�@��@��@��@�@�Y@S�@(�@�g@�@�@ �@"h@M@ƨ@iD@4�@��@�<@�b@Q@5?@#:@�@��@k�@&�@�@Ɇ@�9@z�@H@6@	�@�A@�@�Q@��@@O@�@�y@��@�b@��@Q@	@�)@�j@�-@rG@k�@k�@hs@[W@:�@+@ی@��@�@�u@�.@�D@�D@�@tT@`�@M@��@��@v`@K�@H�@C�@A�@;d@+@�@ߤ@͟@�X@�<@�6@s�@d�@^5@Ta@H�@;�@-@	@@@�>@��@�t@��@�"@e,@?}@�`@��@��@��@��@��@q@Q�@9X@~@��@��@��@�f@o�@a@O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	p�B	p�B	vB	x�B	y�B	{dB	|�B	~wB	~�B	~�B	cB	�iB	��B	�B	�B	�GB	��B	��B	��B	�EB	��B	��B	�	B	��B	��B	��B	��B	�B	�B	��B	��B	�#B	}�B	�;B	��B	�B	ɺB	ЗB	�vB	��B
1�B
G�B
Q�B
PB
RB
J	B
8B
%,B
-�B
)�B
�B
B
 �B
�B
aB
�B
-CB
'�B
1�B
!�B
"4B
,B
B	��B	�8B	�$B	�B	��B	�cB	�B	��B	�B	�B	s�B	`BB	RTB	DB	-�B	 �B	[B	B�]B�dB�B�B�uBňB��B��B��B��B�B��B�B��B� B�,B�fB��B��B��B��B�8B�aB��B��B�RBʌB��B��B��B��B��B	�B	�B	�B	�B	�B	CB	!�B	$tB	/5B	;B	H�B	J=B	J�B	L~B	R�B	[�B	i�B	q�B	�SB	��B	�pB	�SB	��B	�B	��B	�@B	��B	�B	�cB	��B	�B	�-B	�B	��B	̳B	̈́B	˒B	��B	��B	B	��B	�cB	��B	�B	�%B	��B	ǔB	ʌB	̘B	�jB	�jB	͹B	͟B	͟B	�jB	�dB	��B	�PB	��B	�vB	�\B	ΥB	οB	�TB	�TB	�B	�,B	ּB	��B	�WB	ߤB	�pB	ޞB	��B	ٴB	�B	�9B	յB	ԯB	�aB	ԯB	ՁB	ՁB	�gB	յB	�MB	ԯB	�B	��B	өB	�@B	�B	��B	�,B	�B	�,B	�FB	�,B	�B	��B	ՁB	�B	�gB	ՁB	�gB	�gB	�2B	��B	�B	��B	ՁB	��B	ՁB	յB	յB	՛B	�MB	�MB	�B	ՁB	��B	�?B	��B	�B	��B	�KB	��B	��B	ؓB	�1B	�yB	��B	��B	ٚB	�KB	�KB	�1B	��B	�QB	��B	چB	�7B	��B	ٴB	ٚB	��B	�B	ٴB	ٴB	�B	��B	��B	ںB	�=B	��B	ۦB	�CB	�B	�IB	�/B	�IB	ބB	޸B	ޞB	�;B	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�FB	�2B	�B	�8B	�B	��B	�B	�B	�B	�B	�B	�B	�KB	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	�B	�QB	�WB	�wB	�}B	�B	�iB	�}B	�cB	��B	�B	�B	��B	��B	�|B	�B	�nB	�9B	�B	�3B	�B	��B	�B	�hB	��B	�fB	��B	�B	�	B	�fB	�FB	�FB	��B	�}B
 iB
oB
AB
�B
-B
AB
'B
'B
[B
AB
'B
�B
�B
�B
�B
�B
�B
gB
�B
�B
�B
B
B
�B
�B
�B
1B
�B
�B
	7B
	RB
	�B
B

�B

�B
)B
�B
xB
)B
�B
�B
(B
BB
\B
vB
BB
BB
vB
B
BB
�B
�B
�B
}B
B
�B
TB
�B
�B
@B
�B
�B
B
{B
B
SB

B
�B
B
�B
�B
�B
B
�B
eB
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
yB
�B
B
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
�B
kB
�B
qB
�B
�B
)B
]B
xB
�B
xB
xB
�B
B
/B
dB
dB
�B
�B
�B
5B
jB
�B
�B
�B
�B
�B
 \B
 �B
 �B
 �B
!�B
"NB
"�B
#B
"�B
#nB
#�B
$ZB
$&B
$�B
$�B
$@B
$�B
%�B
%�B
%�B
&B
&2B
&�B
'8B
'mB
'8B
'�B
($B
(sB
)*B
)_B
)_B
)�B
)�B
)�B
*B
*�B
+B
+QB
+QB
,�B
-CB
/iB
/�B
0�B
1B
1AB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2B
2aB
3�B
3�B
3�B
3�B
3�B
4B
4B
3�B
3hB
4B
4�B
5B
5�B
5�B
5�B
5�B
5�B
5�B
6+B
6FB
6FB
6FB
6`B
6�B
7B
6�B
6�B
6�B
6�B
6�B
6`B
6+B
6�B
7B
7�B
7�B
8B
8�B
8lB
8�B
9rB
9rB
8�B
8�B
8lB
8�B
8�B
9�B
9�B
:*B
:DB
9�B
:*B
:*B
:�B
:�B
:�B
:xB
:xB
:B
:�B
;0B
;0B
;0B
;dB
;�B
;�B
<B
<�B
=<B
=<B
=VB
=<B
=<B
=qB
=�B
=�B
>B
>]B
>wB
>�B
>�B
?B
?.B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
A�B
B�B
B�B
B�B
B�B
CGB
C{B
C�B
DMB
D�B
EmB
EmB
ESB
E�B
E�B
E�B
F%B
FYB
F�B
F�B
G+B
GEB
GEB
GEB
G+B
G_B
GzB
G�B
G�B
HfB
H1B
HfB
H�B
H�B
H�B
IB
IRB
I�B
I�B
I�B
I�B
J=B
JXB
JrB
J�B
KB
K)B
KxB
K�B
LdB
L�B
L�B
L�B
L�B
MB
MB
M6B
MB
MPB
M�B
M�B
M�B
NVB
N"B
N<B
N�B
OB
O(B
O(B
O�B
PB
P.B
P.B
PbB
P}B
P�B
P�B
P�B
Q B
Q4B
Q4B
QNB
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S@B
S[B
S�B
S�B
S�B
S�B
S�B
TB
TFB
T,B
T�B
T{B
T�B
UB
U2B
U�B
U�B
U�B
VB
V�B
V�B
V�B
W$B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
XyB
X�B
YB
YKB
YeB
Y�B
Y�B
ZB
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[WB
[WB
[�B
\B
\)B
\)B
\xB
\�B
\�B
\�B
]dB
]/B
^B
]�B
]�B
]�B
^5B
^�B
_�B
_pB
_pB
_�B
_�B
_�B
_�B
_�B
`\B
`�B
a-B
a�B
a|B
a�B
bB
bNB
b�B
c:B
cnB
c�B
c�B
d&B
d@B
d�B
eB
e�B
e�B
fB
fLB
f�B
f�B
f�B
gB
gmB
g�B
g�B
hXB
hXB
h�B
h�B
h�B
iB
i*B
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jeB
jB
j�B
j�B
kB
j�B
j�B
kB
kkB
k�B
k�B
lB
l"B
l=B
l=B
l=B
lWB
lqB
lqB
l�B
l�B
mB
mB
mB
m]B
m�B
n}B
n�B
n�B
n}B
ncB
n�B
oOB
p;B
oiB
oOB
oiB
pB
pUB
p�B
qB
q'B
p�B
qAB
q�B
s3B
sMB
r�B
rB
rGB
shB
s�B
s�B
tB
tB
t9B
tnB
t�B
uB
u�B
v`B
v`B
vFB
vzB
wB
w�B
w�B
xB
x8B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
z^B
{�B
{dB
{B
{0B
{JB
{0B
{dB
{�B
{�B
|PB
|jB
|jB
|jB
|jB
|�B
|jB
|�B
}B
}B
}B
}<B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
HB
}B
�B
�B
�B
� B
� B
� B
�B
�OB
�OB
�OB
�OB
�iB
��B
��B
��B
��B
��B
��B
�B
�B
� B
� B
�;B
�UB
��B
�oB
��B
��B
��B
�AB
�[B
��B
�uB
�uB
�uB
�uB
��B
�uB
�[B
�uB
�uB
�AB
�AB
�[B
�[B
�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	q�B	q�B	v`B	x�B	y�B	{dB	|�B	~�B	~�B	~�B	}B	�iB	��B	�B	�B	�GB	��B	��B	��B	�_B	��B	��B	�#B	�	B	��B	�	B	��B	�B	��B	�B	��B	��B	�?B	�B	��B	�fB	�~B	��B	�&B	��B
6�B
MPB
UgB
U�B
W
B
N�B
CGB
5B
7�B
1�B
 �B
B
�B
�B
_B
�B
0�B
,�B
7�B
($B
&�B
1�B
dB
 �B	�xB	�jB	�+B	�-B	� B	��B	��B	�OB	��B	w�B	c B	VB	H�B	1B	$�B	YB		B	�B��B��B�B�YB�XB�B��B��B�`B�B�DB�&B�ZB�LB�8B�>B��B�WB��B��B�DB��BňB��B�=B��B�gB�RB�'B�ZB�^B	�B	�B	SB		RB	�B	�B	!�B	$�B	0UB	;�B	IB	J�B	K)B	MjB	S�B	\�B	i�B	rB	�SB	�)B	�BB	�+B	�yB	�PB	�pB	�tB	�6B	�6B	�iB	��B	�}B	ÖB	�tB	�)B	�PB	ΊB	̳B	ǔB	�3B	�-B	�GB	�4B	��B	�B	�B	�+B	�1B	ʦB	��B	͟B	͹B	�"B	�pB	��B	ΥB	�PB	�B	��B	�B	��B	��B	�BB	�(B	��B	�&B	�@B	�FB	��B	�7B	ܒB	�'B	��B	�VB	�CB	�7B	خB	ּB	�SB	�gB	��B	�gB	�9B	��B	՛B	��B	յB	ՁB	�2B	�{B	�B	��B	ԯB	�FB	�{B	�{B	ԕB	ԯB	�{B	�{B	ԕB	��B	�MB	յB	՛B	ՁB	ՁB	�gB	�gB	՛B	��B	�SB	�9B	��B	�B	�B	�SB	�B	�B	֡B	�9B	�sB	רB	�+B	�yB	��B	�7B	�B	�1B	�1B	��B	�B	ٴB	ٴB	�B	ٚB	ٚB	ٴB	چB	ڠB	�QB	��B	ںB	چB	�7B	�B	ڠB	چB	�B	�B	چB	�qB	�#B	�	B	ۦB	�B	�B	��B	�~B	�~B	�~B	��B	��B	��B	�B	ߤB	ߤB	�vB	�\B	�\B	�B	��B	��B	�B	�B	� B	��B	�B	�LB	��B	�B	��B	�>B	�
B	�B	�fB	��B	��B	��B	�eB	�B	�cB	�wB	��B	�B	�6B	�B	�6B	�B	�B	�B	�B	�kB	�qB	�B	��B	��B	��B	��B	� B	�UB	�B	�B	��B	�-B	��B	��B	��B	�nB	��B	�B	�B	�|B	��B	�B	�FB	��B	�B	��B	��B	�B	��B	��B	�rB	��B
 �B
�B
�B
-B
{B
�B
[B
[B
�B
�B
�B
'B
GB
B
B
�B
3B
�B
�B
�B
�B
gB
MB
3B
B
B
KB
�B
�B
	lB
	�B

	B
^B
)B
DB
�B
JB
�B
�B
jB
�B
\B
\B
vB
�B
�B
�B
�B
\B
vB
�B
�B
B
�B
NB
�B
oB
�B
�B
[B
�B
�B
FB
�B
SB
�B
YB
�B
_B
�B
�B
�B
�B
�B
�B
�B
�B
B
QB
kB
B
�B
�B
�B
�B
�B
�B
KB
�B
�B
�B
B
7B
7B
kB
QB
B
7B
�B
�B
B
�B
#B
�B
B
�B
]B
xB
xB
�B
�B
�B
�B
/B
IB
�B
~B
�B
�B
B
jB
�B
B
B
B
;B
�B
 �B
!B
 �B
!HB
"4B
"hB
"�B
# B
# B
#�B
$B
$�B
$ZB
$�B
$�B
$�B
%FB
%�B
%�B
&B
&LB
&�B
&�B
'RB
'�B
'mB
'�B
(XB
(�B
)yB
)�B
)�B
)�B
)�B
*0B
*eB
*�B
+QB
+�B
+�B
,�B
-�B
/�B
0;B
1B
1AB
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2B
1�B
2B
2GB
2-B
2�B
3�B
3�B
3�B
3�B
3�B
4B
4TB
4B
3�B
4nB
4�B
5?B
5�B
5�B
5�B
6B
6+B
6B
6FB
6zB
6`B
6zB
6�B
7B
7LB
7B
7B
7LB
7LB
72B
6�B
6`B
6�B
72B
7�B
7�B
8RB
9	B
8�B
9	B
9�B
9�B
9	B
8�B
8�B
8�B
9$B
9�B
:B
:^B
:�B
:^B
:xB
:xB
:�B
:�B
:�B
:�B
:�B
:^B
:�B
;JB
;dB
;dB
;B
;�B
<B
<PB
=<B
=VB
=VB
=qB
=VB
=qB
=�B
=�B
=�B
>BB
>wB
>�B
>�B
>�B
?HB
?}B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
A B
A;B
AoB
BAB
B�B
B�B
B�B
B�B
C{B
C�B
DB
D�B
EB
E�B
E�B
EmB
E�B
E�B
FB
FYB
F�B
F�B
F�B
GEB
G_B
G_B
GzB
G_B
G�B
G�B
HB
G�B
H�B
HfB
H�B
H�B
H�B
H�B
IB
I�B
J#B
I�B
I�B
I�B
JXB
JrB
J�B
KB
KDB
K�B
K�B
K�B
L�B
MB
MB
L�B
L�B
MB
M6B
MPB
M6B
MjB
M�B
M�B
N<B
NpB
N<B
NpB
N�B
O(B
OBB
OBB
O�B
P.B
P.B
PHB
P}B
P�B
P�B
P�B
P�B
QB
QNB
QNB
QhB
Q�B
RB
RTB
R�B
R�B
R�B
R�B
R�B
SB
R�B
S[B
SuB
S�B
S�B
S�B
TB
TB
T,B
TaB
TFB
T�B
T�B
UB
UMB
UMB
U�B
U�B
U�B
VSB
V�B
W
B
W
B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
W�B
X+B
X+B
X�B
X�B
YeB
YeB
YB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
\B
\)B
\]B
\CB
\�B
\�B
\�B
]B
]�B
]dB
^B
]�B
]�B
]�B
^jB
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
aHB
a�B
a�B
a�B
b4B
b�B
b�B
cnB
c�B
c�B
dB
d@B
d�B
eB
e`B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
g8B
g�B
g�B
h$B
h�B
hsB
h�B
h�B
h�B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
i�B
jKB
jB
j�B
j�B
j�B
kB
kB
j�B
k6B
k�B
k�B
k�B
l"B
l=B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
m)B
m)B
m)B
m�B
nB
n�B
n�B
n�B
n�B
n}B
n�B
o�B
poB
o�B
oiB
o�B
p!B
pUB
p�B
q'B
qAB
p�B
qAB
q�B
sMB
s�B
r�B
rGB
raB
shB
s�B
tB
tB
t9B
tnB
t�B
t�B
uB
u�B
vzB
vzB
v`B
v�B
wB
w�B
w�B
x8B
xRB
x�B
y$B
yXB
y�B
y�B
y�B
y�B
z*B
zB
zxB
{�B
{B
{0B
{dB
{dB
{JB
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}"B
}"B
}"B
}VB
}VB
}VB
}�B
}�B
~B
}�B
}�B
~B
~B
~(B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
HB
cB
�B
�B
�B
� B
�B
� B
�4B
�4B
�OB
�iB
�OB
�iB
��B
��B
��B
��B
��B
��B
�B
� B
�B
� B
�;B
�UB
�oB
��B
��B
��B
��B
��B
�[B
�uB
��B
�uB
��B
��B
��B
��B
��B
�uB
��B
�uB
�[B
�AB
�[B
�[B
�[33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2��<}�<#�
<#�
<>�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.03(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911100104542019111001045420191110010454202207271133222022072711332220220727113322202207271535532022072715355320220727153553  JA  ARFMdecpA30a                                                                20191030093814  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191030093923  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191030093924  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191030093924  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191030093925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191030093925  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191030093925                      G�O�G�O�G�O�                JA  ARUP                                                                        20191030095435                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191031000000  CF  PSAL_ADJUSTED_QC@=p�@~{G�O�                JM  ARCAJMQC2.0                                                                 20191109160454  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191109160454  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023322  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063553  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                