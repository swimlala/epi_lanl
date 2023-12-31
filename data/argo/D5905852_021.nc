CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-31T09:37:50Z creation;2019-08-31T09:37:51Z conversion to V3.1;2022-08-02T05:12:10Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190831093750  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_021                    2C  D   APEX                            8420                            2.11.2                          846 @���OD 1   @���eC @-�Ϫ͞��c�ě��T1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�ffB�33B�  B�  B���B�  B�  B�33B�ffB�  B�  B���B�  B�  B�  B�  B�33B���B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�B�  B�  B���C�fC  C�3C��C
  C  C�C  C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.L�C/�fC1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z�@{�@�@�p�A
=A?�A_�A33A���A�A��A��
A��
A�(�A�=qA��
B��B  B�B�
B'�
B/�HB7�HB?�BH  BOBWz�B_��Bg�RBo��Bw�HB�L�B�{B��)B�  B�ǮB��fB��B�33B�\)B��B��
B���B��HB��B��fB��B�\B�B�ffB��HBϽqB��HB��fB��B��HB��B���B�
=B��fB�B��
B��fB���C��C��C��CC	�3C�C�C  C�C�C�3C�3C��C��C�RC��C!�RC#�3C%�RC'��C)��C,�C.L�C/�{C1��C3�RC5�RC7�RC9�RC;�RC=�RC?��CA�3CC�CE�3CG��CI�3CK��CM��CP  CQ��CS�qCU��CW��CY�RC[��C]�RC`  Cb�Cd
=Ce�Cg�Ci�Ck�RCm��Co�3Cq�Cs�3Cu��Cw�RCy�RC{��C}�RC��C���C���C��)C���C�  C�HC���C���C��qC���C���C���C���C��qC��qC��qC���C���C��qC��)C��)C��)C��)C���C��)C��qC��RC��RC��)C���C��)C���C��)C��qC���C��)C�  C�  C���C���C���C���C�  C��)C��)C���C��)C��qC��qC��)C��RC���C��qC���C��
C���C��)C���C��)C���C��)C��)C���C���C��)C���C���C���C�  C��qC��qC��)C���C���C��qC��qC��)C���C���C��)C��)C��RC��RC��qC��)C��qC�HC��qC���C��)C��qC�  C��qC���C��)C���C���C���C���C��qC���C���C���C�  C���C���C��qC���C���C��)C���C���C���C��qC���C��)C��qC���C��{C��
C��RC���C���C���C���C���C��qC���D z�D ��D~�D  D~D��D|)D�\D��D�D��D �D��D�\D}qD�qD~�D�D	}qD	�qD
}qD
�)D|�D�qD}qD��D\D�D{�D�qD}qD��D}qD�)D{�D�)D|�D�)D|�D�qD~D�D~D��D{�D��D|)D�)D~D�qD}qD�qD~�D  D~�D��D~D�D��D�D|�D��D~D��D }qD �qD!}qD!�D"|)D"��D#~D#�D$�HD%�D%~�D%��D&\D&�\D'}qD'�qD(~�D(��D)}qD)�qD*}qD*��D+|�D+��D,� D,�\D-}qD-�)D.|)D.��D/~�D/�\D0}qD0�D1~�D1�qD2{�D2��D3|�D3�\D4��D4�\D5~�D5�D6~�D7  D7\D7�\D8� D8�D9~D9�D:~�D:��D;~D;��D<|�D<�)D=z�D=��D>|)D>�qD?~�D?�D@|�D@��DA~DA�DB\DB�qDC|�DC��DD~�DD�DE~DE�\DF~�DF�qDG|�DG�)DH|)DH�)DI}qDI�\DJ~�DJ�DK}qDK�qDL��DMHDM~DM��DN\DN�qDO{�DO�DP� DP�\DQ~�DR  DR��DR��DS|)DS�qDT\DT�DU}qDU�qDV|)DV�qDW~DW��DX~�DX��DY~DY�qDZ~DZ�D[~D[�D\}qD\�qD]}qD]��D^|�D^�qD_}qD_�D`\D`�\Da~Da�Db~�Db�Dc~�Dc�qDd~�De  De� Df  Df}qDf�)Dg~�Dh  Dh~Dh��Di\Di�Dj}qDj�)Dk|)Dk��Dl~Dm  Dm~�Dm��Dn~�Dn�Do~Do��Dp\Dq  Dq~Dq��Dr{�Dr�Ds��Ds�\Dt\Dt�\Du\Dv  Dv�HDw  Dw|)Dw��Dx~�Dx�\Dy~�Dy�Dz~�Dz�\D{~D{�)D|~D|�\D}\D~ �D~� D~��D~D��D�>�D�\D��
D��
D�?
D��D��
D��D�>�D�\D��\D���D�>�D�~fD��D��\D�?�D�\D��\D��
D�>�D�
D��fD��
D�>fD�~fD��\D��\D�?\D�~�D��fD��\D�>�D�~�D���D��\D�@ D�\D��\D��\D�?
D�~�D���D��\D�>�D�~D��
D��
D�>fD�\D���D���D�@RD�� D���D��
D�?�D�\D���D� RD�?
D�~D��
D�  D�?
D�~fD��\D���D�>fD��D��\D��fD�>�D�
D��\D��\D�@ D�\D��
D���D�@RD���D��HD�HD�>�D�~�D��fD���D�>�D�\D��\D��\D�>fD�~�D���D��\D�@ D�
D��\D��\D�>fD�~fD��fD��D�?
D��D��fD���D�?
D�� D��\D��
D�>�D�~D��
D�  D�@ D�� D��
D��
D�?
D��D��\D��
D�?�D�
D��
D��\D�>�D�~fD���D��
D�@ D�� D���D��D�?
D�� D���D��
D�>fD�~�D��
D��fD�>fD�~D��fD���D�?\D�
D���D�  D�@ D�\D��
D���D�?
D�\D���D��\D�?\D��D���D���D�?�D�~D���D��
D�>D�}�D���D� �D�@�D��D���D��\D�?�D��RD���D���D�>fD�~�D���D��D�=qD�\D��\D��fD�?
D�
D��\D���D�>�D�~D��
D���D�?
D�~�D��fD���D�>�D��RD�� D��\D�?�D��D��
D���D�>fD�~fD���D��
D�?\D�~�D��fD���D�?
D�~�D���D���D�@ D�\D��fD���D�?
D�~�D��
D��
D�>fD�~D��fD��fD�>fD�~�D���D���D�?\D�� D���D���D�>�D��D���D� RD�@ D�
D���D��D�>D�~fD��\D��
D�>D�}�D��fD��\D�?\D�\D�� D���D�>fD�
D�� D���D�@ D�
D¾fD���D�@ D�
Dþ�D���D�?
D�\DĿ
D��fD�?\D��D���D�  D�>fD�
Dƿ\D��\D�?
D�\Dǿ
D��\D�>�D�}�DȾfD��
D�?\D�~�Dɿ
D�  D�@ D��Dʿ
D��fD�>fD�}�D˾�D���D�@ D�
D̿
D���D�?
D�
DͿ
D��\D�?
D�
D�� D���D�?�D�~fDϾ�D��
D�?
D��Dп
D��\D�@ Dр Dѿ
D��D�?\DҀRDҿ\D��D�?\DӀ Dӿ�D���D�?\D�
DԾ�D��
D�?
D�\D��RD���D�?\D�\Dֿ�D��
D�?
D�\D׾�D��D�>D�}�Dؽ�D��qD�?
D�\DپD���D�?
D��Dھ�D��D�?�Dۀ D۾�D��qD�>fD��Dܿ\D��\D�?\D��D��RD��\D�>D�\D޾fD��D�?
D�~fD߾D��\D�@ D��D�
D��fD�?
D�\D�
D��\D�?\D��D�
D��\D�?
D�~�D�\D��
D�>�D�~fD侸D��
D�@ D�\D徸D��\D�>�D�~fD�\D��\D�?\D�~�D羸D��\D�?\D��D�\D���D�?\D�RD�
D���D�>D�
D�
D��\D�?\D�~D�fD���D�?�D�\D쾸D�  D�@RD�
D��
D��
D�?�D�~�D�fD��
D�?�D�RD�� D��
D�>D�
D�D���D�>�D��D�\D��
D�?\D� D�
D��fD�?�D��D�\D�  D�?\D�
D��D��D�>fD�
D���D��\D�?\D�\D��\D���D�?
D�~�D���D��
D�?
D�~�D���D��
D�>�D�~�D��qD��D�?
D�~fD��\D��
D�>�D�u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�:A�A�\A�{A�SA��A䗍A䖼A��A�YA�A䖇A��A�_A�uA䒣A�@A䒣A��A�uA�@A�A�+A�fA�lA�aA��A��A��3A�gA�oAڷLAُ(A��A�:�A�A�Y�A�2�A��A�uZA��AǏ�A�T,A��A��yA��&A�,�A�U2A�YKA���A�\A��A�e�A���A��&A��A�`A���A��^A���A���A�
�A���A��GA���A�$A��<A�4nA���A�@�A���A��A��=A��iA�2�A��XA���A���A��A��aA�#�A�0�A}��A{kQAw�rAr,=AoA�Am�Ak�kAb�sA^u%A\�-A[e�AZ�	AVOAO��AN�AL~�AJMAF��AC�AB-wA@�vA@�A>�5A?TaA?0UA?;A>��A:��A4��A2�PA1�A0|A0�A/��A-�A*{JA(�[A'B[A&�uA%��A$y�A#xlA"u�A!Q�A �&A N<A�bA�A�eAm]A �YA �PA"s�A"�A"�oA"��A"{A �A��A\)A�A��AJ#A,=AoiAz�A��AB[AI�AJAA��A�$A�A��A��An/A$A�#A�}AE9A�/A��AOA�RAB[A��A_�A4Aa|A��A�yAz�A�eAS�A7LA�[A�9A"hA
e�A	�|A	��A	~�A	��A	S�A	-A��A�A��Ae�A>�A�A�A�vA�	A�A�KA�~AMA!AxA��A�[AV�A �A )_@���@��E@��@�"h@���@�T�@�|�@�e�@�S�@�h�@�Z@��@��@�!@�^5@�8�@�@��|@�%F@���@���@�33@�W?@��@��@��@�,�@��@��'@�p;@���@��H@�'R@���@���@�J�@�3�@�M@��r@���@�:@�'�@� i@�@��#@�u�@�O�@�"�@��M@�E�@��d@��@� �@���@�~(@��W@�;d@��@��@�@�x@�}@�Dg@��)@�`B@�[W@�{J@��5@�/�@�/�@�@��.@ݨX@���@�J�@�ѷ@�1�@ۃ{@�Ĝ@�s�@ڝI@�Q�@�@�@�;d@�Ĝ@լq@�2a@Խ<@���@Ӽ@�O@��|@҃�@ҝI@���@ӝ�@���@�d�@�(�@��@���@А.@�:�@�O@��a@�+@���@Κ�@�w2@�-@�|�@��@�?�@˭C@��@�}V@Ɂ@ș1@Ƿ�@�C�@ƻ�@��j@�+@��@���@Į}@Đ.@�?�@Ã{@��s@\@�`�@�>B@�7@��^@�y>@��@���@�@@���@��@��@���@�Ov@��@�	�@���@��@�\�@��;@��o@���@�C@���@�g8@�0U@��@�v`@�O@�;@�҉@��$@�u%@��o@��F@��}@���@�N<@���@���@�p;@��@��V@�"�@��@�^5@��@��}@��4@��R@�e@�k�@���@�R�@�x@��w@�x@�a@�33@���@���@�|�@�7@��@�e,@�s@�6z@��@���@�bN@�6�@�b@���@�a@�E9@�33@�@��@�y>@�{@��@��@�J#@�
=@���@��@��@��@�u%@�A�@�#:@���@���@��E@�1�@��@�g�@�4�@��@��_@��@�z@�l"@�]d@�H@��@��[@�g�@��@��'@��r@�6@��)@���@�6z@��<@���@�_�@�-@��@��@�T�@���@�N�@�/�@��@���@�a@�8�@��@��@��y@���@���@��r@�M�@�u@��3@�rG@��@���@��1@���@���@�]d@�J@�dZ@�o@��@���@�_@�?�@��@��@��9@�=@��s@��F@�e�@�H@��W@�[W@���@��_@�V�@�;�@�	�@�خ@��-@��@�S@���@���@�)�@��W@��@���@��@�ݘ@��)@��@���@��h@�p�@�>�@���@��h@��@��@�n�@�$�@���@�w2@�@�҉@�~�@�5?@���@�Z�@�S@���@��R@��D@�W�@�M@�0U@��@��0@�t�@�@O@�Ɇ@�K^@�$�@���@�?}@���@��<@�kQ@��@��@�@�|@�F@��@���@�z�@�{�@�xl@�
�@��w@���@�W?@�"�@��@��?@�`�@��@���@��@�qv@�A @�/�@���@��@���@���@���@�c�@�!@~�@~҉@~�b@~��@~3�@}�X@}Q�@|��@z�@zh
@y�T@ys�@y+@xی@xj@xM@x  @w�@w~�@v�@v5?@u��@u�X@u=�@t�|@t:�@t�@sK�@sH�@r��@r��@rW�@q�T@q��@q��@q��@q��@q�@p�Y@o��@o~�@o!-@n��@n#:@m��@mVm@l�K@l:�@k~�@kJ#@j�@j-@i�@i2a@h�	@h��@h,=@gݘ@g�k@g\)@g�@f��@f+k@e�)@e��@dĜ@d��@c�
@c\)@co@b��@b�@bW�@a��@am]@a�@`�?@`�@_ݘ@_J#@_�@^��@^R�@^@�@^O@^@]�7@]@@\��@\>B@[��@[�4@[iD@[Mj@Z��@Z�@Y��@Y�@X�|@X��@Xb@W�@V��@V6�@U�@Uzx@UV@T�I@TN�@T	�@S�&@S��@S��@S{J@S@R��@R3�@Q��@Q[W@QB�@Pg8@PH@PH@P9X@O�@O��@Oo�@O$t@N�6@NGE@N:*@M�@Mf�@L]d@K�w@KA�@J�R@J��@J�1@J!�@I�@Iu�@I;@H�@H��@HtT@HN�@G�6@G��@G(@F�]@F͟@FC�@E�@EL�@D�@D��@D@Cݘ@C��@C�k@Ce�@B�@B�@B{�@BW�@B �@A�-@A\�@@�@@r�@@%�@?n/@?C@>��@>��@>�@=��@=k�@=%F@<�E@<h�@<!@<G@;��@;$t@:�'@:��@:Q@9��@9p�@9IR@9 \@8��@8Q�@8b@7�;@7��@7P�@6�@6�!@6�r@6;�@6@5�d@5�@5k�@4�f@4�@4c�@4�@3�Q@3��@3�@2��@2($@2�@1�@1�z@1�@1a�@1<6@1�@0ی@0��@0`�@0*�@0@/�[@/s@/�@.�}@.^5@.@�@.+k@. �@-rG@-�@-;@,�@,��@,c�@,H@,�@+�K@+�:@+4�@+ i@*�@*R�@)�n@)\�@)Dg@(�5@(�I@(l"@'�K@'��@'y�@'W?@'C@&�@&��@&�6@&��@&�+@&V@&+k@%�@%��@%s�@%`B@%L�@%@@$�/@$��@$~(@$I�@#�W@#�@#W?@#+@#Y@"�h@"a|@"B[@")�@"O@!ԕ@!�h@!u�@!N<@!2a@!�@ �P@ �[@ ��@ Ft@ $@�@�k@4�@�]@�F@R�@�D@�z@��@c�@B�@(�@V@��@��@�@�@<�@!@�@�a@��@v`@;d@�@�s@�+@Q@-@�)@�-@��@J�@�P@��@��@U2@  @��@qv@�8@�<@��@ff@�@�T@�@�n@�~@a�@=�@@�@�@_@	�@�A@�}@��@iD@O@+@@�M@�h@Ta@	@��@��@�@f�@(�@V@��@��@�o@l"@>B@�@�@خ@Z�@+@�"@�@��@�A@B[@1�@�@�#@|@7L@@�5@��@��@|�@m�@?�@x@�W@��@l�@E9@Y@
��@
��@
�X@
��@
�@
q�@
V@
�@	�T@	��@	�C@	�~@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�:A�A�\A�{A�SA��A䗍A䖼A��A�YA�A䖇A��A�_A�uA䒣A�@A䒣A��A�uA�@A�A�+A�fA�lA�aA��A��A��3A�gA�oAڷLAُ(A��A�:�A�A�Y�A�2�A��A�uZA��AǏ�A�T,A��A��yA��&A�,�A�U2A�YKA���A�\A��A�e�A���A��&A��A�`A���A��^A���A���A�
�A���A��GA���A�$A��<A�4nA���A�@�A���A��A��=A��iA�2�A��XA���A���A��A��aA�#�A�0�A}��A{kQAw�rAr,=AoA�Am�Ak�kAb�sA^u%A\�-A[e�AZ�	AVOAO��AN�AL~�AJMAF��AC�AB-wA@�vA@�A>�5A?TaA?0UA?;A>��A:��A4��A2�PA1�A0|A0�A/��A-�A*{JA(�[A'B[A&�uA%��A$y�A#xlA"u�A!Q�A �&A N<A�bA�A�eAm]A �YA �PA"s�A"�A"�oA"��A"{A �A��A\)A�A��AJ#A,=AoiAz�A��AB[AI�AJAA��A�$A�A��A��An/A$A�#A�}AE9A�/A��AOA�RAB[A��A_�A4Aa|A��A�yAz�A�eAS�A7LA�[A�9A"hA
e�A	�|A	��A	~�A	��A	S�A	-A��A�A��Ae�A>�A�A�A�vA�	A�A�KA�~AMA!AxA��A�[AV�A �A )_@���@��E@��@�"h@���@�T�@�|�@�e�@�S�@�h�@�Z@��@��@�!@�^5@�8�@�@��|@�%F@���@���@�33@�W?@��@��@��@�,�@��@��'@�p;@���@��H@�'R@���@���@�J�@�3�@�M@��r@���@�:@�'�@� i@�@��#@�u�@�O�@�"�@��M@�E�@��d@��@� �@���@�~(@��W@�;d@��@��@�@�x@�}@�Dg@��)@�`B@�[W@�{J@��5@�/�@�/�@�@��.@ݨX@���@�J�@�ѷ@�1�@ۃ{@�Ĝ@�s�@ڝI@�Q�@�@�@�;d@�Ĝ@լq@�2a@Խ<@���@Ӽ@�O@��|@҃�@ҝI@���@ӝ�@���@�d�@�(�@��@���@А.@�:�@�O@��a@�+@���@Κ�@�w2@�-@�|�@��@�?�@˭C@��@�}V@Ɂ@ș1@Ƿ�@�C�@ƻ�@��j@�+@��@���@Į}@Đ.@�?�@Ã{@��s@\@�`�@�>B@�7@��^@�y>@��@���@�@@���@��@��@���@�Ov@��@�	�@���@��@�\�@��;@��o@���@�C@���@�g8@�0U@��@�v`@�O@�;@�҉@��$@�u%@��o@��F@��}@���@�N<@���@���@�p;@��@��V@�"�@��@�^5@��@��}@��4@��R@�e@�k�@���@�R�@�x@��w@�x@�a@�33@���@���@�|�@�7@��@�e,@�s@�6z@��@���@�bN@�6�@�b@���@�a@�E9@�33@�@��@�y>@�{@��@��@�J#@�
=@���@��@��@��@�u%@�A�@�#:@���@���@��E@�1�@��@�g�@�4�@��@��_@��@�z@�l"@�]d@�H@��@��[@�g�@��@��'@��r@�6@��)@���@�6z@��<@���@�_�@�-@��@��@�T�@���@�N�@�/�@��@���@�a@�8�@��@��@��y@���@���@��r@�M�@�u@��3@�rG@��@���@��1@���@���@�]d@�J@�dZ@�o@��@���@�_@�?�@��@��@��9@�=@��s@��F@�e�@�H@��W@�[W@���@��_@�V�@�;�@�	�@�خ@��-@��@�S@���@���@�)�@��W@��@���@��@�ݘ@��)@��@���@��h@�p�@�>�@���@��h@��@��@�n�@�$�@���@�w2@�@�҉@�~�@�5?@���@�Z�@�S@���@��R@��D@�W�@�M@�0U@��@��0@�t�@�@O@�Ɇ@�K^@�$�@���@�?}@���@��<@�kQ@��@��@�@�|@�F@��@���@�z�@�{�@�xl@�
�@��w@���@�W?@�"�@��@��?@�`�@��@���@��@�qv@�A @�/�@���@��@���@���@���@�c�@�!@~�@~҉@~�b@~��@~3�@}�X@}Q�@|��@z�@zh
@y�T@ys�@y+@xی@xj@xM@x  @w�@w~�@v�@v5?@u��@u�X@u=�@t�|@t:�@t�@sK�@sH�@r��@r��@rW�@q�T@q��@q��@q��@q��@q�@p�Y@o��@o~�@o!-@n��@n#:@m��@mVm@l�K@l:�@k~�@kJ#@j�@j-@i�@i2a@h�	@h��@h,=@gݘ@g�k@g\)@g�@f��@f+k@e�)@e��@dĜ@d��@c�
@c\)@co@b��@b�@bW�@a��@am]@a�@`�?@`�@_ݘ@_J#@_�@^��@^R�@^@�@^O@^@]�7@]@@\��@\>B@[��@[�4@[iD@[Mj@Z��@Z�@Y��@Y�@X�|@X��@Xb@W�@V��@V6�@U�@Uzx@UV@T�I@TN�@T	�@S�&@S��@S��@S{J@S@R��@R3�@Q��@Q[W@QB�@Pg8@PH@PH@P9X@O�@O��@Oo�@O$t@N�6@NGE@N:*@M�@Mf�@L]d@K�w@KA�@J�R@J��@J�1@J!�@I�@Iu�@I;@H�@H��@HtT@HN�@G�6@G��@G(@F�]@F͟@FC�@E�@EL�@D�@D��@D@Cݘ@C��@C�k@Ce�@B�@B�@B{�@BW�@B �@A�-@A\�@@�@@r�@@%�@?n/@?C@>��@>��@>�@=��@=k�@=%F@<�E@<h�@<!@<G@;��@;$t@:�'@:��@:Q@9��@9p�@9IR@9 \@8��@8Q�@8b@7�;@7��@7P�@6�@6�!@6�r@6;�@6@5�d@5�@5k�@4�f@4�@4c�@4�@3�Q@3��@3�@2��@2($@2�@1�@1�z@1�@1a�@1<6@1�@0ی@0��@0`�@0*�@0@/�[@/s@/�@.�}@.^5@.@�@.+k@. �@-rG@-�@-;@,�@,��@,c�@,H@,�@+�K@+�:@+4�@+ i@*�@*R�@)�n@)\�@)Dg@(�5@(�I@(l"@'�K@'��@'y�@'W?@'C@&�@&��@&�6@&��@&�+@&V@&+k@%�@%��@%s�@%`B@%L�@%@@$�/@$��@$~(@$I�@#�W@#�@#W?@#+@#Y@"�h@"a|@"B[@")�@"O@!ԕ@!�h@!u�@!N<@!2a@!�@ �P@ �[@ ��@ Ft@ $@�@�k@4�@�]@�F@R�@�D@�z@��@c�@B�@(�@V@��@��@�@�@<�@!@�@�a@��@v`@;d@�@�s@�+@Q@-@�)@�-@��@J�@�P@��@��@U2@  @��@qv@�8@�<@��@ff@�@�T@�@�n@�~@a�@=�@@�@�@_@	�@�A@�}@��@iD@O@+@@�M@�h@Ta@	@��@��@�@f�@(�@V@��@��@�o@l"@>B@�@�@خ@Z�@+@�"@�@��@�A@B[@1�@�@�#@|@7L@@�5@��@��@|�@m�@?�@x@�W@��@l�@E9@Y@
��@
��@
�X@
��@
�@
q�@
V@
�@	�T@	��@	�C@	�~@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	m�B	m]B	l�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m]B	mwB	mCB	l�B	l�B	l�B	l�B	lqB	l"B	k�B	kkB	kB	k6B	k6B	gB	_!B	ZkB	YB	c�B	tnB	fLB	R�B	@ B	7�B	-�B	B	&B	~B	>]B	VmB	�B	�sB	��B
B
"�B
0�B
E�B
_�B
jeB
��B
��B
�MB
��B
��B
cB
�+B
��B
�iB
��B
��B
�NB
�@B
�B
�KB
s�B
'B
$�B
SB
�KB
�B
��B
ΥB
��B
e�B
Q�B
H�B
G+B
@B
(�B
�B	�"B	��B	��B	�cB	�RB	s�B	f�B	W�B	1B	sB	�B	�B	�B�B��B�!B�B�CB�eB�B�B�B�\B��B�VB	 \B	$ZB	�B	�B�BٚB�B�B��B��B�B��B��B�B�B�>B	�B	%B	+B	�B	?B	�B	�B		�B	�B	�B	6�B	DgB	l=B	}�B	��B	�+B	��B	��B	z�B	|�B	�B	h�B	poB	�GB	�TB	��B	�gB	��B	�+B	�`B	�XB	�<B	��B	�B	�B	��B	�B	�B	�8B	�mB	��B	�B	�2B	��B	�fB	�+B	�TB	��B	�nB	�OB	�)B	�qB	�B	�9B	�B	�iB	�sB	��B	��B	�xB	�QB	�]B	�pB	�mB	�LB	�yB	�)B	�B	�:B	��B	�$B	�B	�]B	��B	��B	�B	�B	��B	�5B	�QB	�B	�B	�hB	߾B	ݲB	�B	��B	ۦB	��B	�)B	�B	��B	�IB	�UB	�MB	�FB	��B	�B	�B	�B	�"B	�sB	��B	�OB	�B	�B	�0B	�"B
�B
�B
1B
�B
fB
�B
�B
�B
KB
EB
�B
zB
�B
3B
B
�B
�B
�B
�B
	RB
JB
�B
B
~B
�B
~B
0B

�B
	�B
	�B
�B
fB
�B
+B
B
�B
SB
�B
-B	�.B	�vB	�KB	�B	�B	��B	��B	�-B	�B	�IB	�IB	��B	��B	��B	�FB	��B	�B	�B	��B	��B	��B	�B	�;B	�B	�B	��B	�B	��B	�>B	�
B	�B	�>B	�B	��B	�5B	��B	�LB	��B	��B	�B	�MB	�9B	�+B	�B	��B	�B	��B	��B	�TB	�B	�?B	��B	��B	��B	��B	��B	�B	��B	�B	�oB	��B	��B	� B	��B	�iB	�B	�oB	��B	��B	�MB	�hB	�B	�B	�B	�B	�9B	�B	��B	�B	�MB	�nB	�B	��B	��B	�FB	�RB	�B	��B	��B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B	��B	��B
 4B
�B
AB
�B
-B
uB
oB
 �B	��B
 4B
 iB
 4B
B
UB
�B
�B
B
�B
+B
�B
�B
_B
�B
B
�B
fB
�B
KB
�B

rB
^B
�B
B
B
JB
dB
�B
�B
PB
PB
PB
�B
�B
<B
B
B
<B
�B
�B
�B
pB
�B
�B
(B
vB
�B
bB
�B
�B
.B
�B
�B
 B
bB
�B
�B
�B
�B
4B
�B
�B
B
oB
�B
�B
�B
�B
�B
�B
�B
uB
B
B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
kB
kB
�B
�B
�B
�B
	B
=B
�B
B
�B
�B
/B
/B
dB
�B
pB
�B
pB
 BB
 'B
 \B
 vB
 vB
 \B
!HB
 �B
!|B
!bB
!-B
!|B
"�B
"�B
"�B
"hB
"NB
"4B
"4B
"�B
"�B
#nB
#�B
#�B
#�B
$B
$�B
%�B
%�B
&2B
&�B
'B
'8B
'mB
'RB
'�B
'RB
($B
)*B
)DB
)yB
*B
*�B
*�B
*�B
*�B
*KB
*B
*KB
+B
+kB
+�B
+�B
,B
,"B
,"B
,WB
,�B
,�B
,�B
,�B
-�B
./B
.IB
.IB
.�B
.�B
/5B
/iB
/�B
/�B
0!B
0�B
0�B
0�B
0�B
1�B
2-B
2-B
3�B
3�B
4B
4TB
4nB
4�B
4�B
4�B
5?B
5%B
5%B
5?B
5ZB
5ZB
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6FB
6zB
6FB
6+B
6`B
6zB
6�B
6zB
5�B
5%B
4�B
5%B
5ZB
5tB
5�B
6�B
9	B
9�B
9�B
:^B
:�B
:�B
:�B
:�B
:�B
:�B
:xB
:^B
:^B
;JB
;�B
<B
<B
<B
<6B
<jB
<�B
=B
=<B
=�B
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
>]B
?B
?}B
?}B
?�B
@OB
@4B
@�B
A;B
A;B
A;B
A B
A;B
A�B
A�B
A�B
B'B
A�B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C�B
DB
DMB
D�B
D�B
D�B
EB
EB
EB
EB
EB
E9B
EmB
E�B
E�B
FB
FB
F%B
F?B
F�B
F?B
F�B
F�B
F�B
GEB
G�B
G�B
H�B
HB
H�B
H�B
H�B
IRB
I�B
IlB
I7B
IRB
IlB
IlB
I�B
I�B
J	B
J�B
J�B
J�B
K�B
LB
LJB
LJB
L�B
MB
M6B
MjB
M�B
M�B
NB
NVB
N�B
OB
OBB
O�B
PHB
PHB
P.B
P�B
P�B
Q4B
QNB
QNB
QNB
Q4B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
S&B
SB
S�B
S�B
S�B
S�B
S�B
S�B
TaB
T{B
T{B
T{B
T�B
T�B
U2B
U�B
U�B
VB
V�B
V�B
V�B
V�B
WYB
W�B
WsB
W�B
W�B
XEB
X_B
X_B
XEB
X�B
X�B
YB
YB
YB
Y�B
Y�B
ZB
Z�B
Z�B
[=B
[WB
[�B
[�B
\CB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]�B
]�B
^B
^B
^�B
^�B
_B
_!B
_;B
_;B
_pB
_pB
_�B
_�B
_�B
`B
`BB
`\B
`vB
`�B
`�B
a-B
a�B
a�B
bB
b4B
b4B
b�B
b�B
b�B
cB
cnB
cnB
cnB
c�B
dB
d&B
d�B
d�B
eB
e`B
e�B
f2B
f2B
f�B
gB
g8B
g�B
g�B
h
B
h$B
h>B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
iB
i*B
iDB
i_B
i_B
i�B
i�B
i�B
i�B
i�B
jeB
jeB
j�B
j�B
j�B
kQB
kkB
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
m)B
m�B
m�B
nB
n/B
ncB
n�B
n�B
o B
o5B
oiB
oiB
oiB
o�B
o�B
o�B
o�B
p!B
p;B
poB
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
sB
shB
s�B
tB
t9B
t9B
t�B
t�B
uB
u%B
u?B
u?B
uZB
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
wB
wB
w2B
wfB
w�B
w�B
w�B
x8B
x8B
xRB
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
y�B
zB
zB
y�B
z^B
z�B
z�B
z�B
{0B
{dB
{B
|6B
|PB
|jB
|�B
|�B
}B
}"B
}"B
}�B
}�B
}�B
}�B
~BB
~(B
~BB
~wB
~]B
~�B
B
.B
HB
HB
cB
}B
�B
�B
� B
� B
�4B
�4B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	m�B	mwB	l�B	m�B	m�B	nB	nB	m�B	m�B	m�B	m�B	mwB	m�B	m]B	l�B	l�B	l�B	l�B	l�B	l=B	lB	k�B	k6B	k�B	l=B	i�B	d�B	^B	\xB	f�B	v�B	i*B	VB	CaB	>�B	4�B	"B	YB	 �B	?�B	Y1B	�~B	��B	�wB
B
$�B
2�B
HB
b�B
oB
�&B
��B
�7B
��B
��B
��B
�DB
�SB
��B
��B
�_B
�
B
��B
��B
�TB
}<B
+6B
%�B
S[B
�QB
��B
��B
֡B
��B
h�B
TB
K)B
JXB
E9B
-]B
eB
�B	ؓB	��B	��B	��B	wfB	j�B	`BB	5�B	�B	�B	
rB	zB��B�B�hB�xB�B�B��B�8B��B�bB��B��B	 �B	&LB	%zB	�B�WB�	B��B��B��B��B	 �B�B��B�AB�B��B	B	�B	�B	�B	+B	�B	�B		�B	�B	WB	5�B	CB	k�B	}�B	�EB	��B	�XB	�gB	|B	cB	��B	iDB	pB	�3B	�B	�NB	��B	��B	�zB	�zB	��B	�VB	��B	� B	�B	�FB	�$B	�$B	�B	�
B	�B	��B	�B	��B	�B	��B	�B	�B	�ZB	�;B	�/B	�B	��B	�B	��B	�'B	�B	��B	��B	��B	ڠB	��B	ߊB	��B	�B	�0B	�IB	�B	�B	�B	�sB	�B	�wB	��B	�B	�hB	�B	�B	�oB	�WB	�6B	�B	�TB	��B	�5B	�~B	ݲB	��B	�	B	��B	��B	�]B	�cB	�oB	�hB	��B	��B	�B	�oB	��B	��B	�B	��B	�5B	�B	�B	�dB	�B
[B
�B
�B
	B
�B
�B
�B
�B
	B
�B
�B
B
�B
MB
3B
B
�B
�B
	7B
	�B
�B
B
PB
�B
�B
�B
�B
DB

rB

rB
	lB
	7B
B
�B
�B
EB
B
�B
�B
 B	�aB	�B	�B	��B	�RB	�B	�B	�B	�}B	�B	�B	�'B	��B	��B	�+B	�B	��B	��B	��B	�FB	�9B	�AB	�B	�_B	�XB	�mB	�sB	�B	�XB	�
B	�sB	�B	�B	�B	�B	��B	��B	�%B	�B	�B	�nB	�zB	�RB	�B	�fB	��B	��B	��B	�vB	�%B	�B	�PB	�*B	�fB	��B	�3B	�vB	��B	��B	�B	�OB	�B	� B	�B	��B	�B	�GB	�3B	�B	�B	��B	�B	�B	�B	��B	�B	�TB	�9B	��B	��B	�hB	��B	��B	�`B	��B	��B	�fB	�FB	��B	�B	�B	��B	��B	�B	��B	��B	�	B	�0B	��B	��B	��B	�B	�JB	��B	��B
 �B
'B
[B
�B
{B
�B
�B
 �B	��B
 �B
 �B
 �B
�B
�B
uB
aB
{B
?B
zB
B
�B
�B
�B
KB
�B
�B
�B
�B
	B

�B
�B
�B
0B
JB
~B
�B
�B
�B
jB
�B
�B
"B
<B
pB
<B
<B
pB
�B
�B
�B
�B
�B
B
BB
�B
bB
�B
hB
4B
bB
.B
�B
4B
}B
�B
�B
 B
 B
�B
B
 B
TB
�B
�B
�B
�B
B
B
�B
&B
�B
FB
FB
�B
�B
YB
�B
�B
�B
KB
�B
�B
�B
�B
�B
�B
�B
	B
#B
	B
=B
�B
�B
]B
�B
B
IB
dB
�B
OB
�B
�B
�B
 \B
 BB
 vB
 �B
 �B
 �B
!�B
!-B
!�B
!�B
!|B
!�B
#B
#B
#B
"�B
"�B
"hB
"�B
"�B
#B
#�B
#�B
$&B
$B
$&B
$�B
%�B
%�B
&2B
&�B
'RB
'mB
'�B
'�B
'�B
'�B
(>B
)DB
)�B
)�B
*eB
*�B
+6B
+6B
*�B
*B
*eB
*�B
+6B
+�B
+�B
+�B
,"B
,WB
,WB
,�B
,�B
,�B
-B
-]B
.IB
.cB
.�B
.�B
/ B
.�B
/iB
/�B
/�B
0!B
0UB
0�B
0�B
0�B
1AB
1�B
2GB
2�B
3�B
4B
49B
4�B
4�B
4�B
4�B
5B
5ZB
5?B
5tB
5ZB
5tB
5tB
5�B
5�B
5�B
5�B
5�B
5�B
6+B
6zB
6�B
6`B
6zB
6�B
6�B
6�B
6�B
5�B
5ZB
5%B
5?B
5tB
5�B
6B
6�B
9$B
:B
:*B
:�B
:�B
;B
;B
;B
:�B
:�B
:�B
:xB
:xB
;B
<B
<6B
<6B
<6B
<6B
<�B
<�B
=<B
=qB
=�B
=�B
=�B
>(B
>B
>BB
>�B
?HB
>�B
>wB
?HB
?�B
?�B
?�B
@iB
@iB
@�B
AoB
AoB
AUB
AUB
AoB
A�B
A�B
B'B
B[B
BB
BAB
B�B
B�B
B�B
B�B
CB
CB
C{B
C�B
DB
DMB
D�B
D�B
D�B
D�B
EB
E9B
EB
E9B
E9B
EmB
E�B
E�B
E�B
F%B
F%B
F?B
FtB
F�B
FtB
F�B
GB
GEB
G�B
G�B
G�B
H�B
HfB
H�B
H�B
IB
I�B
I�B
I�B
IlB
I�B
I�B
I�B
J	B
J=B
J=B
J�B
J�B
KDB
K�B
LB
LdB
LdB
L�B
M6B
MPB
M�B
M�B
NB
N<B
N�B
OB
OBB
OvB
O�B
PbB
PbB
PbB
P�B
QB
QhB
QhB
QhB
Q�B
QhB
QNB
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
S[B
S&B
S�B
S�B
S�B
S�B
S�B
T,B
T{B
T�B
T�B
T�B
T�B
UB
UgB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
XB
X_B
XyB
XyB
XyB
YB
X�B
YKB
YKB
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
[WB
[qB
[�B
[�B
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
^B
^B
^5B
^�B
^�B
_!B
_;B
_pB
_VB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`�B
aB
abB
a�B
b4B
b4B
bhB
bhB
b�B
cB
cB
c B
c�B
c�B
c�B
c�B
d&B
d@B
d�B
d�B
e`B
e�B
fB
fLB
fLB
gB
g8B
gmB
h
B
g�B
h$B
h>B
hsB
h�B
h�B
hsB
h�B
h�B
h�B
h�B
i*B
iDB
iDB
i�B
iyB
i�B
i�B
i�B
i�B
jB
jB
jB
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
lB
lB
l=B
lWB
lqB
l�B
l�B
l�B
l�B
mB
mB
mCB
m�B
m�B
n/B
nIB
n}B
n�B
o B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q[B
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
t9B
tTB
tTB
t�B
t�B
u%B
u?B
utB
uZB
u�B
u�B
u�B
u�B
vFB
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
w�B
w�B
xRB
xlB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
zB
z*B
z*B
z*B
z�B
z�B
{B
{B
{JB
{B
{�B
|6B
|jB
|�B
|�B
}B
}"B
}<B
}<B
}�B
}�B
}�B
~B
~]B
~BB
~]B
~�B
~wB
~�B
HB
HB
HB
cB
}B
�B
�B
� B
�B
�B
�4B
�OB
��B
��3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.03(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201909110055162019091100551620190911005516202207271132352022072711323520220727113235202207271535102022072715351020220727153510  JA  ARFMdecpA30a                                                                20190831093714  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190831093750  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190831093750  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190831093751  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190831093751  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190831093751  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190831093751                      G�O�G�O�G�O�                JA  ARUP                                                                        20190831095518                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190901000000  CF  PSAL_ADJUSTED_QC@ff@}p�G�O�                JM  ARCAJMQC2.0                                                                 20190910155516  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190910155516  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023235  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063510  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                