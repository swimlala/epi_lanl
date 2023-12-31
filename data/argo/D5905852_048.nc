CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-28T09:37:55Z creation;2020-05-28T09:37:57Z conversion to V3.1;2022-08-02T05:10:57Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200528093755  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  A30_8420_048                    2C  D   APEX                            8420                            2.11.2                          846 @�Εβ 1   @�ϣ� @/P������b�ۋ�q1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bz  B~  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  BЙ�B�33Bי�B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  CffC�3C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{y�D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D��3D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@|(�@�{@�AffA>=qA^�\A~�RA�
=A�G�A�p�A�\)AυA߅A�\)A��B�
BB�RBB'��B/��B7��B?�\BG�\BO��BW��B_��Bg�BoBy�HB}��B�ǮB���B�ǮB��
B�ǮB��qB���B��HB���B���B��
B���B��)B��)B�B���BýqB��
B��BЀ B�{B�k�Bۙ�Bߣ�B�qB�qB��
B��HB��
B��HB��
B��)C��C��C�C�C	��C\)C�fC�
C�HC��C�fC��C��C�C�C��C!�fC#�C%�fC'�HC)�C+�C-�C/�C1�C3�C5�fC7�C9�C;��C=��C@�CA�HCC�HCE�CG�CI�CK�CM�CO��CQ�CS�3CU�CW��CY�C[�C]�fC_��Ca�Cc��Ce��Cg��Ci��Ck��Cm��Co�fCq��Cs��Cu�qCw�Cy�fC{�fC}��C��C��{C��
C��RC���C���C��{C��RC���C��3C��RC���C��
C���C��)C���C���C��
C��RC��RC��
C��{C��RC���C���C��RC���C��
C���C��3C��{C���C��3C���C���C��
C���C���C���C���C��{C��3C���C���C��)C��{C���C��RC���C��
C��
C���C���C���C��RC���C���C��3C��3C��3C���C���C��{C���C��
C���C��RC��{C��3C��{C��{C��{C��{C��{C��3C��3C��3C��{C���C��{C��{C��{C���C��{C��C��{C���C���C��
C��RC��{C���C���C���C��3C��C��3C��3C��3C��RC��RC��RC���C��{C���C���C���C��RC��
C��3C��C��
C���C��
C��)C���C��
C���C��
C��3C���C��{C���C��3C��
C���C��{C���C��
D y�D ��Dz�D��D{�D�=Dy�D��Dz=D�=Dz�D�=Dy�D�=Dz=D��Dz=D�=D	z=D	��D
{�D
��Dz�D��D{�D��D|�D��D|)D�)Dz�D��D{�D��D{�D��Dy�D��Dy�D��D}qD��Dz�D��D|)D�=Dx�D�=D}qD�)Dz�D��D|)D�)Dz�D��D\D��Dz=D��Dy�D��Dy�D��D |)D ��D!{�D!��D"z�D"��D#x�D#��D$y�D$��D%z�D%��D&z�D&��D'y�D'��D(|�D(�)D)y�D)��D*z=D*��D+z=D+��D,{�D,��D-|�D-��D.}qD.��D/y�D/��D0z�D0�)D1z=D1�=D2|)D2�)D3{�D3�=D4z�D4��D5z�D5�)D6|�D6�)D7z=D7��D8z=D8�)D9|�D9�)D:{�D:��D;z=D;��D<x�D<��D=|)D=�)D>|)D>��D?y�D?��D@z=D@�=DAz=DA��DB|)DB��DCy�DC��DD|�DD��DE{�DE��DF|�DF��DG{�DG�)DH{�DH��DIz�DI�)DJz=DJ��DK|�DK�qDL{�DL�)DM|)DM�qDN}qDN��DOz�DO�=DPx�DP��DQx�DQ��DR|)DR��DS|)DS�)DTz�DT��DU{�DU��DVz�DV��DW|)DW��DXx�DX��DYz�DY��DZz�DZ��D[|)D[��D\y�D\��D]{�D]��D^{�D^��D_z=D_�=D`z=D`��Daz=Da��Dbz=Db�=Dc{�Dc�)Dd|�Dd��Dex�De��Df|)Df�=Dgy�Dg��Dhy�Dh��Diz=Di��Dj|)Dj��Dky�Dk��Dl|�Dl�=Dm{�Dm��Dn|)Dn�)Do|�Do��Dpz=Dp�)Dq|)Dq��Dr|)Dr�=Dsx�Ds��Dt{�Dt�=Duy�Du��Dvx�Dv��Dwz�Dw��DxxRDx��Dyz=Dy��Dz{�Dz��D{w
D{�RD|x�D|��D}|)D}��D~z=D~��Dx�D��D�=�D�}qD��qD��qD�=�D�|�D���D��D�=�D�}�D��D��qD�<�D�|{D��qD��D�>D�}�D��qD��qD�>D�~fD��D���D�=qD�}D���D��D�=qD�}D��D��qD�<�D�|)D��D���D�>D�}qD��{D��D�>fD�~fD��qD��D�=qD�}qD��fD��D�=D�}D��qD��qD�>D�}qD��qD��D�=qD�~fD��fD��fD�=qD�|{D��qD��qD�=�D�~D��qD��D�=qD�|�D��qD���D�<�D�|�D��D��)D�=D�}D��)D���D�=�D�~fD���D��qD�=qD�}�D��fD���D�=D�|{D���D���D�>D�~D��fD��fD�>D�~�D��
D��D�=qD�|{D���D���D�=�D�~D��fD���D�=qD�}qD���D��D�=�D�}D��{D��D�<{D�}D���D���D�=D�}D���D��qD�>D�}qD��D��qD�=D�}D���D��D�<)D�}D���D���D�=�D�~D���D��qD�=qD�|�D��qD��D�=D�{�D��)D��{D�=D�~fD���D���D�>�D�~D���D��qD�=D�~D��D���D�=qD�}�D��{D��{D�<{D�|�D��fD���D�<�D�|�D��D��qD�=qD�}qD��\D��\D�=qD�}qD��qD���D�=D�|{D���D���D�=qD�~D��qD��qD�<�D�|�D��qD���D�<�D�|�D���D��qD�=D�}qD���D��)D�<{D�}qD���D���D�<�D�}D���D��qD�=�D�}qD��qD��{D�<)D�|)D��D��D�<{D�}qD���D��qD�<{D�}qD���D���D�=qD�|�D��D���D�<)D�}qD��D��D�<�D�}D��qD���D�=D�|{D��D���D�=qD�}D��D��D�<{D�}�D���D��D�<{D�~D��
D���D�>D�~D��fD���D�=�D�|�D��qD���D�>fD�~D���D���D�=qD�}D½D��qD�=qD�|)DýqD��D�=D�|)DĽD���D�<�D�}�Dſ\D��fD�=D�}DƼ�D��qD�=qD�|{DǼ�D���D�<�D�}�DȾD��qD�=D�}qDɽD��D�=D�|�DʽqD��D�=D�}D˼{D��D�<�D�|)D̽qD��D�=�D�}qDͼ�D��qD�<{D�}DξD��D�>fD�~fDϾD��D�<�D�|�DнD��D�=qD�|{DѽqD���D�=�D�}qDҽ�D��qD�=�D�~fDӽ�D���D�=D�}�DԾfD��D�>D�}�Dս�D��D�=D�}DֽqD��qD�=�D�|{D׽qD���D�>�D�~fDؾD���D�=�D�}DٽqD��qD�>D�~DھfD���D�=qD�~fD۽qD��D�<{D�|{Dܼ�D��qD�>D�~�DݾD��D�>fD�~fD޼{D���D�<�D�|�D߽qD��)D�<�D�|�D��D��D�>�D�~fD�D��fD�>D�}D��D��D�>D�}�D��D��qD�<�D�}�D�qD��qD�=qD�}D�D���D�=D�}�D�qD���D�=D�|{D��D��qD�=qD�|�D�qD��
D�=�D�|�D��D��qD�=qD�}qD��D��D�<�D�|{D��D��D�=�D�~�D�D���D�=�D�}qD���D��D�>fD�~fDD��D�>D�}�D��D��qD�=qD�~fD�fD��D�=D�}qD�D��D�=qD�}�D�D���D�=�D�}�D�D���D�>D�~D��fD��qD�=D�}D��qD���D�>fD�~fD��D��{D�=qD�}qD���D��)D�<{D�~D��D��D�<)D�}D��qD���D�<{D�}�D��qD���D�>�D�b=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A˂�Aˆ%A˄�A�y	A�x�A�y�A�{�A�{�A�{A�~�A�zDA�x�A�v`A�o A�m]A�pA�gmA�=<A��A��(A��"A��A��A��>A��TA��oA��]A��A���A��oAƌ~A��A��?A�m�A��hA�v+A���A���A�v�A�_�A�GzA�`�A���A���A��A���A��A�[WA���A�'�A�PA�QA��A��A���A�~�A��A�YA�K�A��A��$A�X�A��A��,A��;A�7�A� �A��A�iA�,qA�^A�zA�ӏA��A�r�A�
�A�z�A��YA���A�!-A��A���A���A���A��~A�z�A��FA�+kA�8�A��A�y>A�pA��A{�oAv�As�Aq4Am \AkOAh��AgM�Ad*0Aa �A]ݘA\a�A[?}AY@�AVS�AR/�AOq�AHϫAE��AA4�A=y�A9(�A6˒A4%FA2��A0�#A/V�A.�A,��A*��A)c�A)�A(33A&@�A%�A#�$A#��A$F�A#s�A"��A#z�A#�A#X�A"E9A!J�AsA�A�A�CA��A�BA�CAB�A �A�AȴA�-AcA��AjAVA:�A�`A'�AjA��A��A�4A�KA4nAS&A�A��A5�AOA�A�HA��A+�A�+A�AAȴA:�Au�A6�A��A�uA
��A
J�A	֡A	��Ab�A�@A3�A��ACA�*A�'A͟A�KA�rA/�A�wA/�Aw2A%FA�&A�4AjA��Aa�A�A �wA �$A 1'@�6z@��Z@���@���@�YK@��@��,@�n�@��@�A�@���@�<�@�@���@���@�r�@�7�@�@�o�@�;@��K@�@�K�@��U@�Ta@�$@�ԕ@��@� @�n�@�bN@���@��@��@�u%@��@��@뙚@��K@�tT@�B[@���@�E9@��m@�`B@�&�@��@�g�@�"�@���@�G@��@�\�@��z@�A�@��@��@޵@�R�@���@ݨX@�G�@�2�@�;d@��@�A�@٣n@�u%@רX@�C�@� \@��X@�8�@Ն�@�z@��@�Mj@��@ҭ�@��@�S&@���@�v�@�($@Ϸ�@�J#@��@κ�@Ζ�@�^5@�*�@ͱ[@���@̻�@̄�@�@�o @�Ĝ@ʡb@�r�@��Z@ɘ�@�|�@�ȴ@�9X@��@ǝ�@��@Ƈ�@�	@ż@ł�@�/@��@�:*@��)@Ò:@�V@³h@�Q@�u@��K@�J#@��@���@��`@��I@�D�@�	@��9@���@�&�@��$@�Q�@���@���@�4@��2@��o@�_@��#@���@�/@�ی@�l�@��@���@��:@�7L@���@��_@���@�R�@�	@�u�@���@��@��N@�x�@��@�M@���@�2a@��c@���@��@��@�C�@��@���@��@�l�@�J@��@�^�@��@��'@���@��p@��@�=q@��@��k@�!-@���@�a|@�	@�@�X@���@�K^@��r@���@���@�U�@���@�M@��7@�^�@��)@��@��@�s@�(�@��[@�;�@���@�'�@��X@�*�@��4@��@�`�@�ƨ@��@���@�]�@��@�� @�"h@��@���@��@@��@���@��@�C�@��/@�xl@�	�@��@�M@��@��D@��j@���@���@���@�m]@�RT@� i@���@�{�@�8�@�'R@��;@���@���@�v`@�!�@��R@��@���@�s�@�:*@�	@���@�x@�C�@�8@�$t@�@�p;@�@��r@��@��K@���@��[@�Q�@�.I@��$@�9X@�1@��Q@��P@�m]@�o@���@��.@�Xy@��@��@��w@�w2@�iD@�=@���@��Y@�9X@��m@�P�@�!-@��2@��!@�p;@�2�@��W@��$@�{J@�Y@�͟@��@�R�@��@��Z@��X@�dZ@�c�@�&@�7�@�1@���@�k�@�F@�4@��8@���@�z@�y>@�?�@��@���@��S@���@���@�>�@�%F@��X@��@��@���@���@��@��t@��P@�dZ@�Z�@�6z@���@�E�@�J@��z@�j@�e,@��@���@�kQ@�H@�	�@��T@��@��@��S@��	@�b�@�?}@�ߤ@���@��j@��I@��+@�_@�!�@�u@˒@��@iD@RT@;d@
=@~Ov@}�7@}�@|��@|7�@{�&@{��@{F�@{�@z{�@yzx@x�I@xq@xH@w�]@w��@wS�@v��@u�@u��@uF@t��@t�)@s�@s)_@r��@r��@r@q�7@qa�@q�@p�K@p~(@p�@o˒@oRT@n�@n\�@n@m�=@l�f@lw�@k��@k�@k(@j�@j.�@i�@i��@im]@i�@hS�@h,=@h!@gݘ@g�P@g1�@f�@fh
@f@e�t@e�@d��@de�@dXy@dD�@c��@cv`@b�@b}V@b@�@b+k@a�o@aDg@`Ɇ@`�o@`e�@`A�@`�@_j�@^�H@^�,@^��@^v�@^�@]��@]�h@]`B@]8�@];@\ی@\h�@\6@\	�@[>�@Zff@Z	@Y��@Y��@Y=�@Y!�@X�E@Xu�@X:�@W�W@W�:@WW?@V�@U�>@T�@T��@TM@S�+@S��@S�4@SdZ@R��@Rs�@R($@Q��@Q%@P$@O�@O�;@O��@O�	@OC�@N��@N��@N0U@M�N@M��@M\�@M�@L�/@L��@L[�@LM@Kخ@K�a@KJ#@J��@J�@J�@J�@J{@I�X@I&�@H�@H��@H@G�@G��@G8@G�@FJ�@E��@E�7@EIR@D��@D�U@D�I@D~(@DN�@D:�@C�W@C�*@C�4@C8@B�@B�@B��@BE�@Be@A��@A�T@A�@A�#@A��@A+�@@֡@@�I@@>B@?�F@?��@?W?@>�,@>R�@>5?@=�N@=j@<֡@<��@<y>@<bN@<S�@<�@;dZ@;�@:�@:��@:��@:n�@:6�@9��@9��@9��@9Vm@9!�@8��@8(�@7��@7�}@7��@7a@7�@6��@6v�@6 �@5�@5j@5#�@4�|@4��@4�@4U2@3��@3��@3�:@3|�@3j�@3=@3�@2�@2M�@2�@1�@1��@1s�@1(�@0��@0A�@/��@/��@/��@/�:@/g�@.��@.xl@.L0@.:*@.	@-�@-/@,�U@,�@,��@,Xy@,"h@+�a@+�@@+y�@+)_@*�X@*c @)�@)��@)��@)/@(��@(V�@([�@(*�@'��@'_p@&��@&�6@&��@&� @&��@&q�@&Ta@&e@%�@%�n@%��@%N<@$�@$�p@$��@$z�@$<�@#�@#�V@#�	@#S�@#S@"��@"{�@"-@"$�@!��@!�@!s�@!F@!�@ �4@ z�@ U2@ @�g@��@��@v`@@O@ i@�@��@M�@�>@�d@��@�=@}�@8�@��@��@U2@�@��@��@��@�@��@�4@~�@a@(@�y@�'@��@W�@�@�@��@|@J�@q@�@�@��@_@:�@2�@x@��@~�@�H@�+@B[@�@��@J�@�	@�e@w�@%�@��@��@�P@x@O@C@��@�\@^5@#:@�)@�@c@�@�v@��@y>@?�@��@��@�q@�V@v`@J#@9�@@�@͟@�\@W�@E�@�@��@�@�"@\�@�@�5@�j@y>@oi@U2@1'@@�+@�m@˒@��@��@�@Mj@�@�@
��@
�2@
��@
ff@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A˂�Aˆ%A˄�A�y	A�x�A�y�A�{�A�{�A�{A�~�A�zDA�x�A�v`A�o A�m]A�pA�gmA�=<A��A��(A��"A��A��A��>A��TA��oA��]A��A���A��oAƌ~A��A��?A�m�A��hA�v+A���A���A�v�A�_�A�GzA�`�A���A���A��A���A��A�[WA���A�'�A�PA�QA��A��A���A�~�A��A�YA�K�A��A��$A�X�A��A��,A��;A�7�A� �A��A�iA�,qA�^A�zA�ӏA��A�r�A�
�A�z�A��YA���A�!-A��A���A���A���A��~A�z�A��FA�+kA�8�A��A�y>A�pA��A{�oAv�As�Aq4Am \AkOAh��AgM�Ad*0Aa �A]ݘA\a�A[?}AY@�AVS�AR/�AOq�AHϫAE��AA4�A=y�A9(�A6˒A4%FA2��A0�#A/V�A.�A,��A*��A)c�A)�A(33A&@�A%�A#�$A#��A$F�A#s�A"��A#z�A#�A#X�A"E9A!J�AsA�A�A�CA��A�BA�CAB�A �A�AȴA�-AcA��AjAVA:�A�`A'�AjA��A��A�4A�KA4nAS&A�A��A5�AOA�A�HA��A+�A�+A�AAȴA:�Au�A6�A��A�uA
��A
J�A	֡A	��Ab�A�@A3�A��ACA�*A�'A͟A�KA�rA/�A�wA/�Aw2A%FA�&A�4AjA��Aa�A�A �wA �$A 1'@�6z@��Z@���@���@�YK@��@��,@�n�@��@�A�@���@�<�@�@���@���@�r�@�7�@�@�o�@�;@��K@�@�K�@��U@�Ta@�$@�ԕ@��@� @�n�@�bN@���@��@��@�u%@��@��@뙚@��K@�tT@�B[@���@�E9@��m@�`B@�&�@��@�g�@�"�@���@�G@��@�\�@��z@�A�@��@��@޵@�R�@���@ݨX@�G�@�2�@�;d@��@�A�@٣n@�u%@רX@�C�@� \@��X@�8�@Ն�@�z@��@�Mj@��@ҭ�@��@�S&@���@�v�@�($@Ϸ�@�J#@��@κ�@Ζ�@�^5@�*�@ͱ[@���@̻�@̄�@�@�o @�Ĝ@ʡb@�r�@��Z@ɘ�@�|�@�ȴ@�9X@��@ǝ�@��@Ƈ�@�	@ż@ł�@�/@��@�:*@��)@Ò:@�V@³h@�Q@�u@��K@�J#@��@���@��`@��I@�D�@�	@��9@���@�&�@��$@�Q�@���@���@�4@��2@��o@�_@��#@���@�/@�ی@�l�@��@���@��:@�7L@���@��_@���@�R�@�	@�u�@���@��@��N@�x�@��@�M@���@�2a@��c@���@��@��@�C�@��@���@��@�l�@�J@��@�^�@��@��'@���@��p@��@�=q@��@��k@�!-@���@�a|@�	@�@�X@���@�K^@��r@���@���@�U�@���@�M@��7@�^�@��)@��@��@�s@�(�@��[@�;�@���@�'�@��X@�*�@��4@��@�`�@�ƨ@��@���@�]�@��@�� @�"h@��@���@��@@��@���@��@�C�@��/@�xl@�	�@��@�M@��@��D@��j@���@���@���@�m]@�RT@� i@���@�{�@�8�@�'R@��;@���@���@�v`@�!�@��R@��@���@�s�@�:*@�	@���@�x@�C�@�8@�$t@�@�p;@�@��r@��@��K@���@��[@�Q�@�.I@��$@�9X@�1@��Q@��P@�m]@�o@���@��.@�Xy@��@��@��w@�w2@�iD@�=@���@��Y@�9X@��m@�P�@�!-@��2@��!@�p;@�2�@��W@��$@�{J@�Y@�͟@��@�R�@��@��Z@��X@�dZ@�c�@�&@�7�@�1@���@�k�@�F@�4@��8@���@�z@�y>@�?�@��@���@��S@���@���@�>�@�%F@��X@��@��@���@���@��@��t@��P@�dZ@�Z�@�6z@���@�E�@�J@��z@�j@�e,@��@���@�kQ@�H@�	�@��T@��@��@��S@��	@�b�@�?}@�ߤ@���@��j@��I@��+@�_@�!�@�u@˒@��@iD@RT@;d@
=@~Ov@}�7@}�@|��@|7�@{�&@{��@{F�@{�@z{�@yzx@x�I@xq@xH@w�]@w��@wS�@v��@u�@u��@uF@t��@t�)@s�@s)_@r��@r��@r@q�7@qa�@q�@p�K@p~(@p�@o˒@oRT@n�@n\�@n@m�=@l�f@lw�@k��@k�@k(@j�@j.�@i�@i��@im]@i�@hS�@h,=@h!@gݘ@g�P@g1�@f�@fh
@f@e�t@e�@d��@de�@dXy@dD�@c��@cv`@b�@b}V@b@�@b+k@a�o@aDg@`Ɇ@`�o@`e�@`A�@`�@_j�@^�H@^�,@^��@^v�@^�@]��@]�h@]`B@]8�@];@\ی@\h�@\6@\	�@[>�@Zff@Z	@Y��@Y��@Y=�@Y!�@X�E@Xu�@X:�@W�W@W�:@WW?@V�@U�>@T�@T��@TM@S�+@S��@S�4@SdZ@R��@Rs�@R($@Q��@Q%@P$@O�@O�;@O��@O�	@OC�@N��@N��@N0U@M�N@M��@M\�@M�@L�/@L��@L[�@LM@Kخ@K�a@KJ#@J��@J�@J�@J�@J{@I�X@I&�@H�@H��@H@G�@G��@G8@G�@FJ�@E��@E�7@EIR@D��@D�U@D�I@D~(@DN�@D:�@C�W@C�*@C�4@C8@B�@B�@B��@BE�@Be@A��@A�T@A�@A�#@A��@A+�@@֡@@�I@@>B@?�F@?��@?W?@>�,@>R�@>5?@=�N@=j@<֡@<��@<y>@<bN@<S�@<�@;dZ@;�@:�@:��@:��@:n�@:6�@9��@9��@9��@9Vm@9!�@8��@8(�@7��@7�}@7��@7a@7�@6��@6v�@6 �@5�@5j@5#�@4�|@4��@4�@4U2@3��@3��@3�:@3|�@3j�@3=@3�@2�@2M�@2�@1�@1��@1s�@1(�@0��@0A�@/��@/��@/��@/�:@/g�@.��@.xl@.L0@.:*@.	@-�@-/@,�U@,�@,��@,Xy@,"h@+�a@+�@@+y�@+)_@*�X@*c @)�@)��@)��@)/@(��@(V�@([�@(*�@'��@'_p@&��@&�6@&��@&� @&��@&q�@&Ta@&e@%�@%�n@%��@%N<@$�@$�p@$��@$z�@$<�@#�@#�V@#�	@#S�@#S@"��@"{�@"-@"$�@!��@!�@!s�@!F@!�@ �4@ z�@ U2@ @�g@��@��@v`@@O@ i@�@��@M�@�>@�d@��@�=@}�@8�@��@��@U2@�@��@��@��@�@��@�4@~�@a@(@�y@�'@��@W�@�@�@��@|@J�@q@�@�@��@_@:�@2�@x@��@~�@�H@�+@B[@�@��@J�@�	@�e@w�@%�@��@��@�P@x@O@C@��@�\@^5@#:@�)@�@c@�@�v@��@y>@?�@��@��@�q@�V@v`@J#@9�@@�@͟@�\@W�@E�@�@��@�@�"@\�@�@�5@�j@y>@oi@U2@1'@@�+@�m@˒@��@��@�@Mj@�@�@
��@
�2@
��@
ff@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�zB��B��B�`B�FB�`B�`B�FB�zB�FB�zB�FB�`B�B��B��B�tB�&B�:B��B��B��B��B��B��B��B��B��B�bB��B�ZB�_B�B	JB	B	�B	.}B	C�B	��B
GB
C-B
f2B
\�B
�#B
�~B
r�B
GB
~�B
��B
��BB6�BVmB�dB�/B��B�%Bq�B_�B\�BbhBe�Bc�BaB_!B[	BU�BF?BFBFYB@OBB�B]/BN"BD�B-)B#�BSB|B|�Bv�Bj�B]BR:BE�B6�B�B
�B
��B
�B
�7B
n�B
I�B
+�B
�B	� B	��B	�B	��B	��B	~�B	qAB	ZB	C�B	1B	&�B	/B	�B	uB��B�B�SB�,B��B��B��B�4B�LB�tB��B��B�LB�$B��B��B��B��B��B�}B��B�;B��B�jB��B�B	�B��B��B��B�B��B��B��B	�B	�B	�B	 B	tB	�B	+6B	8�B	NB	a-B	f�B	ncB	{�B	��B	�-B	~BB	w�B	x�B	��B	�{B	�]B	��B	�SB	�_B	��B	��B	�@B	��B	��B	�SB	�B	�/B	��B	��B	�dB	��B	�;B	�-B	�OB	��B	��B	�BB	��B	�AB	��B	�HB	�qB	�DB	��B	��B	��B	�NB	ѷB	�uB	��B	ևB	��B	�mB	�$B	�?B	��B	�B	��B	یB	�QB	�1B	��B	��B	��B	�mB	�
B	�YB	�mB	�2B	�{B	��B	��B	՛B	ՁB	�sB	�YB	�$B	׍B	��B	��B	�sB	�sB	�
B	�sB	�EB	��B	��B	�sB	��B	�B	��B	�YB	׍B	��B	רB	�B	�+B	�yB	�_B	�yB	�B	�1B	��B	�1B	��B	�B	�B	ڠB	ڠB	ڠB	ڠB	چB	�	B	یB	ۦB	��B	��B	��B	�]B	��B	��B	��B	��B	��B	ݲB	��B	ݲB	�B	�OB	�;B	�B	��B	߾B	��B	�'B	�B	�HB	�|B	��B	�NB	�hB	�B	� B	�B	�B	��B	�&B	��B	�tB	��B	�B	�`B	�,B	��B	��B	��B	��B	�B	�B	��B	�fB	�2B	�B	�B	�8B	�>B	�XB	�_B	�0B	�B	�B	�0B	�B	�B	�=B	�=B	��B	�CB	�wB	�]B	��B	�B	�WB	�CB	�B	��B	��B	�CB	��B	��B	�5B	��B	�B	��B	�B	�B	��B	��B	��B	��B	�AB	�vB	�B	��B	��B	��B	�B	�B	�'B	�'B	�B	�GB	�aB	�GB	��B	�B	�B	��B	�B	�B	�9B	��B	�B	��B	�B	��B	�?B	��B	�?B	�?B	�?B	�tB	�ZB	�tB	�B	�B	�B	��B	��B	�FB	��B	��B	��B	��B	��B	�JB	�B	��B	�^B	��B	��B	��B	�B	�8B	�LB	�XB	��B	�0B	�xB	�>B	�B	��B	��B	��B	��B	�XB	��B	��B	��B	�B	�PB	��B	�<B	��B	�(B	�B	�B	�B	�HB	��B
 iB
 B
�B
AB
[B
[B
�B
�B
-B
B
�B
+B
KB
	B
	B
	�B
	�B

	B
)B
)B

�B
DB
DB
B

�B
B
�B
~B
0B
0B
B
~B
�B
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
HB
B
 B
TB
aB
SB
B
�B
B
QB
WB
�B
�B
�B
�B
B
�B
OB
jB
�B
!B
�B
 �B
!|B
!bB
!bB
!B
!|B
!|B
!�B
!�B
"NB
"�B
"�B
"�B
# B
#�B
#nB
#�B
#�B
# B
#�B
#�B
#�B
#�B
#�B
%B
%�B
%�B
#�B
#�B
#�B
$@B
$&B
$&B
$@B
#�B
$�B
&�B
'�B
)*B
)�B
)�B
)�B
*�B
*eB
*�B
,"B
+�B
+�B
+�B
+�B
+6B
*�B
+B
+6B
+�B
,�B
+�B
+QB
+�B
,qB
,�B
-wB
-�B
-�B
-�B
-]B
-�B
.IB
/5B
/B
/�B
/�B
/�B
0;B
0�B
1�B
3MB
3�B
4B
4nB
4�B
4�B
5%B
5ZB
5�B
5tB
5ZB
5ZB
5�B
6zB
6zB
6FB
6�B
6zB
6FB
6FB
6B
6`B
6�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
9�B
9�B
9�B
:*B
9�B
;B
;JB
;JB
;JB
<B
<PB
<�B
<�B
<�B
=<B
=qB
=qB
=�B
=�B
>(B
>]B
>�B
>�B
?.B
?�B
?}B
@4B
@OB
@�B
@�B
@�B
@�B
AB
A�B
A�B
AoB
A�B
A�B
A�B
B'B
B[B
BuB
B�B
CB
CaB
C{B
C{B
CaB
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
FB
F%B
F%B
FYB
GEB
G�B
GzB
GzB
G�B
HKB
H�B
H�B
H�B
H�B
IB
IB
I�B
I�B
I�B
J�B
KB
KDB
K�B
K�B
K�B
K�B
L0B
L~B
L~B
L�B
MB
M6B
MPB
M�B
NpB
N�B
N�B
OB
OBB
OvB
O\B
O�B
PB
PHB
P�B
Q B
Q�B
RB
RB
R B
R B
RTB
R�B
R�B
R�B
S@B
S[B
SuB
S�B
S�B
S�B
S�B
T,B
TFB
T,B
T�B
T�B
T�B
T�B
T�B
U2B
UMB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
VmB
V9B
U�B
U�B
VB
VSB
V9B
V9B
VB
U�B
U�B
VSB
VSB
V9B
VmB
V�B
V�B
V�B
W?B
W�B
W�B
W�B
W�B
X+B
X�B
X�B
X�B
X�B
YKB
YB
Y�B
Y�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
[�B
[�B
[�B
[�B
\CB
]~B
]�B
]�B
^B
]�B
^OB
^jB
^jB
^jB
^�B
^�B
^�B
_!B
_�B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
a-B
a-B
aHB
a-B
abB
a|B
a|B
a�B
a�B
a�B
a�B
bB
bB
b4B
b4B
bhB
b�B
b�B
b�B
cB
cB
c B
c�B
c�B
d@B
dtB
dZB
dZB
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
h�B
i*B
i�B
jKB
jeB
jB
j�B
jKB
j�B
j�B
i�B
j0B
kB
j�B
j�B
j�B
kB
kB
kB
kB
j�B
j�B
j�B
k6B
kQB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
l"B
l�B
l�B
l�B
l�B
l�B
mCB
mwB
mwB
nIB
n}B
n�B
o5B
o�B
p;B
pUB
p�B
p�B
qB
qvB
q[B
q�B
rB
q�B
rB
rGB
raB
r�B
r�B
sB
shB
s�B
s�B
s�B
s�B
s�B
s�B
tB
s�B
tB
t�B
tnB
tnB
t�B
t�B
t�B
u%B
u?B
u%B
u%B
u?B
utB
u�B
u�B
u�B
vB
u�B
vB
v`B
v`B
wB
wLB
w�B
w�B
xB
xRB
x�B
x�B
y	B
yXB
yrB
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
{0B
{JB
{�B
|B
|B
|PB
|jB
|�B
}B
}"B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
~]B
~wB
~wB
~�B
~�B
B
.B
cB
�B
�B
� B
�OB
�OB
�iB
��B
��B
��B
��B
��B
��B
��B
�B
�;B
�oB
�oB
��B
�oB
��B
�'B
�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B�zB�zB�zB�`B��B�`B��B�`B�zB�,B��B��B��B��B��B��B��B��B��B��B��B��B��B�4B�:B�fB��B��B�>B	B	sB	�B	/5B	DB	� B
{B
CaB
f2B
^B
�B
��B
w�B
J�B
�B
��B
�=B&B9�BZ�B��B�B�B�BwLBa|B]�BcBf�Be`Bb�B`B\CBX_BH�BHKBKBE�BEB`'BPbBG�B/�B#�BS[B}�B~�By�BmCB_pBT�BIRB;�B~B
�B
�B
�/B
�PB
sMB
N<B
1AB
oB	�B	ԕB	��B	��B	��B	��B	u%B	^B	GB	3B	(�B	 BB	$B	�B��B�cB�#BٴBѷBB�B��B��B��B��B��B��B�B� B�oB�|B�B�aB��B��B�B��B��BǮB�B	uB�.B�8B��B�GB�lB�xB�BB	B	
�B	[B	�B	�B	�B	+QB	9	B	N"B	aHB	gB	n�B	|PB	��B	��B	�4B	xB	xRB	��B	��B	��B	�4B	�%B	��B	��B	�BB	��B	�EB	��B	�YB	��B	��B	��B	��B	�PB	�.B	�AB	ĜB	�UB	��B	��B	�B	�MB	�GB	��B	�B	�(B	��B	��B	�B	�B	ѷB	�TB	�,B	֡B	�YB	�+B	��B	רB	רB	ٴB	ںB	�xB	��B	��B	��B	ڠB	ٴB	�_B	֡B	�sB	��B	�
B	ՁB	��B	՛B	�yB	�B	�SB	�EB	��B	�sB	��B	�+B	�EB	��B	��B	�sB	�_B	خB	�EB	�+B	��B	�_B	�yB	��B	׍B	��B	�B	�B	�yB	�yB	��B	خB	��B	�B	�B	�eB	��B	�KB	�B	��B	��B	�	B	��B	�	B	�#B	��B	�B	�)B	�]B	�xB	ܒB	�B	�IB	�dB	�IB	�/B	ݘB	�OB	�OB	�OB	޸B	�!B	��B	�vB	�'B	�B	�vB	��B	�|B	�B	��B	�B	�B	�B	�:B	�B	�B	�B	�@B	�tB	�@B	��B	�B	�`B	�B	�B	�LB	�B	�B	�RB	�B	�mB	�8B	�B	�B	�B	�RB	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	��B	�CB	�B	��B	��B	�CB	��B	�B	�B	�CB	��B	�B	�wB	�B	� B	�B	�;B	�oB	�!B	�B	��B	�'B	�'B	�[B	�'B	�B	�B	��B	�GB	�GB	�-B	��B	��B	�vB	�B	�GB	�B	�B	�B	�3B	�3B	�9B	�9B	�B	��B	��B	�ZB	��B	�B	�ZB	�ZB	��B	�B	�tB	�tB	��B	��B	��B	��B	�`B	�`B	�FB	�B	��B	�FB	��B	��B	�JB	�B	�JB	��B	��B	�0B	��B	�0B	�*B	��B	�RB	��B	�fB	��B	�6B	��B	��B	��B	��B	�*B	�>B	�>B	�B	��B	��B	�B	�B	�PB	��B	�VB	��B	��B	��B	�.B	�HB	�cB	��B
 OB
 �B
oB
�B
[B
[B
�B
�B
B
�B
gB
B
+B
fB
	7B
	7B
	�B
	�B

=B
DB
DB
B
xB
xB
DB

�B
DB
0B
�B
dB
dB
~B
�B
"B
pB
(B
�B
.B
B
�B
�B
�B
�B
B
�B
TB
:B
�B
aB
SB
EB
�B
7B
�B
�B
)B
)B
�B
B
OB
B
�B
�B
�B
;B
�B
!-B
!�B
!�B
!�B
!bB
!�B
!�B
"B
"4B
"�B
"�B
#:B
"�B
#nB
#�B
#�B
#�B
#�B
#TB
#�B
$B
#�B
#�B
$@B
%FB
&2B
&fB
#�B
#�B
$B
$tB
$ZB
$tB
$tB
$B
$�B
&�B
'�B
)_B
)�B
)�B
*B
*�B
*B
+B
,WB
,B
,B
,=B
,"B
+kB
+B
+B
+QB
,B
-B
,=B
+�B
+�B
,�B
,�B
-�B
./B
-�B
-�B
-�B
-�B
.}B
/iB
/5B
/�B
/�B
0!B
0oB
0�B
1�B
3�B
3�B
4B
4�B
4�B
5%B
5ZB
5�B
5�B
5�B
5tB
5�B
6`B
6�B
6�B
6zB
6�B
6�B
6zB
6�B
6`B
6�B
7LB
7�B
7�B
8B
88B
8�B
8�B
9$B
9�B
9�B
9�B
:DB
:^B
;JB
;dB
;B
;B
<PB
<jB
<�B
<�B
="B
=�B
=�B
=�B
>(B
>B
>wB
>�B
>�B
?HB
?cB
?�B
?�B
@iB
@iB
@�B
AB
@�B
AB
AUB
A�B
A�B
A�B
A�B
BB
B'B
B[B
BuB
B�B
B�B
CaB
C�B
C�B
C�B
C�B
DB
D3B
D�B
EB
EB
D�B
EB
E�B
FB
F?B
FYB
FYB
F�B
GzB
G�B
G�B
G�B
HKB
H�B
H�B
H�B
IB
IB
I7B
IRB
I�B
I�B
I�B
J�B
K)B
KxB
K�B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
MPB
MjB
M�B
NB
N�B
N�B
OB
O(B
O\B
O�B
O�B
PB
PbB
P}B
P�B
QhB
RB
R B
R:B
R:B
RTB
R�B
R�B
R�B
SB
SuB
SuB
S�B
S�B
S�B
S�B
TB
TFB
TaB
TaB
T�B
T�B
T�B
UB
UB
UgB
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
V�B
VmB
VB
VB
V9B
VmB
VSB
VSB
VB
VB
VB
VmB
V�B
VSB
V�B
V�B
V�B
V�B
WYB
XB
W�B
W�B
X+B
XEB
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
[	B
[�B
[�B
\B
[�B
[�B
[�B
\B
\xB
]�B
]�B
]�B
^5B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_�B
`\B
`vB
`�B
`�B
a-B
aB
a-B
aHB
aHB
abB
abB
a|B
a�B
a�B
a�B
a�B
bB
bB
bNB
b4B
bNB
bhB
b�B
b�B
cB
c B
c B
c:B
cnB
c�B
c�B
dZB
dtB
d�B
d�B
d�B
e,B
e�B
e�B
e�B
fB
fB
f2B
f2B
fLB
f�B
gB
gB
g�B
g�B
g�B
h>B
i*B
i_B
jB
jB
j�B
j�B
j�B
jeB
j�B
j�B
j0B
jeB
kB
j�B
j�B
j�B
kB
kQB
k6B
kB
j�B
kB
kB
kQB
kkB
kkB
k�B
l"B
k�B
lB
k�B
k�B
lB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
ncB
n�B
n�B
oOB
pB
poB
poB
p�B
p�B
qAB
q�B
qvB
q�B
rB
rB
r-B
raB
r|B
r�B
sB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
tB
tTB
t�B
t�B
t�B
t�B
uB
u%B
u?B
uZB
u?B
u?B
uZB
u�B
u�B
u�B
u�B
vB
vB
v+B
vzB
v�B
w2B
w�B
w�B
xB
x8B
xlB
x�B
y$B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
z*B
zxB
z�B
z�B
{0B
{JB
{dB
{�B
|6B
|6B
|jB
|�B
|�B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~B
~B
~wB
~�B
~�B
~�B
~�B
HB
HB
}B
�B
�B
�4B
�OB
�iB
��B
��B
��B
��B
��B
��B
��B
�B
�;B
�oB
��B
�oB
��B
��B
��B
�AB
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006090044562020060900445620200609004456202006091527552020060915275520200609152755202207271538242022072715382420220727153824  JA  ARFMdecpA30a                                                                20200528093749  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200528093755  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200528093756  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200528093756  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200528093756  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200528093757  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200528093757                      G�O�G�O�G�O�                JA  ARUP                                                                        20200528095431                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200529000000  CF  PSAL_ADJUSTED_QC@
=@���G�O�                JM  ARCAJMQC2.0                                                                 20200608154456  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200608154456  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200609062755  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063824  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                