CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-14T12:38:17Z creation;2020-01-14T12:38:24Z conversion to V3.1;2023-06-29T05:50:15Z update;     
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
_FillValue                 �  ],   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200114123817  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_203                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @���eC 1   @��l��@7s�����b�1&�y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB뙚B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@g�@��
@��
AQ�A)�AI�Ai�A���A���A���A���A�(�A���A���A���Bz�B
z�Bz�B{B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bjz�Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
=B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B��B��
B�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2�C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|�RC~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D�D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Du'�Du��Dv'�Dv��Dw'�Dw��Dx'�Dx��Dy'�Dy��Dz'�Dz��D{'�D{��D|'�D|��D}'�D}��D~'�D~��D'�D��D��D�S�D���D���D��D�S�D���D��
D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�P�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D��
D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�P�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D�D���D��D�S�DÓ�D���D��D�S�Dē�D���D��D�S�Dœ�D���D��D�S�DƓ�D���D��D�S�DǓ�D���D��D�S�Dȓ�D���D��D�S�Dɓ�D���D��D�S�Dʓ�D���D��D�S�D˓�D���D��D�S�D̓�D���D��D�S�D͓�D���D��D�S�DΓ�D���D��D�S�Dϓ�D���D��D�S�DГ�D���D��D�S�Dѓ�D���D��D�S�Dғ�D���D��D�S�Dӓ�D���D��D�S�Dԓ�D���D��D�S�DՓ�D���D��D�S�D֓�D���D��D�S�Dד�D���D��D�S�Dؓ�D���D��D�S�Dٓ�D���D��D�S�Dړ�D���D��D�S�Dۓ�D���D��D�S�Dܓ�D���D��D�P�Dݓ�D���D��D�S�Dޓ�D���D��D�S�Dߓ�D���D��D�S�D���D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�P�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D��
D��
D�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AþwAøRAøRA���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��
A��A��A��#A��A��#A��/A��;A��;A��TA��`A��mA��mA��yA��yA��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A��A��A��A��mAð!A�?}A���A���A��A�(�A���A�Q�A�z�A�A�oA���A�7LA�K�A���A��HA�x�A�;dA�9XA��#A���A�K�A�dZA�O�A�-A�  A���A�~�A���A���A�$�A��^A��yA�JA��A�
=A���A��A�1A�9XA���A�K�A�l�A��\A�JA��9A���A�A�O�A�&�A�n�A�^5A�`BA���A�XA���A�;dA�A���A��TA��9A�VA~ĜAz�`Ax�uAw�;Awt�Au��AtJAq�#An�+Al{Ah��Ae�AdbNAcAaA`r�A_+A^A\�jAZ�yAY+AV~�AR��APr�AN{AK�AI��AH{AF9XAEhsAC�AA�A?��A>�A=�7A<�HA<v�A;�A:5?A9A7t�A5��A4ĜA3�A1�PA0v�A/�^A.��A-��A,�A*ȴA*�+A)�A'�TA'VA&�RA&n�A%�#A$�RA#�#A"��A!A!
=A I�AoAM�A�+AjA`BA{A��AVA�A
=A��A��A�mAE�AVAJAp�A�A�^A7LAG�A;dA
��A	|�A �A�A��A�A`BAdZA�FAp�A�yAz�A�A �A�A�A �u@��y@�?}@�(�@�
=@�?}@��m@���@�o@�9X@��@��@�X@�(�@�|�@���@�P@�v�@�@�^5@�Z@��y@���@�@���@�|�@�@���@�1@�K�@�E�@��#@�&�@ܣ�@�|�@�ff@��@�`B@�Ĝ@ו�@�7L@��
@�n�@Ь@��@��;@϶F@�ff@���@Η�@��#@ˍP@�"�@š�@î@+@�V@�@�V@��@���@�$�@�$�@�M�@��!@��+@�=q@�bN@�^5@��7@��@� �@��w@�|�@�\)@�\)@��P@��@�C�@��+@��@���@�l�@�J@��@��/@���@��@��
@�|�@���@�{@���@�7L@��@�&�@��@�  @��
@�t�@�dZ@��P@��w@���@�1@��@��@�A�@��D@�z�@���@��!@�M�@�o@��u@�X@���@�j@�\)@��#@��@��9@��u@�V@���@��@�o@��@�+@�S�@�|�@�1'@�1@��F@�ƨ@�|�@��P@���@���@�Z@���@�A�@�|�@��#@���@��@��;@�b@�A�@��@�"�@�+@�+@�dZ@���@��P@��P@�\)@�@���@�x�@�ȴ@�{@�hs@�/@�?}@�hs@�`B@�^5@�K�@�@�ff@��R@��-@���@�33@�M�@���@��7@�&�@��F@�C�@�;d@��@�Z@�(�@�\)@���@���@���@��@���@�o@��R@���@�n�@���@�9X@�  @�|�@�
=@�@���@�J@���@��T@�@�`B@�?}@�%@�V@���@���@�9X@�b@���@�C�@�
=@�n�@�E�@���@��@���@�`B@���@��j@��@���@�Z@�Q�@��@��F@���@���@�b@��j@��/@��j@�z�@�1'@���@�o@���@�ff@��@��@���@�G�@�%@��@��u@�r�@� �@�b@��@���@�\)@�K�@�@��!@��+@�~�@�ff@�-@�J@�@���@��h@��@�`B@�/@��@�V@���@��`@��@��D@�Z@�(�@��@�  @��
@��@��@�\)@�o@��y@��R@��\@�~�@�ff@�$�@���@��@���@��-@��@�O�@�7L@��@��@��j@��@�A�@�b@�w@;d@~�R@~5?@}�@}��@|��@|�@{�m@{��@{�@z�!@zn�@z�@y�^@y�7@y%@x�9@xQ�@x  @w�@wl�@wK�@vȴ@vV@v$�@u��@u`B@t�/@tz�@t9X@sƨ@s33@s@r��@r~�@r=q@q�@q��@qG�@pĜ@pr�@pA�@p �@o�w@o\)@o�@nȴ@n�R@n�+@nE�@m�T@mp�@l�/@lz�@l9X@k�
@kC�@j�H@j�\@jn�@j�@i�^@iX@i%@h�9@hr�@h �@g��@g��@gK�@f��@fȴ@fff@f{@e@ep�@e�@dz�@d�@c�m@c�F@cS�@b�!@b~�@bM�@aG�@`r�@`  @_�@_�@^�y@^E�@]��@]p�@\�@\��@\I�@[�
@[�@[dZ@["�@Z��@Z�\@Z=q@Z�@Y��@Y&�@XĜ@XA�@W��@W|�@V��@Vv�@V$�@U��@U�-@U`B@U?}@U�@UV@T�@T�@TI�@Sƨ@S�@SdZ@R��@Rn�@R^5@R-@Q�7@Q&�@P��@PbN@P  @O��@O+@O
=@Nȴ@N5?@N@M@M��@MO�@M�@M�@L�@L��@L��@L1@K��@KC�@J�!@J�\@J-@I�7@I7L@H�9@H�@HbN@G�;@G�w@G�P@G�@F�@F�+@Fff@F$�@F@E�T@E�-@E`B@EV@D��@Dz�@D9X@C�m@C�F@C��@Ct�@CC�@C@B��@B~�@B-@A�@A�^@A��@Ahs@A7L@@Ĝ@@bN@@ �@?�@?��@?�P@?+@>�y@>ȴ@>ff@=�@=��@=�@=`B@=/@=V@<�j@<z�@<9X@;�m@;ƨ@;dZ@;"�@:��@:�\@:=q@:�@9�#@9��@9��@9hs@9�@8�`@8�9@8�u@8 �@7�P@7K�@7+@7
=@6ȴ@6�+@6ff@65?@6$�@5@5��@5��@5�@5V@4��@4�D@4(�@3�
@3��@3dZ@3S�@333@3@2�!@2n�@1��@1��@1hs@0��@0�9@0bN@0b@/��@/��@/l�@/;d@.�y@.��@.ff@.V@.$�@-�T@-@-��@-O�@-V@,��@,z�@,I�@,(�@+��@+�m@+�F@+�@+S�@+"�@+o@*��@*�!@*~�@*M�@*=q@*J@)�#@)��@)x�@)7L@(��@(�u@(A�@( �@(b@(  @'�@'��@'�P@'l�@'K�@';d@'
=@&��@&�@&�R@&�+@&V@&{@%�@%��@%�h@%p�@%p�@%O�@%�@$��@$j@$9X@$�@#�m@#�F@#�@#S�@#33@#33@#o@"�@"��@"~�@"n�@"M�@"J@!�@!�^@!�^@!��@!hs@!G�@!%@ �`@ Ĝ@ �9@ �@ 1'@�;@�w@�P@\)@
=@�@��@ff@@�T@�h@p�@/@�/@�j@j@I�@9X@(�@�m@��@t�@S�@C�@@��@��@n�@=q@J@�#@�^@�7@hs@�@�9@�u@�u@�@r�@bN@ �@b@�@��@�w@�P@K�@��@��@�+@v�@E�@$�@�@�-@�@`B@/@��@�D@Z@9X@�@1@��@ƨ@�F@��@dZ@C�@o@�@�H@��@�\@M�@�@�@�^@x�@7L@&�@�@��@��@r�@ �@��@�@�P@K�@�@
=@
=@ȴ@��@�+@V@$�@@@��@p�@O�@?}@/@�@��@��@��@�D@z�@9X@(�@��@�
@��@�@C�@"�@o@@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AþwAøRAøRA���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��
A��A��A��#A��A��#A��/A��;A��;A��TA��`A��mA��mA��yA��yA��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A��A��A��A��mAð!A�?}A���A���A��A�(�A���A�Q�A�z�A�A�oA���A�7LA�K�A���A��HA�x�A�;dA�9XA��#A���A�K�A�dZA�O�A�-A�  A���A�~�A���A���A�$�A��^A��yA�JA��A�
=A���A��A�1A�9XA���A�K�A�l�A��\A�JA��9A���A�A�O�A�&�A�n�A�^5A�`BA���A�XA���A�;dA�A���A��TA��9A�VA~ĜAz�`Ax�uAw�;Awt�Au��AtJAq�#An�+Al{Ah��Ae�AdbNAcAaA`r�A_+A^A\�jAZ�yAY+AV~�AR��APr�AN{AK�AI��AH{AF9XAEhsAC�AA�A?��A>�A=�7A<�HA<v�A;�A:5?A9A7t�A5��A4ĜA3�A1�PA0v�A/�^A.��A-��A,�A*ȴA*�+A)�A'�TA'VA&�RA&n�A%�#A$�RA#�#A"��A!A!
=A I�AoAM�A�+AjA`BA{A��AVA�A
=A��A��A�mAE�AVAJAp�A�A�^A7LAG�A;dA
��A	|�A �A�A��A�A`BAdZA�FAp�A�yAz�A�A �A�A�A �u@��y@�?}@�(�@�
=@�?}@��m@���@�o@�9X@��@��@�X@�(�@�|�@���@�P@�v�@�@�^5@�Z@��y@���@�@���@�|�@�@���@�1@�K�@�E�@��#@�&�@ܣ�@�|�@�ff@��@�`B@�Ĝ@ו�@�7L@��
@�n�@Ь@��@��;@϶F@�ff@���@Η�@��#@ˍP@�"�@š�@î@+@�V@�@�V@��@���@�$�@�$�@�M�@��!@��+@�=q@�bN@�^5@��7@��@� �@��w@�|�@�\)@�\)@��P@��@�C�@��+@��@���@�l�@�J@��@��/@���@��@��
@�|�@���@�{@���@�7L@��@�&�@��@�  @��
@�t�@�dZ@��P@��w@���@�1@��@��@�A�@��D@�z�@���@��!@�M�@�o@��u@�X@���@�j@�\)@��#@��@��9@��u@�V@���@��@�o@��@�+@�S�@�|�@�1'@�1@��F@�ƨ@�|�@��P@���@���@�Z@���@�A�@�|�@��#@���@��@��;@�b@�A�@��@�"�@�+@�+@�dZ@���@��P@��P@�\)@�@���@�x�@�ȴ@�{@�hs@�/@�?}@�hs@�`B@�^5@�K�@�@�ff@��R@��-@���@�33@�M�@���@��7@�&�@��F@�C�@�;d@��@�Z@�(�@�\)@���@���@���@��@���@�o@��R@���@�n�@���@�9X@�  @�|�@�
=@�@���@�J@���@��T@�@�`B@�?}@�%@�V@���@���@�9X@�b@���@�C�@�
=@�n�@�E�@���@��@���@�`B@���@��j@��@���@�Z@�Q�@��@��F@���@���@�b@��j@��/@��j@�z�@�1'@���@�o@���@�ff@��@��@���@�G�@�%@��@��u@�r�@� �@�b@��@���@�\)@�K�@�@��!@��+@�~�@�ff@�-@�J@�@���@��h@��@�`B@�/@��@�V@���@��`@��@��D@�Z@�(�@��@�  @��
@��@��@�\)@�o@��y@��R@��\@�~�@�ff@�$�@���@��@���@��-@��@�O�@�7L@��@��@��j@��@�A�@�b@�w@;d@~�R@~5?@}�@}��@|��@|�@{�m@{��@{�@z�!@zn�@z�@y�^@y�7@y%@x�9@xQ�@x  @w�@wl�@wK�@vȴ@vV@v$�@u��@u`B@t�/@tz�@t9X@sƨ@s33@s@r��@r~�@r=q@q�@q��@qG�@pĜ@pr�@pA�@p �@o�w@o\)@o�@nȴ@n�R@n�+@nE�@m�T@mp�@l�/@lz�@l9X@k�
@kC�@j�H@j�\@jn�@j�@i�^@iX@i%@h�9@hr�@h �@g��@g��@gK�@f��@fȴ@fff@f{@e@ep�@e�@dz�@d�@c�m@c�F@cS�@b�!@b~�@bM�@aG�@`r�@`  @_�@_�@^�y@^E�@]��@]p�@\�@\��@\I�@[�
@[�@[dZ@["�@Z��@Z�\@Z=q@Z�@Y��@Y&�@XĜ@XA�@W��@W|�@V��@Vv�@V$�@U��@U�-@U`B@U?}@U�@UV@T�@T�@TI�@Sƨ@S�@SdZ@R��@Rn�@R^5@R-@Q�7@Q&�@P��@PbN@P  @O��@O+@O
=@Nȴ@N5?@N@M@M��@MO�@M�@M�@L�@L��@L��@L1@K��@KC�@J�!@J�\@J-@I�7@I7L@H�9@H�@HbN@G�;@G�w@G�P@G�@F�@F�+@Fff@F$�@F@E�T@E�-@E`B@EV@D��@Dz�@D9X@C�m@C�F@C��@Ct�@CC�@C@B��@B~�@B-@A�@A�^@A��@Ahs@A7L@@Ĝ@@bN@@ �@?�@?��@?�P@?+@>�y@>ȴ@>ff@=�@=��@=�@=`B@=/@=V@<�j@<z�@<9X@;�m@;ƨ@;dZ@;"�@:��@:�\@:=q@:�@9�#@9��@9��@9hs@9�@8�`@8�9@8�u@8 �@7�P@7K�@7+@7
=@6ȴ@6�+@6ff@65?@6$�@5@5��@5��@5�@5V@4��@4�D@4(�@3�
@3��@3dZ@3S�@333@3@2�!@2n�@1��@1��@1hs@0��@0�9@0bN@0b@/��@/��@/l�@/;d@.�y@.��@.ff@.V@.$�@-�T@-@-��@-O�@-V@,��@,z�@,I�@,(�@+��@+�m@+�F@+�@+S�@+"�@+o@*��@*�!@*~�@*M�@*=q@*J@)�#@)��@)x�@)7L@(��@(�u@(A�@( �@(b@(  @'�@'��@'�P@'l�@'K�@';d@'
=@&��@&�@&�R@&�+@&V@&{@%�@%��@%�h@%p�@%p�@%O�@%�@$��@$j@$9X@$�@#�m@#�F@#�@#S�@#33@#33@#o@"�@"��@"~�@"n�@"M�@"J@!�@!�^@!�^@!��@!hs@!G�@!%@ �`@ Ĝ@ �9@ �@ 1'@�;@�w@�P@\)@
=@�@��@ff@@�T@�h@p�@/@�/@�j@j@I�@9X@(�@�m@��@t�@S�@C�@@��@��@n�@=q@J@�#@�^@�7@hs@�@�9@�u@�u@�@r�@bN@ �@b@�@��@�w@�P@K�@��@��@�+@v�@E�@$�@�@�-@�@`B@/@��@�D@Z@9X@�@1@��@ƨ@�F@��@dZ@C�@o@�@�H@��@�\@M�@�@�@�^@x�@7L@&�@�@��@��@r�@ �@��@�@�P@K�@�@
=@
=@ȴ@��@�+@V@$�@@@��@p�@O�@?}@/@�@��@��@��@�D@z�@9X@(�@��@�
@��@�@C�@"�@o@@
�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�)B
�)B
�)B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�)B
�#B
�#B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�/B
�)B
�/B
�5B
�BB
�HB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�NB
�NB
�NB
�BB
�NB
�ZB
�`B
�ZB
�`B
�sB
�sB
�sB
�yB
�yB:^BiyBt�B�B�B��BɺB�B�#B�BBB\B�B�B�B$�B'�B#�B�B�B�B\BDBVB\BbB	7B{B\B	7BoB\BB��B�B�BBB�'B��B��B�bB�Bn�BbNBI�B49B#�B�B�BPBB
�5B
��B
�=B
�%B
�uB
�B
hsB
W
B
J�B
;dB
+B
B	�B	�B	�B	�sB	�;B	��B	�^B	��B	�bB	z�B	q�B	iyB	_;B	T�B	J�B	B�B	:^B	/B	!�B	bB��B�B�NB�B��BŢB�jB�XB�3B�B��B��B��B��B��B��B��B��B��B�uB�oB�bB�JB�7B�=B�=B�+B�+B�B�%B�7B�B�B�B� B~�B� B}�By�Bw�Bt�Bu�Br�Bp�Bo�BgmBaHB]/B[#B]/BYBP�BJ�BJ�BI�BI�BJ�BI�BH�BJ�BJ�BL�BR�BT�BS�BL�BE�B<jB=qB:^B9XB<jBG�BG�BD�BC�BA�B=qB9XB8RB9XB6FB1'B2-B49B49B2-B0!B2-B=qBE�BE�BF�BG�BG�BI�BG�BI�BH�BG�BB�B@�B?}B@�BB�BD�BI�BJ�BI�BJ�BJ�BJ�BK�BN�BQ�BVBZBYBYBXBVBXBXBZB[#B\)B]/BaHB`BBl�Bu�Bt�Bo�Bx�Bu�Bv�By�B~�B� B~�B� Bz�B|�B�%B�JB�VB�bB�hB�hB��B��B��B��B��B��B�B�B�XB�^B�wB�wB�wBBŢBÖBÖBŢBǮBȴBɺB��BȴBȴB��B��B��B��B�
B�/B�/B�;B�fB�yB�B�B��B��B�B�B�B�B�B�B��B	+B	VB	uB	hB	JB		7B	%B	
=B	DB	VB	bB	oB	bB	bB	{B	�B	�B	�B	"�B	!�B	"�B	%�B	-B	49B	O�B	T�B	YB	XB	YB	T�B	O�B	N�B	S�B	W
B	]/B	aHB	`BB	bNB	cTB	iyB	p�B	t�B	x�B	{�B	}�B	}�B	{�B	r�B	r�B	s�B	s�B	u�B	w�B	z�B	�B	�=B	�7B	�1B	�JB	�=B	�%B	�B	�B	�B	�B	�%B	�B	~�B	~�B	�%B	�7B	�PB	�JB	�JB	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�9B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�^B	�jB	�qB	��B	ƨB	ȴB	ɺB	ɺB	ȴB	ǮB	ƨB	ƨB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
JB
PB
PB
VB
VB
VB
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
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
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
@�B
@�B
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
B�B
B�B
B�B
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
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
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
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
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
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
YB
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
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
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
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
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
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
gmB
gmB
gmB
gmB
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
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
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
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�B
�&B
�,B
�@B
�FB
�XB
�B
�B
��B
�?BA BjBvB�{B�IB�B˒B�B�B�TB�B�BCB;B �B'�B+�B&B#B�BKBBxB�BHB�B
�BB�B
rB,BhB�B��B�B�TB�MB��B��B��B�:B��Bp;Bd�BK�B5�B$�BWBBbB�B
� B
�
B
��B
�_B
��B
�aB
kB
Y�B
L�B
?HB
.}B
MB	�[B	�qB	�hB	�B	�B	уB	�qB	��B	�uB	|B	s3B	j�B	`�B	VSB	LB	DMB	<�B	1vB	%FB	aB	B�hB�B��B��BǔB��B�B�?B�;B�>B��B�\B�;B��B�IB�	B�QB�+B��B�FB�B��B�#B�^B��B��B�fB��B�_B��B��B�gB�[B��B�4B��BB{Bx�Bu�BwBs�Br�Bq�Bh�Bb�B]�B[�B^jBZ7BR BLBK�BK�BKBK�BJrBI�BKxBKBL�BSBU�BUMBNVBF�B<�B>B:�B9	B<6BG�BH1BESBEBBuB>B9�B9	B:xB7B1�B2�B5%B4�B2aB/�B1vB=VBF�BFtBGEBHKBIBJXBH�BJ�BI�BH�BCGBAB@ B@�BB�BEmBJXBJ�BJ	BK)BJ�BKBLBO\BRTBVSBZQBYBY�BYKBV�BX�BX�BZ7B[#B\CB]�BaHB_�BmCBwLBv�Bp�By�Bv+Bv�By�BB�iB�B��Bz�B|�B��B�0B��B�hB�TB��B��B��B��B��B��B��B��B� B�XB��B�HB��B��B�-B��BÖBÖBżBǮB��B�	B��BȴB��BʌBˬB�B��B��B�/B��B��B��B�*B�WB�B�FB��B�IB�B�B�qB�iB��B��B	tB	<B	B	�B	B		�B	�B	
	B	
�B	VB	}B	�B	.B	�B	B	
B	B	�B	"�B	!|B	"�B	%FB	+�B	2|B	OBB	T�B	Y1B	XyB	Y�B	UMB	PB	N�B	S�B	V�B	]IB	aHB	_�B	a�B	b�B	i*B	pUB	t�B	x�B	|B	~(B	~�B	}"B	r�B	r�B	s�B	shB	uZB	wfB	zB	�OB	�#B	�7B	�B	��B	��B	��B	�AB	� B	�B	�;B	��B	�B	~�B	~]B	��B	�7B	�jB	�JB	�B	��B	��B	�B	��B	��B	��B	��B	�IB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�/B	�B	�B	��B	��B	�'B	�B	�-B	�9B	�B	�B	�B	�+B	�B	�8B	�RB	�*B	�B	��B	��B	�YB	ȀB	ɠB	ɺB	��B	��B	ƨB	ƨB	ňB	ƎB	ƎB	ǔB	ȀB	ʦB	˒B	�~B	̳B	ΥB	ΥB	��B	��B	ңB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�'B	�B	�B	�B	� B	�:B	�&B	�FB	�,B	�2B	�8B	�8B	�>B	�>B	�XB	�DB	�KB	�KB	�QB	�kB	�WB	�WB	�]B	�wB	�}B	�cB	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
	B

	B

	B
B
B
B
B
B
6B
6B
"B
<B
<B
BB
.B
.B
HB
4B
NB
4B
 B
:B
:B
@B
@B
@B
@B
FB
aB
MB
MB
gB
mB
mB
YB
?B
?B
YB
sB
_B
yB
�B
�B
�B
�B
�B
WB
qB
�B
�B
�B
~B
~B
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
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
.�B
/ B
/ B
/�B
0B
/�B
/�B
0�B
1�B
0�B
2B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
4B
4B
3�B
4B
4�B
5B
5B
5�B
6B
6B
6�B
7B
7B
8B
8B
9	B
9	B
9$B
9$B
:DB
:DB
;0B
;0B
;0B
;0B
;JB
<6B
<6B
<PB
<6B
=<B
=<B
=<B
=<B
=<B
=<B
>BB
>BB
>BB
>BB
?.B
?HB
?HB
@OB
@OB
@OB
@OB
AUB
A;B
AUB
AUB
AUB
AUB
AUB
AoB
BuB
B[B
B[B
B[B
B[B
B[B
B[B
B[B
B[B
B[B
CGB
C-B
CaB
C{B
CaB
DMB
DgB
DMB
DgB
EmB
ESB
FtB
FtB
GzB
GzB
H�B
H�B
G_B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
JrB
JrB
J�B
JrB
KxB
K�B
K�B
KxB
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
OvB
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
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
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
_B
_B
_B
_B
^�B
_B
_B
_�B
_�B
_�B
`B
`B
_�B
`B
`B
`B
`B
`�B
aB
aB
a�B
a�B
bB
bB
a�B
a�B
c B
c B
cB
c B
d&B
d&B
dB
d&B
dB
d&B
d&B
eB
e,B
e,B
e,B
e,B
e,B
eB
e,B
e,B
e,B
f2B
f2B
f2B
f2B
f2B
gB
g8B
g8B
gRB
h>B
hXB
h>B
hXB
h>B
iDB
iDB
i*B
iB
i_B
jKB
jKB
jKB
jKB
jKB
jKB
jKB
kQB
kQB
k6B
kQB
k6B
kQB
kQB
l=B
lWB
lWB
l=B
lWB
lWB
lWB
lqB
m]B
m]B
mCB
mCB
mCB
m]B
nI1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.62(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001200037232020012000372320200120003723202306231720092023062317200920230623172009202001210028052020012100280520200121002805  JA  ARFMdecpA19c                                                                20200114213807  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200114123817  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200114123820  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200114123820  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200114123821  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200114123821  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200114123821  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200114123821  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200114123823  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200114123824                      G�O�G�O�G�O�                JA  ARUP                                                                        20200114125437                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200114153329  CV  JULD            G�O�G�O�F�ؤ                JM  ARCAJMQC2.0                                                                 20200119153723  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200119153723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200120152805  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082009  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                