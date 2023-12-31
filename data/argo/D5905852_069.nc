CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-25T15:40:12Z creation;2020-12-25T15:40:13Z conversion to V3.1;2022-08-17T01:55:25Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201225154012  20220818091506  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  A30_8420_069                    2C  D   APEX                            8420                            2.11.2                          846 @�Q��� 1   @�Q�d�~�@.�`A�7L�cOv_خ1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BBffBG��BP  BW33B^ffBj  BnffBx  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  CL�C�fC�3C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dy��Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D���D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @.�R@mp�@��R@�  A(�A;�A[�
A{�A�A�A�  A�=qA�{A��
A��A�  B(�B��B�
B�
B'
=B/�B7�BAp�BF�\BO  BV(�B]33BhBmp�Bv��B~��B�p�B��B�ffB�aHB�z�B�.B��=B��B�L�B�p�B�p�B��{B��\B�p�B�p�B�u�B�u�B�z�B�z�B�z�B�k�B�ffB�k�B߀ B�\B癚B�\BB�=B�� B�p�B�k�C��C�3C��C�qC	�RC��C� C�C�CCCǮC�C��Cp�C��C!��C#��C%�RC'��C)�qC+��C-�qC/�RC1��C3�qC5� C7�RC9��C;�C=� C?� CA�qCC� CE�qCG��CI�RCK� CMCO�qCQ�CS��CU� CW��CY��C[��C]C_��Ca�RCc��Ce��Cg�RCi� Ck� Cm��Co�RCq�qCsCu� Cw� Cy�qC{�qC}� CC��qC���C��)C��)C���C�ٚC���C��qC�� C��)C�ٚC�޸C��HC�� C���C��)C��HC�� C�޸C��qC���C��)C�� C��C�޸C��)C��HC�޸C��)C��HC�� C�޸C��)C��RC���C���C�ٚC�޸C��C��)C�޸C�� C��qC��HC��C��C�޸C�� C��C��HC��HC�� C�� C��)C��)C�� C��C�� C�� C��HC�޸C��)C��)C�� C�� C�޸C�޸C��HC�޸C��)C�޸C��HC��)C��)C��)C�޸C�޸C��)C���C��RC��RC��)C��)C��)C�� C�� C��qC�޸C��qC��)C��)C�޸C��HC�޸C��qC�޸C�ٚC��RC���C��)C�޸C�� C��C�� C�޸C��HC���C��C��qC��)C�޸C��HC�޸C��qC��qC��qC�޸C�޸C�޸C�� C�޸C��qC��qC�޸C���C���C�޸C�޸D nD �\Dn�D�\Dn�D��DnD�Do\D��Dp�D�\Dp D�Dn�D�Dn�D�\D	n�D	�qD
nD
�qDnD�\Do\D�\Do\D��DnD�DmqD��Dn�D�Dp�D��Dp�D�\DqHD�Do\D�Dq�D�Do\D�HDr�D�Do\D�HDp�D�DqHD��Dp D�\DnD�\Dp�D� Do\D�Do\D�\D p�D �3D!s3D!�D"o\D"�D#mqD#�D$o\D$�D%nD%�D&o\D&�qD'nD'� D(o\D(�D)n�D)�D*o\D*�\D+p D+�D,o\D,� D-n�D-�\D.n�D.�D/p D/� D0n�D0�D1nD1�D2p�D2�\D3l�D3�qD4n�D4�D5n�D5�\D6mqD6�D7p�D7�\D8o\D8� D9n�D9�D:mqD:�D;n�D;� D<qHD<��D=p D=�\D>mqD>�D?mqD?�\D@q�D@�DAmqDA�qDBp�DB�HDCo\DC��DDnDD�\DEn�DE�DFp DF�DGp DG� DHo\DH�DIr�DI�\DJmqDJ��DKo\DK�HDLp�DL�DMn�DM�DNs3DN�DOo\DO�\DPp�DP�DQnDQ�DRo\DR�DSn�DS�\DTp DT� DUo\DU�\DVn�DV�DWo\DW�DXo\DX�\DYo\DY�\DZnDZ�D[p�D[�\D\nD\�D]p D]�\D^n�D^�qD_o\D_� D`p D`�Dal�Da�Dbn�Db�DcmqDc�DdqHDd�\Den�De�Dfp�Df�\Dgl�Dg�Dhp�Dh�\Dio\Di�Djo\Dj�\Dkp Dk�Dlo\Dl��Dmq�Dm�\DnmqDn�Dop�Do�\Dpl)Dp�qDqnDq�)DrnDr� Dso\Ds�Dtn�Dt�\Dup Du�HDvqHDv�Dwl)Dw�Dxn�Dx�Dyn�Dy�DzmqDz�\D{qHD{�D|p D|�D}o\D}�D~n�D~��Do\D�D�6D�v�D��\D��RD�8�D�x�D���D�� D�7�D�w
D���D��\D�8RD�x�D��
D���D�6fD�vfD���D��fD�6D�w�D�� D��D�7�D�x�D���D��fD�7\D�x D��fD���D�7
D�w\D�� D��RD�8�D�w�D���D��\D�7\D�x�D���D��RD�8RD�xRD��RD���D�6�D�vfD��
D�� D�8 D�w
D��\D��RD�8 D�w\D��
D���D�8RD�x�D���D���D�7�D�x D��\D��\D�7�D�w�D�� D��\D�7�D�x�D��RD��
D�7�D�w\D��
D�� D�8�D�w
D��
D��\D�7�D�w�D�� D�� D�8RD�w�D���D�� D�8RD�xRD���D���D�7�D�x D���D���D�7\D�w
D��\D�� D�8 D�w�D�� D��\D�7\D�w�D��\D��
D�6�D�v�D��
D��RD�7�D�v�D���D���D�7�D�x D��
D���D�7\D�v�D���D�� D�8RD�x D�� D�� D�8RD�x�D���D���D�7�D�v�D��
D��\D�8 D�xRD���D��
D�7\D�w\D��\D�� D�7\D�v�D���D��fD�7\D�xRD��RD��
D�7�D�w�D�� D�� D�7
D�w�D���D���D�7
D�w\D��RD�� D�6�D�w
D��
D��fD�6fD�v�D��
D��\D�7�D�x D���D���D�7\D�w\D�� D��\D�6�D�w�D��\D���D�8 D�w�D���D��\D�7
D�w
D�� D���D�7
D�w
D�� D���D�6fD�w\D��\D���D�7
D�w�D��\D��\D�8 D�x D��RD�� D�7
D�w
D�� D�� D�7
D�w�D���D���D�8RD�x D�� D�� D�8 D�w
D���D���D�7\D�w
D��\D���D�7�D�w�D��\D��
D�7\D�w�D��\D��\D�8 D�x D���D��RD�8RD�x�D��\D��fD�7�D�w�D�� D��RD�8�D�yHD�� D�� D�7\D�w�D���D��\D�8 D�w\D��\D���D�7�D�xRD¸ D��\D�7�D�w�D÷�D��RD�8RD�w�Dķ\D�� D�8�D�xRDŷ�D���D�8RD�x DƸRD�� D�7\D�w
DǷ\D���D�7�D�w\Dȷ
D���D�8 D�w\Dɷ
D���D�6�D�w\DʸRD���D�8�D�x�D˷�D���D�6fD�vfD̸RD���D�7
D�w\D͹HD��RD�8 D�x DθRD��RD�7\D�v�D϶fD��\D�8�D�x�Dи�D�� D�8 D�v�DѶ�D��RD�8RD�w
DҸ D��RD�6�D�w
Dӷ\D��\D�7�D�v�DԷ
D��
D�6�D�v�Dշ
D��\D�7�D�x Dֶ�D�� D�8�D�w\D׶�D���D�8RD�xRDظ D��RD�8RD�x Dٷ\D��
D�6fD�v�Dڸ D�� D�8 D�w�D۶fD��D�7\D�xRDܸ D���D�7\D�w�Dݸ D�� D�7�D�w�D޸ D�� D�8RD�x�D߷�D��\D�8�D�x�D�RD��\D�6�D�x DᷮD��\D�7�D�xRDⷮD���D�8 D�w\D㷮D���D�6�D�xRD�RD���D�7�D�x D差D��\D�7�D�x D�
D��\D�7�D�v�D�
D��
D�6�D�w
D跮D���D�8 D�x�D鷮D���D�7
D�x D�RD���D�8RD�x D뷮D�� D�6fD�vD�\D��\D�7�D�xRD���D�� D�8 D�x�D�RD��\D�7
D�v�D�
D���D�7�D�x D�\D��fD�7
D�x D� D���D�7\D�w�D�\D���D�8 D�w\D�\D��\D�7�D�w
D��D��\D�7\D�w�D��RD��
D�6D�uqD��\D���D�6�D�w�D��\D���D�7\D�w�D���D���D�7�D�w�D��RD��\D�6fD�w
D���D���D�7\D�v�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ߤA��5A��pA���A��BA��A���A��A��A���A��A��A��A���A��A��,A��8A���A��A��&A��]A���A��A��)A���A̫6A���A�(�A�bA�!A�A�A��?A���A�B�A�rGA��A��2A�V�A�\�A��zA��hA��[A��nA�P}A��iA��)A�n�A�ٴA�F�A���A���A�>wA�7�A��*A���A��7A�o A�N�A���A�^5A�*eA���A���A��zA�2-A�uZA��HA��A�!bA��A�a�A�2-A���A�a�A���At�At�Aq#:Aa�A_�'AZX�AW��AR��AN�YAM#�AI��AG�AE�LAB-wA>��A;�&A;bNA;T�A;4A;�A:�A:�}A::*A9�A8�A7�$A9��A;A A<S�A<.IA;W�A;,�A:l"A9�A8��A8l"A7�$A75?A6��A6��A6'RA5�jA5�bA4�	A4\�A4 iA3�bA3�4A32�A3 iA2.�A1��A1Z�A0K�A/�kA/&A/VA.�UA-tTA,��A,�YA+�A+
=A*{�A)�`A)A�A(Z�A(0�A()�A(�A((A'�A&�MA&�A%��A$�	A$!�A#��A#7LA"��A!D�A �Am]A�Au�A��Am]A�A��A�A�A
=A�A=qAxAC�A�A��A�XA�AE�A�A_A�A�oA'RA��AjA-wA4A�Ah�A��A-�A��A��Ah
A=A�nA�A��AS�A�fA�A?}A�]A� A_pA��A��Aa�AOA$�AA�A�A�AjAm]A	A�A;dA��AM�AیA\)AG�A�A
�zA
hsA	� A	��A	S&A	E�A		�A��A�jAT�A�ZA�=A[�AȴA+�A��AA�A�A͟AM�A(�A�2A�4A�A �\@��@�!-@�b@��z@��`@�-@�e,@�@���@���@��r@�%�@��@��@�8�@�@�O@��@��f@�w@�9@�_@��'@��K@��@��@�o�@�@@�!@봢@�J�@꒣@�K^@�� @�H�@藍@�?@�zx@�{@��@�
=@�	l@�(@�	l@���@�tT@���@��@��x@�+k@�M@��+@��@ߙ�@ޤ�@�M�@݅@��]@��B@��@�ϫ@�)_@֖�@���@���@��9@հ�@ՠ'@�e�@��@�͟@�p;@ӨX@�6z@Ҍ@�%�@ѳ�@ДF@��@�l�@��@���@�V@ˌ~@���@�ԕ@�/@�kQ@��}@ǲ�@ǃ{@�J�@�;@��@�g8@�
�@��N@���@��W@å�@�f�@��@�@�-@���@�S�@��@���@���@�[�@�Z�@�S@��I@�Q@�$@�	@��@��@���@�m]@�@@�~�@�5?@��@��4@���@�M@�-w@��T@��@���@���@�kQ@�.�@�dZ@��f@�͟@���@�@�@��N@�@���@�l"@�R�@��D@�j�@���@���@�W�@���@�[W@��@���@�%�@��@@��:@��h@��7@�w2@�IR@���@�6�@� �@���@�S�@��@�ߤ@��@�<�@���@�[W@��@���@�M�@��3@�J#@��@���@�?@���@���@�x�@�+�@��m@�r�@�x@�ƨ@��[@���@��{@�7L@��@��|@�y>@�:�@��H@��@�u�@��@��#@�x�@�S�@�%@���@��F@�i�@�[�@�M�@�.�@�~@��.@���@���@�e,@�(�@���@�^5@���@�y�@�@@��@���@�W�@�D�@�;�@���@��@���@�ȴ@��1@�Xy@�1�@��@���@���@�o @���@��@�ȴ@�l"@��@��@�%�@�%�@��@���@���@�k�@�@��@���@��y@���@���@�]d@��@��	@�U�@��@��@���@��@���@��S@�a�@�G�@� \@��	@�ں@��b@�K^@��)@��g@���@���@���@��	@�"�@��}@�c @��@��z@�S�@���@��@���@�B[@�>B@��Z@��k@�O�@���@���@��+@�E�@��@���@�Q�@�Y@��@��@�y>@�*�@��@��;@���@�b�@���@�kQ@��@��@�rG@�8@�;@���@�\�@��F@�^�@�'�@���@���@�L0@�(�@���@���@���@�s�@�5�@��@��_@�Ft@��@6z@~��@~!�@}j@|�e@{�@{�@z� @z4@y@xbN@w��@vu%@vB[@v�@u�9@u�C@uo @t��@t`�@t�@s�6@s��@s��@s��@sv`@s|�@s�4@s8@sY@r��@r0U@q�@q��@p�o@p@o=@n��@nYK@m��@m	l@lM@l  @k.I@j
�@i��@ic�@i4@i!�@i�@h�)@hQ�@h'R@gخ@g��@gZ�@g)_@f�c@f��@f�r@f8�@e�@e�z@em]@e	l@d�E@d��@d��@dr�@dM@d~@c��@c�V@cj�@b�@b�X@b�F@a��@`��@`��@`m�@`U2@_ƨ@_,�@^�]@^��@^��@^$�@]�C@]Q�@]�@\ �@[C�@Z��@Z�s@Z��@Z{@Y�@Y�@X�Y@X4n@W��@W33@Vں@V�6@V��@Vq�@V0U@Vu@U�@U��@U0�@T��@T��@Tz�@TI�@Sj�@Rh
@R-@R
�@Q�Z@Q�@Qk�@P��@Ph�@O��@Ox@O4�@O�@N͟@N��@N\�@NOv@N@�@N�@M�@M@M�@M<6@L��@L�Y@K��@K)_@K�@J�M@J�@J�L@J^5@J&�@I�@HĜ@HU2@H?�@H<�@H:�@H%�@G�&@G��@Go@F��@F�R@F��@Fu%@E��@E��@D��@Db@C�[@Cqv@B�H@B��@BJ�@A�@A�@A�t@A�M@ADg@A%@@��@@4n@?��@?iD@>��@>kQ@>Ta@>L0@>C�@>u@<�@<PH@<-�@<%�@<1@;�g@;��@;�F@;��@;o�@;�@;�@:�s@:�@:�!@:��@:p;@:H�@:($@9��@9a�@9G�@9�@8��@8�@8֡@8��@8�9@8�4@8z�@8D�@8�@81@7�]@7�g@7�@7�@5�)@5�C@5�"@5u�@5u�@5c�@5�@4��@4��@4I�@3�g@3E9@3o@2�M@2��@2��@2}V@2xl@2ff@2B[@1��@1�^@1x�@1G�@1�@0�5@0��@0H@01'@0,=@0(�@0!@/�@/@.�M@.��@.R�@.u@-�#@-��@-s�@-(�@-V@-�@,��@,!@+�&@+�k@+8@*�'@*
�@)ԕ@)@)�@)IR@(��@(��@(��@(C-@(�@'�]@'�W@'��@'��@'Mj@''�@'�@&�8@&�]@&W�@%��@%c@%k�@%+�@%@%V@%	l@$�@$��@$Ft@#�@#�}@#��@#�F@#Z�@#@"�s@"��@"-@!�@!�'@!m]@!2a@!!�@!+@!@ �K@ �$@ �u@ j@ -�@ �@�@�f@�@��@s�@kQ@a|@�@�-@�@��@`B@�@�@��@��@y>@]d@1@�0@��@g�@A�@�@�8@ں@�1@�@�X@��@Q�@�@��@e�@M@�q@j�@@��@=q@��@�@f�@X@4@�/@�Y@V�@�@�@�@��@��@|�@Z�@��@��@�!@��@J�@�@��@�@��@�7@rG@S&@@%@�	@��@�@7�@1@�@��@dZ@,�@�h@M�@ �@��@ϫ@��@��@c�@/@*0@�@��@��@6@��@��@��@�P@\)@@O@3311111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ߤA��5A��pA���A��BA��A���A��A��A���A��A��A��A���A��A��,A��8A���A��A��&A��]A���A��A��)A���A̫6A���A�(�A�bA�!A�A�A��?A���A�B�A�rGA��A��2A�V�A�\�A��zA��hA��[A��nA�P}A��iA��)A�n�A�ٴA�F�A���A���A�>wA�7�A��*A���A��7A�o A�N�A���A�^5A�*eA���A���A��zA�2-A�uZA��HA��A�!bA��A�a�A�2-A���A�a�A���At�At�Aq#:Aa�A_�'AZX�AW��AR��AN�YAM#�AI��AG�AE�LAB-wA>��A;�&A;bNA;T�A;4A;�A:�A:�}A::*A9�A8�A7�$A9��A;A A<S�A<.IA;W�A;,�A:l"A9�A8��A8l"A7�$A75?A6��A6��A6'RA5�jA5�bA4�	A4\�A4 iA3�bA3�4A32�A3 iA2.�A1��A1Z�A0K�A/�kA/&A/VA.�UA-tTA,��A,�YA+�A+
=A*{�A)�`A)A�A(Z�A(0�A()�A(�A((A'�A&�MA&�A%��A$�	A$!�A#��A#7LA"��A!D�A �Am]A�Au�A��Am]A�A��A�A�A
=A�A=qAxAC�A�A��A�XA�AE�A�A_A�A�oA'RA��AjA-wA4A�Ah�A��A-�A��A��Ah
A=A�nA�A��AS�A�fA�A?}A�]A� A_pA��A��Aa�AOA$�AA�A�A�AjAm]A	A�A;dA��AM�AیA\)AG�A�A
�zA
hsA	� A	��A	S&A	E�A		�A��A�jAT�A�ZA�=A[�AȴA+�A��AA�A�A͟AM�A(�A�2A�4A�A �\@��@�!-@�b@��z@��`@�-@�e,@�@���@���@��r@�%�@��@��@�8�@�@�O@��@��f@�w@�9@�_@��'@��K@��@��@�o�@�@@�!@봢@�J�@꒣@�K^@�� @�H�@藍@�?@�zx@�{@��@�
=@�	l@�(@�	l@���@�tT@���@��@��x@�+k@�M@��+@��@ߙ�@ޤ�@�M�@݅@��]@��B@��@�ϫ@�)_@֖�@���@���@��9@հ�@ՠ'@�e�@��@�͟@�p;@ӨX@�6z@Ҍ@�%�@ѳ�@ДF@��@�l�@��@���@�V@ˌ~@���@�ԕ@�/@�kQ@��}@ǲ�@ǃ{@�J�@�;@��@�g8@�
�@��N@���@��W@å�@�f�@��@�@�-@���@�S�@��@���@���@�[�@�Z�@�S@��I@�Q@�$@�	@��@��@���@�m]@�@@�~�@�5?@��@��4@���@�M@�-w@��T@��@���@���@�kQ@�.�@�dZ@��f@�͟@���@�@�@��N@�@���@�l"@�R�@��D@�j�@���@���@�W�@���@�[W@��@���@�%�@��@@��:@��h@��7@�w2@�IR@���@�6�@� �@���@�S�@��@�ߤ@��@�<�@���@�[W@��@���@�M�@��3@�J#@��@���@�?@���@���@�x�@�+�@��m@�r�@�x@�ƨ@��[@���@��{@�7L@��@��|@�y>@�:�@��H@��@�u�@��@��#@�x�@�S�@�%@���@��F@�i�@�[�@�M�@�.�@�~@��.@���@���@�e,@�(�@���@�^5@���@�y�@�@@��@���@�W�@�D�@�;�@���@��@���@�ȴ@��1@�Xy@�1�@��@���@���@�o @���@��@�ȴ@�l"@��@��@�%�@�%�@��@���@���@�k�@�@��@���@��y@���@���@�]d@��@��	@�U�@��@��@���@��@���@��S@�a�@�G�@� \@��	@�ں@��b@�K^@��)@��g@���@���@���@��	@�"�@��}@�c @��@��z@�S�@���@��@���@�B[@�>B@��Z@��k@�O�@���@���@��+@�E�@��@���@�Q�@�Y@��@��@�y>@�*�@��@��;@���@�b�@���@�kQ@��@��@�rG@�8@�;@���@�\�@��F@�^�@�'�@���@���@�L0@�(�@���@���@���@�s�@�5�@��@��_@�Ft@��@6z@~��@~!�@}j@|�e@{�@{�@z� @z4@y@xbN@w��@vu%@vB[@v�@u�9@u�C@uo @t��@t`�@t�@s�6@s��@s��@s��@sv`@s|�@s�4@s8@sY@r��@r0U@q�@q��@p�o@p@o=@n��@nYK@m��@m	l@lM@l  @k.I@j
�@i��@ic�@i4@i!�@i�@h�)@hQ�@h'R@gخ@g��@gZ�@g)_@f�c@f��@f�r@f8�@e�@e�z@em]@e	l@d�E@d��@d��@dr�@dM@d~@c��@c�V@cj�@b�@b�X@b�F@a��@`��@`��@`m�@`U2@_ƨ@_,�@^�]@^��@^��@^$�@]�C@]Q�@]�@\ �@[C�@Z��@Z�s@Z��@Z{@Y�@Y�@X�Y@X4n@W��@W33@Vں@V�6@V��@Vq�@V0U@Vu@U�@U��@U0�@T��@T��@Tz�@TI�@Sj�@Rh
@R-@R
�@Q�Z@Q�@Qk�@P��@Ph�@O��@Ox@O4�@O�@N͟@N��@N\�@NOv@N@�@N�@M�@M@M�@M<6@L��@L�Y@K��@K)_@K�@J�M@J�@J�L@J^5@J&�@I�@HĜ@HU2@H?�@H<�@H:�@H%�@G�&@G��@Go@F��@F�R@F��@Fu%@E��@E��@D��@Db@C�[@Cqv@B�H@B��@BJ�@A�@A�@A�t@A�M@ADg@A%@@��@@4n@?��@?iD@>��@>kQ@>Ta@>L0@>C�@>u@<�@<PH@<-�@<%�@<1@;�g@;��@;�F@;��@;o�@;�@;�@:�s@:�@:�!@:��@:p;@:H�@:($@9��@9a�@9G�@9�@8��@8�@8֡@8��@8�9@8�4@8z�@8D�@8�@81@7�]@7�g@7�@7�@5�)@5�C@5�"@5u�@5u�@5c�@5�@4��@4��@4I�@3�g@3E9@3o@2�M@2��@2��@2}V@2xl@2ff@2B[@1��@1�^@1x�@1G�@1�@0�5@0��@0H@01'@0,=@0(�@0!@/�@/@.�M@.��@.R�@.u@-�#@-��@-s�@-(�@-V@-�@,��@,!@+�&@+�k@+8@*�'@*
�@)ԕ@)@)�@)IR@(��@(��@(��@(C-@(�@'�]@'�W@'��@'��@'Mj@''�@'�@&�8@&�]@&W�@%��@%c@%k�@%+�@%@%V@%	l@$�@$��@$Ft@#�@#�}@#��@#�F@#Z�@#@"�s@"��@"-@!�@!�'@!m]@!2a@!!�@!+@!@ �K@ �$@ �u@ j@ -�@ �@�@�f@�@��@s�@kQ@a|@�@�-@�@��@`B@�@�@��@��@y>@]d@1@�0@��@g�@A�@�@�8@ں@�1@�@�X@��@Q�@�@��@e�@M@�q@j�@@��@=q@��@�@f�@X@4@�/@�Y@V�@�@�@�@��@��@|�@Z�@��@��@�!@��@J�@�@��@�@��@�7@rG@S&@@%@�	@��@�@7�@1@�@��@dZ@,�@�h@M�@ �@��@ϫ@��@��@c�@/@*0@�@��@��@6@��@��@��@�P@\)@@O@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B>]B>�B>�B>wB>]B=�B>(B>(B>]B>�B>(B>(B>BB>BB>B=qB=qB=VB=�B=�B=B=B=<B=�BJ#B*�B�0B	qB,�B0�BD�Bd&BkBm�BtnB{�B��B@iBA�B��B��B�5BߊB~wB�NB�?B��BQB�B�B�bB�B�B~B�B�}B��B�B�B�qB�WB�mB�B��B�HB~BBW�B%FB_B
�)B
�=B
�~B
��B
raB
X�B
J�B	��B	��B	�SB	E�B	9�B	!�B	(B�8B��BܒB�,B�B��B��B�9B��B�.BуB�B�*B	�B	4B	�B	:B	1�B	`'B	��B	ևB	�B
�B
-B
 �B
,qB
1�B
5�B
;JB
C�B
LB
PHB
T�B
\xB
cB
c:B
b�B
abB
h�B
o5B
r�B
xlB
��B
�^B
��B
��B
�DB
��B
��B
�gB
�SB
� B
}�B
{0B
x�B
vB
tnB
r�B
q�B
q�B
p;B
p!B
o�B
oOB
oiB
ncB
lWB
j�B
g�B
d�B
a�B
_!B
]~B
Z�B
T�B
O�B
M�B
RTB
N"B
J�B
F�B
C�B
="B
?�B
FtB
KxB
IRB
JXB
IB
H�B
F?B
A�B
<B
;�B
AB
BAB
CGB
B�B
AoB
>�B
=�B
=VB
BuB
Q�B
]�B
h�B
gRB
eB
d�B
ffB
g�B
e`B
cTB
bB
a-B
b�B
dB
c�B
c�B
b�B
bhB
a|B
b4B
b�B
bhB
b�B
bNB
bB
abB
aHB
`�B
bB
c:B
a�B
b�B
`�B
^�B
]�B
[�B
\�B
\�B
\]B
[WB
X�B
W�B
VmB
U�B
T�B
SB
P}B
M6B
K�B
J�B
J	B
IB
G_B
E�B
D�B
DMB
C-B
AoB
>BB
<PB
:�B
7fB
4�B
0oB
-CB
+�B
.IB
.�B
-�B
,B
+6B
*�B
)DB
%�B
%,B
$�B
$B
#�B
#TB
#�B
#TB
!bB
VB
B
]B
�B
�B
CB
)B
�B
=B
QB
B
�B
�B
_B
�B
�B
�B
B
�B
oB
�B
�B
�B
�B
}B
.B
}B
�B
\B
B
B
B
�B
�B
�B
vB
\B
BB
�B
BB
�B
"B
(B
�B
bB
�B
�B
�B
�B
hB
NB
�B
4B
�B
�B
bB
�B
BB
�B
�B
B
B

�B
B
B

�B

�B

�B

�B

�B

�B
B
)B
)B
)B
)B

=B
	�B

#B

�B

�B

�B

�B

�B

�B

�B
�B
B
�B
�B
B
0B
B
dB
�B
~B
~B
~B
dB
dB
dB
~B
B
�B
JB
�B

=B

�B
	RB
�B
?B
B
�B
SB
B
�B
mB
mB
%B
B
%B
_B
zB
	B
	�B
	�B
	�B
	lB

XB
B
DB
B
�B
DB

�B
	RB
	RB
	�B
	�B

	B

�B
�B
0B
B
B
�B
VB
"B
�B
�B
BB
�B
�B
�B
�B
.B
bB
HB
�B
hB
�B
TB
[B
�B
�B
�B
YB
EB
yB
�B
yB
�B
�B
�B
�B
�B
B
�B
kB
�B
�B
�B
�B
)B
)B
�B
�B
�B
�B
B
�B
IB
~B
�B
�B
�B
5B
B
OB
�B
�B
�B
�B
OB
5B
�B
5B
 vB
 'B
 BB
�B
�B
pB
;B
;B
VB
VB
 vB
 BB
 vB
 B
 �B
!B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"hB
"�B
#:B
#TB
#�B
$&B
%B
%�B
&B
&�B
'B
'B
&�B
'B
'B
'B
'B
&�B
'B
'mB
($B
(XB
)*B
)yB
)�B
)_B
*�B
+�B
,�B
-wB
-B
.IB
.�B
.�B
.�B
/ B
/ B
/�B
/�B
/�B
0;B
0UB
1AB
1vB
2aB
3�B
3�B
4B
4TB
4�B
4�B
5%B
5B
5B
4�B
5�B
6+B
6+B
6�B
6�B
7�B
7�B
7�B
7�B
8B
9	B
9�B
9�B
9�B
9�B
9�B
9�B
:DB
:DB
:^B
:�B
:�B
;dB
<B
<PB
<jB
=B
="B
=<B
=�B
>B
>�B
>�B
?HB
?}B
@4B
@OB
AB
A�B
A�B
A�B
A�B
A�B
BB
BAB
B�B
B�B
CB
CB
C-B
CGB
CGB
CGB
C-B
C�B
C�B
C�B
C�B
DB
D3B
EB
ESB
E�B
E�B
E�B
FB
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I7B
I7B
I�B
I�B
I�B
I�B
I�B
J	B
J	B
JXB
J�B
JXB
J�B
KB
KDB
K^B
K^B
KxB
KxB
K�B
K�B
K�B
K�B
LB
LB
K�B
MB
M6B
MjB
MjB
MPB
M�B
N<B
NpB
NVB
N�B
N�B
OB
OB
OB
O�B
PbB
PbB
PHB
P�B
P�B
P�B
QNB
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
SB
S@B
S[B
S[B
S�B
S�B
TaB
T�B
T�B
T�B
U�B
V�B
V�B
V�B
V�B
V�B
W
B
W�B
W�B
XB
X_B
X�B
X�B
X�B
X�B
Y1B
Y1B
Y1B
YeB
YeB
YB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[=B
[=B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]�B
^B
^jB
^jB
^jB
^jB
^�B
^�B
_�B
`B
`B
`'B
`\B
`\B
`�B
aB
a-B
aHB
a�B
b4B
b�B
c B
c�B
d@B
dtB
d�B
e`B
e`B
e`B
e`B
ezB
f�B
gB
gB
gB
gB
gRB
gmB
gmB
gmB
g�B
g�B
g�B
h
B
h
B
h
B
h
B
h
B
h$B
h$B
hsB
h�B
h�B
i*B
iDB
iDB
i_B
iyB
i_B
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i_B
iDB
iDB
iyB
i�B
iyB
i�B
i�B
i�B
i�B
j�B
kB
k6B
kB
kkB
kkB
kkB
kQB
kQB
kQB
kkB
kQB
kkB
k�B
k�B
k�B
l"B
lWB
lWB
lqB
lWB
l"B
mB
m)B
mB
mCB
mwB
m�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n}B
n�B
o B
o�B
p�B
p�B
p�B
p�B
p�B
q[B
q[B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
rGB
raB
raB
raB
r�B
s�B
s�B
s�B
s�B
tB
s�B
s�B
s�B
tTB
t�B
u%B
u?B
uZB
u�B
vB
vzB
vzB
vzB
wB
w2B
wfB
w�B
w�B
x8B
x8B
xRB
x�B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
z*B
z*B
zDB
zDB
z*B
z�B
z�B
{B
{0B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
}qB
}�B
}�B
}�B
~B
}�B
~BB
~�B
~�B
~�B
~�B
~wB
~BB
~]B
~wB
~�B
.B
B
�B
�B
�iB
��B
�B
�B
� B
�oB
��B
�B
�[B
��B
��B
��B
�B
�B
�GB
��B
��B
��B
��B
��B
�3B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�9B
�mB
��B
�B
�%B
�?B
��B
��B
�+B
��B
�1B
�KB
�fB
��B
��B
��B
�B
��B
��B
�B
�B
��B
�#B
�	B
�#B
�XB
��B
��B
��11111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B6`B6�B6�B6zB6`B5�B6B6+B6`B6�B6+B6B6FB6FB6B5tB5tB5ZB5�B5�B5B5B5ZB6BC�G�O�G�O�B	~B$&B+B>(B^�BeBh
Bq[B{B�IB:*B;dB��B��B�yB�IBxRB��B�HB�AB�B�B��B��BżB�WB�B�B��B��B�'B��B��B�B��B��B��B��Bz�BUgB!�B�B
�KB
��B
��B
�AB
n�B
WsB
S�B	�)B	��B	�	B	B'B	7�B	�B	�B��BބBؓB��BȚB�\B�B� BȚB�fB��B�B�|B	3B		�B	
�B	�B	*�B	VB	�B	�PB	�;B	��B	��B
B
%FB
*�B
.IB
4B
<B
DgB
H�B
MB
T�B
[qB
[�B
[qB
Y�B
a-B
gmB
j�B
p�B
{B
��B
�XB
��B
��B
HB
|�B
}"B
~�B
y�B
vzB
tB
qvB
n�B
m)B
k�B
j�B
i�B
h>B
h$B
g�B
g�B
hXB
gmB
e,B
c�B
`'B
]dB
Z�B
XB
W
B
S�B
M�B
H1B
FYB
J�B
F�B
C�B
?�B
<�B
4�B
7�B
>�B
C�B
A�B
C-B
AoB
A�B
?.B
:�B
4�B
3�B
9$B
:xB
;�B
;JB
:B
6�B
5�B
5B
9�B
IB
U�B
a-B
_�B
]dB
\�B
^�B
`BB
^B
[�B
Z�B
Y�B
[=B
\xB
\B
\)B
Z�B
Z�B
Y�B
ZQB
Z�B
Z�B
Z�B
ZkB
Z7B
YB
YeB
YB
Z�B
[�B
Z�B
[#B
YKB
WYB
VB
S�B
U2B
T�B
T�B
S�B
QNB
O�B
N�B
NVB
M�B
K�B
IB
E�B
D3B
CB
B�B
A�B
@ B
=�B
<�B
<�B
<B
:�B
6�B
4�B
3MB
0;B
./B
)yB
%�B
$@B
&�B
'mB
&LB
$@B
#�B
#:B
"B
B
dB
/B
]B
B
�B
�B
�B
B
B
�B
B
B
�B
FB
aB
B
�B
�B
�B
B
�B
�B
HB
}B
(B
�B
�B

�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
KB
B
B
�B
�B
EB
�B
�B
�B
KB
KB
�B
�B
zB
_B
�B
�B
	�B
	�B
	�B
	�B
	�B
	B
	�B
	�B
	RB
�B
B
�B
�B
tB
�B
�B
�B
�B
�B
-B
B
B
�B
�B
B
GB
GB
GB
{B
aB
uB
'B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
gB
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
9B
�B
�B
gB
�B
aB
B
 OB	�BB	�(B	�(B	��B	�wB	��B	��B	��B	�wB	�]B	��B	��B	�}B
UB
B
�B
AB
�B
�B
aB
�B
GB
�B
{B
�B
oB
;B
�B
�B
'B
-B
3B
MB
9B
YB
�B
YB
?B
�B
+B
�B
�B
�B
�B
KB
fB
�B
�B
	B
	�B
	�B

�B
�B
�B
6B
B
�B
HB
}B
�B
�B
�B
�B
�B
4B
hB
B
 B
�B
�B
@B
�B
�B
FB
aB
�B
�B
�B
�B
2B
B
gB
�B
�B
�B
�B
�B
SB
�B
9B
B
B
�B
SB
SB
B
�B
yB
EB
yB
B
�B
�B
YB
?B
sB
�B
yB
_B
�B
EB
�B
�B
�B
B
B
�B
�B
�B
�B
kB
�B
kB
�B
WB
�B
)B
CB
/B
�B
OB
�B
!B
!B
B
!B
;B
!B
B
B
VB
�B
 BB
 \B
!-B
!�B
!�B
!�B
#B
#�B
%B
%�B
%FB
&fB
&�B
'B
'B
'B
'8B
'�B
'�B
'�B
(XB
(sB
)_B
)�B
*�B
+�B
+�B
,WB
,qB
,�B
-B
-CB
-B
-)B
-)B
-�B
.IB
.cB
.�B
/ B
/�B
/�B
/�B
/�B
0UB
1'B
1�B
2B
2B
2B
1�B
2B
2GB
2aB
2|B
2�B
2�B
3�B
4B
4�B
4�B
5%B
5?B
5tB
5�B
6FB
6�B
7B
7fB
7�B
8lB
8�B
9>B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
:�B
:�B
;B
;0B
;0B
;0B
;0B
;0B
;JB
;�B
;�B
;�B
<6B
<PB
<jB
=<B
=�B
=�B
=�B
=�B
>]B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
AB
AUB
A;B
A�B
A�B
A�B
A�B
A�B
B'B
BB
B[B
B�B
B[B
B�B
CB
CGB
CGB
CaB
C{B
C{B
C�B
C�B
C�B
C�B
DB
D3B
DMB
E9B
E9B
E�B
EmB
E�B
FB
FYB
F�B
FYB
F�B
F�B
G+B
G+B
GEB
HB
HfB
HfB
H�B
H�B
H�B
H�B
IlB
I�B
I�B
J#B
J�B
J�B
KB
J�B
KB
KDB
KxB
KxB
K�B
K�B
L~B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
OB
OBB
O�B
O�B
P.B
PbB
P�B
P�B
P�B
QB
QB
QNB
Q4B
Q�B
QhB
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
SB
S@B
S[B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
V9B
VmB
VmB
V�B
V�B
W
B
W
B
W�B
XB
XB
XEB
XyB
X_B
X�B
Y1B
Y1B
YKB
Y�B
ZQB
Z�B
[=B
[�B
\xB
\�B
\�B
]dB
]dB
]~B
]~B
]�B
^�B
_!B
_B
_!B
_;B
_VB
_VB
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`'B
`'B
`B
`'B
`BB
`�B
`�B
`�B
a-B
a-B
a-B
aHB
abB
a|B
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a|B
abB
a-B
abB
a�B
a|B
a�B
a�B
a�B
bB
b�B
c B
cTB
c B
cnB
cTB
cTB
cTB
cnB
cnB
c�B
cTB
cnB
c�B
c�B
c�B
d&B
d@B
d@B
dZB
dtB
d@B
eB
e,B
eB
eFB
e�B
e�B
ezB
e�B
e�B
e�B
fB
e�B
e�B
f�B
f�B
f�B
g8B
g�B
h�B
h�B
h�B
h�B
h�B
i_B
i_B
i_B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
jKB
jeB
jeB
j�B
kB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
lWB
l�B
mCB
m)B
m]B
m�B
n/B
n�B
n}B
n�B
oB
oOB
oiB
o�B
o�B
p;B
p!B
poB
p�B
q[B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rGB
rGB
rGB
raB
r�B
r�B
s3B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
u�B
u�B
u�B
vB
vB
vB
v`B
v�B
v�B
v�B
v�B
v�B
vFB
v`B
v�B
v�B
wfB
wLB
w�B
x8B
xlB
x�B
x�B
y	B
y$B
yrB
y�B
zB
zxB
z�B
z�B
{B
{B
{0B
{dB
{�B
{�B
{�B
|B
|B
|PB
|�B
|jB
|�B
|�B
|�B
|�B
}B
}B
}"B
}<B
}qB
~B
~B
~(B
~]B
~�B
~�B
.B
�B
�B
�OB
�iB
��B
��B
��B
��B
��B
�B
� B
�;B
��B
�'B
�'B
�'B
�uB
�uB
�uB
�u33111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<&��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�z�<#�
<#�
<O<�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.26(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9998(+-0.0001), deepest deltaS=-0.008(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-1500(dbar) is excluded in mapping;                                                                                                                                       Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202101070056562021010700565620210107005656202101070200492021010702004920210107020049202207271540572022072715405720220727154057  JA  ARFMdecpA30a                                                                20201225153933  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201225154012  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201225154012  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201225154012  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201225154012  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20201225154012  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20201225154013  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201225154013                      G�O�G�O�G�O�                JA  ARUP                                                                        20201225155211                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20201227000000  CF  PSAL_ADJUSTED_QC@?\)BW33G�O�                JM  ARCAJMQC2.0                                                                 20210106155656  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210106155656  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20210106170049  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  TEMP_ADJUSTED_QCBP
=BP
=G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727064057  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091506                      G�O�G�O�G�O�                