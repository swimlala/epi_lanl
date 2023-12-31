CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-05T12:39:58Z creation;2020-12-05T12:39:59Z conversion to V3.1;2022-08-17T01:55:30Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201205123958  20220818091506  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA  A30_8420_067                    2C  D   APEX                            8420                            2.11.2                          846 @�L�fff�1   @�L�Ӡm @/-5�Xy>�b��n.�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A���A�  A�  A�33A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BZ  B_��Bh��Bn  Bx  B��B���B�  B�  B�ffB�ffB�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�33B�  B�  B�ffB�  B�  B�  B�  C   C  C  C�C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(Q�@p��@��@���A��A<��A]G�A}A�z�A�(�A��
A�z�A�z�Aޣ�A�(�A���B��B{B(�B(�B'�B/G�B7\)B?�BG=qBOQ�BY33B^�HBh  BmQ�Bw33B~��B��B��=B���B�\B���B��\B���B���B��3B�B�aHB��\B��B��\B���B��)Bó3B�Q�B�u�Bπ BӅBמ�BۮB���B� B�B��B�z�B�{B��{B��{B���C��C�{C�HC��C	�qC� C�CC� C� C�CǮC��C�{C��CC!�C#ǮC%��C'�\C)��C+�\C-ٚC/ٚC1ǮC3� C5�qC7�C9�C;� C=�C?�CACC�=CE�\CG�\CI��CK�=CM�=CO��CQ�\CS�\CU�\CW��CY�C[�C]�=C_�CaǮCc��CeǮCg�Ci��Ck��Cm�=Co� Cq�qCsCuCw�Cy�\C{��C}�=CC��fC��fC��C��C��fC��C��C���C��=C��C��fC��C��fC���C��C�� C��HC��C��fC���C��=C��=C��C��fC���C��fC�� C���C��C��=C��C��=C��C��C��fC��C���C��C��fC��C��C��fC���C��fC��fC��C���C��fC��fC��C��C��C��fC��C��fC��fC��C��fC���C��C��C��C��C��HC��HC��C���C��C��C��C��C���C��fC��C��C��C��C��C���C��C���C��C��fC��C��HC��C���C��C��C���C���C���C��C���C��C��HC���C���C��fC��C���C��C���C���C��C��C��C��C��C���C��=C���C���C���C��C��fC���C�� C��C���C��C��C��fC��C��fC��=C���C��D r�D �Ds3D�DqHD�3Ds�D�{Ds�D�3Dq�D��Ds3D�Dr�D�Dr�D��D	q�D	�HD
s3D
�3Dr�D�Dp�D��Dr�D��Dr�D�{Ds�D�Dp�D�Dr�D�HDs3D�{DuD�HDp D��Dq�D�3Dq�D�HDqHD�\Dq�D��Du�D��Ds�D��DqHD�HDqHD�3Dt{D��Ds3D��Dt{D��D p�D �D!uD!��D"q�D"�D#s3D#�D$p�D$�HD%q�D%��D&vfD&�D's3D'�D(s3D(�{D)t{D)�3D*q�D*�D+r�D+��D,q�D,��D-t{D-��D.s3D.��D/r�D/�3D0t{D0�3D1q�D1�3D2q�D2�D3uD3�{D4qHD4�HD5r�D5��D6s�D6�D7r�D7�3D8uD8��D9s3D9��D:r�D:�3D;s�D;�3D<s�D<��D=r�D=�D>qHD>�D?u�D?��D@s3D@�3DAqHDA�DBqHDB��DCp�DC��DDs�DD��DEqHDE�HDFqHDF�DGq�DG��DHq�DH��DIr�DI��DJq�DJ�DKt{DK�DLp�DL� DMp�DM�DNqHDN�3DOs3DO�HDPr�DP�3DQs3DQ�3DRq�DR��DSs3DS�DTs3DT�{DUt{DU��DVq�DV��DWt{DW�{DXs3DX�DYs�DY��DZt{DZ��D[r�D[�D\r�D\�D]q�D]�HD^r�D^�D_q�D_�HD`p�D`��Das3Da��Dbr�Db�Dcs3Dc�{DduDd�DeqHDe��Dfr�Df�{Dgs�Dg�{DhuDh��Dip�Di�Djq�Dj�Dkp�Dk�Dlq�Dl��Dms3Dm�3Dns3Dn��DoqHDo�DpvfDp��Dqq�Dq� Drq�Dr�DsuDs�Dto\Dt� Dup Du�Dvp�Dv�Dwp�Dw�DxqHDx�HDyqHDy�Dzr�Dz�{D{q�D{��D|r�D|�3D}s3D}�D~t{D~�Dp�D�HD�8�D�x D���D���D�9HD�x�D���D���D�9�D�x�D��HD���D�8 D�x�D���D�� D�8RD�x�D���D���D�9�D�y�D���D���D�8�D�x�D���D��HD�8�D�xRD���D��HD�9�D�x�D���D���D�:=D�yHD��RD��RD�8�D�x�D���D��HD�8�D�xRD���D���D�8�D�x�D��HD���D�8�D�xRD���D���D�8RD�yHD���D���D�:=D�z=D��HD���D�8RD�x�D���D��=D�9HD�x D���D���D�9�D�xRD��\D���D�8�D�y�D���D���D�9HD�x D���D���D�9HD�yHD���D���D�9�D�x D���D���D�7�D�xRD���D���D�:�D�y�D��HD���D�8�D�xRD�� D���D�8 D�xRD���D��RD�8 D�x D�� D��\D�8�D�y�D���D�� D�8�D�yHD���D���D�9HD�z=D��=D���D�8�D�x�D���D��HD�8�D�x D��HD���D�8�D�y�D���D���D�:�D�y�D��RD�� D�8�D�y�D���D��HD�9�D�y�D���D���D�9HD�xRD���D��=D�9�D�yHD���D���D�:=D�x�D���D���D�9HD�y�D���D��=D�9�D�yHD���D��=D�9�D�y�D��=D��HD�9�D�z�D���D���D�9HD�x�D��HD��HD�9HD�yHD���D���D�8�D�x�D��HD���D�8�D�z=D���D���D�8 D�xRD���D��HD�9HD�x�D���D��=D�9�D�x�D���D���D�9�D�yHD���D���D�9�D�y�D��HD���D�8�D�x�D�� D�� D�8 D�x�D��HD���D�9�D�y�D���D���D�7�D�x�D���D���D�:�D�z�D��=D���D�9HD�x�D��RD���D�:�D�y�D��HD���D�8�D�yHD���D���D�9�D�y�D���D���D�9�D�y�D��HD���D�9�D�x�D��RD���D�9HD�y�D���D��HD�9HD�x�D���D��RD�8�D�y�D¹HD�� D�8�D�z�Dú�D��HD�8�D�x�Dĸ�D���D�:�D�yHDŹHD���D�8�D�xRDƷ�D���D�:=D�y�Dǹ�D���D�:�D�y�Dȸ�D���D�9�D�x�Dɸ�D���D�:=D�y�Dʸ�D���D�8�D�x�D˹HD��HD�9HD�x�D̹HD��HD�8�D�x�D͸ D��RD�8�D�yHDθ�D���D�8�D�xRDϸ�D���D�:=D�yHDи�D��HD�9HD�y�DѹHD��RD�7�D�xRDҹHD���D�:�D�y�Dӹ�D��=D�9�D�x�DԷ�D���D�8RD�y�DչHD�� D�9HD�y�Dָ�D���D�9�D�x�D׸�D��HD�8�D�yHDغ=D���D�:�D�y�DٹHD��HD�8RD�x�DڹHD���D�9HD�x�D۸�D��HD�9HD�yHDܸ�D���D�9�D�y�DݹHD���D�9HD�x�D޹�D���D�8�D�x�D߸�D���D�9HD�y�D�HD��HD�8�D�x�DẏD���D�8�D�y�D�HD���D�8�D�yHD��D���D�8�D�yHD�HD�� D�8RD�x�D�HD��HD�8�D�y�D�HD��HD�9�D�yHD�HD���D�:=D�y�D�RD���D�9�D�y�D鹚D��HD�8�D�xRD�RD���D�9�D�w�D� D���D�9HD�y�D��D���D�:=D�z=D���D���D�8RD�x D� D��HD�9�D�x�D�RD��RD�9�D�x�D�D���D�:=D�x�D�D��=D�9HD�x D��D���D�9HD�y�D��D��=D�:�D�y�D��HD���D�8 D�x�D��HD��=D�9HD�x�D���D���D�8�D�x�D��HD���D�:=D�x�D��HD���D�9HD�x�D�� D��HD�:�D�z=D���D��HD�8RD�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A���A��A��RA��RA�ɺA�ÖA��aA��'A���A���A���A��0A��A��BA���A��gA��sA��2A���A��A���A�ܒA��gA��KA�iyAɞ�AǸA�b�A�u�A�[#A��>A�J�A�4A�B�A�IA�PA��?A�g�A�P�A�)*A��SA�l"A�W
A�PHA�R�A���A��A��A���A�O�A��A�YKA�(�A��Ay��AorGAfVAe/Ad�QAd:�Ac.�Ab��AaPHA_
�AZ�&AU��AN��AKU�AI  AHQ�AED�ABt�A?� A=e�A<A;A9�)A9.IA8�XA8�=A9�rA:�A;Q�A:(A8��A7z�A5�LA7?A7uA6��A6�'A6J�A5��A6J�A5�6A5��A5�AA4��A3b�A2��A2W?A2bA1s�A0��A/��A/xA.o A-A,FtA,�A+�]A+��A+�A*�	A*uA*4A*DgA*�A*�RA*B�A)�zA)e�A)xA(�_A(DgA'�A'`�A&�pA&qvA&S�A%_�A#��A#�wA#�hA#T�A"�vA"�"A"PHA"�A!��A!�$A!	�A �3A ��A Z�A�A�AA�9A4nA��A(�A�A�:A{A��A�FA��AB�A�.A�MA��A�eAbNAeA�OA�A�"Ah�APHA�?AN�A��Am]A��A�9A�A�pAO�A��A��AԕA\)AںA�\A�A�VA!-A��A@OA��A�A
��A
dZA
/�A	�cA	�FA	E9A	'�A��A�~AB�A�A��A3�A�,AB�A�Au�AJA�$AS�A�=A �A �A �zA $�@�g�@��@�*�@�\�@�5�@��@��@���@���@�(�@���@��@���@�.I@���@�!�@��T@�y�@��@�(�@���@�5?@�@�@�+�@��@�}@�1�@��Z@�@��y@�!@�҉@���@��@�-@�'�@��@�!�@�M@��5@��@�hs@�b@�g8@��T@�5�@�B[@�0@�?�@�q@���@��@�͟@��@�ϫ@�@O@�n�@��j@�|�@�K^@��@��@؆Y@��3@�F�@�v�@���@�&@�D�@��@Ӕ�@�\�@��@�͟@ѩ*@��@Мx@�R�@�z�@А.@��@ϰ�@�s@�o@�#:@͓�@�;d@�ں@�=q@˛=@��y@��'@�҉@���@�Ov@ɮ@ɘ�@�x�@�^�@�(�@���@�+k@�Y�@��@�Ɇ@�2�@žw@�/@Ĵ9@�s�@�A�@�b@õt@�Y�@\@���@�C@��z@�kQ@��@��k@�7L@��)@���@�9X@��@���@��@�h�@�*�@�t�@�(�@��8@�l�@��@�|@���@�ƨ@�4@���@�U2@��@��w@�j@��8@���@���@�~�@�l"@�a|@�M@�#:@�J@�G@��m@��@���@�M@��@��@�x@�+@��,@��F@�l�@���@�P�@���@��T@���@���@��C@��k@�Z�@�$t@�v�@�Ta@�4n@���@�x�@�*0@���@�S�@�1�@��@���@�Z�@���@���@�r�@��}@���@�Q@�bN@�PH@��@��@���@�u�@� \@��u@�kQ@�$@��g@�qv@�W?@�Q�@�E9@�	l@���@�xl@�E�@��@���@�X@�&@��2@�^5@��a@�v`@�E9@��@���@�kQ@�{@��@���@�m]@�S�@�H�@�4�@��@��@��@���@���@��A@�Xy@���@��'@��@���@�tT@��@��d@��@�C�@��@�@���@�`�@�c�@�_@�H@�0U@��@�_@��@��@��@��n@�{J@�@��E@��U@��e@�~�@�Z�@�A�@�1'@�!�@��@��z@���@��f@�j�@�A @�C@��@�v�@�J@���@�o @�6z@��@��@��4@�U2@��@��@�rG@�S�@��?@�p;@�x@��T@���@�U�@�5�@��@��@�Ɇ@��@�C�@��@�]�@��M@���@�~(@��@�A�@�	@�G�@��y@���@�Z@��]@���@���@�U�@�&�@��@���@�\�@� �@���@���@��@�`B@��@���@��b@�g8@�'R@�
�@�ݘ@��@�7L@���@�J�@�b@�_@��F@���@�P�@�#�@���@��@��@�?�@�!�@��T@���@�o @�A @�;@���@���@���@�L0@�-�@�_@���@�m]@�A @�+@��@���@�p;@�+@C@~z@~R�@~;�@}��@}��@}IR@}�@|��@{�a@{U�@{�@z�h@z@�@z�@y��@y@y�S@yzx@y�@x[�@wݘ@w�{@v�@vW�@vM�@v-@u�n@u-w@toi@s��@s�w@s"�@r��@q��@q�n@q�@p��@p~(@o�+@o�f@oA�@n��@n8�@m��@m�7@m�@lz�@k��@jߤ@jn�@j@�@j�@is�@h�p@g�+@f��@fxl@f�@e��@d�`@d_@c�w@c_p@b��@ba|@a��@a=�@a@@`M@`x@_�Q@_�@^	@]��@\�?@\H@[��@[x@["�@Z͟@ZkQ@Z&�@Y�@Y��@Y*0@X��@X?�@W��@WdZ@V�"@VE�@U�o@U�d@U?}@U*0@T��@TZ@T%�@S� @SK�@R��@Ri�@Q�d@QA @P�@PD�@O�Q@O{J@N��@N��@N5?@N-@N�@M��@L�j@L$@Kخ@K��@KK�@J�8@J��@J#:@I��@I@@H�j@H7�@G�@G˒@G��@F��@F�6@F� @F��@Fc @F�@E�#@E��@Ew2@EA @D��@DN�@D!@C��@CP�@CS@B�@B�@A�T@A��@A��@A��@AQ�@A(�@@֡@@�@@D�@@(�@?��@?��@?s@?P�@>�@>��@>�A@>n�@>)�@=�>@=�@=+@<ی@<�)@<��@<�.@<M@<�@;��@;خ@;�K@;��@;b�@;A�@;$t@:�"@:ȴ@:u%@:�@9��@9��@9c�@9;@8��@8��@8U2@7��@7�@7;d@6��@65?@5�C@5Q�@5%@4��@4A�@3��@3O@2��@23�@1��@1�7@1&�@0�)@0H@/��@/��@/x@/X�@/�@.v�@.@�@.$�@-��@-�@-x�@-S&@-@@,�I@,[�@,!@+��@+�6@+��@+e�@+>�@*�y@*�@*\�@*B[@*.�@*	@)��@)�"@)2a@)q@)�@(�o@(x@'�@'��@'�k@'b�@'�@&��@&��@&J�@&0U@%��@%��@%�N@%��@%��@%c@%4@%%@$�E@$��@$`�@$�@#�@#�F@#��@#]�@#J#@#Y@"��@"�'@"��@"�@!�Z@!�@!��@!�@ �?@ ��@ y>@ Q�@ 1'@ !@ �@�@�g@�@��@�k@�{@b�@o@�'@��@p;@d�@#:@J@��@�#@��@��@J�@�|@�@��@ �@�A@��@��@H�@$t@�@��@��@d�@Ta@E�@ �@��@hs@T�@�@��@�[@��@D�@(�@	�@�@@|�@P�@E9@8@C@�b@-@�@�@��@=�@֡@�j@�9@u�@�@��@�&@��@l�@J#@�@
=@��@�F@3�@�@��@X@G�@�@�9@�4@N�@7�@7�@2�@1@�[@a@6z@$t@�8@ߤ@��@��@YK@Ta@Q@H�@$�@�j@�t@}�@(�@�@�@�@�E@��@��@��@y>@e�@N�@6@!@�@�w@|�@a@\)@RT@>�@(@�@
�@
�m@
~�@
Ta@
	@	�Z1111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A���A��A��RA��RA�ɺA�ÖA��aA��'A���A���A���A��0A��A��BA���A��gA��sA��2A���A��A���A�ܒA��gA��KG�O�G�O�G�O�G�O�G�O�A�[#A��>A�J�A�4A�B�A�IA�PA��?A�g�A�P�A�)*A��SA�l"A�W
A�PHA�R�A���A��A��A���A�O�A��A�YKA�(�A��Ay��AorGAfVAe/Ad�QAd:�Ac.�Ab��AaPHA_
�AZ�&AU��AN��AKU�AI  AHQ�AED�ABt�A?� A=e�A<A;A9�)A9.IA8�XA8�=A9�rA:�A;Q�A:(A8��A7z�A5�LA7?A7uA6��A6�'A6J�A5��A6J�A5�6A5��A5�AA4��A3b�A2��A2W?A2bA1s�A0��A/��A/xA.o A-A,FtA,�A+�]A+��A+�A*�	A*uA*4A*DgA*�A*�RA*B�A)�zA)e�A)xA(�_A(DgA'�A'`�A&�pA&qvA&S�A%_�A#��A#�wA#�hA#T�A"�vA"�"A"PHA"�A!��A!�$A!	�A �3A ��A Z�A�A�AA�9A4nA��A(�A�A�:A{A��A�FA��AB�A�.A�MA��A�eAbNAeA�OA�A�"Ah�APHA�?AN�A��Am]A��A�9A�A�pAO�A��A��AԕA\)AںA�\A�A�VA!-A��A@OA��A�A
��A
dZA
/�A	�cA	�FA	E9A	'�A��A�~AB�A�A��A3�A�,AB�A�Au�AJA�$AS�A�=A �A �A �zA $�@�g�@��@�*�@�\�@�5�@��@��@���@���@�(�@���@��@���@�.I@���@�!�@��T@�y�@��@�(�@���@�5?@�@�@�+�@��@�}@�1�@��Z@�@��y@�!@�҉@���@��@�-@�'�@��@�!�@�M@��5@��@�hs@�b@�g8@��T@�5�@�B[@�0@�?�@�q@���@��@�͟@��@�ϫ@�@O@�n�@��j@�|�@�K^@��@��@؆Y@��3@�F�@�v�@���@�&@�D�@��@Ӕ�@�\�@��@�͟@ѩ*@��@Мx@�R�@�z�@А.@��@ϰ�@�s@�o@�#:@͓�@�;d@�ں@�=q@˛=@��y@��'@�҉@���@�Ov@ɮ@ɘ�@�x�@�^�@�(�@���@�+k@�Y�@��@�Ɇ@�2�@žw@�/@Ĵ9@�s�@�A�@�b@õt@�Y�@\@���@�C@��z@�kQ@��@��k@�7L@��)@���@�9X@��@���@��@�h�@�*�@�t�@�(�@��8@�l�@��@�|@���@�ƨ@�4@���@�U2@��@��w@�j@��8@���@���@�~�@�l"@�a|@�M@�#:@�J@�G@��m@��@���@�M@��@��@�x@�+@��,@��F@�l�@���@�P�@���@��T@���@���@��C@��k@�Z�@�$t@�v�@�Ta@�4n@���@�x�@�*0@���@�S�@�1�@��@���@�Z�@���@���@�r�@��}@���@�Q@�bN@�PH@��@��@���@�u�@� \@��u@�kQ@�$@��g@�qv@�W?@�Q�@�E9@�	l@���@�xl@�E�@��@���@�X@�&@��2@�^5@��a@�v`@�E9@��@���@�kQ@�{@��@���@�m]@�S�@�H�@�4�@��@��@��@���@���@��A@�Xy@���@��'@��@���@�tT@��@��d@��@�C�@��@�@���@�`�@�c�@�_@�H@�0U@��@�_@��@��@��@��n@�{J@�@��E@��U@��e@�~�@�Z�@�A�@�1'@�!�@��@��z@���@��f@�j�@�A @�C@��@�v�@�J@���@�o @�6z@��@��@��4@�U2@��@��@�rG@�S�@��?@�p;@�x@��T@���@�U�@�5�@��@��@�Ɇ@��@�C�@��@�]�@��M@���@�~(@��@�A�@�	@�G�@��y@���@�Z@��]@���@���@�U�@�&�@��@���@�\�@� �@���@���@��@�`B@��@���@��b@�g8@�'R@�
�@�ݘ@��@�7L@���@�J�@�b@�_@��F@���@�P�@�#�@���@��@��@�?�@�!�@��T@���@�o @�A @�;@���@���@���@�L0@�-�@�_@���@�m]@�A @�+@��@���@�p;@�+@C@~z@~R�@~;�@}��@}��@}IR@}�@|��@{�a@{U�@{�@z�h@z@�@z�@y��@y@y�S@yzx@y�@x[�@wݘ@w�{@v�@vW�@vM�@v-@u�n@u-w@toi@s��@s�w@s"�@r��@q��@q�n@q�@p��@p~(@o�+@o�f@oA�@n��@n8�@m��@m�7@m�@lz�@k��@jߤ@jn�@j@�@j�@is�@h�p@g�+@f��@fxl@f�@e��@d�`@d_@c�w@c_p@b��@ba|@a��@a=�@a@@`M@`x@_�Q@_�@^	@]��@\�?@\H@[��@[x@["�@Z͟@ZkQ@Z&�@Y�@Y��@Y*0@X��@X?�@W��@WdZ@V�"@VE�@U�o@U�d@U?}@U*0@T��@TZ@T%�@S� @SK�@R��@Ri�@Q�d@QA @P�@PD�@O�Q@O{J@N��@N��@N5?@N-@N�@M��@L�j@L$@Kخ@K��@KK�@J�8@J��@J#:@I��@I@@H�j@H7�@G�@G˒@G��@F��@F�6@F� @F��@Fc @F�@E�#@E��@Ew2@EA @D��@DN�@D!@C��@CP�@CS@B�@B�@A�T@A��@A��@A��@AQ�@A(�@@֡@@�@@D�@@(�@?��@?��@?s@?P�@>�@>��@>�A@>n�@>)�@=�>@=�@=+@<ی@<�)@<��@<�.@<M@<�@;��@;خ@;�K@;��@;b�@;A�@;$t@:�"@:ȴ@:u%@:�@9��@9��@9c�@9;@8��@8��@8U2@7��@7�@7;d@6��@65?@5�C@5Q�@5%@4��@4A�@3��@3O@2��@23�@1��@1�7@1&�@0�)@0H@/��@/��@/x@/X�@/�@.v�@.@�@.$�@-��@-�@-x�@-S&@-@@,�I@,[�@,!@+��@+�6@+��@+e�@+>�@*�y@*�@*\�@*B[@*.�@*	@)��@)�"@)2a@)q@)�@(�o@(x@'�@'��@'�k@'b�@'�@&��@&��@&J�@&0U@%��@%��@%�N@%��@%��@%c@%4@%%@$�E@$��@$`�@$�@#�@#�F@#��@#]�@#J#@#Y@"��@"�'@"��@"�@!�Z@!�@!��@!�@ �?@ ��@ y>@ Q�@ 1'@ !@ �@�@�g@�@��@�k@�{@b�@o@�'@��@p;@d�@#:@J@��@�#@��@��@J�@�|@�@��@ �@�A@��@��@H�@$t@�@��@��@d�@Ta@E�@ �@��@hs@T�@�@��@�[@��@D�@(�@	�@�@@|�@P�@E9@8@C@�b@-@�@�@��@=�@֡@�j@�9@u�@�@��@�&@��@l�@J#@�@
=@��@�F@3�@�@��@X@G�@�@�9@�4@N�@7�@7�@2�@1@�[@a@6z@$t@�8@ߤ@��@��@YK@Ta@Q@H�@$�@�j@�t@}�@(�@�@�@�@�E@��@��@��@y>@e�@N�@6@!@�@�w@|�@a@\)@RT@>�@(@�@
�@
�m@
~�@
Ta@
	@	�Z1111111111111111111111111144444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�9B�TB�TB�TB�TB�TB�nB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB	[B	�B�oB	IB
0B
"�B
?�B
NB
z�B
��B
�,B
��B
�>B
��B
��B
lqB
;�B
($B
bB
1B
aB	�B	�^B	�aB	�B	�_B	��B	��B	�
B	��B	fLB	LdB	IB	G�B	E�B	E�B	E�B	EB	B�B	:xB	7B	.�B	PHB	W�B	jB	h�B	a�B	Z�B	[�B	a�B	ezB	i�B	n}B	s�B	z�B	�FB	��B	ªB	��B	��B	��B	�xB	�]B
B
�B
7LB
A B
<PB
O(B
T{B
ZB
_�B
\CB
R�B
N�B
Q�B
X�B
Y�B
W�B
W�B
W�B
TFB
P}B
K�B
N�B
W�B
]dB
\]B
^�B
`'B
bNB
e�B
lqB
wB
w�B
v`B
t�B
t�B
vFB
u?B
t9B
r-B
p�B
q�B
x�B
shB
nIB
n�B
r�B
u�B
x�B
xB
v�B
u?B
utB
uZB
q�B
o�B
n/B
l�B
i�B
n�B
k�B
i_B
e�B
bhB
a�B
dB
c�B
c�B
b�B
^B
Z�B
YKB
Y�B
[WB
\B
[�B
[�B
]B
\�B
Y�B
YB
Z�B
X�B
V�B
XyB
YB
YeB
\)B
\�B
[�B
ZQB
YB
X�B
X+B
V9B
T�B
R�B
PHB
M�B
KxB
J�B
L~B
J�B
I�B
F%B
C�B
B�B
B�B
BuB
A;B
A�B
C�B
E�B
EB
C�B
D3B
C�B
BuB
>BB
9$B
/�B
 \B
�B
�B
=B
_B
eB
CB
�B
B
�B
�B
�B
�B
�B
�B
B
�B
oB
oB
 B
�B
�B
�B
�B
�B
�B

B
SB
�B
sB
�B
{B
[B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
	lB

rB
�B
pB
�B
"B
(B
NB
,B
�B
�B
B
�B
&B
�B
�B
�B
hB
 B
�B
.B
BB
VB
�B
�B
�B

�B
	B
fB
�B
�B
�B
�B
�B
zB
zB
_B
1B
fB
�B
B
�B
B
�B
	�B
�B
�B
6B
�B
�B
�B

	B
	B
�B
B
1B
�B
�B
�B
	B

�B

�B

�B

	B
	�B
	B
	B
�B
�B
�B
�B
�B
B
B
�B
�B
gB
�B
SB
�B
�B
�B
�B
[B
;B	��B	��B	�]B	��B	�wB	�B	��B	�]B	��B	��B	�qB	��B	��B	��B	��B	��B
 OB
;B
 �B	�}B	��B
 �B
 �B
 OB
 �B
;B
�B
�B
'B
uB
�B
�B
aB
{B
GB
aB
�B
mB
B
�B
�B
�B
1B
�B
�B
�B
	�B

=B

�B
dB
B
B
�B
�B
B
JB
�B
�B
jB
B
<B
pB
�B
\B
\B
BB
\B
�B
4B
NB
 B
�B
&B
MB
�B
mB
SB
9B
mB
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
7B
eB
EB
�B
�B
�B
�B
�B
B
7B
7B
�B
�B
�B
WB
�B
�B
�B
�B
B
)B
B
�B
�B
�B
B
�B
5B
OB
�B
�B
dB
�B
�B
�B
�B
B
~B
~B
pB
 'B
 \B
 vB
 �B
 �B
 �B
 �B
 �B
!HB
 �B
 �B
 'B
 'B
�B
VB
!B
VB
pB
�B
!B
�B
�B
�B
 B
 �B
 �B
 �B
!|B
!�B
!�B
"B
"NB
"4B
"hB
"hB
"�B
#:B
#�B
#�B
#�B
$�B
%�B
%�B
%FB
%`B
%�B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'mB
'�B
(�B
)*B
)�B
*B
*B
+kB
+kB
+�B
,qB
,�B
-B
.cB
.�B
/5B
/�B
0oB
0�B
1[B
1�B
1�B
2GB
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
5�B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
88B
8lB
8�B
8�B
8�B
9XB
9XB
9rB
9�B
9�B
9�B
9�B
:^B
:�B
:^B
:DB
:xB
:xB
:�B
:�B
;B
;JB
;B
<B
<�B
=VB
=�B
=�B
=�B
=�B
>B
>B
>(B
>�B
>�B
>�B
?.B
?�B
?cB
?}B
?�B
?�B
?}B
?�B
@iB
@OB
@�B
A B
@�B
@�B
@�B
AB
AUB
A�B
A�B
A�B
B[B
B�B
C-B
C-B
C�B
C�B
C�B
D�B
DMB
DgB
D�B
EB
EB
ESB
E�B
E�B
F�B
G_B
G�B
GzB
GEB
G�B
H1B
H�B
I�B
I�B
I�B
J#B
J�B
J�B
KDB
KDB
KDB
K�B
LdB
L~B
L~B
MB
MB
MB
M�B
N<B
NpB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
OBB
O�B
PHB
Q B
QB
QNB
Q�B
RoB
S&B
S&B
S[B
S@B
S�B
S�B
T,B
TB
TaB
T�B
T{B
TaB
T�B
T�B
T�B
U2B
U�B
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
W
B
W�B
X_B
X�B
X�B
YKB
YeB
YB
X�B
YKB
X�B
YKB
YKB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
ZkB
ZkB
Z�B
[WB
[WB
[�B
[�B
[�B
\)B
\]B
\]B
\�B
\�B
\�B
]B
]IB
]~B
]~B
]�B
^B
^B
^jB
^OB
^jB
^jB
^�B
^�B
_pB
_�B
`'B
`BB
`BB
`vB
`�B
aB
aB
a-B
aHB
a�B
a�B
a�B
a�B
bNB
b�B
c B
cnB
cTB
c:B
c�B
dZB
d�B
d�B
d�B
ezB
e�B
e�B
ffB
f�B
f�B
gB
g8B
gRB
g�B
h
B
hsB
iB
iDB
iDB
i�B
i�B
jeB
kB
kkB
k�B
k�B
k�B
k�B
lqB
lqB
lqB
l�B
mB
mCB
m]B
mwB
m�B
n/B
n}B
n�B
n�B
n�B
o B
oB
o�B
o�B
p;B
p;B
pUB
pUB
p�B
qB
qvB
q�B
qvB
q�B
q�B
q�B
q�B
r-B
r|B
r�B
s3B
sMB
s�B
s�B
t�B
t�B
uB
u?B
u?B
uZB
u�B
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wfB
wfB
w�B
xB
xB
x8B
xRB
x�B
y$B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zDB
zDB
zxB
z�B
{B
{dB
{dB
{dB
{�B
{�B
{�B
{�B
|B
|B
|PB
|�B
|jB
|�B
}"B
}<B
}qB
}�B
}�B
}�B
}�B
~BB
~wB
~�B
~�B
~�B
~�B
.B
}B
}B
�B
�B
�B
�B
�OB
�4B
�iB
��B
��B
��B
��B
��B
��B
� B
�UB
�UB
��B
�oB
�oB
��B
��B
��B
��B
�B
�AB
�uB
��B
��B
��B
�GB
�-B
�B
�{B
��B
�B
��B
��B
��B
�9B
��B
��B
�%B
�B
�B
��B
�B
��B
��B
��B
��B
�B
�+B
�+B
��B
��B
��B
��B
��B
��B
�1B
�KB
��B
��B
��B
��B
�B
�B
�RB
�lB
�lB
��B
��B
��B
��B
��B
�	B
�#B
�XB
�XB
�=B
�=B
�rB
��B
��B
��B
�)B
�xB
��B
��B
��1111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B�B�cB�IB�cB�}B�cB�IB�cB�cB�}B�/B�/B�/B�IB�IB�}B��B��B�|B��G�O�G�O�G�O�G�O�G�O�B
B
# B
?B
PHB
y$B
��B
��B
��B
��B
��B
��B
q�B
;�B
%�B
JB
GB	�wB	�RB	��B	�B	�B	��B	̈́B	��B	��B	�_B	h�B	G�B	CaB	B[B	@�B	@�B	A�B	B[B	B'B	;dB	88B	,qB	LdB	S�B	g�B	e�B	_B	V�B	WYB	\�B	`�B	dB	h�B	mB	s�B	��B	��B	��B	��B	��B	��B	��B	�B

#B
sB
1vB
;B
5�B
IB
NVB
TB
ZQB
W?B
MjB
H�B
L0B
S&B
TFB
RoB
RoB
R�B
OvB
J�B
E�B
H�B
Q�B
W�B
V�B
X�B
Y�B
[�B
^�B
fB
q'B
q�B
poB
n�B
oB
p;B
oiB
ncB
lqB
j�B
l"B
s�B
ncB
h>B
hsB
l�B
o�B
r�B
q�B
p�B
oB
o�B
o�B
k�B
i�B
hXB
gRB
c�B
i*B
fB
c�B
_�B
\CB
\B
^OB
]�B
^B
]/B
X_B
T�B
S�B
TB
UgB
VB
VB
V9B
W�B
W�B
TB
SuB
T�B
R�B
Q4B
R�B
S[B
S[B
VSB
WYB
V9B
T{B
SuB
SB
RoB
P}B
N�B
MB
J�B
G�B
E�B
EB
F�B
E9B
C�B
@iB
=�B
=B
=B
<�B
;0B
;�B
=�B
?�B
?HB
>(B
>wB
>BB
<�B
8�B
4�B
*�B
�B
�B
YB
�B
TB
uB
�B
B
.B
�B
�B
BB
�B
�B
�B
VB
�B
dB
�B
B
�B
�B
�B
�B
�B
�B
B
}B
B
hB
vB
VB
6B
�B
�B
�B
�B
�B
VB
PB

#B
+B
B
{B
�B
�B
�B
�B
fB
	RB
xB
B
�B
B
pB
�B
�B
B
�B
�B
�B
B

�B

XB
	lB
fB
�B
�B
�B
�B
-B
�B
�B
AB
B
 �B
 �B
UB
oB
;B
'B
�B
[B
'B
�B
 �B
 iB
aB
�B
�B
+B
�B
%B
�B
�B
�B
�B
AB
AB
�B
[B
�B
-B
�B
�B
MB
�B
�B
B
GB
�B
 �B
 �B	��B	��B	�(B	�B	��B	�]B	�BB	��B	�cB
 B	��B	��B	��B	�PB	�0B	��B	��B	�RB	��B	�lB	�B	�*B	�8B	��B	��B	��B	��B	��B	�	B	�$B	��B	��B	�B	��B	��B	��B	��B	��B	�DB	��B	�B	�JB	��B	��B	�6B	�PB	��B	�"B	�<B	�"B	�VB	��B	��B
  B
 �B
 �B
 �B
B
AB
�B
�B
�B
MB
B
%B
�B
�B
�B
�B
�B
YB
�B
_B
_B
�B
1B
fB
�B
	7B
	B
	B
	RB

�B
B
DB
B
^B
�B
�B
HB
HB
.B
B
HB
�B
�B
�B
�B
�B
�B
[B
uB
uB
�B
�B
aB
�B
{B
,B
@B
 B
�B
�B
�B
�B
�B
�B
,B
B
aB
{B
aB
2B
�B
�B
�B
�B
�B
�B
B
�B
sB
�B
�B
�B
EB
EB
�B
�B
?B
�B
�B
mB
sB
�B
YB
$B
1B
�B
B
7B
kB
kB
�B
�B
�B
#B
�B
�B
�B
�B
�B
B
�B
B
1B
�B
�B
�B
�B
�B
�B
kB
�B
�B
qB
�B
�B
�B
B
�B
CB
]B
�B
B
~B
�B
�B
�B
�B
pB
!B
;B
VB
VB
�B
�B
 'B
 vB
 �B
!B
!bB
!�B
"�B
"�B
#�B
#�B
$@B
%`B
%`B
%�B
&LB
&�B
&�B
(
B
(�B
)B
)�B
*eB
*�B
+6B
+�B
+�B
,B
,�B
,�B
-�B
-wB
-�B
-]B
-]B
-�B
-�B
.�B
/�B
0�B
0�B
0�B
1'B
1[B
1�B
1�B
2B
2-B
2aB
2|B
2|B
33B
3B
3MB
3�B
3hB
3�B
3�B
4B
4nB
49B
49B
4TB
49B
4nB
4�B
4�B
5%B
5tB
5�B
6�B
72B
7LB
7fB
7�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
9XB
9$B
9>B
9XB
9rB
9XB
9�B
:DB
:*B
:^B
:�B
:�B
:�B
:�B
:�B
;JB
;�B
;B
;�B
<6B
<�B
<�B
=B
=�B
=�B
=�B
>]B
>B
>BB
>�B
>�B
>�B
?HB
?�B
@ B
@�B
AUB
AoB
A;B
A;B
A�B
B'B
B�B
C�B
C�B
C�B
DB
D�B
D�B
EB
EB
EB
E�B
F%B
F?B
FtB
F�B
F�B
F�B
G�B
HB
HfB
H�B
H�B
H�B
H�B
HfB
HfB
H�B
H�B
H�B
IB
IlB
J=B
J�B
J�B
K)B
K�B
L0B
MB
MB
MB
MB
MjB
M�B
N"B
M�B
N<B
NpB
NVB
N<B
NpB
N�B
N�B
OB
O\B
OvB
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P}B
PHB
P�B
P�B
Q�B
R:B
R�B
R�B
SB
S&B
R�B
R�B
S&B
R�B
R�B
S&B
S@B
S[B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T�B
U2B
UB
U�B
U�B
U�B
U�B
VB
VB
V�B
V�B
V�B
V�B
W
B
W?B
W?B
W�B
W�B
W�B
X+B
XB
X+B
X+B
XyB
X�B
Y1B
Y�B
Y�B
ZB
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
[�B
\)B
\]B
\�B
]/B
\�B
]B
]�B
^B
^jB
^�B
^�B
_;B
_pB
_�B
`'B
`�B
`�B
`�B
aB
a-B
a|B
a�B
bhB
cB
b�B
c B
c�B
c�B
d@B
d�B
e,B
e`B
e`B
e�B
e�B
f2B
fLB
f2B
f�B
f�B
gB
gB
gRB
g�B
g�B
h>B
h>B
hsB
h�B
h�B
h�B
i*B
i�B
i�B
i�B
jB
jB
jKB
j�B
kB
kQB
kQB
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
mB
mwB
mwB
ncB
ncB
n�B
n�B
oB
oB
oOB
oiB
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
p�B
p�B
q'B
qAB
q[B
q�B
q�B
q�B
r-B
r�B
r�B
sB
sB
shB
sMB
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
t9B
t�B
t�B
uB
uB
u%B
utB
u�B
utB
u�B
u�B
u�B
vB
v`B
v+B
vzB
v�B
wB
w2B
wLB
w�B
w�B
w�B
xB
x8B
xRB
xlB
x�B
x�B
y	B
yXB
y>B
yrB
y�B
y�B
y�B
zB
y�B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{dB
{0B
{dB
{dB
{JB
{�B
{�B
|B
|6B
|�B
|�B
|�B
}B
|�B
}B
}<B
}�B
}�B
~�B
~�B
~�B
~�B
}B
cB
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
��B
��B
�UB
�UB
�oB
�oB
��B
��B
��B
�B
�[B
��B
��B
��B
��B
��B
��B
�-B
�B
�-B
�GB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
�9B
�mB
��B
��3311111111111111111111111144444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<zb:G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<j&�<?�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<SW�<>:<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.21(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9998(+-0.0000), deepest deltaS=-0.006(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-1500(dbar) is excluded in mapping;                                                                                                                                       Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202012170047402020121700474020201217004740202012170200422020121702004220201217020042202207271540422022072715404220220727154042  JA  ARFMdecpA30a                                                                20201205123953  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201205123958  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201205123959  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201205123959  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201205123959  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20201205123959  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20201205123959  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201205123959                      G�O�G�O�G�O�                JA  ARUP                                                                        20201205125159                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201216154740  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201216154740  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20201216170042  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  PSAL_ADJUSTED_QC@5Bx
=G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  TEMP_ADJUSTED_QCBZ
=Bx
=G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727064042  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091506                      G�O�G�O�G�O�                