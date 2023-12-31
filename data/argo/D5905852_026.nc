CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-20T09:38:47Z creation;2019-10-20T09:38:49Z conversion to V3.1;2022-08-02T05:11:56Z update;     
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
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191020093847  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_026                    2C  D   APEX                            8420                            2.11.2                          846 @��x�� 1   @��y6��@/�]c�e��c�vȴ9X1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B���B���B�  B�  B�  B���B�  B�  B�ffB�  B���B�  B�33B�33BΙ�B�  B�  B�  B�ffB�33B���B왚BB�ffB�  B�  C   C  C  C  C  C
  C  C  C  C  CffC��C  C  C  C  C   C!�fC$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�@�Q�@���A (�A ��AAp�A_�
A�A�  A�(�A��A�{A�  A�Q�A�Q�B   B
=B
=B�B��B(  B0�B8�B@{BH{BP
=BX�B`33Bh{Bp�Bx�B�
=B�  B�B�\B��
B���B��
B��qB�B��B�\B���B���B��B�z�B�
=B��)B���B�#�B�.BΔ{B�
=B�{B��B�\)B�L�B��)B왚BB� B��B���B���C�qC  C  CC
�C�C�qCC�Cp�C�
C�RC  C�C�C �C!�C#��C%�3C(  C*C,  C.�C0�C2  C3�qC6�C8�C:�C<  C>�C@  CA�qCD  CFCH  CI�qCL�CN�CP)CRCT  CV  CX�CZ
=C\C^  C_�qCa�qCd�Cf  Ch  Cj  Ck�qCm��Cp  Cr�Ct�Cv�Cx�Cz�C{�qC~�C�HC�  C��C�C�HC�  C��C�fC��C��C��C�  C���C�HC�  C�HC�fC�HC��)C��)C�  C�C�C��C�  C�  C�  C��C��C�
=C�HC�HC��C�HC�HC�  C�HC�C��C�  C���C��qC��qC�  C��qC�HC�HC�  C��C�HC�HC��C��C�C��C���C�  C��C�  C��qC��qC���C��qC�  C��C��C�C�C�  C���C���C�C�HC�  C�HC�  C�HC��C�  C��qC��C��C��C��C��C�fC�  C��)C���C���C�HC�HC��C�C��C�  C��C�HC�  C�C�HC��)C���C�  C��C�  C�HC�C���C���C�C��C��C�fC��C�  C�  C���C�  C�C�HC�HC�HC�HC�  C�  C�HC�HD �D ��D  D��DHD��D�D��D�D\D�qD~DHD��D�\D\D�\D��D	 �D	�HD
 �D
\D  D� D  D�HDHD��DHD� D  D� DHD\D��D��D�D��D�D��D �D��D  D� D�\D\D3D�3D �D��D�D� D�\D\DHD�HD �D�HD �D�HD�D� D�\D\D �D ��D!HD!��D"�D"��D#HD#��D$�D$� D$�\D%~�D%��D&� D' �D'��D(�D(�3D) �D)�HD*HD*� D+ �D+\D+�\D,�HD-�D-�HD.HD.��D/ �D/�HD03D0��D13D1��D1��D2� D3�D3��D4�D4��D5HD5��D6  D6\D7�D7�HD8  D8��D9HD9\D:�D:��D; �D;��D< �D<~�D<�\D=� D>  D>� D? �D?��D@HD@�HDAHDA��DB�DB��DCHDC�HDDHDD\DD�qDE~�DF �DF\DF��DG��DH �DH\DI �DI~�DJ �DJ�3DKHDK�HDL3DL�HDM �DM��DNHDN��DOHDO� DP  DP��DQHDQ��DRHDR�HDSHDS�HDT �DT\DT�\DU� DU��DV��DWHDW��DX  DX� DYHDY��DZHDZ�HD[�D[��D\  D\��D] �D]�HD^HD^�HD_ �D_�HD`�D`��Da �Da�HDb�Db��DcHDc��Dd  Dd~Dd�\De� De��Df~�Df��Dg��Dh�Dh�HDi �Di� Dj  Dj�HDkHDk��Dl�Dl� Dl�\Dm� Dm�\Dn\Do �Do��Do��Dp� DqHDq��Dq�Dr\Ds �Ds� Dt  Dt��Du�Du��Du�\Dv�HDwHDw~Dw��Dx\Dy �Dy��Dz�Dz~�D{  D{��D{��D|��D}�D}�3D~  D~}qDHD��D�=D�B�D���D���D� �D�@�D���D��RD�  D�@RD���D��RD� RD�@�D���D���D��D�@�D���D��RD� �D�AHD��RD���D� �D�@�D���D���D� �D�@ D�\D�� D�HD�@RD��RD��RD� RD�@RD�� D���D��D�AHD��HD���D� RD�@�D�� D�� D� �D�@�D��RD��HD�HD�@�D��RD��RD� �D�AHD���D���D���D�@ D��RD���D��\D�?�D��RD�� D� RD�@ D�� D���D��\D�@RD���D��HD�  D�?\D��D���D�HD�@�D���D���D� �D�@RD��RD���D� �D�@�D���D���D�  D�?\D�� D�� D��\D�@ D��RD��RD�  D�@�D���D��HD� �D�?�D�� D��RD� RD�@ D��RD�� D� RD�@RD��RD���D���D�?\D���D�� D�  D�A�D���D���D���D�@�D�� D���D��\D�?�D�� D���D� �D�@RD��RD���D� �D�@RD�\D�� D� RD�@ D��D���D� �D�@ D��D�� D���D�@ D��RD�� D� �D�?�D�� D���D��\D�@RD�� D���D���D�?�D��RD���D��\D�@RD�� D���D��\D�@RD���D�� D��\D�?\D�\D�� D���D�?\D��RD���D��D�B=D���D���D� �D�@RD�� D���D� RD�@�D��RD���D�  D�@RD��RD��HD� RD�?\D�\D�� D� RD�@RD���D��HD� �D�?�D��RD���D� RD�@�D���D�� D� RD�@ D��RD���D� �D�@�D��D��RD�HD�@�D��RD���D� �D�AHD���D��RD�  D�@�D�� D��
D���D�@RD��RD���D�  D�?�D���D���D� RD�@RD�\D��\D��\D�@ D��RD���D��\D�?\D�� D���D�  D�?\D�~�D�� D�HD�@ D�~�D��\D��\D�@ D���D���D��
D�@ D D¿�D� �D�A�DÀ�D�� D�  D�@�DĀ�D���D���D�?�DŀRD���D� RD�@ DƀRD���D� RD�@�Dǀ D�� D� �D�@�DȀRDȿ�D�  D�?�D��D�� D���D�?\D��D��RD�HD�AHDˀRD��RD� �D�@�D́�D��HD�HD�@ D��D��RD� �D�@�D΀ D�� D���D�?�D��DϿ�D�  D�@�DЀ Dп�D� �D�@�D��D��RD�  D�@ DҀ�D���D� �D�@ DӀ D��RD� �D�@�DԁHD���D� �D�@�DՀ�D���D���D�@RDցHD���D� �D�@�D׀RD���D� RD�@ D؀ Dؿ\D��\D�@�DفHD���D�  D�@ Dڀ D���D� �D�A�Dۀ�Dۿ�D� RD�AHD܁�D��HD��D�@�D݀RDݿ\D���D�@ DހRD��RD���D�@ D߀�D�� D� RD�@�D���D���D�  D�@ D� D΅D��
D�@ D��D�\D���D�@�D�HD���D� �D�@�D��D��RD� �D�@RD�\D��RD��D�@�D怤D�� D� �D�AHD�RD��RD� �D�@�D�RD�� D� RD�@ D�RD�� D���D�?\D�
D�� D� �D�@RD�RD뿮D��\D�@�D�RD���D�HD�@�D��D��HD� �D�@RD� D�
D���D�@�DD���D� RD�?\D�\D�� D� �D�@ D� D�� D� �D�A�D�D�� D� RD�@�D� D�
D�  D�A�D�HD���D� RD�@RD���D���D�HD�?\D�\D���D� �D�@�D���D���D��D�@RD���D��=D��D�AHD���D���D��D�@RD�
D���D� RD�7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ҽA��A߸�A߻0A߷�A߷A߱'A߰UA߰�A߳�A߱�Aߩ_Aߦ�AߨXAߩ�A߯�A߶�A߶�Aߴ�A��?A��QA�8�A�RTA�N<A�A�A�;�A�-�A���A��A���A��+A��A�T�A�XyA݊rA�g�A�A�T�Aي�A؟VA�1�A��A���A͎VA��A�;0Aǲ�A�hA��YA�)�A�+A���A�T,A�_�A���A��A�/OA�1�A���A�1A��uA���A�l�A���A���A���A��A�-�A~��A}OAz�EAuOAp��An8�AiL0Ae��A`�A``BA^=qA\HAU��AR`�AM�AK+kAJK^AG��AEo�AA|�A>��A<�A9�KA9`BA8��A8!A7^�A5�A4��A3��A21A2_A0��A/sA-�MA,�[A+:*A*GEA)�fA&�A&(�A#�wA"��A"�A"FtA!��A!�A ��A�#AzxAoA�Ah�A6zA�)A��Av`Aa|AJ�A�A�A�A��A��AA�AخA�"A}�A4A�A�MA��A�8AtTAI�A)_A�A��A�9A��A�A��A��A\�A��A8�A#�A�A~�A�Ax�A�,AR�A!A1A�Ar�AN<AOA�PAg8A?�A.�AA��A��A��A$�Al�A
�A
qvA
 �A	��A	o�A	($A��A��AM�A�zA�Az�A�HA�1A��A1�A�&A�FA��A��AA��AI�A��A��AcA7�A ںA ��A �@�s@�Z@�ƨ@�Ta@��W@���@�,�@�s�@��$@��@���@��B@��.@�-�@��@���@���@�b@�;d@��@�@��@�j@��@��v@��@�;d@미@��E@�c @��@�J@��	@�\�@���@�@��@�Y@�`B@�@�S�@��@��@�Dg@�h
@���@ݨX@�q@܃@�b@۔�@��@ښ@�Z@��@�O�@�w�@�p�@���@�l"@�!@պ^@�%F@�ݘ@��@���@�[�@�8�@��@�u@�	@��>@��;@�خ@ѷ@ч�@�U�@�(�@��@��M@��/@��'@�^5@�|�@�'�@���@��@��'@�d�@��g@�4@��@���@�ff@��@ˀ4@�Q�@�q@��H@�*�@ɷ�@ɑh@�9�@ȷ�@�~@���@ǜ@�j�@Ʊ�@�5?@�rG@�4@�s�@û0@�@½<@�d�@�5?@�u@�{J@��@�kQ@�[�@�:�@��@�˒@���@�a�@��@���@�]d@��@���@�g�@�@@��@��2@�Ɇ@���@���@�-@��Q@���@��4@�Y�@�A @��	@���@��@�0U@���@��V@�dZ@�&�@��@��4@�u%@�"h@��3@��P@�J#@��@�g8@���@�B�@��@�ں@�e�@�b@���@�e�@�ߤ@�!�@��W@���@�� @�U�@���@�~(@�Q�@��r@���@�c�@�Y@���@�Q@��@��@��K@��V@�x@���@���@��@�>B@��]@��&@��X@�\�@�&�@���@�z@�_@���@��a@��$@��M@�A @���@��@��@��S@�@�`�@��m@���@�e,@�>�@�%@��@���@��u@��@�e�@�*�@��@���@��[@��$@�S@�w�@�;�@�o @�=@�!-@��@��@�-@��3@���@�X@�9�@�'�@��P@�ں@��@�C-@��@��&@��n@�[W@� \@���@�V@�*�@��@��7@�6z@���@��_@�GE@�!@� �@���@�<6@� i@�ی@���@��@�^�@�9�@�Y@���@��$@���@���@�c @�\�@�R�@�;�@��V@�N<@���@���@��m@���@���@�v�@�W�@�0U@�1@��[@�c�@�0�@�)_@���@��<@��@���@�s�@�C-@��@���@�~�@�`B@�Dg@�0�@��@��@���@���@�c�@�$@��A@�خ@��k@�dZ@�B�@�%F@��U@�p;@�?�@�!�@��t@�dZ@��@���@���@�h
@�K^@�<�@�e@���@�33@�Y@���@��Y@�E�@�b@���@�m]@�<6@���@��A@�d�@�=q@��@�{@��]@��z@��@�]�@�Dg@�&�@��2@�ȴ@��e@�z@�^5@�3�@���@��@���@�j@�?}@�.I@�@���@���@��@�q�@�Q�@��@��@��@��q@�4�@�
=@�	l@� i@���@�M�@�;�@�M@���@���@��:@�x�@�t�@�s@�j@�33@��@��!@�h
@�=q@��@�6@33@@~�H@~~�@~B[@}�@|q@{��@{�@{�P@{W?@{@O@z�@z��@z�1@z@�@yJ�@x֡@x-�@w�Q@w�g@w��@w��@wY@v��@vJ@u��@u��@uhs@u&�@t�$@t�@t`�@t  @rߤ@r_�@rJ@q��@q�~@qL�@p�9@pK^@p�@oj�@n�2@nYK@n{@m��@mQ�@l��@lb@k��@kƨ@k��@k��@kU�@k!-@j��@j�r@jq�@jv�@i�@i��@i��@i�=@i��@iQ�@h�v@h-�@g��@gC�@f�+@fe@e:�@d��@d�@co�@b͟@b{@a�T@a|@af�@a@`֡@`4n@_��@_@^B[@]�t@]��@]5�@\�$@\Z@\<�@[�&@[{J@Z��@Z0U@Y�T@Y�@X�|@Xoi@W��@W]�@W(@V͟@V�F@V-@U�#@U�~@T�P@S�@R��@Rp;@Q��@Q��@QY�@P�@O�@O�K@O�[@O�@OU�@N�B@Na|@N1�@N4@M�^@MO�@Lj@K��@KE9@K�@J��@I��@I�M@H��@H�@G�+@G�@G]�@F�@FM�@Fe@ET�@D�P@D�)@DbN@C�r@C\)@B�F@BZ�@B@�@A��@A��@A/@@�@@�4@@V�@@G@?��@>�@>v�@>8�@=�.@=�@=��@=^�@<�@<@;y�@;�@:��@:	@9�^@9�'@9w2@8ѷ@8w�@8m�@8bN@8'R@7��@7!-@6�]@6�+@6H�@6�@5��@5�N@5��@5`B@5@5*0@5�@4�@4��@4�_@4h�@4H@4'R@3�
@3��@3v`@3'�@2�@2W�@1��@1rG@1/@0�P@0ѷ@0~(@02�@0 �@/��@/��@/�F@/�f@/J#@.�@.�F@.^5@.W�@..�@-�H@-u�@-Vm@-=�@-�@,��@,֡@,�@+��@+�6@+��@+P�@+1�@+$t@+o@+�@+ i@*�8@*�<@*�1@*R�@*�@)��@)��@)`B@)A @(�K@(�@(�@(m�@(6@'�&@'��@'�@'��@'��@'��@'~�@'!-@&�s@&�}@&�A@&GE@&�@%��@%�@%��@%�n@%�=@%��@%k�@%k�@%Y�@%B�@$�5@$��@$r�@$,=@$7@$�@#�r@#��@#.I@"�@"�h@"�b@"��@"Ta@".�@"_@!��@!��@!�@ ѷ@ ��@ �@ N�@��@ƨ@�4@��@��@=q@�h@Vm@�@��@��@`�@S�@H@*�@�@�@��@��@i�@d�@_�@($@	@��@�3@�@�-@�X@��@�=@�h@�@X@@@�/@��@>B@�@�@�V@l�@E9@=@6z@+@�@�s@��@z@8�@8�@$�@4@�@�'@�@j@<6@�@�f@Ɇ@��@~(@�o@�o@q@6@M@�&@�q@e�@+@�c@��@��@҉@��@�<@��@Q@&�@4@��@�d@�@N<@4@!�@�@w�@M@��@�
@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ҽA��A߸�A߻0A߷�A߷A߱'A߰UA߰�A߳�A߱�Aߩ_Aߦ�AߨXAߩ�A߯�A߶�A߶�Aߴ�A��?A��QA�8�A�RTA�N<A�A�A�;�A�-�A���A��A���A��+A��A�T�A�XyA݊rA�g�A�A�T�Aي�A؟VA�1�A��A���A͎VA��A�;0Aǲ�A�hA��YA�)�A�+A���A�T,A�_�A���A��A�/OA�1�A���A�1A��uA���A�l�A���A���A���A��A�-�A~��A}OAz�EAuOAp��An8�AiL0Ae��A`�A``BA^=qA\HAU��AR`�AM�AK+kAJK^AG��AEo�AA|�A>��A<�A9�KA9`BA8��A8!A7^�A5�A4��A3��A21A2_A0��A/sA-�MA,�[A+:*A*GEA)�fA&�A&(�A#�wA"��A"�A"FtA!��A!�A ��A�#AzxAoA�Ah�A6zA�)A��Av`Aa|AJ�A�A�A�A��A��AA�AخA�"A}�A4A�A�MA��A�8AtTAI�A)_A�A��A�9A��A�A��A��A\�A��A8�A#�A�A~�A�Ax�A�,AR�A!A1A�Ar�AN<AOA�PAg8A?�A.�AA��A��A��A$�Al�A
�A
qvA
 �A	��A	o�A	($A��A��AM�A�zA�Az�A�HA�1A��A1�A�&A�FA��A��AA��AI�A��A��AcA7�A ںA ��A �@�s@�Z@�ƨ@�Ta@��W@���@�,�@�s�@��$@��@���@��B@��.@�-�@��@���@���@�b@�;d@��@�@��@�j@��@��v@��@�;d@미@��E@�c @��@�J@��	@�\�@���@�@��@�Y@�`B@�@�S�@��@��@�Dg@�h
@���@ݨX@�q@܃@�b@۔�@��@ښ@�Z@��@�O�@�w�@�p�@���@�l"@�!@պ^@�%F@�ݘ@��@���@�[�@�8�@��@�u@�	@��>@��;@�خ@ѷ@ч�@�U�@�(�@��@��M@��/@��'@�^5@�|�@�'�@���@��@��'@�d�@��g@�4@��@���@�ff@��@ˀ4@�Q�@�q@��H@�*�@ɷ�@ɑh@�9�@ȷ�@�~@���@ǜ@�j�@Ʊ�@�5?@�rG@�4@�s�@û0@�@½<@�d�@�5?@�u@�{J@��@�kQ@�[�@�:�@��@�˒@���@�a�@��@���@�]d@��@���@�g�@�@@��@��2@�Ɇ@���@���@�-@��Q@���@��4@�Y�@�A @��	@���@��@�0U@���@��V@�dZ@�&�@��@��4@�u%@�"h@��3@��P@�J#@��@�g8@���@�B�@��@�ں@�e�@�b@���@�e�@�ߤ@�!�@��W@���@�� @�U�@���@�~(@�Q�@��r@���@�c�@�Y@���@�Q@��@��@��K@��V@�x@���@���@��@�>B@��]@��&@��X@�\�@�&�@���@�z@�_@���@��a@��$@��M@�A @���@��@��@��S@�@�`�@��m@���@�e,@�>�@�%@��@���@��u@��@�e�@�*�@��@���@��[@��$@�S@�w�@�;�@�o @�=@�!-@��@��@�-@��3@���@�X@�9�@�'�@��P@�ں@��@�C-@��@��&@��n@�[W@� \@���@�V@�*�@��@��7@�6z@���@��_@�GE@�!@� �@���@�<6@� i@�ی@���@��@�^�@�9�@�Y@���@��$@���@���@�c @�\�@�R�@�;�@��V@�N<@���@���@��m@���@���@�v�@�W�@�0U@�1@��[@�c�@�0�@�)_@���@��<@��@���@�s�@�C-@��@���@�~�@�`B@�Dg@�0�@��@��@���@���@�c�@�$@��A@�خ@��k@�dZ@�B�@�%F@��U@�p;@�?�@�!�@��t@�dZ@��@���@���@�h
@�K^@�<�@�e@���@�33@�Y@���@��Y@�E�@�b@���@�m]@�<6@���@��A@�d�@�=q@��@�{@��]@��z@��@�]�@�Dg@�&�@��2@�ȴ@��e@�z@�^5@�3�@���@��@���@�j@�?}@�.I@�@���@���@��@�q�@�Q�@��@��@��@��q@�4�@�
=@�	l@� i@���@�M�@�;�@�M@���@���@��:@�x�@�t�@�s@�j@�33@��@��!@�h
@�=q@��@�6@33@@~�H@~~�@~B[@}�@|q@{��@{�@{�P@{W?@{@O@z�@z��@z�1@z@�@yJ�@x֡@x-�@w�Q@w�g@w��@w��@wY@v��@vJ@u��@u��@uhs@u&�@t�$@t�@t`�@t  @rߤ@r_�@rJ@q��@q�~@qL�@p�9@pK^@p�@oj�@n�2@nYK@n{@m��@mQ�@l��@lb@k��@kƨ@k��@k��@kU�@k!-@j��@j�r@jq�@jv�@i�@i��@i��@i�=@i��@iQ�@h�v@h-�@g��@gC�@f�+@fe@e:�@d��@d�@co�@b͟@b{@a�T@a|@af�@a@`֡@`4n@_��@_@^B[@]�t@]��@]5�@\�$@\Z@\<�@[�&@[{J@Z��@Z0U@Y�T@Y�@X�|@Xoi@W��@W]�@W(@V͟@V�F@V-@U�#@U�~@T�P@S�@R��@Rp;@Q��@Q��@QY�@P�@O�@O�K@O�[@O�@OU�@N�B@Na|@N1�@N4@M�^@MO�@Lj@K��@KE9@K�@J��@I��@I�M@H��@H�@G�+@G�@G]�@F�@FM�@Fe@ET�@D�P@D�)@DbN@C�r@C\)@B�F@BZ�@B@�@A��@A��@A/@@�@@�4@@V�@@G@?��@>�@>v�@>8�@=�.@=�@=��@=^�@<�@<@;y�@;�@:��@:	@9�^@9�'@9w2@8ѷ@8w�@8m�@8bN@8'R@7��@7!-@6�]@6�+@6H�@6�@5��@5�N@5��@5`B@5@5*0@5�@4�@4��@4�_@4h�@4H@4'R@3�
@3��@3v`@3'�@2�@2W�@1��@1rG@1/@0�P@0ѷ@0~(@02�@0 �@/��@/��@/�F@/�f@/J#@.�@.�F@.^5@.W�@..�@-�H@-u�@-Vm@-=�@-�@,��@,֡@,�@+��@+�6@+��@+P�@+1�@+$t@+o@+�@+ i@*�8@*�<@*�1@*R�@*�@)��@)��@)`B@)A @(�K@(�@(�@(m�@(6@'�&@'��@'�@'��@'��@'��@'~�@'!-@&�s@&�}@&�A@&GE@&�@%��@%�@%��@%�n@%�=@%��@%k�@%k�@%Y�@%B�@$�5@$��@$r�@$,=@$7@$�@#�r@#��@#.I@"�@"�h@"�b@"��@"Ta@".�@"_@!��@!��@!�@ ѷ@ ��@ �@ N�@��@ƨ@�4@��@��@=q@�h@Vm@�@��@��@`�@S�@H@*�@�@�@��@��@i�@d�@_�@($@	@��@�3@�@�-@�X@��@�=@�h@�@X@@@�/@��@>B@�@�@�V@l�@E9@=@6z@+@�@�s@��@z@8�@8�@$�@4@�@�'@�@j@<6@�@�f@Ɇ@��@~(@�o@�o@q@6@M@�&@�q@e�@+@�c@��@��@҉@��@�<@��@Q@&�@4@��@�d@�@N<@4@!�@�@w�@M@��@�
@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�?B��B��B�tB��B�tB��B��B��B��B�+B��B�<B��B��B	hB	;B	6�B	S�B	m�B	y>B	�9B	��B	�6B	�B	�YB	��B	�EB	�-B	�'B	�B	��B	�-B	�+B	��B	�)B	��B	�?B	�bB	��B	}VB	|�B	o�B	i�B	��B	��B	�B	�LB	��B	��B	�B	��B	�4B	�&B	�B	��B	�>B	��B	��B	��B	� B	�yB
^B
B
 �B	�B	�BB	��B	��B	�}B	��B	��B	��B	u�B	e�B	O(B	<PB	#�B	�B	�B	�B�B�:B�gB��B�B��B�0B��B�}B�]B�B�RB��B�
B��B�B�)B��B��B�iB��B�PB��B�B�	B�_B�B��B��B�/B��B�>B	dB	�B	�B	B	QB	/B	/B	7�B	BB	C{B	D�B	H�B	MB	N<B	PHB	UB	[�B	_B	aB	bNB	e�B	h>B	jB	q'B	s�B	v`B	yrB	zB	�iB	��B	�?B	�XB	��B	��B	��B	�B	�B	��B	�nB	�>B	��B	�gB	ŢB	��B	�?B	�B	��B	�$B	�^B	�B	��B	�B	��B	�]B	��B	�tB	��B	�B	�B	ŢB	�B	�?B	�?B	�%B	�B	�tB	�B	��B	ƨB	ƨB	��B	��B	��B	�zB	��B	�B	ɺB	�lB	��B	�XB	�XB	˒B	�~B	�B	��B	��B	�PB	��B	̈́B	��B	��B	��B	�BB	��B	�bB	��B	� B	�B	�hB	҉B	��B	�:B	� B	ңB	�&B	�B	��B	�,B	�,B	�aB	�aB	��B	�B	��B	��B	ԕB	ԕB	�MB	�B	�,B	�&B	ӏB	�&B	�FB	�B	�,B	ӏB	��B	�B	ѝB	�NB	��B	�&B	�[B	�B	�FB	յB	ևB	��B	��B	�B	�FB	�,B	�B	ּB	�EB	�B	��B	�B	��B	�B	چB	ںB	�B	�1B	��B	��B	�?B	�B	��B	�9B	רB	�#B	��B	��B	�B	�B	��B	�VB	�pB	�pB	��B	�B	�B	��B	�-B	�B	�zB	�B	�B	��B	�B	��B	�_B	�yB	�eB	�KB	�B	�B	�B	�eB	��B	�B	�B	��B	�kB	�B	�B	�B	�B	��B	�qB	�wB	�IB	�B	��B	��B	�wB	��B	�B	�oB	��B	�B	�B	�[B	��B	��B	�B	�B	�B	�3B	�B	�TB	��B	��B	��B	��B	��B	��B	��B	�RB	��B	�XB	��B	��B	��B	�DB	��B	�JB	��B	�B	�B	�6B	�PB	�6B	��B	��B	��B	��B	�"B	��B	�B	�(B	��B	��B	��B	��B	�.B	��B
 �B
 B
 �B
;B
�B
�B
B
'B
B
MB
3B
B
�B
�B
9B
SB
mB
�B
YB
?B
�B
zB
�B
KB
KB
KB
fB
fB
	lB
	lB
	RB
	�B
	�B
	�B

#B

=B

=B

�B

rB

�B
�B
�B
�B
�B
�B
�B
JB
dB
�B
�B
�B
\B
�B
bB
�B
B
4B
�B
�B
�B
�B
B
TB
�B
oB
 B
�B
B
�B
�B
[B
�B
�B
�B
TB
B
:B
 B
�B
&B
,B
{B
2B
�B
SB
mB
�B
$B
YB
�B
�B
�B
�B
�B
+B
B
B
B
�B
B
B
�B
�B
�B
B
�B
�B
�B
�B
B
B
)B
CB
]B
]B
)B
�B
IB
/B
~B
�B
�B
�B
B
B
jB
�B
�B
�B
 B
 B
�B
 vB
 �B
 vB
 vB
 �B
 �B
 �B
!|B
!�B
!�B
"4B
"4B
"�B
"�B
#nB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$@B
$�B
$�B
%zB
%�B
&2B
&fB
'B
'mB
'�B
'�B
(
B
(>B
)_B
)yB
)_B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
,=B
,�B
,�B
,�B
,�B
-B
-CB
-�B
.B
./B
.}B
.�B
.cB
.�B
.�B
/B
/�B
0�B
1B
0�B
0�B
0�B
1'B
1B
1[B
1AB
1vB
1�B
1�B
2B
2|B
2�B
2�B
2�B
4TB
4nB
4nB
4TB
5%B
5ZB
5tB
5�B
5�B
5�B
6B
5�B
5�B
5�B
5�B
5�B
6`B
6�B
6�B
6�B
6�B
6`B
6FB
6B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6+B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7LB
7LB
72B
7LB
7fB
7�B
8B
7�B
7�B
7�B
7�B
8RB
8RB
8lB
8lB
9rB
9rB
9�B
9�B
:B
:B
:^B
:DB
:*B
:DB
:�B
;dB
;JB
;dB
;JB
;�B
<PB
<jB
<PB
<�B
<jB
<�B
<jB
<�B
=B
=�B
>]B
>�B
?.B
?HB
?HB
?cB
?HB
?�B
?�B
?�B
?�B
?�B
@OB
AB
AoB
A�B
B[B
B�B
C{B
CaB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
FB
FtB
F�B
F�B
F�B
F�B
G�B
HB
G�B
G�B
HfB
H�B
I�B
I�B
I�B
I�B
J=B
JrB
J�B
J�B
J�B
K�B
L0B
LdB
L�B
MB
L�B
M�B
NB
N"B
N<B
N"B
NpB
N�B
OBB
OvB
O�B
O�B
O�B
P�B
QB
QhB
QNB
Q�B
Q�B
Q�B
Q�B
R:B
RTB
R�B
R�B
S[B
T,B
S�B
S�B
TB
S�B
TFB
T�B
U2B
VB
U�B
U�B
VSB
VSB
VmB
VB
VSB
V�B
V�B
V�B
W?B
W�B
W�B
W�B
W�B
W�B
X_B
YB
Y�B
ZB
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
\B
\CB
\CB
\]B
\xB
\�B
]/B
]/B
]/B
]IB
]IB
]~B
]�B
^B
^jB
^�B
^�B
^�B
_B
^�B
_B
_�B
_;B
_VB
_pB
_�B
`BB
`�B
`�B
`�B
aB
a-B
abB
a|B
a�B
b4B
bNB
bNB
bNB
b�B
b�B
cB
c B
c:B
cTB
cTB
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e`B
e�B
e�B
fB
e�B
fB
ffB
f�B
f�B
f�B
gmB
g�B
h$B
h$B
h$B
h$B
h$B
h$B
h�B
h�B
h�B
i�B
jKB
jeB
jB
jB
j�B
kB
k6B
kQB
kkB
kkB
kQB
kkB
k�B
lB
lWB
l�B
l�B
l�B
l�B
mB
mwB
m�B
n/B
n/B
nIB
n�B
n�B
n�B
oOB
o5B
o�B
o�B
o�B
o�B
p;B
pUB
pUB
poB
p�B
qB
q[B
q�B
q�B
q�B
q�B
rB
rB
rB
rB
rB
rB
q�B
rGB
rGB
r|B
raB
rGB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
s�B
s�B
s�B
s�B
tB
tnB
t�B
t�B
t�B
t�B
t�B
u?B
uZB
utB
utB
u?B
utB
u�B
u�B
u�B
vB
v+B
v`B
vzB
vzB
vzB
v�B
w2B
wB
w2B
wLB
w�B
w�B
w�B
xB
xlB
x�B
x�B
y	B
y$B
y	B
y	B
y$B
yXB
y�B
zB
zB
z*B
z^B
z�B
z�B
z�B
z�B
{0B
{�B
|B
|6B
|PB
|jB
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�ZB�%B�%B��B��B��B��B��B��B�B�FB��B�<B��B	  B	hB	VB	7B	S�B	m�B	x�B	�B	��B	�6B	�'B	�tB	��B	ǔB	�GB	�AB	�AB	�B	ĜB	��B	�]B	��B	�=B	�KB	�uB	��B	cB	��B	v�B	s�B	��B	��B	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	ȀB	�wB	��B	�mB	��B
<B
�B
?B	�zB	�&B	�B	�B	��B	��B	��B	�YB	{B	kkB	S�B	@�B	%FB	 �B	�B	�B��B�XB�+B��B�lBňB��B�B�'B�UB�
B�>B��B�_B��B�}B��B��B�yB�AB�rB�BBԕB��B�xB�B�CB׍B�_B�iB�2B��B	PB	�B	�B	�B	�B	�B	/OB	8�B	B[B	C�B	E9B	H�B	M6B	NpB	P�B	U�B	\�B	_pB	a|B	b�B	e�B	h�B	kB	rGB	tB	vzB	y�B	z*B	��B	�7B	��B	��B	��B	�!B	��B	��B	��B	�B	��B	��B	��B	ĶB	��B	�tB	�B	�mB	�HB	��B	��B	�dB	��B	��B	��B	��B	�B	��B	�mB	�SB	�SB	��B	�?B	ƎB	ƎB	��B	��B	�+B	ƨB	ƎB	�B	�B	�+B	�EB	�1B	�B	ȚB	�B	ʌB	�=B	��B	ʦB	��B	�B	�B	��B	�VB	ΊB	��B	̈́B	��B	�"B	�VB	�BB	��B	�bB	� B	уB	ѷB	ѝB	�:B	��B	�:B	ңB	ҽB	�@B	өB	��B	�2B	�aB	�{B	ԯB	ԯB	�aB	ԯB	ՁB	�MB	��B	�MB	��B	յB	��B	��B	�FB	�B	��B	ԕB	��B	ԕB	ӏB	ѝB	�B	��B	�@B	ӏB	�B	ԕB	ԕB	�9B	�?B	��B	�gB	�{B	�{B	ԕB	ՁB	�$B	خB	�B	�7B	�QB	�7B	�kB	�#B	�WB	چB	�B	�1B	�EB	רB	��B	�SB	�mB	��B	�=B	�B	�B	�B	�5B	��B	�pB	ߊB	ߤB	�B	�'B	�'B	�B	�HB	�:B	��B	�
B	��B	��B	�B	�B	�B	��B	��B	�eB	��B	��B	��B	�B	�B	�6B	�QB	�QB	�B	�B	��B	�B	�"B	�=B	�B	��B	��B	�}B	�cB	��B	��B	�cB	��B	�B	�B	�AB	��B	�B	�GB	�3B	�-B	��B	��B	�MB	��B	�B	�B	��B	�FB	�2B	��B	��B	�B	�RB	�	B	�rB	��B	��B	�*B	�xB	�B	�JB	��B	�6B	�B	�PB	��B	�jB	�B	��B	��B	��B	�VB	�B	�BB	�wB	��B	��B	��B	�HB	��B
 4B
 �B
UB
 B
�B
�B
B
[B
�B
{B
gB
3B
MB
gB
�B
mB
�B
�B
%B
�B
�B
B
�B
B
fB
fB
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B

XB

rB

�B

�B

�B
B
�B
�B
�B
�B
B
JB
~B
�B
B
B
(B
�B
�B
�B
�B
4B
NB
�B
�B
�B
B
:B
oB
�B
�B
�B
:B
[B
@B
�B
uB
B
,B
B
�B
:B
TB
:B
�B
&B
,B
�B
�B
B
�B
�B
$B
YB
�B
B
�B
�B
�B
EB
yB
KB
KB
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
)B
B
CB
]B
xB
]B
]B
CB
~B
dB
�B
�B
�B
�B
B
5B
jB
�B
!B
�B
 'B
 'B
 'B
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
"NB
"NB
"�B
#B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$&B
$tB
$�B
%,B
%�B
&B
&fB
&�B
'B
'�B
'�B
(
B
(>B
(�B
)�B
)�B
)�B
)�B
*B
*0B
+B
+�B
+�B
,B
,qB
-B
,�B
,�B
-B
-)B
-�B
-�B
./B
.IB
.�B
.�B
.}B
.�B
.�B
/B
/�B
0�B
1'B
0�B
0�B
0�B
1AB
1'B
1vB
1[B
1�B
1�B
1�B
2GB
2�B
2�B
33B
33B
4TB
4�B
4�B
4�B
5ZB
5tB
5�B
6B
6B
5�B
6+B
5�B
5�B
6B
5�B
6+B
6�B
6�B
6�B
6�B
6�B
6�B
6`B
6+B
6B
5�B
5�B
6+B
5�B
5�B
5�B
5�B
6FB
6�B
7B
6�B
6�B
6�B
6�B
6�B
7LB
7LB
7LB
7LB
7�B
7�B
7�B
8B
8B
7�B
7�B
8B
8lB
8lB
8�B
8�B
9�B
9�B
9�B
9�B
:*B
:DB
:�B
:^B
:^B
:^B
:�B
;B
;dB
;�B
;B
<B
<jB
<�B
<jB
<�B
<jB
<�B
<jB
<�B
="B
=�B
>�B
?B
?B
?HB
?cB
?cB
?}B
?�B
@ B
?�B
@B
@ B
@�B
A;B
A�B
A�B
B�B
B�B
C�B
C{B
C�B
C�B
C�B
DB
D�B
D�B
E9B
E�B
E�B
FB
F?B
F�B
F�B
F�B
F�B
GEB
G�B
H1B
H1B
H1B
H�B
IB
I�B
I�B
J	B
J	B
JrB
J�B
J�B
J�B
KDB
LB
LdB
L�B
L�B
M6B
M6B
M�B
N"B
N<B
NVB
N<B
N�B
N�B
OBB
O�B
O�B
O�B
PHB
P�B
QNB
Q�B
Q�B
Q�B
RB
Q�B
R B
RTB
RoB
R�B
S&B
S�B
TaB
TFB
S�B
TB
TB
T{B
T�B
UgB
V9B
VB
VB
VSB
VmB
V�B
V9B
VmB
V�B
W
B
W$B
WYB
XB
XB
W�B
W�B
W�B
X�B
YeB
ZB
ZQB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[qB
[�B
\B
[�B
[�B
[�B
\)B
\]B
\]B
\xB
\�B
]B
]/B
]/B
]/B
]IB
]dB
]�B
]�B
^5B
^jB
^�B
^�B
^�B
_!B
_B
_;B
_�B
_;B
_pB
_�B
`B
`\B
`vB
`�B
aB
a-B
aHB
a|B
a�B
a�B
bNB
bNB
bhB
b�B
b�B
c B
c B
c:B
cTB
cnB
c�B
dB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
e�B
fB
e�B
f2B
f�B
f�B
f�B
f�B
g�B
g�B
h$B
h$B
h$B
h$B
hXB
h>B
h�B
h�B
iB
i�B
jeB
jeB
jB
jB
j�B
kB
kQB
k6B
kkB
kkB
kkB
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
mCB
m�B
m�B
n/B
nIB
ncB
n�B
n�B
o B
oiB
o�B
o�B
o�B
pB
o�B
pUB
poB
p�B
p�B
q'B
qAB
q�B
q�B
q�B
rB
rB
r-B
rB
rB
r-B
rB
rGB
rB
raB
raB
r|B
raB
raB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sB
s3B
sMB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
uB
uZB
utB
u�B
utB
u?B
u�B
u�B
u�B
vB
v+B
vFB
v`B
v�B
v�B
v�B
v�B
w2B
wB
wLB
wfB
w�B
w�B
w�B
x8B
x�B
x�B
x�B
y$B
y>B
y	B
y$B
y$B
yrB
y�B
zB
zB
zDB
zxB
z�B
z�B
z�B
z�B
{dB
{�B
|6B
|PB
|jB
|jB
|�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1<�_<#�
<#�
<#�
<49X<5��<h�<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910310051002019103100510020191031005100202207271133142022072711331420220727113314202207271535462022072715354620220727153546  JA  ARFMdecpA30a                                                                20191020093759  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191020093847  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191020093848  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191020093849  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191020093849  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191020093849  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191020093849                      G�O�G�O�G�O�                JA  ARUP                                                                        20191020095432                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191022000000  CF  PSAL_ADJUSTED_QC@�
@�
G�O�                JM  ARCAJMQC2.0                                                                 20191030155100  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191030155100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023314  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063546  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                